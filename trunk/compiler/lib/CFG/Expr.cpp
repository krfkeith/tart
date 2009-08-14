/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Expr.h"
#include "tart/CFG/Type.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// Utility functions

#ifdef EXPR_TYPE
#undef EXPR_TYPE
#endif

#define EXPR_TYPE(x) #x,

namespace {
const char * ExprTypeNames[] = {
#include "tart/CFG/ExprType.def"
};
}

const char * exprTypeName(Expr::ExprType type) {
  uint32_t index = (uint32_t)type;
  if (index < Expr::TypeCount) {
    return ExprTypeNames[index];
  }
  return "<Invalid Expr Type>";
}

void formatExprList(FormatStream & out, const ExprList & exprs) {
  for (ExprList::const_iterator it = exprs.begin(); it != exprs.end(); ++it) {
    if (it != exprs.begin()) {
      out << ", ";
    }
    
    out << *it;
  }
}

void formatExprTypeList(FormatStream & out, const ExprList & exprs) {
  for (ExprList::const_iterator it = exprs.begin(); it != exprs.end(); ++it) {
    if (it != exprs.begin()) {
      out << ", ";
    }
    
    out << (*it)->type();
  }
}

void formatTypeList(FormatStream & out, const TypeList & types) {
  for (TypeList::const_iterator it = types.begin(); it != types.end(); ++it) {
    if (it != types.begin()) {
      out << ", ";
    }
    
    out << *it;
  }
}

bool isErrorResult(const Type * ex) {
  return BadType::instance.isEqual(ex);
}

/// -------------------------------------------------------------------
/// Expr

ErrorExpr Expr::ErrorVal;

const ExprList Expr::emptyList;

void Expr::format(FormatStream & out) const {
  out << exprTypeName(exprType_);
}

void Expr::trace() const {
  safeMark(type_);
}

/// -------------------------------------------------------------------
/// ErrorExpr
ErrorExpr::ErrorExpr()
  : Expr(Invalid, SourceLocation(), &BadType::instance)
{}

/// -------------------------------------------------------------------
/// UnaryExpr
bool UnaryExpr::isSideEffectFree() const {
  return arg_->isSideEffectFree();
}

bool UnaryExpr::isConstant() const {
  return arg_->isConstant();
}

bool UnaryExpr::isSingular() const {
  return getType()->isSingular() && arg_->isSingular();
}

void UnaryExpr::format(FormatStream & out) const {
  switch (exprType()) {
    case NoOp:
      out << arg_;
      break;
    
    case Not:
      out << "not " << arg_;
      break;
    
    default:
      out << exprTypeName(exprType()) << "(" << arg_ << ")";
      break;
  }
}

void UnaryExpr::trace() const {
  Expr::trace();
  safeMark(arg_);
}

/// -------------------------------------------------------------------
/// BinaryExpr
bool BinaryExpr::isSideEffectFree() const {
  return first_->isSideEffectFree() && second_->isSideEffectFree();
}

bool BinaryExpr::isConstant() const {
  return first_->isConstant() && second_->isConstant();
}

bool BinaryExpr::isSingular() const {
  return getType()->isSingular() && first_->isSingular() && second_->isSingular();
}

void BinaryExpr::format(FormatStream & out) const {
  switch (exprType()) {
    case RefEq:
      out << first_ << " is " << second_;
      break;

    case ElementRef:
      out << first_ << "[" << second_ << "]";
      break;

    default:
      out << exprTypeName(exprType()) << "(" << first_ << ", " << second_ << ")";
      break;
  }
}

void BinaryExpr::trace() const {
  Expr::trace();
  safeMark(first_);
  safeMark(second_);
}

/// -------------------------------------------------------------------
/// ArglistExpr

bool ArglistExpr::areArgsSideEffectFree() const {
  for (ExprList::const_iterator it = args_.begin(); it != args_.end(); ++it) {
    if (!(*it)->isSideEffectFree()) {
      return false;
    }
  }
  
  return true;
}

bool ArglistExpr::isSingular() const {
  for (ExprList::const_iterator it = args_.begin(); it != args_.end(); ++it) {
    if (!(*it)->isSingular()) {
      return false;
    }
  }
  
  return true;
}

void ArglistExpr::trace() const {
  Expr::trace();
  markList(args_.begin(), args_.end());
}

/// -------------------------------------------------------------------
/// LValueExpr
LValueExpr::LValueExpr(const SourceLocation & loc, Expr * base, ValueDefn * value)
  : Expr(LValue, loc, value->getType())
  , base_(base)
  , value_(value)
{
}

bool LValueExpr::isSingular() const {
  return (base_ == NULL || base_->isSingular()) && value_->isSingular();
}

void LValueExpr::format(FormatStream & out) const {
  if (base_ != NULL) {
    out << base_ << "." << value_;
  } else {
    out << value_;
  }
}

void LValueExpr::trace() const {
  Expr::trace();
  safeMark(base_);
  value_->mark();
}

Expr * LValueExpr::constValue(Expr * in) {
  if (LValueExpr * lv = dyn_cast<LValueExpr>(in)) {
    if (lv->value()->defnType() == Defn::Let) {
      if (VariableDefn * var = dyn_cast<VariableDefn>(lv->value())) {
        if (ConstantExpr * cexp = dyn_cast<ConstantExpr>(var->initValue())) {
          return cexp;
        }
      }
    }
  }
  
  return in;
}

/// -------------------------------------------------------------------
/// ScopeNameExpr
bool ScopeNameExpr::isSingular() const { return true; }

void ScopeNameExpr::format(FormatStream & out) const {
  out << value_;
}

void ScopeNameExpr::trace() const {
  Expr::trace();
  safeMark(value_);
}

/// -------------------------------------------------------------------
/// AssignmentExpr

void AssignmentExpr::format(FormatStream & out) const {
  if (exprType() == Expr::PostAssign) {
    out << toExpr() << " (=) " << fromExpr();
  } else {
    out << toExpr() << " = " << fromExpr();
  }
}

/// -------------------------------------------------------------------
/// InitVarExpr

InitVarExpr::InitVarExpr(
    const SourceLocation & loc, VariableDefn * v, Expr * expr) 
  : Expr(InitVar, loc, v->getType())
  , var(v)
  , initExpr(expr)
{}

bool InitVarExpr::isSingular() const {
  return initExpr->isSingular() && var->isSingular();
}

void InitVarExpr::format(FormatStream & out) const {
  out << var << " = " << initExpr;
}

/// -------------------------------------------------------------------
/// CallExpr

bool CallExpr::isSingular() const {
  if (!ArglistExpr::isSingular()) {
    return false;
  }
  
  if (!candidates_.empty()) {
    return candidates_.size() == 1 && candidates_.front()->method()->isSingular();
  }
  
  return function_ != NULL && function_->getType()->isSingular();
}

Type * CallExpr::getSingularParamType(int index) {
  Type * singularType = NULL;
  for (Candidates::iterator it = candidates_.begin(); it != candidates_.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    Type * ty = (*it)->paramType(index);
    if (singularType == NULL) {
      singularType = ty;
    } else if (!ty->isEqual(singularType)) {
      return NULL;
    }
  }
  
  return singularType;
}

Type * CallExpr::getSingularResultType() {
  Type * singularType = NULL;
  for (Candidates::iterator it = candidates_.begin(); it != candidates_.end(); ++it) {
    CallCandidate * cc = *it;
    if (cc->isCulled()) {
      continue;
    }

    Type * ty = cc->resultType();
    if (cc->method()->isCtor()) {
      ty = cc->method()->functionType()->selfParam()->getType();
    }

    if (singularType == NULL) {
      singularType = ty;
    } else if (!ty->isEqual(singularType)) {
      return NULL;
    }
  }
  
  return singularType;
}

CallCandidate * CallExpr::getSingularCandidate() {
  CallCandidate * singularCandidate = NULL;
  for (Candidates::iterator it = candidates_.begin(); it != candidates_.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    if (singularCandidate == NULL) {
      singularCandidate = (*it);
    } else {
      return NULL;
    }
  }
  
  return singularCandidate;
}

bool CallExpr::hasAnyCandidates() const {
  for (Candidates::const_iterator it = candidates_.begin(); it != candidates_.end(); ++it) {
    if (!(*it)->isCulled()) {
      return true;
    }
  }
  
  return false;
}

void CallExpr::format(FormatStream & out) const {
  if (function_ != NULL) {
    if (out.getShowType()) {
      out << "(" << function_ << ")";
    } else {
      out << function_;
    }
  } else if (candidates_.size() == 1) {
    FunctionDefn * func = candidates_.front()->method();
    if (out.getShowType()) {
      out << "(" << func << ")";
    } else {
      out << func;
    }
  } else {
    out << "{" << candidates_.size() << " candidates}";
  }

  out << "(";
  for (ExprList::const_iterator it = args_.begin(); it != args_.end(); ++it) {
    if (it != args_.begin()) {
      out << ", ";
    }
    
    out << (*it);
    if (out.getShowType()) {
      out << ":" << (*it)->getType();
    }
  }
  out << ") ";

  if (out.getShowType() && expectedReturnType) {
    out << "-> " << expectedReturnType << " ";
  }
}

void CallExpr::trace() const {
  ArglistExpr::trace();
  markList(candidates_.begin(), candidates_.end());
}

/// -------------------------------------------------------------------
/// FnCallExpr

void FnCallExpr::format(FormatStream & out) const {
  if (out.getShowType()) {
    out << "(" << function_ << ")";
  } else {
    out << function_;
  }

  out << "(";
  formatExprList(out, args_);
  out << ")";
}

bool FnCallExpr::isSingular() const {
  return (function_->isSingular() && ArglistExpr::isSingular());
}

void FnCallExpr::trace() const {
  ArglistExpr::trace();
  function_->mark();
}

/// -------------------------------------------------------------------
/// NewExpr
bool NewExpr::isSingular() const {
  return getType()->isSingular();
}

void NewExpr::format(FormatStream & out) const {
  out << "new " << getType();
}

/// -------------------------------------------------------------------
/// CastExpr
void CastExpr::format(FormatStream & out) const {
  if (exprType() == ImplicitCast) {
    out << "implicitCast<" << getType() << ">(" << arg() << ")";
  } else {
    out << "cast<" << getType() << ">(" << arg() << ")";
  }
}

/// -------------------------------------------------------------------
/// BinaryOpcodeExpr

bool BinaryOpcodeExpr::isSingular() const {
  return getType()->isSingular() && first()->isSingular() && second()->isSingular();
}

bool BinaryOpcodeExpr::isSideEffectFree() const {
  return first()->isSideEffectFree() && second()->isSideEffectFree();
}

void BinaryOpcodeExpr::format(FormatStream & out) const {
  switch (opCode_) {
    case llvm::Instruction::Add: {
      out << first() << " + " << second();
      break;
    }

    case llvm::Instruction::Sub: {
      out << first() << " - " << second();
      break;
    }

    case llvm::Instruction::Mul: {
      out << first() << " * " << second();
      break;
    }

    case llvm::Instruction::SDiv:
    case llvm::Instruction::UDiv:
    case llvm::Instruction::FDiv: {
      out << first() << " / " << second();
      break;
    }
    
    default: {
      out << "BinaryOpcode(" << first() << ", " << second() << ")";
      break;
    }
  }
}

/// -------------------------------------------------------------------
/// CompareExpr
CompareExpr::CompareExpr(const SourceLocation & loc, Predicate pred)
  : BinaryExpr(Compare, loc, &BoolType::instance)
  , predicate(pred)
{}

/** Constructor. */
CompareExpr::CompareExpr(const SourceLocation & loc, Predicate pred,
    Expr * f, Expr * s)
  : BinaryExpr(Compare, loc, &BoolType::instance, f, s)
  , predicate(pred)
{}

void CompareExpr::format(FormatStream & out) const {
  const char * oper;
  switch (getPredicate()) {
    case llvm::CmpInst::FCMP_OEQ:
    case llvm::CmpInst::FCMP_UEQ:
    case llvm::CmpInst::ICMP_EQ:
      oper = "==";
      break;
  
    case llvm::CmpInst::FCMP_ONE:
    case llvm::CmpInst::FCMP_UNE:
    case llvm::CmpInst::ICMP_NE:
      oper = "!=";
      break;
  
    case llvm::CmpInst::FCMP_OGT:
    case llvm::CmpInst::FCMP_UGT:
    case llvm::CmpInst::ICMP_UGT:
    case llvm::CmpInst::ICMP_SGT:
      oper = ">";
      break;
  
    case llvm::CmpInst::FCMP_OLT:
    case llvm::CmpInst::FCMP_ULT:
    case llvm::CmpInst::ICMP_ULT:
    case llvm::CmpInst::ICMP_SLT:
      oper = "<";
      break;
  
    case llvm::CmpInst::FCMP_OGE:
    case llvm::CmpInst::FCMP_UGE:
    case llvm::CmpInst::ICMP_UGE:
    case llvm::CmpInst::ICMP_SGE:
      oper = ">=";
      break;
  
    case llvm::CmpInst::FCMP_OLE:
    case llvm::CmpInst::FCMP_ULE:
    case llvm::CmpInst::ICMP_ULE:
    case llvm::CmpInst::ICMP_SLE:
      oper = ">=";
      break;
    
    default:
      DFAIL("IllegalState");
  }
  
  out << first() << " " << oper << " " << second();
}

/// -------------------------------------------------------------------
/// IRValueExpr
void IRValueExpr::format(FormatStream & out) const {
  out << "<IRValue>";
}

/// -------------------------------------------------------------------
/// LocalCallExpr
void LocalCallExpr::format(FormatStream & out) const {
  out << "local call " << target_ << " return=" << returnState_;
}

/// -------------------------------------------------------------------
/// InstanceOfExpr
InstanceOfExpr::InstanceOfExpr(const SourceLocation & loc, Expr * value, Type * ty)
  : Expr(InstanceOf, loc, &BoolType::instance)
  , value_(value)
  , toType(ty)
{}
  
void InstanceOfExpr::format(FormatStream & out) const {
  out << value_ << " isa " << toType;
}

void InstanceOfExpr::trace() const {
  Expr::trace();
  safeMark(value_);
  safeMark(toType);
}

bool InstanceOfExpr::isSingular() const {
  return value_->isSingular() && toType->isSingular();
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Expr.h"
#include "tart/CFG/Type.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Closure.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// Utility functions

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
  return ex == NULL || BadType::instance.isEqual(ex);
}

// -------------------------------------------------------------------
// Expr

ErrorExpr Expr::ErrorVal;

const ExprList Expr::emptyList;

const Type * Expr::canonicalType() const {
  return dealias(type());
}

void Expr::format(FormatStream & out) const {
  out << exprTypeName(exprType_);
}

void Expr::trace() const {
  safeMark(type_);
}

// -------------------------------------------------------------------
// ErrorExpr
ErrorExpr::ErrorExpr()
  : Expr(Invalid, SourceLocation(), &BadType::instance)
{}

// -------------------------------------------------------------------
// UnaryExpr
bool UnaryExpr::isSideEffectFree() const {
  return arg_->isSideEffectFree();
}

bool UnaryExpr::isConstant() const {
  return arg_->isConstant();
}

bool UnaryExpr::isSingular() const {
  return type()->isSingular() && arg_->isSingular();
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

// -------------------------------------------------------------------
// BinaryExpr
bool BinaryExpr::isSideEffectFree() const {
  return first_->isSideEffectFree() && second_->isSideEffectFree();
}

bool BinaryExpr::isConstant() const {
  return first_->isConstant() && second_->isConstant();
}

bool BinaryExpr::isSingular() const {
  return type()->isSingular() && first_->isSingular() && second_->isSingular();
}

bool BinaryExpr::isLValue() const {
  if (exprType() == ElementRef) {
    return first_->isLValue();
  }

  return false;
}

void BinaryExpr::format(FormatStream & out) const {
  switch (exprType()) {
    case RefEq:
      out << first_ << " is " << second_;
      break;

    case ElementRef:
      out << first_ << "[" << second_ << "]";
      break;

    case And:
      out << first_ << " and " << second_;
      break;

    case Or:
      out << first_ << " or " << second_;
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

// -------------------------------------------------------------------
// ArglistExpr

bool ArglistExpr::areArgsSideEffectFree() const {
  for (ExprList::const_iterator it = args_.begin(); it != args_.end(); ++it) {
    if (!(*it)->isSideEffectFree()) {
      return false;
    }
  }

  return true;
}

bool ArglistExpr::areArgsConstant() const {
  for (ExprList::const_iterator it = args_.begin(); it != args_.end(); ++it) {
    if (!(*it)->isConstant()) {
      return false;
    }
  }

  return true;
}

void ArglistExpr::appendArg(Expr * en) {
  DASSERT(en != NULL);
  args_.push_back(en);
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

// -------------------------------------------------------------------
// LValueExpr

LValueExpr * LValueExpr::get(ValueDefn * val) {
  return new LValueExpr(val->location(), NULL, val);
}

LValueExpr::LValueExpr(const SourceLocation & loc, Expr * base, ValueDefn * value)
  : Expr(LValue, loc, value->type())
  , base_(base)
  , value_(value)
{
}

bool LValueExpr::isSingular() const {
  return (base_ == NULL || base_->isSingular()) && value_->isSingular();
}

void LValueExpr::format(FormatStream & out) const {
  if (base_ != NULL) {
    int saveOptions = out.formatOptions();
    out.setFormatOptions(saveOptions & ~Format_Type);
    out << base_;
    out.setFormatOptions(saveOptions);
    out << "." << value_->name();
    if (out.getShowType()) {
      out << ":" << value_->type();
    }
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
        if (ConstantExpr * cexp = dyn_cast_or_null<ConstantExpr>(var->initValue())) {
          return cexp;
        }
      }
    }
  }

  return in;
}

bool LValueExpr::isLValue() const {
  if (value_->defnType() == Defn::Let) {
    return false;
  }

  if (base_ == NULL || base_->type()->isReferenceType()) {
    return true;
  }

  return base_->isLValue();
}

// -------------------------------------------------------------------
// ScopeNameExpr
bool ScopeNameExpr::isSingular() const { return true; }

void ScopeNameExpr::format(FormatStream & out) const {
  out << value_;
}

void ScopeNameExpr::trace() const {
  Expr::trace();
  safeMark(value_);
}

// -------------------------------------------------------------------
// AssignmentExpr

AssignmentExpr::AssignmentExpr(const SourceLocation & loc, Expr * to, Expr * from)
  : Expr(Assign, loc, to->type())
  , fromExpr_(from)
  , toExpr_(to)
{
  DASSERT(to != NULL);
  DASSERT(from != NULL);
}

AssignmentExpr::AssignmentExpr(ExprType k, const SourceLocation & loc, Expr * to, Expr * from)
  : Expr(k, loc, to->type())
  , fromExpr_(from)
  , toExpr_(to)
{
  DASSERT(to != NULL);
  DASSERT(from != NULL);
}

void AssignmentExpr::format(FormatStream & out) const {
  if (exprType() == Expr::PostAssign) {
    out << toExpr() << " (=) " << fromExpr();
  } else {
    out << toExpr() << " = " << fromExpr();
  }
}

// -------------------------------------------------------------------
// MultiAssignExpr

MultiAssignExpr::MultiAssignExpr(const SourceLocation & loc, const Type * type)
  : ArglistExpr(MultiAssign, loc, type)
{
}

// -------------------------------------------------------------------
// InitVarExpr

InitVarExpr::InitVarExpr(
    const SourceLocation & loc, VariableDefn * v, Expr * expr)
  : Expr(InitVar, loc, v->type())
  , var(v)
  , initExpr_(expr)
{
  DASSERT_OBJ(expr != NULL, expr);
  DASSERT_OBJ(var->initValue() == NULL, var);
}

bool InitVarExpr::isSingular() const {
  return initExpr_->isSingular() && var->isSingular();
}

void InitVarExpr::format(FormatStream & out) const {
  out << var << " = " << initExpr_;
}

// -------------------------------------------------------------------
// BoundMethodExpr

BoundMethodExpr::BoundMethodExpr(const SourceLocation & loc, Expr * selfArg, FunctionDefn * method,
    const Type * type)
  : Expr(BoundMethod, loc, type)
  , selfArg_(selfArg)
  , method_(method)
{
}

bool BoundMethodExpr::isSingular() const {
  return (selfArg_ == NULL || selfArg_->isSingular()) && method_->isSingular();
}

void BoundMethodExpr::format(FormatStream & out) const {
  if (selfArg_ != NULL) {
    out << selfArg_ << "." << method_->name();
  } else {
    out << method_;
  }
}

void BoundMethodExpr::trace() const {
  Expr::trace();
  safeMark(selfArg_);
  method_->mark();
}

// -------------------------------------------------------------------
// CallExpr

bool CallExpr::isSingular() const {
  if (!ArglistExpr::isSingular()) {
    return false;
  }

  if (!candidates_.empty()) {
    return candidates_.size() == 1 && candidates_.front()->isSingular();
  }

  return function_ != NULL && function_->isSingular();
}

const Type * CallExpr::singularParamType(int index) {
  const Type * singularType = NULL;
  for (Candidates::iterator it = candidates_.begin(); it != candidates_.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type *  ty = (*it)->paramType(index);
    if (singularType == NULL) {
      singularType = ty;
    } else if (!ty->isEqual(singularType)) {
      return NULL;
    }
  }

  return singularType;
}

const Type * CallExpr::singularResultType() {
  const Type * singularType = NULL;
  for (Candidates::iterator it = candidates_.begin(); it != candidates_.end(); ++it) {
    CallCandidate * cc = *it;
    if (cc->isCulled()) {
      continue;
    }

    const Type * ty = cc->resultType();
    if (cc->method() != NULL && exprType() == Expr::Construct && cc->method()->isCtor()) {
      ty = cc->functionType()->selfParam()->type();
    }

    if (singularType == NULL) {
      singularType = ty;
    } else if (!ty->isEqual(singularType)) {
      return NULL;
    }
  }

  return singularType;
}

CallCandidate * CallExpr::singularCandidate() {
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
    //if (out.getShowType()) {
    //  out << "(" << function_ << ")";
    //} else {
      out << function_;
    //}
  } else if (candidates_.size() == 1) {
    FunctionDefn * func = candidates_.front()->method();
    if (func == NULL) {
      out << "(" << candidates_.front()->base() << ")";
    } else if (out.getShowType()) {
      out << "(" << func << ")";
    } else {
      out << func;
    }
  } else if (!candidates_.empty()) {
    FunctionDefn * func = candidates_.front()->method();
    out << func->name();
    //out << "{" << candidates_.size() << " candidates}";
  } else {
    out << "<no candidates>";
  }

  out << "(";
  for (ExprList::const_iterator it = args_.begin(); it != args_.end(); ++it) {
    if (it != args_.begin()) {
      out << ", ";
    }

    out << *it;
    /*if (out.getShowType()) {
      out << ":" << (*it)->type();
    }*/
  }
  out << ") ";

  if (out.getShowType() && expectedReturnType_) {
    out << "-> " << expectedReturnType_ << " ";
  }
}

void CallExpr::trace() const {
  ArglistExpr::trace();
  markList(candidates_.begin(), candidates_.end());
}

// -------------------------------------------------------------------
// SpecializeExpr

  /** Return true if there is at least one non-culled candidate. */
bool SpecializeExpr::hasAnyCandidates() const {
  DFAIL("IMPLEMENT");
}

  // Overridden methods

void SpecializeExpr::format(FormatStream & out) const {
  if (candidates_.size() == 1) {
    SpCandidate * sp = *candidates_.begin();
    out << sp->def()->name();
  } else {
    SpCandidate * sp = *candidates_.begin();
    out << sp->def()->name();
    out << "... {" << candidates_.size() << " candidates}";
  }

  out << "[...";
  /*for (ExprList::const_iterator it = args_.begin(); it != args_.end(); ++it) {
    if (it != args_.begin()) {
      out << ", ";
    }

    out << (*it);
  }*/
  out << "] ";
}

bool SpecializeExpr::isSingular() const {
  return false;
}

void SpecializeExpr::trace() const {
  markList(candidates_.begin(), candidates_.end());
  args_->mark();
}

// -------------------------------------------------------------------
// FnCallExpr

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

// -------------------------------------------------------------------
// IndirectCallExpr

void IndirectCallExpr::format(FormatStream & out) const {
  if (out.getShowType()) {
    out << "(" << function_ << ")";
  } else {
    out << function_;
  }

  out << "(";
  formatExprList(out, args_);
  out << ")";
}

bool IndirectCallExpr::isSingular() const {
  return (function_->isSingular() && ArglistExpr::isSingular());
}

void IndirectCallExpr::trace() const {
  ArglistExpr::trace();
  function_->mark();
}

// -------------------------------------------------------------------
// NewExpr
bool NewExpr::isSingular() const {
  return type()->isSingular();
}

void NewExpr::format(FormatStream & out) const {
  out << "new " << type();
}

// -------------------------------------------------------------------
// CastExpr
CastExpr * CastExpr::bitCast(Expr * value, const Type * toType) {
  return new CastExpr(Expr::BitCast, value->location(), toType, value);
}

CastExpr * CastExpr::upCast(Expr * value, const Type * toType) {
  return new CastExpr(Expr::UpCast, value->location(), toType, value);
}

CastExpr * CastExpr::tryCast(Expr * value, const Type * toType) {
  return new CastExpr(Expr::TryCast, value->location(), toType, value);
}

CastExpr * CastExpr::dynamicCast(Expr * value, const Type * toType) {
  return new CastExpr(Expr::DynamicCast, value->location(), toType, value);
}

void CastExpr::format(FormatStream & out) const {
  out << exprTypeName(exprType()) << "<" << type() << ">(" << arg() << ")";
}

/// -------------------------------------------------------------------
/// ClosureScopeExpr

void ClosureEnvExpr::addMember(Defn * d) {
  DASSERT_OBJ(d->definingScope() == NULL, d);
  SymbolTable::Entry * entry = members_.add(d);
  d->setDefiningScope(this);
}

bool ClosureEnvExpr::lookupMember(const char * name, DefnList & defs, bool inherit) const {
  const SymbolTable::Entry * entry = members_.findSymbol(name);
  if (entry != NULL) {
    defs.append(entry->begin(), entry->end());
    return true;
  }

  if (parentScope_ != NULL) {
    return parentScope_->lookupMember(name, defs, inherit);
  }

  return false;
}

void ClosureEnvExpr::format(FormatStream & out) const {
  out << "<closure>";
}

void ClosureEnvExpr::trace() const {
  safeMark(parentScope_->asLocalScope());
}

void ClosureEnvExpr::dumpHierarchy(bool full) const {
  std::string out;
  out.append("[closure]");
  members_.getDebugSummary(out);
  diag.writeLnIndent(out);
}

// -------------------------------------------------------------------
// BinaryOpcodeExpr

bool BinaryOpcodeExpr::isSingular() const {
  return type()->isSingular() && first()->isSingular() && second()->isSingular();
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

// -------------------------------------------------------------------
// CompareExpr
CompareExpr::CompareExpr(const SourceLocation & loc, Predicate pred)
  : BinaryExpr(Compare, loc, &BoolType::instance)
  , predicate_(pred)
{}

/** Constructor. */
CompareExpr::CompareExpr(const SourceLocation & loc, Predicate pred,
    Expr * f, Expr * s)
  : BinaryExpr(Compare, loc, &BoolType::instance, f, s)
  , predicate_(pred)
{}

void CompareExpr::format(FormatStream & out) const {
  const char * oper;
  switch (predicate_) {
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
      oper = "<=";
      break;

    default:
      DFAIL("IllegalState");
  }

  out << first() << " " << oper << " " << second();
}

// -------------------------------------------------------------------
// IRValueExpr
void IRValueExpr::format(FormatStream & out) const {
  out << "<IRValue>";
}

// -------------------------------------------------------------------
// LocalCallExpr
void LocalCallExpr::format(FormatStream & out) const {
  out << "local call " << target_ << " return=" << returnState_;
}

// -------------------------------------------------------------------
// TupleCtorExpr
void TupleCtorExpr::format(FormatStream & out) const {
  out << "TupleCtor(";
  formatExprList(out, args_);
  out << ")";
}

// -------------------------------------------------------------------
// InstanceOfExpr
InstanceOfExpr::InstanceOfExpr(const SourceLocation & loc, Expr * value, const Type * ty)
  : Expr(InstanceOf, loc, &BoolType::instance)
  , value_(value)
  , toType_(ty)
{}

void InstanceOfExpr::format(FormatStream & out) const {
  out << value_ << " isa " << toType_;
}

void InstanceOfExpr::trace() const {
  Expr::trace();
  safeMark(value_);
  safeMark(toType_);
}

bool InstanceOfExpr::isSingular() const {
  return value_->isSingular() && toType_->isSingular();
}

} // namespace tart

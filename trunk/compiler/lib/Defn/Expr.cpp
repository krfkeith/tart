/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/FunctionDefn.h"

#include "tart/Type/Type.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Expr/Closure.h"
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
#include "tart/Expr/ExprType.def"
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
  return ex == NULL || &BadType::instance == ex;
}

bool any(ExprList::const_iterator first, ExprList::const_iterator last,
    bool (Expr::*func)() const) {
  while (first != last) {
    const Expr * e = *first++;
    if ((e->*func)()) {
      return true;
    }
  }
  return false;
}

bool all(ExprList::const_iterator first, ExprList::const_iterator last,
    bool (Expr::*func)() const) {
  while (first != last) {
    const Expr * e = *first++;
    if (!(e->*func)()) {
      return false;
    }
  }
  return true;
}

// -------------------------------------------------------------------
// Expr

ErrorExpr Expr::ErrorVal;
UnaryExpr Expr::VoidVal(Expr::NoOp, SourceLocation(), &VoidType::instance, NULL);

const ExprList Expr::emptyList;

const QualifiedType  Expr::canonicalType() const {
  return dealias(type());
}

void Expr::format(FormatStream & out) const {
  out << exprTypeName(exprType_);
}

void Expr::trace() const {
  safeMark(type_.unqualified());
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

LValueExpr::LValueExpr(const SourceLocation & loc, Expr * base, ValueDefn * value)
  : Expr(LValue, loc, value->type())
  , base_(base)
  , value_(value)
{
  if (value->storageClass() == Storage_Instance && base == NULL) {
    diag.fatal(loc) << "Instance member with no base: " << value;
  }
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
          if (cexp->exprType() != Expr::ConstObjRef) {
            return cexp;
          }
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

void AssignmentExpr::trace() const {
  Expr::trace();
  safeMark(fromExpr_);
  safeMark(toExpr_);
}

// -------------------------------------------------------------------
// MultiAssignExpr

MultiAssignExpr::MultiAssignExpr(const SourceLocation & loc, QualifiedType type)
  : ArglistExpr(MultiAssign, loc, type)
{
}

// -------------------------------------------------------------------
// InitVarExpr

InitVarExpr::InitVarExpr(
    const SourceLocation & loc, VariableDefn * v, Expr * expr)
  : Expr(InitVar, loc, v->type())
  , var_(v)
  , initExpr_(expr)
{
  DASSERT_OBJ(expr != NULL, expr);
  DASSERT_OBJ(var_->initValue() == NULL, var_);
}

bool InitVarExpr::isSingular() const {
  return initExpr_->isSingular() && var_->isSingular();
}

void InitVarExpr::format(FormatStream & out) const {
  out << var_ << " = " << initExpr_;
}

void InitVarExpr::trace() const {
  Expr::trace();
  var_->mark();
  initExpr_->mark();
}

// -------------------------------------------------------------------
// ClearVarExpr

ClearVarExpr::ClearVarExpr(VariableDefn * v)
  : Expr(ClearVar, v->location(), v->type())
  , var_(v)
{
}

bool ClearVarExpr::isSingular() const {
  return var_->isSingular();
}

void ClearVarExpr::format(FormatStream & out) const {
  out << "clear " << var_;
}

void ClearVarExpr::trace() const {
  Expr::trace();
  var_->mark();
}

// -------------------------------------------------------------------
// BoundMethodExpr

BoundMethodExpr::BoundMethodExpr(const SourceLocation & loc, Expr * selfArg, FunctionDefn * method,
    QualifiedType type)
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

QualifiedType CallExpr::singularParamType(int index) {
  QualifiedType singularType = NULL;
  for (Candidates::iterator it = candidates_.begin(); it != candidates_.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    QualifiedType ty = (*it)->paramType(index);
    if (singularType.isNull()) {
      singularType = ty;
    } else if (!TypeRelation::isEqual(ty, singularType)) {
      return NULL;
    }
  }

  return singularType;
}

QualifiedType CallExpr::singularResultType() {
  QualifiedType singularType = NULL;
  for (Candidates::iterator it = candidates_.begin(); it != candidates_.end(); ++it) {
    CallCandidate * cc = *it;
    if (cc->isCulled()) {
      continue;
    }

    QualifiedType ty = cc->resultType();
    if (singularType.isNull()) {
      singularType = ty;
    } else if (!TypeRelation::isEqual(ty, singularType)) {
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
  out << ")";

  if (out.getShowType() && expectedReturnType_) {
    out << " -> " << expectedReturnType_;
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
  safeMark(selfArg_);
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
CastExpr * CastExpr::bitCast(Expr * value, QualifiedType toType) {
  return new CastExpr(Expr::BitCast, value->location(), toType, value);
}

CastExpr * CastExpr::upCast(Expr * value, QualifiedType toType) {
  return new CastExpr(Expr::UpCast, value->location(), toType, value);
}

CastExpr * CastExpr::tryCast(Expr * value, QualifiedType toType) {
  return new CastExpr(Expr::TryCast, value->location(), toType, value);
}

CastExpr * CastExpr::dynamicCast(Expr * value, QualifiedType toType) {
  return new CastExpr(Expr::DynamicCast, value->location(), toType, value);
}

CastExpr * CastExpr::qualCast(Expr * value, QualifiedType toType) {
  return new CastExpr(Expr::QualCast, value->location(), toType, value);
}

void CastExpr::format(FormatStream & out) const {
  out << exprTypeName(exprType()) << "<" << type() << ">(" << arg() << ")";
}

/// -------------------------------------------------------------------
/// ClosureScopeExpr

void ClosureEnvExpr::addMember(Defn * d) {
  DASSERT_OBJ(d->definingScope() == NULL, d);
  envType_->members().add(d);
  d->setDefiningScope(this);
}

bool ClosureEnvExpr::lookupMember(StringRef name, DefnList & defs, bool inherit) const {
  // If the closure variable has already been seen once, return the previous definition.
  const SymbolTable::Entry * entry = envType_->members().findSymbol(name);
  if (entry != NULL) {
    defs.append(entry->begin(), entry->end());
    return true;
  }

  if (parentScope_ != NULL) {
    DefnList parentDefs;

    // Search all of the scopes that extend from the scope that the closure is defined
    // in to the parameter scope of the enclosing function.
    Scope * foundScope = NULL;
    for (Scope * s = parentScope_; s != NULL; s = s->parentScope()) {
      if (s->lookupMember(name, parentDefs, inherit)) {
        foundScope = s;
        break;
      }

      if (s == finalScope_) {
        break;
      }
    }

    // If the closure variable is accessible from a parent definition, then
    // create a copy of the variable in the current closure environment.
    if (foundScope != NULL) {
      DASSERT(parentDefs.size() > 0);
      Defn * de = parentDefs.front();
      if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
        if (var->storageClass() == Storage_Local || var->isClosureVar()) {
          DASSERT(parentDefs.size() == 1);
          VariableDefn * closureVar = NULL;
          DASSERT(var->module() != NULL);
          switch (var->defnType()) {
            case Defn::Let:
              closureVar = new VariableDefn(Defn::Let,
                  var->module(), var->name(),
                  LValueExpr::get(var->location(), foundScope->baseExpr(), var));
              break;
            case Defn::Var:
            case Defn::Parameter:
              closureVar = new VariableDefn(Defn::Var,
                  var->module(), var->name(),
                  LValueExpr::get(var->location(), foundScope->baseExpr(), var));
              break;

            case Defn::MacroArg:
              // Don't do anything with macro args.
              defs.append(parentDefs.begin(), parentDefs.end());
              return true;

            default:
              DFAIL("Invalid variable type");
          }

          if (closureVar != NULL) {
            closureVar->setType(var->type());
            closureVar->addTrait(Defn::Singular);
            closureVar->setFlag(VariableDefn::ClosureVar, true);
            closureVar->setStorageClass(Storage_Instance);
            closureVar->setQualifiedName(closureVar->name());
            closureVar->setParentDefn(envType_->typeDefn());
            envType_->addMember(closureVar);
            defs.push_back(closureVar);
            return true;
          }
        } else {
          DFAIL("Closure access to non-variables not yet supported.");
        }
      }

      defs.append(parentDefs.begin(), parentDefs.end());
      return true;
    }
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
  StrFormatStream out;
  out << "[closure]";
  envType_->members().getDebugSummary(out);
  diag.writeLnIndent(out.str());
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

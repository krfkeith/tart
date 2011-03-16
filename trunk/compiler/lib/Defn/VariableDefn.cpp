/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/VariableDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Expr/Exprs.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// VariableDefn

VariableDefn::VariableDefn(DefnType dtype, Module * m, const char * name, Expr * value)
  : ValueDefn(dtype, m, name)
  , type_(value ? value->type() : NULL)
  , sharedRefType_(NULL)
  , flags_(dtype == Defn::Let ? Constant : 0)
  , initValue_(value)
  , irValue_(NULL)
  , memberIndex_(0)
  , memberIndexRecursive_(0)
{}

VariableDefn::VariableDefn(DefnType dtype, Module * m, const ASTDecl * de)
  : ValueDefn(dtype, m, de)
  , type_(NULL)
  , sharedRefType_(NULL)
  , flags_(dtype == Defn::Let ? Constant : 0)
  , initValue_(NULL)
  , irValue_(NULL)
  , memberIndex_(0)
  , memberIndexRecursive_(0)
{}

void VariableDefn::trace() const {
  ValueDefn::trace();
  safeMark(type_);
  safeMark(initValue_);
}

bool VariableDefn::hasStorage() const {
  if (defnType() == Defn::Parameter) {
    return false;
  } else if (defnType() != Defn::Let) {
    return true;
  }

  // Local and parameter let-variables are not considered to have storage because
  // their values are normally held in SSA variables. However, reference types need
  // memory locations so that they can be traced.
  if (storageClass() == Storage_Local) {
    return type()->containsReferenceType();
    //return false;
  }

  // A 'let' expression needs storage if the initializer expression is not a constant,
  // or if it's a object reference.
  DASSERT_OBJ(passes().isFinished(VariableDefn::VariableTypePass), this);
  if (ConstantExpr * ce = dyn_cast_or_null<ConstantExpr>(initValue_)) {
    switch (ce->exprType()) {
      case Expr::ConstObjRef:
      case Expr::ConstNArray:
      case Expr::ConstEmptyArray:
        return true;

      default:
        return false;
    }
  }

  return true;
}

void VariableDefn::setIsAssignedTo() {
  if (!isAssignedTo()) {
    setFlag(AssignedTo, true);
    VariableDefn * outer = closureBinding();
    if (outer != NULL) {
      outer->setIsAssignedTo();
    }
  }
}

VariableDefn * VariableDefn::closureBinding() const {
  return isClosureVar() ?
    cast<VariableDefn>(cast<LValueExpr>(initValue_)->value()) :
    NULL;
}


void VariableDefn::format(FormatStream & out) const {
  /*if (out.isVerbose()) {
    switch (defnType()) {
      case Defn::Let: out << "let "; break;
      case Defn::Var: out << "var "; break;
      default:
        break;
    }
  }*/

  if (out.getShowQualifiedName() && storageClass() != Storage_Local) {
    if (out.getShowType() && enclosingClassDefn()) {
      out << enclosingClassDefn() << "." << name_;
    } else {
      out << qname_;
    }
  } else {
    out << name_;
  }

  if (out.getShowType() && type_ != NULL) {
    out << ":" << type_;
  }

  if ((out.getShowInitializer() || defnType() == MacroArg) && initValue_) {
    out << "=" << initValue_;
  }
}


} // namespace tart

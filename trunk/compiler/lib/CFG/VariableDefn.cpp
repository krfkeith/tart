/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/VariableDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// VariableDefn
void VariableDefn::trace() const {
  ValueDefn::trace();
  type_.trace();
  safeMark(initValue_);
}

bool VariableDefn::hasStorage() const {
  if (defnType() != Defn::Let) {
    return true;
  }

  // Local and parameter let-variables are not considered to have storage because
  // their values are normally held in SSA variables.
  if (storageClass() == Storage_Local || storageClass() == Storage_Param) {
    return false;
  }

  // A 'let' expression needs storage if the initializer expression is not a constant,
  // or if it's a object reference.
  DASSERT_OBJ(passes().isFinished(VariableDefn::VariableTypePass), this);
  if (ConstantExpr * ce = dyn_cast_or_null<ConstantExpr>(initValue_)) {
    if (ce->exprType() != Expr::ConstObjRef && ce->exprType() != Expr::ConstNArray) {
      return false;
    }
  }

  return true;
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

  if (out.getShowType() && type_.isDefined()) {
    out << ":" << type_;
  }

  if (out.getShowInitializer() && initValue_) {
    out << "=" << initValue_;
  }
}


} // namespace tart

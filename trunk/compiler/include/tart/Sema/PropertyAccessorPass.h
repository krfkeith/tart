/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_PROPERTYACCESSORPASS_H
#define TART_SEMA_PROPERTYACCESSORPASS_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

namespace tart {

class PropertyDefn;

/// -------------------------------------------------------------------
/// Function pass expands all property accesses to function calls.
class PropertyAccessorPass : public CFGPass {
private:
  Module * module_;
  Defn * subject_;

  PropertyAccessorPass(Module * module, Defn * subject) : module_(module), subject_(subject) {}
  Expr * runImpl(Expr * in);

  Expr * visitLValue(LValueExpr * in);
//  Expr * visitCall(CallExpr * in);
  Expr * visitAssign(AssignmentExpr * in);
  Expr * visitPostAssign(AssignmentExpr * in);

  Expr * getPropertyValue(SLC & loc, Expr * basePtr, PropertyDefn * prop);
  Expr * setPropertyValue(SLC & loc, Expr * basePtr, PropertyDefn * prop, Expr * value);
//  Expr * getParamPropertyValue(SLC & loc, CallExpr * call);
//  Expr * setParamPropertyValue(SLC & loc, CallExpr * call, Expr * value);
public:

  /** Run this pass on the specified expression. */
  static Expr * run(Module * module, Defn * subject, Expr * e) {
    PropertyAccessorPass instance(module, subject);
    return instance.runImpl(e);
  }
};

} // namespace tart

#endif

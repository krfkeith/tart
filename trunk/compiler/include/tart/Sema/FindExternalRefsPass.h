/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_FINDEXTERNALREFSPASS_H
#define TART_SEMA_FINDEXTERNALREFSPASS_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

namespace tart {

class Module;

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class FindExternalRefsPass : public CFGPass {
  Module * module;

  FindExternalRefsPass(Module * m) : module(m) {}
  Defn * runImpl(Defn * in);
  void addSymbol(Defn * de);
  bool addFunction(FunctionDefn * de);

public:

  /** Run this pass on the specified expression. */
  static Defn * run(Module * m, Defn * in);

  Expr * visitLValue(LValueExpr * in);
  Expr * visitFnCall(FnCallExpr * in);
  Expr * visitNew(NewExpr * in);
  Expr * visitArrayLiteral(ArrayLiteralExpr * in);
  Expr * visitInstanceOf(InstanceOfExpr * in);
};

} // namespace tart

#endif

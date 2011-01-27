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
class SystemClass;

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class FindExternalRefsPass : public CFGPass {
public:

  /** Run this pass on the specified expression. */
  static Defn * run(Module * m, Defn * in);

  void visit(FunctionDefn * in);
  Expr * visitLValue(LValueExpr * in);
  Expr * visitBoundMethod(BoundMethodExpr * in);
  Expr * visitFnCall(FnCallExpr * in);
  Expr * visitNew(NewExpr * in);
  Expr * visitArrayLiteral(ArrayLiteralExpr * in);
  Expr * visitTypeLiteral(TypeLiteralExpr * in);
  Expr * visitInstanceOf(InstanceOfExpr * in);
  Expr * visitConstantObjectRef(ConstantObjectRef * in);
  Expr * visitCast(CastExpr * in);
  Expr * visitClosureScope(ClosureEnvExpr * in);

private:
  Module * module_;
  FunctionDefn * function_;

  FindExternalRefsPass(Module * m)
    : module_(m)
    , function_(NULL)
  {}

  Defn * runImpl(Defn * in);
  void addSymbol(Defn * de);
  bool addFunction(FunctionDefn * de);
  bool addTypeRef(const Type * type);
};

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_SEMA_FINALIZETYPESPASS_H
#define TART_SEMA_FINALIZETYPESPASS_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

namespace tart {
  
class UnionType;
  
/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class FinalizeTypesPass : public CFGPass {
public:

  /** Run this pass on the specified expression. */
  static Expr * run(Expr * in);
  
  Expr * addCastIfNeeded(Expr * in, Type * toType);

  Expr * visitLValue(LValueExpr * in);
  Expr * visitScopeName(ScopeNameExpr * in);
  Expr * visitElementRef(BinaryExpr * in);

  Expr * visitAssign(AssignmentExpr * in);
  Expr * visitPostAssign(AssignmentExpr * in);
  Expr * visitCall(CallExpr * in);
  Expr * visitInstantiate(InstantiateExpr * in);
  Expr * visitCast(CastExpr * in);
  Expr * visitConstantObjectRefOf(InstanceOfExpr * in);
  Expr * visitRefEq(BinaryExpr * in);
  //Expr * visitNot(UnaryExpr * in);

private:
  FinalizeTypesPass() {}
  Expr * runImpl(Expr * in);
  Expr * visitUnionTest(InstanceOfExpr * in, Expr * value, UnionType * from, Type * to);
};

} // namespace tart

#endif

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
  //static Expr * run(Expr * in);
  static Expr * run(Defn * source, Expr * in);

  Expr * addCastIfNeeded(Expr * in, const Type * toType);

  Expr * visitLValue(LValueExpr * in);
  Expr * visitBoundMethod(BoundMethodExpr * in);
  Expr * visitScopeName(ScopeNameExpr * in);
  Expr * visitElementRef(BinaryExpr * in);

  Expr * visitAssign(AssignmentExpr * in);
  Expr * visitPostAssign(AssignmentExpr * in);
  Expr * visitCall(CallExpr * in);
  Expr * visitInstantiate(InstantiateExpr * in);
  Expr * visitCast(CastExpr * in);
  Expr * visitInstanceOf(InstanceOfExpr * in);
  Expr * visitRefEq(BinaryExpr * in);

private:
  Defn * subject_;

  FinalizeTypesPass(Defn * subject) : subject_(subject) {}
  Expr * runImpl(Expr * in);
  Expr * visitUnionTest(InstanceOfExpr * in, Expr * value, const UnionType * from, const Type * to);
  Expr * visitAssignImpl(AssignmentExpr * in);
  bool coerceArgs(CallCandidate * cd, const ExprList & args, ExprList & coercedArgs);
  Expr * visitIndirectCall(CallExpr * in);
  Defn * doPatternSubstitutions(SLC & loc, Defn * def, BindingEnv & env);
  Expr * handleUnboxCast(CastExpr * in);
};

} // namespace tart

#endif

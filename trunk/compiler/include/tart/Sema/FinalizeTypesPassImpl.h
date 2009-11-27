/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_FINALIZETYPESPASSIMPL_H
#define TART_SEMA_FINALIZETYPESPASSIMPL_H

#ifndef TART_SEMA_FINALIZETYPESPASS_H
#include "tart/Sema/FinalizeTypesPass.h"
#endif

namespace tart {

class UnionType;

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class FinalizeTypesPassImpl : public FinalizeTypesPass {
public:

  FinalizeTypesPassImpl(Defn * subject) : subject_(subject) {}

  /** Run this pass on the specified expression. */
  //static Expr * run(Expr * in);
  static Expr * run(Defn * source, Expr * in);

private:
  Defn * subject_;

  Expr * visitUnionTest(InstanceOfExpr * in, Expr * value, const UnionType * from, const Type * to);
  Expr * visitAssignImpl(AssignmentExpr * in);
  Expr * visitIndirectCall(CallExpr * in);
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

  bool coerceArgs(CallCandidate * cd, const ExprList & args, ExprList & coercedArgs);
  Defn * doPatternSubstitutions(SLC & loc, Defn * def, BindingEnv & env);

  Expr * addCastIfNeeded(Expr * in, const Type * toType);
  Expr * handleUnboxCast(CastExpr * in);
};

} // namespace tart

#endif

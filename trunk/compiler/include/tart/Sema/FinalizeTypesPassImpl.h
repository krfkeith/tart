/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_FINALIZETYPESPASSIMPL_H
#define TART_SEMA_FINALIZETYPESPASSIMPL_H

#ifndef TART_SEMA_FINALIZETYPESPASS_H
#include "tart/Sema/FinalizeTypesPass.h"
#endif

#ifndef TART_SEMA_EXPRANALYZER_H
#include "tart/Sema/ExprAnalyzer.h"
#endif

namespace tart {

class UnionType;
class AmbiguousPhiType;

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class FinalizeTypesPassImpl : public FinalizeTypesPass {
public:

  FinalizeTypesPassImpl(Defn * subject, BindingEnv & env)
    : subject_(subject), env_(env) {}

  /** Run this pass on the specified expression. */
  static Expr * run(Defn * source, Expr * in, BindingEnv & env);

private:
  Defn * subject_;
  BindingEnv & env_;

  Expr * visitConstantInteger(ConstantInteger * in);
  Expr * visitUnionTest(InstanceOfExpr * in, Expr * value, const UnionType * from, const Type * to);
  Expr * visitAssignImpl(AssignmentExpr * in);
  Expr * visitCallExpr(CallExpr * in);
  Expr * visitLValue(LValueExpr * in);
  Expr * visitBoundMethod(BoundMethodExpr * in);
  Expr * visitScopeName(ScopeNameExpr * in);
  Expr * visitElementRef(BinaryExpr * in);
  Expr * visitAssign(AssignmentExpr * in);
  Expr * visitPostAssign(AssignmentExpr * in);
  Expr * visitCall(CallExpr * in);
  Expr * visitCast(CastExpr * in);
  Expr * visitInstanceOf(InstanceOfExpr * in);
  Expr * visitRefEq(BinaryExpr * in);
  Expr * visitTupleCtor(TupleCtorExpr * in);
  Expr * visitTypeLiteral(TypeLiteralExpr * in);
  Expr * visitIf(IfExpr * in);
  Expr * visitSwitch(SwitchExpr * in);
  Expr * visitMatch(MatchExpr * in);
  //Expr * visitInitVar(InitVarExpr * in);

  bool coerceArgs(CallCandidate * cd, const ExprList & args, ExprList & coercedArgs);
  Defn * doPatternSubstitutions(SLC & loc, Defn * def, const TypeVarMap & varValues);

  Expr * addCastIfNeeded(Expr * in, const Type * toType, unsigned options = AO_IMPLICIT_CAST);
  Expr * handleUnboxCast(CastExpr * in);
  const Type * getCommonPhiType(const AmbiguousPhiType * phi);
};

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_CFGPASS_H
#define TART_SEMA_CFGPASS_H

#ifndef TART_EXPR_CONSTANT_H
#include "tart/Expr/Constant.h"
#endif

namespace tart {

class ClosureEnvExpr;
class LValueExpr;
class BoundMethodExpr;
class ScopeNameExpr;
class AssignmentExpr;
class MultiAssignExpr;
class CallExpr;
class FnCallExpr;
class IndirectCallExpr;
class NewExpr;
class CastExpr;
class BinaryOpcodeExpr;
class CompareExpr;
class InstanceOfExpr;
class InitVarExpr;
class ClearVarExpr;
class ArrayLiteralExpr;
class TupleCtorExpr;
class ClosureEnvExpr;
class SharedValueExpr;
class SeqExpr;
class IfExpr;
class WhileExpr;
class ForExpr;
class ForEachExpr;
class SwitchExpr;
class CaseExpr;
class MatchExpr;
class MatchAsExpr;
class TryExpr;
class CatchExpr;
class ThrowExpr;
class ReturnExpr;
class BranchExpr;
class LocalProcedureExpr;

/// -------------------------------------------------------------------
/// Mixin class that handles iteration over the CFG expression tree and
/// allows for arbitrary replacements.
class CFGPass {
public:
  virtual ~CFGPass() {}

  virtual void visit(FunctionDefn * in);
  virtual Expr * visitStmtExpr(Expr * in) { return visitExpr(in); }
  virtual Expr * visitTermExpr(Expr * in) { return visitExpr(in); }

  virtual Expr * visitExpr(Expr * in);
  virtual Expr * visitConstantInteger(ConstantInteger * in) { return in; }
  virtual Expr * visitConstantFloat(ConstantFloat * in) { return in; }
  virtual Expr * visitConstantString(ConstantString * in) { return in; }
  virtual Expr * visitConstantNull(ConstantNull * in) { return in; }
  virtual Expr * visitConstantObjectRef(ConstantObjectRef * in);
  virtual Expr * visitConstantNativeArray(ConstantNativeArray * in);
  virtual Expr * visitConstantEmptyArray(ConstantEmptyArray * in);
  virtual Expr * visitTypeLiteral(TypeLiteralExpr * in) { return in; }

  virtual Expr * visitLValue(LValueExpr * in);
  virtual Expr * visitBoundMethod(BoundMethodExpr * in);
  virtual Expr * visitScopeName(ScopeNameExpr * in);
  virtual Expr * visitElementRef(BinaryExpr * in);

  virtual Expr * visitAssign(AssignmentExpr * in);
  virtual Expr * visitPostAssign(AssignmentExpr * in);
  virtual Expr * visitMultiAssign(MultiAssignExpr * in);
  virtual Expr * visitCall(CallExpr * in);
  virtual Expr * visitFnCall(FnCallExpr * in);
  virtual Expr * visitIndirectCall(IndirectCallExpr * in);
  virtual Expr * visitNew(NewExpr * in);
  virtual Expr * visitCast(CastExpr * in);
  virtual Expr * visitBinaryOpcode(BinaryOpcodeExpr * in);
  virtual Expr * visitCompare(CompareExpr * in);
  virtual Expr * visitInstanceOf(InstanceOfExpr * in);
  virtual Expr * visitRefEq(BinaryExpr * in);
  virtual Expr * visitPtrDeref(UnaryExpr * in);
  virtual Expr * visitNot(UnaryExpr * in);
  virtual Expr * visitComplement(UnaryExpr * in);
  virtual Expr * visitLogicalOper(BinaryExpr * in);
  virtual Expr * visitInitVar(InitVarExpr * in);
  virtual Expr * visitClearVar(ClearVarExpr * in);
  virtual Expr * visitProg2(BinaryExpr * in);
  virtual Expr * visitArrayLiteral(ArrayLiteralExpr * in);
  virtual Expr * visitTupleCtor(TupleCtorExpr * in);
  virtual Expr * visitClosureScope(ClosureEnvExpr * in);
  virtual Expr * visitSharedValue(SharedValueExpr * in);

  virtual Expr * visitSeq(SeqExpr * in);
  virtual Expr * visitIf(IfExpr * in);
  virtual Expr * visitWhile(WhileExpr * in);
  virtual Expr * visitDoWhile(WhileExpr * in);
  virtual Expr * visitFor(ForExpr * in);
  virtual Expr * visitForEach(ForEachExpr * in);
  virtual Expr * visitSwitch(SwitchExpr * in);
  virtual Expr * visitCase(CaseExpr * in);
  virtual Expr * visitMatch(MatchExpr * in);
  virtual Expr * visitMatchAs(MatchAsExpr * in);
  virtual Expr * visitThrow(ThrowExpr * in);
  virtual Expr * visitReturn(ReturnExpr * in);
  virtual Expr * visitYield(ReturnExpr * in);
  virtual Expr * visitTry(TryExpr * in);
  virtual Expr * visitCatch(CatchExpr * in);
  virtual Expr * visitBreak(BranchExpr * in);
  virtual Expr * visitContinue(BranchExpr * in);
  virtual Expr * visitLocalProcedure(LocalProcedureExpr * in);
  virtual Expr * visitLocalReturn(BranchExpr * in);

protected:
  void visitExprArgs(ArglistExpr * in);
  void visitExprList(ExprList & args);
};

} // namespace tart

#endif

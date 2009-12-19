/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_CFGPASS_H
#define TART_SEMA_CFGPASS_H

#ifndef TART_CFG_CONSTANT_H
#include "tart/CFG/Constant.h"
#endif

namespace tart {

class ClosureEnvExpr;

/// -------------------------------------------------------------------
/// Mixin class that handles iteration over the CFG expression tree and
/// allows for arbitrary replacements.
class CFGPass {
public:
  virtual ~CFGPass() {}

  virtual void visit(FunctionDefn * in);
  virtual void visitBlock(Block * in);
  virtual Expr * visitStmtExpr(Expr * in) { return visitExpr(in); }
  virtual Expr * visitTermExpr(Expr * in) { return visitExpr(in); }

  virtual Expr * visitExpr(Expr * in);
  virtual Expr * visitConstantInteger(ConstantInteger * in) { return in; }
  virtual Expr * visitConstantFloat(ConstantFloat * in) { return in; }
  virtual Expr * visitConstantString(ConstantString * in) { return in; }
  virtual Expr * visitConstantNull(ConstantNull * in) { return in; }
  virtual Expr * visitConstantObjectRef(ConstantObjectRef * in);
  virtual Expr * visitConstantNativeArray(ConstantNativeArray * in);
  virtual Expr * visitTypeLiteral(TypeLiteralExpr * in) { return in; }

  virtual Expr * visitLValue(LValueExpr * in);
  virtual Expr * visitBoundMethod(BoundMethodExpr * in);
  virtual Expr * visitScopeName(ScopeNameExpr * in);
  virtual Expr * visitElementRef(BinaryExpr * in);

  virtual Expr * visitAssign(AssignmentExpr * in);
  virtual Expr * visitPostAssign(AssignmentExpr * in);
  virtual Expr * visitCall(CallExpr * in);
  virtual Expr * visitFnCall(FnCallExpr * in);
  virtual Expr * visitIndirectCall(CallExpr * in);
  virtual Expr * visitNew(NewExpr * in);
  virtual Expr * visitCast(CastExpr * in);
  virtual Expr * visitBinaryOpcode(BinaryOpcodeExpr * in);
  virtual Expr * visitCompare(CompareExpr * in);
  virtual Expr * visitInstanceOf(InstanceOfExpr * in);
  virtual Expr * visitRefEq(BinaryExpr * in);
  virtual Expr * visitPtrDeref(UnaryExpr * in);
  virtual Expr * visitNot(UnaryExpr * in);
  virtual Expr * visitLogicalOper(BinaryExpr * in);
  virtual Expr * visitInitVar(InitVarExpr * in);
  virtual Expr * visitProg2(BinaryExpr * in);
  virtual Expr * visitArrayLiteral(ArrayLiteralExpr * in);
  virtual Expr * visitClosureScope(ClosureEnvExpr * in);

protected:
  void visitExprArgs(ArglistExpr * in);
};

} // namespace tart

#endif

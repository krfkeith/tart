/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_FUNCTIONMERGEPASS_H
#define TART_SEMA_FUNCTIONMERGEPASS_H

#ifndef TART_CFG_CONSTANT_H
#include "tart/CFG/Constant.h"
#endif

namespace tart {

//class ClosureEnvExpr;
class LValueExpr;
//class BoundMethodExpr;
//class ScopeNameExpr;
class AssignmentExpr;
//class MultiAssignExpr;
//class CallExpr;
class FnCallExpr;
class NewExpr;
class CastExpr;
class BinaryOpcodeExpr;
class CompareExpr;
//class InstanceOfExpr;
class InitVarExpr;
//class ClearVarExpr;
//class ArrayLiteralExpr;
//class TupleCtorExpr;
//class ClosureEnvExpr;
//class SharedValueExpr;

/// -------------------------------------------------------------------
/// Analysis pass that determines whether or not two functions can
/// be merged into a single definition.
class FunctionMergePass {
public:
  bool visit(FunctionDefn * from, FunctionDefn * to);

  FunctionMergePass() : showMessages_(false) {}

private:
  bool visitBlock(Block * from, Block * to);
//  bool visitStmtExpr(Expr * from, Expr * to) { return visitExpr(in); }
//  bool visitTermExpr(Expr * from, Expr * to) { return visitExpr(in); }

  bool visitExpr(Expr * from, Expr * to);
//  bool visitConstantNull(ConstantNull * from, ConstantNull * to) { return in; }
//  bool visitConstantObjectRef(ConstantObjectRef * from, ConstantObjectRef * to);
//  bool visitConstantNativeArray(ConstantNativeArray * from, ConstantNativeArray * to);
//  Expr * visitTypeLiteral(TypeLiteralExpr * from, TypeLiteralExpr * to) { return in; }

  bool visitLValue(LValueExpr * from, LValueExpr * to);
//  bool visitBoundMethod(BoundMethodExpr * from, BoundMethodExpr * to);
//  bool visitScopeName(ScopeNameExpr * from, ScopeNameExpr * to);
  bool visitElementRef(BinaryExpr * from, BinaryExpr * to);

  bool visitAssign(AssignmentExpr * from, AssignmentExpr * to);
//  bool visitMultiAssign(MultiAssignExpr * from, MultiAssignExpr * to);
//  bool visitCall(CallExpr * from, CallExpr * to);
  bool visitFnCall(FnCallExpr * from, FnCallExpr * to);
//  bool visitIndirectCall(CallExpr * from, CallExpr * to);
  bool visitNew(NewExpr * from, NewExpr * to);
  bool visitCast(CastExpr * from, CastExpr * to);
  bool visitUnionCtorCast(CastExpr * from, CastExpr * to);
  bool visitBinaryOpcode(BinaryOpcodeExpr * from, BinaryOpcodeExpr * to);
  bool visitCompare(CompareExpr * from, CompareExpr * to);
//  bool visitInstanceOf(InstanceOfExpr * from, InstanceOfExpr * to);
//  bool visitRefEq(BinaryExpr * from, BinaryExpr * to);
//  bool visitPtrDeref(UnaryExpr * from, UnaryExpr * to);
  bool visitNot(UnaryExpr * from, UnaryExpr * to);
  bool visitComplement(UnaryExpr * from, UnaryExpr * to);
  bool visitLogicalOper(BinaryExpr * from, BinaryExpr * to);
  bool visitInitVar(InitVarExpr * from, InitVarExpr * to);
//  bool visitClearVar(ClearVarExpr * from, ClearVarExpr * to);
//  bool visitProg2(BinaryExpr * from, BinaryExpr * to);
//  bool visitArrayLiteral(ArrayLiteralExpr * from, ArrayLiteralExpr * to);
//  bool visitTupleCtor(TupleCtorExpr * from, TupleCtorExpr * to);
//  bool visitClosureScope(ClosureEnvExpr * from, ClosureEnvExpr * to);
//  bool visitSharedValue(SharedValueExpr * from, SharedValueExpr * to);

  bool visitExprArgs(ArglistExpr * from, ArglistExpr * to);

  bool visitFunctionRef(FunctionDefn * from, FunctionDefn * to);

  bool areTypesCompatible(const Type * from, const Type * to);

  void reportDifference(const char * msg, Formattable * from, Formattable * to);
  void reportDifference(Formattable * from, Formattable * to);

  bool showMessages_;
};

} // namespace tart

#endif

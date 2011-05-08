/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/CFGPass.h"

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Expr/Closure.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// CFGPass
void CFGPass::visit(FunctionDefn * in) {
  in->setBody(visitExpr(in->body()));
}

Expr * CFGPass::visitExpr(Expr * in) {
  if (in == NULL) {
    return NULL;
  }

  switch (in->exprType()) {
    case Expr::Invalid:
      return &Expr::ErrorVal;

    case Expr::TypeCount:
      DFAIL("Invalid");

    case Expr::NoOp:
      return in;

    case Expr::ConstInt:
      return visitConstantInteger(static_cast<ConstantInteger *>(in));

    case Expr::ConstFloat:
      return visitConstantFloat(static_cast<ConstantFloat *>(in));

    case Expr::ConstString:
      return visitConstantString(static_cast<ConstantString *>(in));

    case Expr::TypeLiteral:
      return visitTypeLiteral(static_cast<TypeLiteralExpr *>(in));

    case Expr::ConstNull:
      return visitConstantNull(static_cast<ConstantNull *>(in));

    case Expr::ConstObjRef:
      return visitConstantObjectRef(static_cast<ConstantObjectRef *>(in));

    case Expr::ConstNArray:
      return visitConstantNativeArray(static_cast<ConstantNativeArray *>(in));

    case Expr::ConstEmptyArray:
      return visitConstantEmptyArray(static_cast<ConstantEmptyArray *>(in));

    case Expr::LValue:
      return visitLValue(static_cast<LValueExpr *>(in));

    case Expr::BoundMethod:
      return visitBoundMethod(static_cast<BoundMethodExpr *>(in));

    case Expr::ScopeName:
      return visitScopeName(static_cast<ScopeNameExpr *>(in));

    case Expr::ElementRef:
      return visitElementRef(static_cast<BinaryExpr *>(in));

    case Expr::Assign:
      return visitAssign(static_cast<AssignmentExpr *>(in));

    case Expr::PostAssign:
      return visitPostAssign(static_cast<AssignmentExpr *>(in));

    case Expr::MultiAssign:
      return visitMultiAssign(static_cast<MultiAssignExpr *>(in));

    case Expr::Call:
    case Expr::ExactCall:
    //case Expr::ICall:
    case Expr::Construct:
      return visitCall(static_cast<CallExpr *>(in));

    case Expr::FnCall:
    case Expr::CtorCall:
    case Expr::VTableCall:
      return visitFnCall(static_cast<FnCallExpr *>(in));

    case Expr::IndirectCall:
      return visitIndirectCall(static_cast<IndirectCallExpr *>(in));

    case Expr::New:
      return visitNew(static_cast<NewExpr *>(in));

    case Expr::ImplicitCast:
    case Expr::ExplicitCast:
    case Expr::UpCast:
    case Expr::TryCast:
    case Expr::DynamicCast:
    case Expr::UnboxCast:
    case Expr::Truncate:
    case Expr::SignExtend:
    case Expr::ZeroExtend:
    case Expr::IntToFloat:
    case Expr::BitCast:
    case Expr::UnionCtorCast:
    case Expr::UnionMemberCast:
    case Expr::CheckedUnionMemberCast:
      return visitCast(static_cast<CastExpr *>(in));

    case Expr::BinaryOpcode:
      return visitBinaryOpcode(static_cast<BinaryOpcodeExpr *>(in));

    case Expr::Compare:
      return visitCompare(static_cast<CompareExpr *>(in));

    case Expr::InstanceOf:
      return visitInstanceOf(static_cast<InstanceOfExpr *>(in));

    case Expr::RefEq:
      return visitRefEq(static_cast<BinaryExpr *>(in));

    case Expr::PtrDeref:
      return visitPtrDeref(static_cast<UnaryExpr *>(in));

    case Expr::Not:
      return visitNot(static_cast<UnaryExpr *>(in));

    case Expr::And:
    case Expr::Or:
      return visitLogicalOper(static_cast<BinaryExpr *>(in));

    case Expr::Complement:
      return visitComplement(static_cast<UnaryExpr *>(in));

    case Expr::InitVar:
      return visitInitVar(static_cast<InitVarExpr *>(in));

    case Expr::ClearVar:
      return visitClearVar(static_cast<ClearVarExpr *>(in));

    case Expr::Prog2:
      return visitProg2(static_cast<BinaryExpr *>(in));

    case Expr::ArrayLiteral:
      return visitArrayLiteral(static_cast<ArrayLiteralExpr *>(in));

    case Expr::TupleCtor:
      return visitTupleCtor(static_cast<TupleCtorExpr *>(in));

    case Expr::ClosureEnv:
      return visitClosureScope(static_cast<ClosureEnvExpr *>(in));

    case Expr::IRValue:
      return in;

    case Expr::SharedValue:
      return visitSharedValue(static_cast<SharedValueExpr *>(in));

    case Expr::Seq:
      return visitSeq(static_cast<SeqExpr *>(in));

    case Expr::If:
      return visitIf(static_cast<IfExpr *>(in));

    case Expr::While:
      return visitWhile(static_cast<WhileExpr *>(in));

    case Expr::DoWhile:
      return visitDoWhile(static_cast<WhileExpr *>(in));

    case Expr::For:
      return visitFor(static_cast<ForExpr *>(in));

    case Expr::ForEach:
      return visitForEach(static_cast<ForEachExpr *>(in));

    case Expr::Switch:
      return visitSwitch(static_cast<SwitchExpr *>(in));

    case Expr::Match:
      return visitMatch(static_cast<MatchExpr *>(in));

    case Expr::MatchAs:
      return visitMatchAs(static_cast<MatchAsExpr *>(in));

    case Expr::Throw:
      return visitThrow(static_cast<ThrowExpr *>(in));

    case Expr::Return:
      return visitReturn(static_cast<ReturnExpr *>(in));

    case Expr::Yield:
      return visitYield(static_cast<ReturnExpr *>(in));

    case Expr::Try:
      return visitTry(static_cast<TryExpr *>(in));

    case Expr::Catch:
      return visitCatch(static_cast<CatchExpr *>(in));

    case Expr::Break:
      return visitBreak(static_cast<BranchExpr *>(in));

    case Expr::Continue:
      return visitContinue(static_cast<BranchExpr *>(in));

    case Expr::LocalReturn:
      return visitLocalReturn(static_cast<BranchExpr *>(in));

    case Expr::LocalProcedure:
      return visitLocalProcedure(static_cast<LocalProcedureExpr *>(in));

    case Expr::Case:
      return visitCase(static_cast<CaseExpr *>(in));

    case Expr::With:
      DFAIL("Implement");

    case Expr::PatternVar:
    case Expr::TypeName:
    case Expr::Specialize:
      diag.error(in) << "Invalid expression type: " << exprTypeName(in->exprType());
      break;
  }

  DFAIL("Fall through");
}

Expr * CFGPass::visitConstantObjectRef(ConstantObjectRef * in) {
  visitExprList(in->members());
  return in;
}

Expr * CFGPass::visitConstantNativeArray(ConstantNativeArray * in) {
  visitExprList(in->elements());
  return in;
}

Expr * CFGPass::visitConstantEmptyArray(ConstantEmptyArray * in) {
  return in;
}

Expr * CFGPass::visitLValue(LValueExpr * in) {
  in->setBase(visitExpr(in->base()));
  return in;
}

Expr * CFGPass::visitBoundMethod(BoundMethodExpr * in) {
  in->setSelfArg(visitExpr(in->selfArg()));
  return in;
}

Expr * CFGPass::visitScopeName(ScopeNameExpr * in) {
  return in;
}

Expr * CFGPass::visitElementRef(BinaryExpr * in) {
  in->setFirst(visitExpr(in->first()));
  in->setSecond(visitExpr(in->second()));
  return in;
}

Expr * CFGPass::visitAssign(AssignmentExpr * in) {
  in->setFromExpr(visitExpr(in->fromExpr()));
  in->setToExpr(visitExpr(in->toExpr()));
  return in;
}

Expr * CFGPass::visitPostAssign(AssignmentExpr * in) {
  in->setFromExpr(visitExpr(in->fromExpr()));
  in->setToExpr(visitExpr(in->toExpr()));
  return in;
}

Expr * CFGPass::visitMultiAssign(MultiAssignExpr * in) {
  visitExprArgs(in);
  return in;
}

Expr * CFGPass::visitCall(CallExpr * in) {
  //in->setFunction(visitExpr(in->function()));
  visitExprArgs(in);
  return in;
}

Expr * CFGPass::visitFnCall(FnCallExpr * in) {
  //visit(in->function());
  in->setSelfArg(visitExpr(in->selfArg()));
  visitExprArgs(in);
  return in;
}

Expr * CFGPass::visitIndirectCall(IndirectCallExpr * in) {
  visitExpr(in->function());
  //in->setSelfArg(visitExpr(in->selfArg()));
  visitExprArgs(in);
  return in;
}

Expr * CFGPass::visitNew(NewExpr * in) {
  return in;
}

Expr * CFGPass::visitCast(CastExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitBinaryOpcode(BinaryOpcodeExpr * in) {
  in->setFirst(visitExpr(in->first()));
  in->setSecond(visitExpr(in->second()));
  return in;
}

Expr * CFGPass::visitCompare(CompareExpr * in) {
  in->setFirst(visitExpr(in->first()));
  in->setSecond(visitExpr(in->second()));
  return in;
}

Expr * CFGPass::visitInstanceOf(InstanceOfExpr * in) {
  in->setValue(visitExpr(in->value()));
  return in;
}

Expr * CFGPass::visitRefEq(BinaryExpr * in) {
  in->setFirst(visitExpr(in->first()));
  in->setSecond(visitExpr(in->second()));
  return in;
}

Expr * CFGPass::visitPtrDeref(UnaryExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitLogicalOper(BinaryExpr * in) {
  in->setFirst(visitExpr(in->first()));
  in->setSecond(visitExpr(in->second()));
  return in;
}

Expr * CFGPass::visitNot(UnaryExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitComplement(UnaryExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitInitVar(InitVarExpr * in) {
  Expr * e = visitExpr(in->initExpr());
  DASSERT(e != NULL);
  in->setInitExpr(e);
  return in;
}

Expr * CFGPass::visitClearVar(ClearVarExpr * in) {
  return in;
}

Expr * CFGPass::visitProg2(BinaryExpr * in) {
  in->setFirst(visitExpr(in->first()));
  in->setSecond(visitExpr(in->second()));
  return in;
}

Expr * CFGPass::visitArrayLiteral(ArrayLiteralExpr * in) {
  visitExprArgs(in);
  return in;
}

Expr * CFGPass::visitTupleCtor(TupleCtorExpr * in) {
  visitExprArgs(in);
  return in;
}

Expr * CFGPass::visitSharedValue(SharedValueExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitSeq(SeqExpr * in) {
  visitExprArgs(in);
  if (!in->args().empty()) {
    in->setType(in->args().back()->type());
  }
  return in;
}

Expr * CFGPass::visitIf(IfExpr * in) {
  in->setTest(visitExpr(in->test()));
  in->setThenVal(visitExpr(in->thenVal()));
  in->setElseVal(visitExpr(in->elseVal()));
  return in;
}

Expr * CFGPass::visitWhile(WhileExpr * in) {
  in->setTest(visitExpr(in->test()));
  in->setBody(visitExpr(in->body()));
  return in;
}

Expr * CFGPass::visitDoWhile(WhileExpr * in) {
  in->setTest(visitExpr(in->test()));
  in->setBody(visitExpr(in->body()));
  return in;
}

Expr * CFGPass::visitFor(ForExpr * in) {
  in->setInit(visitExpr(in->init()));
  in->setTest(visitExpr(in->test()));
  in->setIncr(visitExpr(in->incr()));
  in->setBody(visitExpr(in->body()));
  return in;
}

Expr * CFGPass::visitForEach(ForEachExpr * in) {
  in->setIterator(visitExpr(in->iterator()));
  in->setNext(visitExpr(in->next()));
  in->setTest(visitExpr(in->test()));
  visitExprList(in->assigns());
  in->setBody(visitExpr(in->body()));
  return in;
}

Expr * CFGPass::visitSwitch(SwitchExpr * in) {
  in->setValue(visitExpr(in->value()));
  visitExprArgs(in);
  in->setElseCase(visitExpr(in->elseCase()));
  return in;
}

Expr * CFGPass::visitCase(CaseExpr * in) {
  visitExprArgs(in);
  in->setBody(visitExpr(in->body()));
  return in;
}

Expr * CFGPass::visitMatch(MatchExpr * in) {
  visitExprArgs(in);
  in->setValue(visitExpr(in->value()));
  in->setElseCase(visitExpr(in->elseCase()));
  return in;
}

Expr * CFGPass::visitMatchAs(MatchAsExpr * in) {
  in->setTest(visitExpr(in->test()));
  in->setInit(visitExpr(in->init()));
  in->setBody(visitExpr(in->body()));
  return in;
}

Expr * CFGPass::visitTry(TryExpr * in) {
  visitExprArgs(in);
  in->setBody(visitExpr(in->body()));
  in->setFinallyBlock(visitExpr(in->finallyBlock()));
  return in;
}

Expr * CFGPass::visitCatch(CatchExpr * in) {
  in->setBody(visitExpr(in->body()));
  return in;
}

Expr * CFGPass::visitThrow(ThrowExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitReturn(ReturnExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitYield(ReturnExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitBreak(BranchExpr * in) {
  return in;
}

Expr * CFGPass::visitContinue(BranchExpr * in) {
  return in;
}

Expr * CFGPass::visitLocalProcedure(LocalProcedureExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitLocalReturn(BranchExpr * in) {
  return in;
}

void CFGPass::visitExprArgs(ArglistExpr * in) {
  visitExprList(in->args());
}

Expr * CFGPass::visitClosureScope(ClosureEnvExpr * in) {
  return in;
}

void CFGPass::visitExprList(ExprList & args) {
  for (ExprList::iterator it = args.begin(); it != args.end(); ++it) {
    *it = visitExpr(*it);
  }
}

} // namespace tart

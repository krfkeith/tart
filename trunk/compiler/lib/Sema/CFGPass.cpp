/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/CFGPass.h"

#include "tart/CFG/Exprs.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/Closure.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// CFGPass
void CFGPass::visit(FunctionDefn * in) {
  BlockList & blocks = in->blocks();
  for (BlockList::iterator it = blocks.begin(); it != blocks.end(); ++it) {
    visitBlock(*it);
  }
}

void CFGPass::visitBlock(Block * in) {
  ExprList & types = in->exprs();
  for (ExprList::iterator it = types.begin(); it != types.end(); ++it) {
    *it = visitStmtExpr(*it);
  }

  ExprList & ttypes = in->termExprs();
  for (ExprList::iterator it = ttypes.begin(); it != ttypes.end(); ++it) {
    *it = visitTermExpr(*it);
  }
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

    case Expr::PatternVar:
      DFAIL("PatternVar");

    default:
      break;
  }

  diag.error(in) << "Expr type not handled: " << exprTypeName(in->exprType());
  DFAIL("Fall through");
}

Expr * CFGPass::visitConstantObjectRef(ConstantObjectRef * in) {
  ExprList & members = in->members();
  for (ExprList::iterator it = members.begin(); it != members.end(); ++it) {
    *it = visitExpr(*it);
  }

  return in;
}

Expr * CFGPass::visitConstantNativeArray(ConstantNativeArray * in) {
  ExprList & elements = in->elements();
  for (ExprList::iterator it = elements.begin(); it != elements.end(); ++it) {
    *it = visitExpr(*it);
  }

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

void CFGPass::visitExprArgs(ArglistExpr * in) {
  ExprList & args = in->args();
  for (ExprList::iterator it = args.begin(); it != args.end(); ++it) {
    *it = visitExpr(*it);
  }
}

Expr * CFGPass::visitClosureScope(ClosureEnvExpr * in) {
  return in;
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/CFGPass.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/Block.h"
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

    case Expr::ConstType:
      return visitConstantType(static_cast<ConstantType *>(in));

    case Expr::ConstNull:
      return visitConstantNull(static_cast<ConstantNull *>(in));

    case Expr::ConstObjRef:
      return visitConstantObjectRef(static_cast<ConstantObjectRef *>(in));

    case Expr::LValue:
      return visitLValue(static_cast<LValueExpr *>(in));

    case Expr::ScopeName:
      return visitScopeName(static_cast<ScopeNameExpr *>(in));

    case Expr::ElementRef:
      return visitElementRef(static_cast<BinaryExpr *>(in));

    case Expr::Assign:
      return visitAssign(static_cast<AssignmentExpr *>(in));

    case Expr::PostAssign:
      return visitPostAssign(static_cast<AssignmentExpr *>(in));

    case Expr::Call:
    case Expr::ExactCall:
    //case Expr::ICall:
    case Expr::Construct:
      return visitCall(static_cast<CallExpr *>(in));

    case Expr::FnCall:
    case Expr::CtorCall:
    case Expr::VTableCall:
      return visitFnCall(static_cast<FnCallExpr *>(in));

    case Expr::New:
      return visitNew(static_cast<NewExpr *>(in));

    case Expr::Instantiate:
      return visitInstantiate(static_cast<InstantiateExpr *>(in));

    case Expr::ImplicitCast:
    case Expr::ExplicitCast:
    case Expr::UpCast:
    case Expr::TryCast:
    case Expr::DynamicCast:
    case Expr::Truncate:
    case Expr::SignExtend:
    case Expr::ZeroExtend:
    case Expr::BitCast:
    case Expr::UnionCtorCast:
    case Expr::UnionMemberCast:
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

    case Expr::InitVar:
      return visitInitVar(static_cast<InitVarExpr *>(in));

    case Expr::Prog2:
      return visitProg2(static_cast<BinaryExpr *>(in));

    case Expr::ArrayLiteral:
      return visitArrayLiteral(static_cast<ArrayLiteralExpr *>(in));

    case Expr::IRValue:
      return in;

    case Expr::PatternVar:
      DFAIL("PatternVar");

    case Expr::PtrCall:
      DFAIL("PtrCall");
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

Expr * CFGPass::visitLValue(LValueExpr * in) {
  in->setBase(visitExpr(in->base()));
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

Expr * CFGPass::visitCall(CallExpr * in) {
  visitExprArgs(in);
  return in;
}

Expr * CFGPass::visitFnCall(FnCallExpr * in) {
  visit(in->function());
  in->setSelfArg(visitExpr(in->selfArg()));
  visitExprArgs(in);
  return in;
}

Expr * CFGPass::visitNew(NewExpr * in) {
  return in;
}

Expr * CFGPass::visitInstantiate(InstantiateExpr * in) {
  in->setBase(visitExpr(in->base()));
  ExprList & args = in->args();
  for (ExprList::iterator it = args.begin(); it != args.end(); ++it) {
    *it = visitExpr(*it);
  }
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

Expr * CFGPass::visitNot(UnaryExpr * in) {
  in->setArg(visitExpr(in->arg()));
  return in;
}

Expr * CFGPass::visitInitVar(InitVarExpr * in) {
  in->setInitExpr(visitExpr(in->getInitExpr()));
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

void CFGPass::visitExprArgs(ArglistExpr * in) {
  ExprList & args = in->args();
  for (ExprList::iterator it = args.begin(); it != args.end(); ++it) {
    *it = visitExpr(*it);
  }
}

} // namespace tart

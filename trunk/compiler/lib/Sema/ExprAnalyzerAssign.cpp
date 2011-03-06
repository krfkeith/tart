/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Exprs.h"
#include "tart/CFG/PropertyDefn.h"
#include "tart/CFG/TupleType.h"

#include "tart/Sema/ExprAnalyzer.h"

namespace tart {

Expr * ExprAnalyzer::reduceAssign(const ASTOper * ast) {
  if (ast->arg(0)->nodeType() == ASTNode::Tuple) {
    return reduceMultipleAssign(ast);
  }

  Expr * lhs = reduceValueRef(ast->arg(0), true);
  if (isErrorResult(lhs)) {
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(lhs->type() != NULL, lhs);
  Expr * rhs = reduceExpr(ast->arg(1), lhs->type());
  if (isErrorResult(rhs)) {
    return &Expr::ErrorVal;
  }

  return reduceStoreValue(astLoc(ast), lhs, rhs);
}

Expr * ExprAnalyzer::reducePostAssign(const ASTOper * ast) {
  // PostAssign modifies the value, but returns the value before it was modified.
  Expr * lvalue = reduceValueRef(ast->arg(0), true);
  if (isErrorResult(lvalue)) {
    return &Expr::ErrorVal;
  }

  Expr * newValue = reduceExpr(ast->arg(1), lvalue->type());

  if (CallExpr * call = dyn_cast<CallExpr>(lvalue)) {
    if (LValueExpr * lval = dyn_cast<LValueExpr>(call->function())) {
      if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
        Expr * setProp = reduceSetParamPropertyValue(astLoc(ast), call, newValue);
        if (isErrorResult(setProp)) {
          return &Expr::ErrorVal;
        }

        DFAIL("Implement PostAssign of indexed property");
      }
    }
  }

  return new AssignmentExpr(Expr::PostAssign, astLoc(ast), lvalue, newValue);
}

Expr * ExprAnalyzer::reduceMultipleAssign(const ASTOper * ast) {
  const ASTOper * lhs = static_cast<const ASTOper *>(ast->arg(0));
  ConstTypeList varTypes;
  ExprList dstVars;
  for (ASTNodeList::const_iterator it = lhs->args().begin(); it != lhs->args().end(); ++it) {
    Expr * dstLVal = reduceValueRef(*it, true);
    if (isErrorResult(dstLVal)) {
      return &Expr::ErrorVal;
    }

    DASSERT_OBJ(dstLVal->type() != NULL, dstLVal);
    dstVars.push_back(dstLVal);
    varTypes.push_back(dstLVal->type());
  }

  TupleType * tt = TupleType::get(varTypes.begin(), varTypes.end());
  Expr * rhs = inferTypes(subject_, reduceExpr(ast->arg(1), tt), tt, true);
  if (isErrorResult(rhs)) {
    return &Expr::ErrorVal;
  }

  // Create a multi-assign node.
  MultiAssignExpr * ma = new MultiAssignExpr(astLoc(ast), rhs->type());
  if (TupleCtorExpr * tce = dyn_cast<TupleCtorExpr>(rhs)) {
    for (size_t i = 0; i < dstVars.size(); ++i) {
      ma->appendArg(reduceStoreValue(astLoc(ast), dstVars[i], tce->arg(i)));
    }
  } else {
    rhs = SharedValueExpr::get(rhs);
    for (size_t i = 0; i < dstVars.size(); ++i) {
      Expr * srcVal = new BinaryExpr(
          Expr::ElementRef, rhs->location(), tt->member(i),
          rhs, ConstantInteger::getUInt32(i));

      ma->appendArg(reduceStoreValue(astLoc(ast), dstVars[i], srcVal));
    }
  }

  return ma;
}

Expr * ExprAnalyzer::reduceAugmentedAssign(const ASTOper * ast) {
  const char * assignOperName;
  ASTIdent * infixOperIdent;
  switch (int(ast->nodeType())) {
    case ASTNode::AssignAdd:
    assignOperName = "assignAdd";
    infixOperIdent = &ASTIdent::operatorAdd;
    break;

    case ASTNode::AssignSub:
    assignOperName = "assignSubtract";
    infixOperIdent = &ASTIdent::operatorSub;
    break;

    case ASTNode::AssignMul:
    assignOperName = "assignMultiply";
    infixOperIdent = &ASTIdent::operatorMul;
    break;

    case ASTNode::AssignDiv:
    assignOperName = "assignDivide";
    infixOperIdent = &ASTIdent::operatorDiv;
    break;

    case ASTNode::AssignMod:
    assignOperName = "assignModulus";
    infixOperIdent = &ASTIdent::operatorMod;
    break;

    case ASTNode::AssignBitAnd:
    assignOperName = "assignBitAnd";
    infixOperIdent = &ASTIdent::operatorBitAnd;
    break;

    case ASTNode::AssignBitOr:
    assignOperName = "assignBitOr";
    infixOperIdent = &ASTIdent::operatorBitOr;
    break;

    case ASTNode::AssignBitXor:
    assignOperName = "assignBitXor";
    infixOperIdent = &ASTIdent::operatorBitXor;
    break;

    case ASTNode::AssignRSh:
    assignOperName = "assignRShift";
    infixOperIdent = &ASTIdent::operatorRSh;
    break;

    case ASTNode::AssignLSh:
    assignOperName = "assignLShift";
    infixOperIdent = &ASTIdent::operatorLSh;
    break;
  }

  // Attempt to call augmented assignment operator as member function
  ASTMemberRef augMethodRef(astLoc(ast), const_cast<ASTNode *>(ast->arg(0)), assignOperName);
  ASTNodeList args;
  args.push_back(const_cast<ASTNode *>(ast->arg(1)));
  Expr * callExpr = callName(astLoc(ast), &augMethodRef, args, NULL, true);
  if (callExpr != NULL) {
    return callExpr;
  }

  // Otherwise call the regular infix operator and assign to the same location.
  Expr * callResult = callName(astLoc(ast), infixOperIdent, ast->args(), NULL, false);
  if (isErrorResult(callResult)) {
    return callResult;
  }

  Expr * lhs = reduceValueRef(ast->arg(0), true);
  if (isErrorResult(lhs)) {
    return &Expr::ErrorVal;
  }

  return reduceStoreValue(astLoc(ast), lhs, callResult);
}

}

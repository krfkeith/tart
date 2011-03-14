/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/Stmt.h"

#include "tart/Expr/Exprs.h"
#include "tart/Defn/Module.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Type/UnionType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/FinalizeTypesPass.h"

namespace tart {

/// -------------------------------------------------------------------
/// ExprAnalyzer

Expr * ExprAnalyzer::inferTypes(Expr * expr, const Type * expectedType) {
  expr = inferTypes(subject_, expr, expectedType);
  if (isErrorResult(expr)) {
    return expr;
  }

  DASSERT_OBJ(expr->isSingular(), expr);
  return expr;
}

Expr * ExprAnalyzer::inferTypes(Defn * subject, Expr * expr, const Type * expected,
    bool tryCoerciveCasts) {
  if (isErrorResult(expr)) {
    return NULL;
  }

  // If it's a reference to a type, then just return it even if it's non-
  // singular.
  if (expr->exprType() == Expr::TypeLiteral) {
    return static_cast<TypeLiteralExpr *> (expr);
  }

  if (expr && !expr->isSingular()) {
    expr = TypeInferencePass::run(subject->module(), expr, expected);
  }

  expr = FinalizeTypesPass::run(subject, expr, tryCoerciveCasts);
  if (!expr->isSingular()) {
    diag.fatal(expr) << "Non-singular expression: " << expr;
    return NULL;
  }

  return expr;
}

Expr * ExprAnalyzer::reduceExpr(const ASTNode * ast, const Type * expected) {
  Expr * result = reduceExprImpl(ast, expected);
  if (result != NULL) {
    DASSERT(result->exprType() < Expr::TypeCount);
    if (result->type() == NULL) {
      diag.fatal() << "Expression '" << result << "' has no type.";
      DFAIL("MissingType");
    }
  }

  return result;
}

Expr * ExprAnalyzer::reduceExprImpl(const ASTNode * ast, const Type * expected) {
  switch (ast->nodeType()) {
    case ASTNode::Null:
      return reduceNull(ast);

    case ASTNode::LitInt:
      return reduceIntegerLiteral(static_cast<const ASTIntegerLiteral *> (ast));

    case ASTNode::LitFloat:
      return reduceFloatLiteral(static_cast<const ASTFloatLiteral *> (ast));

    case ASTNode::LitDouble:
      return reduceDoubleLiteral(static_cast<const ASTDoubleLiteral *> (ast));

    case ASTNode::LitString:
      return reduceStringLiteral(static_cast<const ASTStringLiteral *> (ast));

    case ASTNode::LitChar:
      return reduceCharLiteral(static_cast<const ASTCharLiteral *> (ast));

    case ASTNode::LitBool:
      return reduceBoolLiteral(static_cast<const ASTBoolLiteral *> (ast));

    case ASTNode::BuiltIn:
      return reduceBuiltInDefn(static_cast<const ASTBuiltIn *> (ast));

    case ASTNode::Id:
    case ASTNode::Member:
    case ASTNode::GetElement:
    case ASTNode::Specialize:
      // Specialize works here because lookupName() does explicit specialization
      // for us.
      return reduceLoadValue(ast);

    case ASTNode::Call:
      return reduceCall(static_cast<const ASTCall *> (ast), expected);

    case ASTNode::Assign:
      return reduceAssign(static_cast<const ASTOper *> (ast));

    case ASTNode::AssignAdd:
    case ASTNode::AssignSub:
    case ASTNode::AssignMul:
    case ASTNode::AssignDiv:
    case ASTNode::AssignMod:
    case ASTNode::AssignBitAnd:
    case ASTNode::AssignBitOr:
    case ASTNode::AssignBitXor:
    case ASTNode::AssignRSh:
    case ASTNode::AssignLSh:
      return reduceAugmentedAssign(static_cast<const ASTOper *> (ast));

    case ASTNode::PostAssign:
      return reducePostAssign(static_cast<const ASTOper *> (ast));

    case ASTNode::Is:
    case ASTNode::IsNot:
      return reduceRefEqualityTest(static_cast<const ASTOper *> (ast));

    case ASTNode::In:
    case ASTNode::NotIn:
      return reduceContainsTest(static_cast<const ASTOper *> (ast));

    case ASTNode::IsInstanceOf:
      return reduceTypeTest(static_cast<const ASTOper *> (ast));

    case ASTNode::LogicalAnd:
    case ASTNode::LogicalOr:
      return reduceLogicalOper(static_cast<const ASTOper *> (ast));

    case ASTNode::LogicalNot:
      return reduceLogicalNot(static_cast<const ASTOper *> (ast));

    case ASTNode::Complement:
      return reduceComplement(static_cast<const ASTOper *> (ast));

    case ASTNode::ArrayLiteral:
      return reduceArrayLiteral(static_cast<const ASTOper *> (ast), expected);

    case ASTNode::Tuple:
      return reduceTuple(static_cast<const ASTOper *> (ast), expected);

    case ASTNode::AnonFn:
      return reduceAnonFn(static_cast<const ASTFunctionDecl *> (ast), expected);

    case ASTNode::Block:
      return reduceBlockStmt(static_cast<const BlockStmt *>(ast), expected);

    case ASTNode::If:
      return reduceIfStmt(static_cast<const IfStmt *>(ast), expected);

    case ASTNode::While:
      return reduceWhileStmt(static_cast<const WhileStmt *>(ast), expected);

    case ASTNode::DoWhile:
      return reduceDoWhileStmt(static_cast<const DoWhileStmt *>(ast), expected);

    case ASTNode::For:
      return reduceForStmt(static_cast<const ForStmt *>(ast), expected);

    case ASTNode::ForEach:
      return reduceForEachStmt(static_cast<const ForEachStmt *>(ast), expected);

    case ASTNode::Switch:
      return reduceSwitchStmt(static_cast<const SwitchStmt *>(ast), expected);

    case ASTNode::Match:
      return reduceMatchStmt(static_cast<const MatchStmt *>(ast), expected);

    case ASTNode::Throw:
      return reduceThrowStmt(static_cast<const ThrowStmt *>(ast), expected);

    case ASTNode::Return:
      return reduceReturnStmt(static_cast<const ReturnStmt *>(ast), expected);

    case ASTNode::Yield:
      return reduceYieldStmt(static_cast<const ReturnStmt *>(ast), expected);

    case ASTNode::Try:
      return reduceTryStmt(static_cast<const TryStmt *>(ast), expected);

    case ASTNode::Break:
      return reduceBreakStmt(static_cast<const Stmt *>(ast), expected);

    case ASTNode::Continue:
      return reduceContinueStmt(static_cast<const Stmt *>(ast), expected);

    case ASTNode::Expression:
      return reduceExpr(static_cast<const ExprStmt *>(ast)->value(), expected);

    case ASTNode::TypeVar:
      diag.error(ast) << "Pattern variable used outside of pattern.";
      return &Expr::ErrorVal;

    default:
      diag.fatal(ast) << "Unimplemented expression type: '" << nodeTypeName(ast->nodeType()) << "'";
      DFAIL("Unimplemented");
  }
}

Expr * ExprAnalyzer::reduceAttribute(const ASTNode * ast) {
  switch (ast->nodeType()) {
    case ASTNode::Id:
    case ASTNode::Member:
    return callName(ast->location(), ast, ASTNodeList(), NULL);

    case ASTNode::Call: {
      const ASTCall * call = static_cast<const ASTCall *> (ast);
      const ASTNode * callable = call->func();
      const ASTNodeList & args = call->args();
      if (callable->nodeType() == ASTNode::Id || callable->nodeType() == ASTNode::Member) {
        return callName(call->location(), callable, args, NULL);
      }

      diag.fatal(call) << "Invalid attribute expression " << call;
      return &Expr::ErrorVal;
    }

    default:
    return reduceExpr(ast, NULL);
  }
}

Expr * ExprAnalyzer::reduceConstantExpr(const ASTNode * ast, Type * expected) {
  Expr * expr = analyze(ast, expected);
  if (isErrorResult(expr)) {
    return NULL;
  }

  Expr * letConst = LValueExpr::constValue(expr);
  if (letConst != NULL) {
    expr = letConst;
  }

  if (!expr->isConstant()) {
    diag.error(expr) << "Non-constant expression: " << expr << " [" << exprTypeName(
        expr->exprType()) << "]";
    return NULL;
  }

  return expr;
}

Expr * ExprAnalyzer::reduceTemplateArgExpr(const ASTNode * ast, bool doInference) {
  Expr * expr = reduceExpr(ast, NULL);
  if (doInference) {
    expr = inferTypes(subject(), expr, NULL);
  }

  if (isErrorResult(expr)) {
    return NULL;
  }

  Expr * letConst = LValueExpr::constValue(expr);
  if (letConst != NULL) {
    expr = letConst;
  }

  if (!expr->isConstant()) {
    diag.error(expr) << "Non-constant expression: " << expr << " [" << exprTypeName(
        expr->exprType()) << "]";
    return NULL;
  }

  return expr;
}

Expr * ExprAnalyzer::reduceRefEqualityTest(const ASTOper * ast) {
  bool invert = (ast->nodeType() == ASTNode::IsNot);
  Expr * first = reduceExpr(ast->arg(0), NULL);
  Expr * second = reduceExpr(ast->arg(1), NULL);

  if (first != NULL && second != NULL) {
    DASSERT_OBJ(first->type() != NULL, first);
    DASSERT_OBJ(second->type() != NULL, second);

    Expr * result = new BinaryExpr(Expr::RefEq, ast->location(),
        &BoolType::instance, first, second);
    if (invert) {
      result = new UnaryExpr(Expr::Not, ast->location(), &BoolType::instance, result);
    }

    return result;
  }

  return NULL;
}

Expr * ExprAnalyzer::reduceContainsTest(const ASTOper * ast) {
  DASSERT(ast->count() == 2);
  bool invert = (ast->nodeType() == ASTNode::NotIn);

  // Otherwise call the regular infix operator and assign to the same location.
  Expr * base = reduceExpr(ast->arg(0), NULL);
  if (isErrorResult(base)) {
    return base;
  }


  ASTNodeList args;
  args.push_back(const_cast<ASTNode *>(ast->arg(0)));
  ASTNode * methodAst = new ASTMemberRef(
      ast->location(), const_cast<ASTNode *>(ast->arg(1)), "contains");
  Expr * callResult = callName(ast->location(), methodAst, args, &BoolType::instance, false);

  if (isErrorResult(callResult)) {
    return callResult;
  }

  if (invert) {
    callResult = new UnaryExpr(Expr::Not, ast->location(), &BoolType::instance, callResult);
  }

  return callResult;
}

Expr * ExprAnalyzer::reduceTypeTest(const ASTOper * ast) {
  Expr * value = reduceExpr(ast->arg(0), NULL);
  TypeAnalyzer ta(module(), activeScope());
  Type * type = ta.typeFromAST(ast->arg(1));
  if (type == NULL) {
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(value->type() != NULL, value);
  DASSERT_OBJ(value->isSingular(), value);

  if (value->type()->isEqual(type)) {
    return ConstantInteger::getConstantBool(ast->location(), true);
  }

  if (CompositeType * ctd = dyn_cast<CompositeType>(type)) {
    DASSERT_OBJ(value->type() != NULL, value);
    if (const CompositeType * valueClass = static_cast<const CompositeType *>(value->type())) {
      if (valueClass->isSubtype(ctd)) {
        return ConstantInteger::getConstantBool(ast->location(), true);
      }
    }

    return new InstanceOfExpr(ast->location(), value, ctd);
  }

  // See if the value is a union.
  if (const UnionType * ut = dyn_cast<UnionType>(value->type())) {
    return new InstanceOfExpr(ast->location(), value, type);
  }

  diag.debug(ast) << "Unsupported isa test to type " << type;
  DFAIL("IllegalState");
}

Expr * ExprAnalyzer::reduceLogicalOper(const ASTOper * ast) {
  ASTNode::NodeType op = ast->nodeType();
  ASTIdent * opIdent = (op == ASTNode::LogicalAnd ?
      &ASTIdent::operatorLogicalAnd : &ASTIdent::operatorLogicalOr);

  ASTNodeList args;
  args.push_back(const_cast<ASTNode *>(ast->arg(0)));
  args.push_back(const_cast<ASTNode *>(ast->arg(1)));
  return callName(ast->location(), opIdent, args, NULL, true);
}

Expr * ExprAnalyzer::reduceLogicalNot(const ASTOper * ast) {
  Expr * value = reduceExpr(ast->arg(0), &BoolType::instance);

  if (isErrorResult(value)) {
    return value;
  }

  if (ConstantExpr * cval = dyn_cast<ConstantExpr>(value)) {
    if (ConstantInteger * cint = dyn_cast<ConstantInteger>(cval)) {
      return ConstantInteger::getConstantBool(ast->location(), cint->value() == 0);
    } else if (ConstantNull * cnull = dyn_cast<ConstantNull>(cval)) {
      return ConstantInteger::getConstantBool(ast->location(), true);
    }

    diag.error(ast) << "Invalid argument for logical 'not' expression";
    return &Expr::ErrorVal;
  }

  value = BoolType::instance.implicitCast(ast->location(), value);
  if (isErrorResult(value)) {
    return value;
  }

  return new UnaryExpr(
      Expr::Not, ast->location(), &BoolType::instance, value);
}

Expr * ExprAnalyzer::reduceComplement(const ASTOper * ast) {
  Expr * value = reduceExpr(ast->arg(0), NULL);

  if (isErrorResult(value)) {
    return value;
  }

  if (ConstantExpr * cval = dyn_cast<ConstantExpr>(value)) {
    if (ConstantInteger * cint = dyn_cast<ConstantInteger>(cval)) {
      return ConstantInteger::get(ast->location(), cint->type(),
          cast<llvm::ConstantInt>(llvm::ConstantInt::get(
              cint->value()->getType(),
              ~cint->value()->getValue())));
    }

    diag.error(ast) << "Invalid argument for bitwise 'not' expression";
    return &Expr::ErrorVal;
  }

  if (!value->type()->isIntType()) {
    diag.error(ast) << "Invalid argument for bitwise 'not' expression";
    return &Expr::ErrorVal;
  }

  return new UnaryExpr(
      Expr::Complement, ast->location(), value->type(), value);
}

Expr * ExprAnalyzer::reduceSetParamPropertyValue(const SourceLocation & loc, CallExpr * call,
    Expr * value) {

  LValueExpr * lval = cast<LValueExpr>(call->function());
  PropertyDefn * prop = cast<PropertyDefn>(lval->value());
  Expr * basePtr = lvalueBase(lval);

  if (!analyzeProperty(prop, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  FunctionDefn * setter = prop->setter();
  if (setter == NULL) {
    diag.error(loc) << "Attempt to set value of read-only property '" << prop << "'";
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(prop->isSingular(), prop);
  DASSERT_OBJ(setter->isSingular(), prop);

  if (!analyzeFunction(setter, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  // TODO: Type check args against function signature.

  ExprList castArgs;
  size_t argCount = call->args().size();
  for (size_t i = 0; i < argCount; ++i) {
    Expr * arg = call->arg(i);
    Expr * castArg = setter->functionType()->param(i)->type()->implicitCast(arg->location(), arg);
    if (isErrorResult(castArg)) {
      return &Expr::ErrorVal;
    }

    castArgs.push_back(castArg);
  }

  Expr::ExprType callType = Expr::FnCall;
  if (basePtr->type()->typeClass() == Type::Interface ||
      (basePtr->type()->typeClass() == Type::Class && !setter->isFinal())) {
    callType = Expr::VTableCall;
  }

  // TODO: Handle multiple overloads of the same property.
  // TODO: Change this to a CallExpr for type inference.
  FnCallExpr * setterCall = new FnCallExpr(callType, loc, setter, basePtr);
  setterCall->setType(prop->type());
  setterCall->args().append(castArgs.begin(), castArgs.end());
  // TODO: Remove this cast when we do the above.
  if (!value->isSingular()) {
    value = inferTypes(subject(), value, prop->type());
    if (isErrorResult(value)) {
      return value;
    }
  }

  value = prop->type()->implicitCast(loc, value);
  if (value != NULL) {
    setterCall->appendArg(value);
  }

  module()->addSymbol(setter);
  return setterCall;
}

} // namespace tart

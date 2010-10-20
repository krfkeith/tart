/* ================================================================ *
 TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Exprs.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Constant.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PropertyDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/Closure.h"
#include "tart/Objects/Builtins.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Sema/FinalizeTypesPass.h"
#include <llvm/DerivedTypes.h>

namespace tart {

/// -------------------------------------------------------------------
/// ExprAnalyzer

ExprAnalyzer::ExprAnalyzer(Module * mod, Scope * parent, Defn * subject,
    FunctionDefn * currentFunction)
  : AnalyzerBase(mod, parent, subject, currentFunction) {
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
    expr = TypeInferencePass::run(expr, expected);
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
      return reduceAnonFn(static_cast<const ASTFunctionDecl *> (ast));

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

Expr * ExprAnalyzer::reduceNull(const ASTNode * ast) {
  return new ConstantNull(ast->location());
}

Expr * ExprAnalyzer::reduceIntegerLiteral(const ASTIntegerLiteral * ast) {
  return new ConstantInteger(
      ast->location(),
      &UnsizedIntType::instance,
      llvm::ConstantInt::get(llvm::getGlobalContext(), ast->value()));
}

Expr * ExprAnalyzer::reduceFloatLiteral(const ASTFloatLiteral * ast) {
  return new ConstantFloat(
      ast->location(),
      &FloatType::instance,
      llvm::ConstantFP::get(llvm::getGlobalContext(), ast->value()));
}

Expr * ExprAnalyzer::reduceDoubleLiteral(const ASTDoubleLiteral * ast) {
  return new ConstantFloat(
      ast->location(),
      &DoubleType::instance,
      llvm::ConstantFP::get(llvm::getGlobalContext(), ast->value()));
}

Expr * ExprAnalyzer::reduceCharLiteral(const ASTCharLiteral * ast) {
  return new ConstantInteger(
      ast->location(),
      &CharType::instance,
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), ast->value(), false));
}

Expr * ExprAnalyzer::reduceStringLiteral(const ASTStringLiteral * ast) {
  return new ConstantString(ast->location(), ast->value());
}

Expr * ExprAnalyzer::reduceBoolLiteral(const ASTBoolLiteral * ast) {
  return ConstantInteger::getConstantBool(ast->location(), ast->value());
}

Expr * ExprAnalyzer::reduceBuiltInDefn(const ASTBuiltIn * ast) {
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(ast->value())) {
    return tdef->asExpr();
  } else {
    DFAIL("Implement");
    //diag.fatal(ast) << "'" << def->name() << "' is not a type";
    //return &BadType::instance;
  }
}

Expr * ExprAnalyzer::reduceAnonFn(const ASTFunctionDecl * ast) {
  TypeAnalyzer ta(module, activeScope);
  FunctionType * ftype = ta.typeFromFunctionAST(ast);

  if (ftype != NULL) {
    if (ast->body() != NULL) {
      ClosureEnvExpr * env = new ClosureEnvExpr(ast->location(), activeScope);
      TypeDefn * envTypeDef = new TypeDefn(module, ".env", NULL);
      envTypeDef->addTrait(Defn::Singular);
      envTypeDef->addTrait(Defn::Nonreflective);
      envTypeDef->addTrait(Defn::Synthetic);
      envTypeDef->setStorageClass(Storage_Instance);
      envTypeDef->setDefiningScope(activeScope);
      envTypeDef->createQualifiedName(currentFunction_);
      CompositeType * envType = new CompositeType(Type::Struct, envTypeDef, activeScope);
      envTypeDef->setTypeValue(envType);
      env->setType(envType);

      ParameterDefn * selfParam = new ParameterDefn(module, "self");
      selfParam->setFlag(ParameterDefn::ClosureEnv, true);
      selfParam->setFlag(ParameterDefn::Reference, true);
      selfParam->setInitValue(env);
      selfParam->setType(env->type());
      selfParam->setInternalType(env->type());
      selfParam->addTrait(Defn::Singular);
      //selfParam->addTrait(Defn::Final);
      selfParam->setStorageClass(Storage_Instance);

      ftype->setSelfParam(selfParam);

      // TODO: It's possible to have an anon fn outside of a function. Deal with that later.
      DASSERT(currentFunction_ != NULL);
      FunctionDefn * fn =  new FunctionDefn(Defn::Function, module, ast);
      fn->createQualifiedName(currentFunction_);
      fn->setFunctionType(ftype);
      fn->setStorageClass(Storage_Local);
      fn->setParentDefn(currentFunction_);
      fn->copyTrait(currentFunction_, Defn::Synthetic);
      fn->setFlag(FunctionDefn::Final);
      fn->addTrait(Defn::Singular);
      fn->parameterScope().addMember(selfParam);
      fn->setDefiningScope(activeScope);

      if (!analyzeFunction(fn, Task_PrepEvaluation)) {
        return &Expr::ErrorVal;
      }

      return new BoundMethodExpr(ast->location(), env, fn,
          new BoundMethodType(fn->functionType()));
    } else {
      // It's merely a function type declaration
      if (ftype->returnType() != NULL) {
        ftype->setReturnType(&VoidType::instance);
      }

      return new TypeLiteralExpr(ast->location(), ftype);
    }
  }

  return &Expr::ErrorVal;
}

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

  return reduceStoreValue(ast->location(), lhs, rhs);
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
        Expr * setProp = reduceSetParamPropertyValue(ast->location(), call, newValue);
        if (isErrorResult(setProp)) {
          return &Expr::ErrorVal;
        }

        DFAIL("Implement PostAssign of indexed property");
      }
    }
  }

  return new AssignmentExpr(Expr::PostAssign, ast->location(), lvalue, newValue);
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
  MultiAssignExpr * ma = new MultiAssignExpr(ast->location(), rhs->type());
  if (TupleCtorExpr * tce = dyn_cast<TupleCtorExpr>(rhs)) {
    for (size_t i = 0; i < dstVars.size(); ++i) {
      ma->appendArg(reduceStoreValue(ast->location(), dstVars[i], tce->arg(i)));
    }
  } else {
    rhs = SharedValueExpr::get(rhs);
    for (size_t i = 0; i < dstVars.size(); ++i) {
      Expr * srcVal = new BinaryExpr(
          Expr::ElementRef, rhs->location(), tt->member(i),
          rhs, ConstantInteger::getUInt32(i));

      ma->appendArg(reduceStoreValue(ast->location(), dstVars[i], srcVal));
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
  ASTMemberRef augMethodRef(ast->location(), const_cast<ASTNode *>(ast->arg(0)), assignOperName);
  ASTNodeList args;
  args.push_back(const_cast<ASTNode *>(ast->arg(1)));
  Expr * callExpr = callName(ast->location(), &augMethodRef, args, NULL, true);
  if (callExpr != NULL) {
    return callExpr;
  }

  // Otherwise call the regular infix operator and assign to the same location.
  Expr * callResult = callName(ast->location(), infixOperIdent, ast->args(), NULL, false);
  if (isErrorResult(callResult)) {
    return callResult;
  }

  Expr * lhs = reduceValueRef(ast->arg(0), true);
  if (isErrorResult(lhs)) {
    return &Expr::ErrorVal;
  }

  return reduceStoreValue(ast->location(), lhs, callResult);
}

Expr * ExprAnalyzer::reduceLoadValue(const ASTNode * ast) {
  Expr * lvalue = reduceValueRef(ast, false);
  if (isErrorResult(lvalue)) {
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(lvalue->type() != NULL, lvalue);

  if (CallExpr * call = dyn_cast<CallExpr>(lvalue)) {
    if (LValueExpr * lval = dyn_cast<LValueExpr>(call->function())) {
      if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
        checkAccess(ast->location(), prop);
        return reduceGetParamPropertyValue(ast->location(), call);
      }
    }
  }

  // TODO: Check for indexer...

  return lvalue;
}

Expr * ExprAnalyzer::reduceStoreValue(const SourceLocation & loc, Expr * lhs, Expr * rhs) {
  if (LValueExpr * lval = dyn_cast<LValueExpr>(lhs)) {
    // If it's a property reference, convert it into a method call.
    if (VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {
      // The only time we may assign to a 'let' variable is in a constructor, and only
      // if the variable is an instance member of that class.
      if (var->defnType() == Defn::Let) {
        if (var->storageClass() == Storage_Instance) {
          if (FunctionDefn * subjectFn = dyn_cast<FunctionDefn>(subject_)) {
            if (subjectFn->isCtor() && subjectFn->parentDefn() == var->parentDefn()) {
              return new AssignmentExpr(Expr::Assign, loc, lhs, rhs);
            }
          }
        }

        diag.error(loc) << "Assignment to immutable value '" << var << "'";
      } else if (var->defnType() == Defn::MacroArg) {
        // Special case for macro arguments which may be lvalues but weren't evaluated that way.
        Expr * macroVal = var->initValue();
        switch (macroVal->exprType()) {
          case Expr::LValue:
            break;

          case Expr::FnCall: {
            // Transform a property get into a property set.
            FnCallExpr * fnCall = static_cast<FnCallExpr *>(macroVal);
            FunctionDefn * fn = fnCall->function();
            if (PropertyDefn * prop = dyn_cast<PropertyDefn>(fn->parentDefn())) {
              DASSERT(fn == prop->getter());
              if (prop->setter() != NULL) {
                FnCallExpr * setterCall = new FnCallExpr(
                    fnCall->exprType(),
                    fnCall->location(),
                    prop->setter(),
                    fnCall->selfArg());
                setterCall->setType(&VoidType::instance);
                setterCall->args().append(fnCall->args().begin(), fnCall->args().end());
                setterCall->args().push_back(rhs);
                return setterCall;
              }
            } else if (IndexerDefn * idx = dyn_cast<IndexerDefn>(fn->parentDefn())) {
              DASSERT(fn == idx->getter());
              if (idx->setter() != NULL) {
                FnCallExpr * setterCall = new FnCallExpr(
                    fnCall->exprType(),
                    fnCall->location(),
                    idx->setter(),
                    fnCall->selfArg());
                setterCall->setType(&VoidType::instance);
                setterCall->args().append(fnCall->args().begin(), fnCall->args().end());
                setterCall->args().push_back(rhs);
                return setterCall;
              }
            }

            diag.error(lval) << "Not an l-value: " << macroVal;
            break;
          }

          default:
            diag.error() << "Macro param expr type not handled: " <<
                exprTypeName(macroVal->exprType());
            DASSERT(false);
            break;
        }
      }
    }
  } else if (CallExpr * call = dyn_cast<CallExpr>(lhs)) {
    if (LValueExpr * lval = dyn_cast<LValueExpr>(call->function())) {
      if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
        return reduceSetParamPropertyValue(loc, call, rhs);
      }
    }

    diag.error(lhs) << "lvalue expected on left side of assignment: " << lhs;
  }

  return new AssignmentExpr(Expr::Assign, loc, lhs, rhs);
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
  TypeAnalyzer ta(module, activeScope);
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

Expr * ExprAnalyzer::reduceArrayLiteral(const ASTOper * ast, const Type * expected) {
  DefnList fnlist;
  if (lookupTemplateMember(fnlist, Builtins::typeArray->typeDefn(), "of", ast->location())) {
    DASSERT(fnlist.size() == 1);
    CallExpr * call = new CallExpr(Expr::Call, ast->location(), NULL);
    call->setExpectedReturnType(expected);
    addOverload(call, Builtins::typeArray->typeDefn()->asExpr(),
        cast<FunctionDefn>(fnlist.front()), ast->args());
    if (!reduceArgList(ast->args(), call)) {
      return &Expr::ErrorVal;
    }

    call->setType(reduceReturnType(call));
    return call;
  }

  DFAIL("Array type not found for array literal");
}

Expr * ExprAnalyzer::reduceTuple(const ASTOper * ast, const Type * expected) {
  DASSERT(ast->count() >= 2);

  const TupleType * ttype = dyn_cast_or_null<TupleType>(dealias(expected));
  if (ttype != NULL) {
    if (ast->count() != ttype->numTypeParams()) {
      diag.error(ast) << "Type of '" << ast << "' does not match '" << expected << "'";
    }
  }

  TupleCtorExpr * tuple = new TupleCtorExpr(ast->location(), NULL);
  ConstTypeList types;
  size_t numParams = ast->count();
  bool isSingular = true;
  for (size_t i = 0; i < numParams; ++i) {
    const Type * elType = ttype != NULL ? ttype->typeParam(i) : NULL;
    Expr * el = reduceExpr(ast->args()[i], elType);
    if (isErrorResult(el)) {
      return &Expr::ErrorVal;
    }

    if (!el->type()->isSingular()) {
      isSingular = false;
    }

    tuple->args().push_back(el);
    types.push_back(dealias(el->type()));
  }

  const Type * tupleType;
  if (isSingular) {
    tupleType = TupleType::get(types);
  } else {
    tupleType = new TupleOfConstraint(tuple);
  }

  tuple->setType(tupleType);
  return tuple;
}

Expr * ExprAnalyzer::reduceValueRef(const ASTNode * ast, bool store) {
  if (ast->nodeType() == ASTNode::GetElement) {
    return reduceElementRef(static_cast<const ASTOper *>(ast), store);
  } else {
    return reduceSymbolRef(ast, store);
  }
}

Expr * ExprAnalyzer::reduceSymbolRef(const ASTNode * ast, bool store) {
  ExprList values;
  lookupName(values, ast, LOOKUP_REQUIRED);

  if (values.size() == 0) {
    // Undefined symbol (error already reported).
    return &Expr::ErrorVal;
  } else if (values.size() > 1) {
    diag.error(ast) << "Multiply defined symbol " << ast;
    return &Expr::ErrorVal;
  } else {
    Expr * value = values.front();
    if (LValueExpr * lval = dyn_cast<LValueExpr>(value)) {
      return reduceLValueExpr(lval, store);
    } else if (TypeLiteralExpr * typeExpr = dyn_cast<TypeLiteralExpr>(value)) {
      return typeExpr;
    } else if (ScopeNameExpr * scopeName = dyn_cast<ScopeNameExpr>(value)) {
      return scopeName;
    } else {
      DFAIL("Not an LValue");
    }
  }
}

Expr * ExprAnalyzer::reduceElementRef(const ASTOper * ast, bool store) {
  // TODO: We might want to support more than 1 array index.
  DASSERT_OBJ(ast->count() >= 1, ast);
  if (ast->count() == 1) {
    TypeAnalyzer ta(module, activeScope);
    Type * elemType = ta.typeFromAST(ast->arg(0));
    if (elemType == NULL) {
      return &Expr::ErrorVal;
    }
    return new TypeLiteralExpr(ast->location(), getArrayTypeForElement(elemType));
//    Expr * elementExpr = reduceExpr(ast->arg(0), NULL);
//    if (TypeLiteralExpr * elementType = dyn_cast_or_null<TypeLiteralExpr>(elementExpr)) {
//      return new TypeLiteralExpr(ast->location(), getArrayTypeForElement(elementType->value()));
//    }
//
//    diag.debug(ast) << "Preceding expression was " << elementExpr;
//    diag.error(ast) << "Type expression expected before []";
//    return &Expr::ErrorVal;
  }

  // If it's a name, see if it's a specializable name.
  const ASTNode * base = ast->arg(0);
  Expr * arrayExpr;
  if (base->nodeType() == ASTNode::Id || base->nodeType() == ASTNode::Member) {
    ExprList values;
    lookupName(values, base, LOOKUP_REQUIRED);

    if (values.size() == 0) {
      //diag.error(base) << "Undefined symbol " << base;
      //diag.writeLnIndent("Scopes searched:");
      //dumpScopeHierarchy();
      return &Expr::ErrorVal;
    } else {
      // For type names and function names, brackets always mean specialize, not index.
      bool isTypeOrFunc = false;
      for (ExprList::iterator it = values.begin(); it != values.end(); ++it) {
        if (isa<TypeLiteralExpr>(*it)) {
          isTypeOrFunc = true;
          break;
        } else if (LValueExpr * lv = dyn_cast<LValueExpr>(*it)) {
          if (isa<FunctionDefn>(lv->value())) {
            isTypeOrFunc = true;
            break;
          }
        }
      }

      if (isTypeOrFunc) {
        ASTNodeList args;
        args.insert(args.begin(), ast->args().begin() + 1, ast->args().end());
        Expr * specResult = specialize(ast->location(), values, args, true);
        if (specResult != NULL) {
          return specResult;
        } else {
          return &Expr::ErrorVal;
        }
      }

      if (values.size() > 1) {
        diag.error(base) << "Multiply defined symbol " << base;
        return &Expr::ErrorVal;
      }

      arrayExpr = values.front();
      if (LValueExpr * lval = dyn_cast<LValueExpr>(arrayExpr)) {
        arrayExpr = reduceLValueExpr(lval, store);
      } else {
        diag.error(base) << "Expression " << base << " is neither an array type nor a template";
        return &Expr::ErrorVal;
      }
    }
  } else {
    // TODO: We might want to support more than 1 array index.
    DASSERT_OBJ(ast->count() == 2, ast);
    arrayExpr = reduceExpr(base, NULL);
  }

  if (isErrorResult(arrayExpr)) {
    return &Expr::ErrorVal;
  }

  // What we want is to determine whether or not the expression is a template. Or even a type.

  const Type * arrayType = dealias(arrayExpr->type());
  DASSERT_OBJ(arrayType != NULL, arrayExpr);

  // Reduce all of the arguments
  ExprList args;
  for (size_t i = 1; i < ast->count(); ++i) {
    Expr * arg = reduceExpr(ast->arg(i), NULL);
    if (isErrorResult(arg)) {
      return &Expr::ErrorVal;
    }

    args.push_back(arg);
  }

  if (args.empty()) {
    diag.fatal(ast) << "Array element access with no arguments";
    return &Expr::ErrorVal;
  }

  // Memory address type
  if (const AddressType * maType = dyn_cast<AddressType>(arrayType)) {
    const Type * elemType = maType->typeParam(0);
    if (!AnalyzerBase::analyzeType(elemType, Task_PrepTypeComparison)) {
      return &Expr::ErrorVal;
    }

    DASSERT_OBJ(elemType != NULL, maType);

    if (args.size() != 1) {
      diag.fatal(ast) << "Incorrect number of array index dimensions";
    }

    // TODO: Attempt to cast arg to an integer type of known size.

    // First dereference the pointer and then get the element.
    return new BinaryExpr(Expr::ElementRef, ast->location(), elemType, arrayExpr, args[0]);
  }

  // Do automatic pointer dereferencing
  /*if (NativePointerType * npt = dyn_cast<NativePointerType>(arrayType)) {
   if (!AnalyzerBase::analyzeType(npt, Task_PrepTypeComparison)) {
   return &Expr::ErrorVal;
   }

   arrayType = npt->typeParam(0);
   arrayExpr = new UnaryExpr(Expr::PtrDeref, arrayExpr->location(), arrayType, arrayExpr);
   }*/

  // Handle native arrays.
  if (const NativeArrayType * naType = dyn_cast<NativeArrayType>(arrayType)) {
    if (!AnalyzerBase::analyzeType(naType, Task_PrepTypeComparison)) {
      return &Expr::ErrorVal;
    }

    const Type * elemType = naType->typeParam(0);
    DASSERT_OBJ(elemType != NULL, naType);

    if (args.size() != 1) {
      diag.fatal(ast) << "Incorrect number of array subscripts";
      return &Expr::ErrorVal;
    }

    Expr * indexExpr = args[0];
    if (Int32Type::instance.canConvert(indexExpr, Conversion::Coerce) == Incompatible) {
      diag.fatal(ast) << "Native array subscript must be integer type, but is " << indexExpr->type();
      return &Expr::ErrorVal;
    }

    // TODO: Attempt to cast arg to an integer type of known size.
    return new BinaryExpr(Expr::ElementRef, ast->location(), elemType, arrayExpr, indexExpr);
  }

  // Handle flexible arrays.
  if (const FlexibleArrayType * faType = dyn_cast<FlexibleArrayType>(arrayType)) {
    if (!AnalyzerBase::analyzeType(faType, Task_PrepTypeComparison)) {
      return &Expr::ErrorVal;
    }

    const Type * elemType = faType->typeParam(0);
    DASSERT_OBJ(elemType != NULL, faType);

    if (args.size() != 1) {
      diag.fatal(ast) << "Incorrect number of array subscripts";
      return &Expr::ErrorVal;
    }

    Expr * indexExpr = args[0];
    if (Int32Type::instance.canConvert(indexExpr, Conversion::Coerce) == Incompatible) {
      diag.fatal(ast) << "Flexible array subscript must be integer type, but is " <<
          indexExpr->type();
      return &Expr::ErrorVal;
    }

    // TODO: Attempt to cast arg to an integer type of known size.
    return new BinaryExpr(Expr::ElementRef, ast->location(), elemType, arrayExpr, indexExpr);
  }

  // Handle tuples
  if (const TupleType * tt = dyn_cast<TupleType>(arrayType)) {
    if (args.size() != 1) {
      diag.fatal(ast) << "Incorrect number of array subscripts";
      return &Expr::ErrorVal;
    }

    Expr * indexExpr = args[0];
    const Type * indexType = indexExpr->type();
    if (Int32Type::instance.canConvert(indexExpr, Conversion::Coerce) == Incompatible) {
      diag.fatal(args[0]) << "Tuple subscript must be integer type, is " << indexType;
      return &Expr::ErrorVal;
    }

    if (ConstantInteger * cint = dyn_cast<ConstantInteger>(indexExpr)) {
      const llvm::APInt & indexVal = cint->intValue();
      if (indexVal.isNegative() || indexVal.getZExtValue() >= tt->numTypeParams()) {
        diag.fatal(args[0]) << "Tuple subscript out of range";
        return &Expr::ErrorVal;
      } else if (store) {
        diag.fatal(args[0]) << "Tuples are immutable";
        return &Expr::ErrorVal;
      }

      uint32_t index = uint32_t(indexVal.getZExtValue());
      return new BinaryExpr(Expr::ElementRef, ast->location(), tt->member(index), arrayExpr, cint);
    } else {
      diag.fatal(args[0]) << "Tuple subscript must be an integer constant";
      return &Expr::ErrorVal;
    }
  }

  // See if the type has any indexers defined.
  DefnList indexers;
  if (arrayType->typeDefn() != NULL) {
    if (!AnalyzerBase::analyzeType(arrayType, Task_PrepMemberLookup)) {
      return &Expr::ErrorVal;
    }
  }

  if (arrayType->memberScope() == NULL ||
      !arrayType->memberScope()->lookupMember(istrings.idIndex, indexers, true)) {
    diag.fatal(ast) << "Type " << arrayType << " does not support element reference operator";
    return &Expr::ErrorVal;
  }

  DASSERT(!indexers.empty());

  // For now, we only allow a single indexer to be defined. Later we will allow
  // overloading both by argument type and return type.
  if (indexers.size() > 1) {
    diag.fatal(ast) << "Multiple indexer definitions are currently not supported";
    return &Expr::ErrorVal;
  }

  PropertyDefn * indexer = cast<PropertyDefn>(indexers.front());
  if (!analyzeProperty(indexer, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  // CallExpr type used to hold the array reference and parameters.
  LValueExpr * callable = LValueExpr::get(ast->location(), arrayExpr, indexer);
  CallExpr * call = new CallExpr(Expr::Call, ast->location(), callable);
  call->args().append(args.begin(), args.end());
  call->setType(indexer->type());
  return call;
}

Expr * ExprAnalyzer::reduceGetParamPropertyValue(const SourceLocation & loc, CallExpr * call) {

  LValueExpr * lval = cast<LValueExpr>(call->function());
  PropertyDefn * prop = cast<PropertyDefn>(lval->value());
  Expr * basePtr = lvalueBase(lval);
  //const ASTNodeList & args = call->args();

  if (!analyzeProperty(prop, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(prop->isSingular(), prop);
  DASSERT_OBJ(prop->getter()->isSingular(), prop);

  FunctionDefn * getter = prop->getter();
  if (getter == NULL) {
    diag.fatal(prop) << "No getter for property '" << prop->name() << "'";
    return &Expr::ErrorVal;
  }

  if (!analyzeFunction(getter, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(getter->returnType()->isEqual(prop->type()), getter);
  DASSERT_OBJ(!getter->returnType()->isVoidType(), getter);

#if 0

  CallExpr * call = new CallExpr(Expr::Call, loc, NULL);
  call->setExpectedReturnType(prop->type());
  if (!addOverload(call, basePtr, getter, args)) {
    return &Expr::ErrorVal;
  }

  if (!reduceArgList(args, call)) {
    return &Expr::ErrorVal;
  }

  if (call->candidates().empty()) {
    // Generate the calling signature in a buffer.
    std::stringstream callsig;
    FormatStream fs(callsig);
    fs << Format_Dealias << basePtr << "[";
    formatExprTypeList(fs, call->args());
    fs << "]";
    fs << " -> " << prop->type();

    diag.error(loc) << "No matching method for call to " << callsig.str() << ", candidates are:";
    /*for (ExprList::iterator it = results.begin(); it != results.end(); ++it) {
     Expr * resultMethod = *it;
     if (LValueExpr * lval = dyn_cast<LValueExpr>(*it)) {
     diag.info(lval->value()) << Format_Type << lval->value();
     } else {
     diag.info(*it) << *it;
     }
     }*/

    return &Expr::ErrorVal;
  }

  call->setType(reduceReturnType(call));
  return call;

#else

  // TODO: Type check args against function signature.

  ExprList castArgs;
  size_t argCount = call->args().size();
  for (size_t i = 0; i < argCount; ++i) {
    const Type * paramType = getter->functionType()->param(i)->type();
    Expr * arg = call->arg(i);
    arg = inferTypes(subject(), arg, paramType);
    Expr * castArg = paramType->implicitCast(arg->location(), arg);
    if (isErrorResult(castArg)) {
      return &Expr::ErrorVal;
    }

    castArgs.push_back(castArg);
  }

  Expr::ExprType callType = Expr::FnCall;
  if (basePtr->type()->typeClass() == Type::Interface ||
      (basePtr->type()->typeClass() == Type::Class && !getter->isFinal())) {
    callType = Expr::VTableCall;
  }

  FnCallExpr * getterCall = new FnCallExpr(callType, loc, getter, basePtr);
  getterCall->setType(prop->type());
  getterCall->args().append(castArgs.begin(), castArgs.end());
  module->addSymbol(getter);
  return getterCall;
#endif
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

  module->addSymbol(setter);
  return setterCall;
}

Expr * ExprAnalyzer::reduceLValueExpr(LValueExpr * lvalue, bool store) {
  ValueDefn * valueDefn = lvalue->value();
  DASSERT(valueDefn != NULL);
  analyzeDefn(valueDefn, Task_PrepTypeComparison);
  DASSERT(valueDefn->type() != NULL);
  lvalue->setType(valueDefn->type());
  if (ParameterDefn * param = dyn_cast<ParameterDefn>(valueDefn)) {
    lvalue->setType(param->internalType());
  }

  checkAccess(lvalue->location(), valueDefn);
  switch (valueDefn->storageClass()) {
    case Storage_Global:
    case Storage_Static:
      lvalue->setBase(NULL);

      // If it's a let-variable, then make sure that the initialization value
      // gets evaluated.
      if (valueDefn->defnType() == Defn::Let &&
          valueDefn->module() != module &&
          valueDefn->module() != NULL) {
        analyzeDefn(valueDefn, Task_PrepConstruction);
      }
      break;

    case Storage_Local:
      lvalue->setBase(NULL);
      break;

    case Storage_Instance: {
      Expr * base = lvalueBase(lvalue);
      if (base == NULL || base->exprType() == Expr::ScopeName) {
        diag.error(lvalue) << "Attempt to reference non-static member " <<
        valueDefn->name() << " with no object";
        return &Expr::ErrorVal;
      }

      lvalue->setBase(base);

      // TODO: Handle type names and such

      break;
    }

    case Storage_Class:
    case Storage_Closure:
    default:
      DFAIL("Invalid storage class");
  }

  if (lvalue->base() != NULL && lvalue->base()->type()->typeClass() == Type::NAddress) {
    lvalue->setBase(new UnaryExpr(Expr::PtrDeref, lvalue->base()->location(),
        lvalue->base()->type()->typeParam(0), lvalue->base()));
  }

  return lvalue;
}

/** Given an LValue, return the base expression. */
Expr * ExprAnalyzer::lvalueBase(LValueExpr * lval) {
  Expr * base = lval->base();
  if (LValueExpr * lvbase = dyn_cast_or_null<LValueExpr>(base)) {
    return reduceLValueExpr(lvbase, false);
  }

  return base;
}

Expr * ExprAnalyzer::doImplicitCast(Expr * in, const Type * toType, bool tryCoerce) {
  DASSERT(in != NULL);
  if (isErrorResult(toType)) {
    return in;
  }

  in = LValueExpr::constValue(in);
  if (!AnalyzerBase::analyzeType(toType, Task_PrepTypeComparison)) {
    return in;
  }

  Expr * castExpr = NULL;
  ConversionRank rank = toType->convert(Conversion(in, &castExpr));
  DASSERT(rank == Incompatible || castExpr != NULL);

  if (rank == Incompatible && tryCoerce) {
    // Try a coercive cast. Note that we don't do this in 'convert' because it
    // can't handle building the actual call expression.
    castExpr = tryCoerciveCast(in, toType);
    if (castExpr != NULL) {
      Expr * result = inferTypes(subject_, castExpr, toType, false);
      return result;
    }
  }

  compatibilityWarning(in->location(), rank, in, toType);
  if (isErrorResult(castExpr)) {
    return in;
  }

  return castExpr;
}

Defn * ExprAnalyzer::findBestSpecialization(SpecializeExpr * spe) {
  const SpCandidateList & candidates = spe->candidates();
  ConversionRank bestRank = Incompatible;
  for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
    SpCandidate * sp = *it;
    bestRank = std::max(bestRank, sp->updateConversionRank());
  }

  if (bestRank == Incompatible) {
    if (!spe->args()->containsBadType()) {
      SpCandidate * front = *candidates.begin();
      diag.error(spe) << "No candidate found for '" << front->def()->name() <<
          "' which matches template arguments [" << spe->args() << "]:";
      diag.info(spe) << "Candidates are:";
      for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
        SpCandidate * sp = *it;
        sp->updateConversionRank(); // Helps with debugging.
        diag.info(sp->def()) << Format_Type << sp->def() << " [" << sp->conversionRank() << "]";
      }
    }
    return NULL;
  }

  SpCandidateList bestCandidates;
  for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
    SpCandidate * sp = *it;
    if (sp->conversionRank() == bestRank) {
      bool addNew = true;
      for (SpCandidateList::iterator bc = bestCandidates.begin(); bc != bestCandidates.end();) {
        bool newIsBetter = sp->isMoreSpecific(*bc);
        bool oldIsBetter = (*bc)->isMoreSpecific(sp);

        if (newIsBetter) {
          if (!oldIsBetter) {
            /*if (ShowInference) {
              diag.debug() << Format_Type << "Culling [" << (*ms)->method() <<
                  "] because [" << call->method() << "] is more specific";
            }*/
            bc = bestCandidates.erase(bc);
            continue;
          }
        } else if (oldIsBetter) {
          /*if (ShowInference) {
            diag.debug() << Format_Type << "Culling [" << call->method() << "] because [" <<
                (*ms)->method() << "] is more specific";
          }*/

          addNew = false;
        }

        ++bc;
      }

      if (addNew) {
        bestCandidates.push_back(sp);
      }
    }
  }

  if (bestCandidates.size() > 1) {
    SpCandidate * front = *candidates.begin();
    diag.error(spe) << "Ambiguous matches for '" << front->def()->name() <<
        "' which matches template arguments [" << spe->args() << "]:";
    diag.info(spe) << "Candidates are:";
    for (SpCandidateList::const_iterator it = bestCandidates.begin(); it != bestCandidates.end();
        ++it) {
      SpCandidate * sp = *it;
      diag.info(sp->def()) << Format_Type << sp->def() << " [" << sp->conversionRank() << "]";
    }
    DFAIL("Implement better culling of SpCandidates");
  }

  SpCandidate * spBest = *bestCandidates.begin();
  if (spBest->def()->hasUnboundTypeParams()) {
    return spBest->def()->templateSignature()->instantiate(spe->location(), spBest->env());
  }

  return spBest->def();
}

Expr * ExprAnalyzer::doBoxCast(Expr * in) {
  const Type * fromType = dealias(in->type());
  FunctionDefn * coerceFn = coerceToObjectFn(fromType);
  FnCallExpr * call = new FnCallExpr(Expr::FnCall, in->location(), coerceFn, NULL);
  call->appendArg(in);
  call->setType(Builtins::typeObject);
  return call;
}

/** Given a type, return the coercion function to convert it to a reference type. */
FunctionDefn * ExprAnalyzer::coerceToObjectFn(const Type * type) {
  DASSERT(!type->isReferenceType());
  DASSERT(type->typeClass() != Type::NAddress);
  DASSERT(type->typeClass() != Type::NArray);
  DASSERT(type->typeClass() != Type::FlexibleArray);
  DASSERT(type->isSingular());

  TypePair conversionKey(type, Builtins::typeObject.get());
  ConverterMap::iterator it = module->converters().find(conversionKey);
  if (it != module->converters().end()) {
    return it->second;
  }

  FunctionDefn * coerceFn = Builtins::objectCoerceFn();
  TemplateSignature * coerceTemplate = coerceFn->templateSignature();

  DASSERT_OBJ(coerceTemplate->paramScope().count() == 1, type);
  // Do analysis on template if needed.
  if (coerceTemplate->ast() != NULL) {
    DefnAnalyzer da(&Builtins::module, &Builtins::module, &Builtins::module, NULL);
    da.analyzeTemplateSignature(coerceFn);
  }

  BindingEnv env;
  env.addSubstitution(coerceTemplate->patternVar(0), type);
  FunctionDefn * coercer = cast<FunctionDefn>(coerceTemplate->instantiate(SourceLocation(), env));
  analyzeFunction(coercer, Task_PrepTypeComparison);
  DASSERT(coercer->isSingular());
  module->converters()[conversionKey] = coercer;
  module->addSymbol(coercer);
  //diag.info() << Format_Verbose << "Generated coercer " << coercer;
  return coercer;
}

Expr * ExprAnalyzer::doUnboxCast(Expr * in, const Type * toType) {
  FunctionDefn * valueOfMethod = getUnboxFn(in->location(), toType);
  if (valueOfMethod == NULL) {
    return NULL;
  }

  DASSERT(valueOfMethod->isSingular());
  FnCallExpr * call = new FnCallExpr(Expr::FnCall, in->location(), valueOfMethod, NULL);
  call->appendArg(doImplicitCast(in, Builtins::typeObject));
  call->setType(valueOfMethod->returnType());
  return call;
}

FunctionDefn * ExprAnalyzer::getUnboxFn(SLC & loc, const Type * toType) {
  ExprList methods;
  TypePair conversionKey(Builtins::typeObject.get(), toType);
  ConverterMap::iterator it = module->converters().find(conversionKey);
  if (it != module->converters().end()) {
    return it->second;
  }

  //diag.debug(loc) << Format_Type << "Defining unbox function for " << toType;
  analyzeTypeDefn(Builtins::typeRef->typeDefn(), Task_PrepMemberLookup);
  findInScope(methods, "valueOf", Builtins::typeRef->memberScope(), NULL, loc, NO_PREFERENCE);
  DASSERT(!methods.empty());
  Expr * valueOf = specialize(loc, methods, TupleType::get(toType));
  FunctionDefn * valueOfMethod;
  if (SpecializeExpr * spe = dyn_cast<SpecializeExpr>(valueOf)) {
    valueOfMethod = cast_or_null<FunctionDefn>(findBestSpecialization(spe));
    if (valueOfMethod == NULL) {
      return NULL;
    }
  } else if (LValueExpr * lval = dyn_cast<LValueExpr>(valueOf)) {
    valueOfMethod = cast<FunctionDefn>(lval->value());
  } else {
    diag.error(loc) << "Unknown expression " << valueOf;
    DFAIL("IllegalState");
  }

  analyzeFunction(valueOfMethod, Task_PrepTypeComparison);
  module->converters()[conversionKey] = valueOfMethod;
  module->addSymbol(valueOfMethod);
  //diag.info() << Format_Verbose << "Generated boxer " << valueOfMethod;
  return valueOfMethod;
}

}

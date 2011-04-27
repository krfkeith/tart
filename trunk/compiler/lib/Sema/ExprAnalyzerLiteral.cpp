/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/Stmt.h"

#include "tart/Expr/Exprs.h"
#include "tart/Defn/Module.h"
#include "tart/Expr/Constant.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Defn/Template.h"
#include "tart/Expr/Closure.h"
#include "tart/Defn/NamespaceDefn.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

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
#include <llvm/ADT/StringExtras.h>

namespace tart {

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

Expr * ExprAnalyzer::reduceAnonFn(const ASTFunctionDecl * ast, const Type * expected) {
  TypeAnalyzer ta(module(), activeScope());
  FunctionType * ftype = ta.typeFromFunctionAST(ast);

  if (ftype != NULL) {
    if (ftype->returnType() == NULL) {
      // TODO - deduce the return type from the expected type.
      ftype->setReturnType(&VoidType::instance);
    }

    if (ast->body() != NULL) {
      // Generate a unique name for this closure.
      std::string closureName(llvm::itostr(currentFunction_->closureEnvs().size() + 1));

      // The hidden parameter that points to the closure environment
      ParameterDefn * envParam = new ParameterDefn(module(), "#env");

      TypeDefn * envTypeDef = new TypeDefn(module(), istrings.intern(closureName), NULL);
      envTypeDef->createQualifiedName(currentFunction_);
      envTypeDef->addTrait(Defn::Singular);
      envTypeDef->addTrait(Defn::Synthetic);
      envTypeDef->setStorageClass(Storage_Instance);
      envTypeDef->setDefiningScope(activeScope());

      // Use a composite type to represent the closure environment.
      CompositeType * interfaceType = getFunctionInterfaceType(ftype);
      DASSERT(interfaceType->isSingular());

      CompositeType * envType = new CompositeType(Type::Class, envTypeDef, activeScope());
      envTypeDef->setTypeValue(envType);
      envType->setClassFlag(CompositeType::Closure, true);
      envType->setSuper(static_cast<CompositeType *>(Builtins::typeObject));
      envType->bases().push_back(Builtins::typeObject);
      envType->bases().push_back(interfaceType);
      module_->addSymbol(Builtins::typeObject->typeDefn());
      module_->addSymbol(envTypeDef);

      // Prepare the Function interface
      if (!analyzeType(interfaceType, Task_PrepMemberLookup)) {
        return &Expr::ErrorVal;
      }

      LValueExpr * envBaseExpr = LValueExpr::get(ast->location(), NULL, envParam);
      envBaseExpr->setType(envType);

      ClosureEnvExpr * env = new ClosureEnvExpr(
          ast->location(), activeScope(), &currentFunction_->parameterScope(), envType, envBaseExpr);
      env->setType(envType);
      currentFunction_->closureEnvs().push_back(env);

      envParam->setFlag(ParameterDefn::Reference, true);
      envParam->setInitValue(env);
      envParam->setType(env->type());
      envParam->setInternalType(env->type());
      envParam->addTrait(Defn::Singular);
      envParam->setStorageClass(Storage_Local);

      ftype->setSelfParam(envParam);

      // TODO: It's possible to have an anon fn outside of a function. Deal with that later.
      DASSERT(currentFunction_ != NULL);
      FunctionDefn * fn = new FunctionDefn(Defn::Function, module(), ast);
      fn->setFunctionType(ftype);
      fn->createQualifiedName(envTypeDef);
      fn->setStorageClass(Storage_Instance);
      fn->setParentDefn(envTypeDef);
      fn->copyTrait(currentFunction_, Defn::Synthetic);
      fn->setFlag(FunctionDefn::Nested);
      fn->setFlag(FunctionDefn::Final);
      fn->addTrait(Defn::Singular);
      fn->parameterScope().addMember(envParam);

      envType->memberScope()->addMember(fn);
      fn->setDefiningScope(env);

      // Prepare the environment for lookups
      if (!analyzeType(envType, Task_PrepMemberLookup)) {
        return &Expr::ErrorVal;
      }

      // Analyze the function body. Doing this will also cause additional fields
      // to be added to the environment type.
      if (!analyzeFunction(fn, Task_PrepEvaluation)) {
        return &Expr::ErrorVal;
      }

      // Append any closures nested within 'fn' to the next level up.
      currentFunction_->closureEnvs().append(fn->closureEnvs().begin(), fn->closureEnvs().end());

      // Finish analysis of the environment type.
      if (!analyzeType(envType, Task_PrepCodeGeneration)) {
        return &Expr::ErrorVal;
      }
      DASSERT(envType->super() != NULL);
      return env;
    } else {
      // It's merely a function type declaration
      return new TypeLiteralExpr(ast->location(), ftype);
    }
  }

  return &Expr::ErrorVal;
}

Expr * ExprAnalyzer::reduceArrayLiteral(const ASTOper * ast, const Type * expected) {
  if (expected == &AnyType::instance) {
    expected = NULL;
  }

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


}

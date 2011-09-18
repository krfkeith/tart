/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/Stmt.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/NamespaceDefn.h"

#include "tart/Expr/Closure.h"

#include "tart/Expr/Exprs.h"
#include "tart/Expr/Constant.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/AmbiguousTypeParamType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/FinalizeTypesPass.h"

#include "llvm/DerivedTypes.h"
#include "llvm/ADT/StringExtras.h"

namespace tart {

Expr * ExprAnalyzer::reduceNull(const ASTNode * ast) {
  return new ConstantNull(ast->location());
}

Expr * ExprAnalyzer::reduceIntegerLiteral(const ASTIntegerLiteral * ast) {
  llvm::ConstantInt * cint = llvm::ConstantInt::get(llvm::getGlobalContext(), ast->value());
  return new ConstantInteger(ast->location(), UnsizedIntType::get(cint), cint);
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

Expr * ExprAnalyzer::reduceAnonFn(const ASTFunctionDecl * ast, QualifiedType expected) {
  TypeAnalyzer ta(module(), activeScope());
  FunctionType * ftype = ta.typeFromFunctionAST(ast);
  size_t paramCount = ftype->params().size();

  if (ftype != NULL) {
    if (ftype->returnType().isNull()) {
      if (expected) {
        // Check if the expected type is a specialization of the 'tart.core.Function' interface
        // and return the 0th parameter if so.
        QualifiedType returnType = AmbiguousTypeParamType::forType(
            expected, Builtins::typeFunction, 0);
        if (!returnType) {
          diag.error(ast) << "Can't deduce function return type from type " << expected;
          return &Expr::ErrorVal;
        }
        //diag.debug() << expected << " : " << returnType;
        ftype->setReturnType(returnType);
      } else {
        ftype->setReturnType(&VoidType::instance);
      }
    }

    // See if ftype has any unspecified param types.
    for (size_t i = 0; i < paramCount; ++i) {
      ParameterDefn * param = ftype->param(i);
      if (!param->type()) {
        if (!expected) {
          diag.error(ast) << "Can't deduce type of function parameter " << i + 1 << ".";
          return &Expr::ErrorVal;
        } else {
          // Check if the expected type is a specialization of the 'tart.core.Function' interface
          // and return the Nth parameter if so.
          QualifiedType paramListType = AmbiguousTypeParamType::forType(
              expected, Builtins::typeFunction, 1);
          if (!paramListType) {
            diag.error(ast) << "Can't deduce type of function parameter " << i + 1 << " << from " <<
                expected << ".";
            return &Expr::ErrorVal;
          }
          QualifiedType paramType = AmbiguousTypeParamType::forType(paramListType, NULL, i);
          if (!paramType) {
            diag.error(ast) << "Can't deduce type of function parameter " << i + 1 << " << from " <<
                expected << ".";
            return &Expr::ErrorVal;
          }
          diag.debug() << expected << " : " << paramListType << " : " << paramType;
          param->setType(paramType);
          DASSERT(!param->isVariadic());
          param->setInternalType(paramType);
        }
      }
    }

    if (ast->body() != NULL) {
#if 0
      // Use a composite type to represent the closure environment.
      CompositeType * interfaceType = getFunctionInterfaceType(ftype);

      ClosureEnvExpr * ce = new ClosureEnvExpr(
          ast->location(), activeScope(), &currentFunction_->parameterScope(), ftype, ast);
      ce->setType(interfaceType);
      return ce;
#else
      // Generate a unique name for this closure.
      std::string closureName(llvm::itostr(currentFunction_->closureEnvs().size() + 1));

      // Use a composite type to represent the closure environment.
      CompositeType * interfaceType = const_cast<CompositeType *>(getFunctionInterfaceType(ftype));
      //DASSERT(interfaceType->isSingular());

      // The type definition of the environment object.
      TypeDefn * envTypeDef = new TypeDefn(module(), module_->internString(closureName), NULL);
      envTypeDef->createQualifiedName(currentFunction_);
      envTypeDef->addTrait(Defn::Singular);
      envTypeDef->addTrait(Defn::Synthetic);
      envTypeDef->setStorageClass(Storage_Instance);
      envTypeDef->setDefiningScope(activeScope());

      CompositeType * envType = new CompositeType(Type::Class, envTypeDef, activeScope());
      envTypeDef->setValue(envType);
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

      // The hidden parameter that points to the closure environment
      ParameterDefn * envParam = new ParameterDefn(module(), "#env");
      LValueExpr * envBaseExpr = LValueExpr::get(ast->location(), NULL, envParam);
      envBaseExpr->setType(envType);

      ClosureEnvExpr * env = new ClosureEnvExpr(
          ast->location(), activeScope(), &currentFunction_->parameterScope(),
          envType, envBaseExpr);
      env->setType(envType);
      currentFunction_->closureEnvs().push_back(env);

      if (!interfaceType->isSingular()) {
        return env;
      }

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

      envType->mutableMemberScope()->addMember(fn);
      fn->setDefiningScope(env);

      // Prepare the environment for lookups
      if (!analyzeType(envType, Task_PrepMemberLookup)) {
        return &Expr::ErrorVal;
      }

      // Analyze the function body. Doing this will also cause additional fields
      // to be added to the environment type.
      // TODO: We can't do this yet!
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
#endif
    } else {
      // It's merely a function type declaration
      return new TypeLiteralExpr(ast->location(), ftype);
    }
  }

  return &Expr::ErrorVal;
}

Expr * ExprAnalyzer::reduceArrayLiteral(const ASTOper * ast, QualifiedType expected) {
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

Expr * ExprAnalyzer::reduceTuple(const ASTOper * ast, QualifiedType expected) {
  DASSERT(ast->count() >= 2);

  if (expected && expected.isMutable()) {
    diag.error(ast) << "Tuples cannot be made mutable";
  }

  // TODO: Add qualifier bits when done - although note that tuples are always immutable.
  const TupleType * ttype = dyn_cast_or_null<TupleType>(dealias(expected).unqualified());
  if (ttype != NULL) {
    if (ast->count() != ttype->numTypeParams()) {
      diag.error(ast) << "Type of '" << ast << "' does not match '" << expected << "'";
    }
  }

  TupleCtorExpr * tuple = new TupleCtorExpr(ast->location(), NULL);
  QualifiedTypeList types;
  size_t numParams = ast->count();
  bool isSingular = true;
  for (size_t i = 0; i < numParams; ++i) {
    QualifiedType elType = ttype != NULL ? ttype->typeParam(i) : QualifiedType::NONE;
    Expr * el = reduceExpr(ast->args()[i], elType);
    if (isErrorResult(el)) {
      return &Expr::ErrorVal;
    }

    if (elType && elType->isSingular()) {
      ConversionRank rank;
      llvm::tie(rank, el) = TypeConversion::convert(el, elType, TypeConversion::COERCE);
      DASSERT(el != NULL);
    }

    if (!el->type()->isSingular()) {
      isSingular = false;
    }

    tuple->args().push_back(el);
    types.push_back(dealias(el->type()));
  }

  tuple->setType(TupleType::get(types));
  return tuple;
}

}

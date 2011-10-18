/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/ASTDecl.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"

#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/Module.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeFunction.h"
#include "tart/Type/UnionType.h"

#include "tart/Objects/Builtins.h"

namespace tart {

Type * TypeAnalyzer::typeFromAST(const ASTNode * ast) {
  QualifiedType qual = qualifiedTypeFromAST(ast);
  if (qual.qualifiers() != 0) {
    diag.error(ast) << "Type modifier not valid in this context " << qual;
  }
  return const_cast<Type *>(qual.unqualified());
}

QualifiedType TypeAnalyzer::qualifiedTypeFromAST(const ASTNode * ast, unsigned defaultQualifiers) {
  if (ast == NULL) {
    return NULL;
  }

  const SourceLocation & loc = ast->location();
  switch (ast->nodeType()) {
    case ASTNode::Id:
    case ASTNode::QName:
    case ASTNode::Member:
    case ASTNode::Specialize: {
      // Most of the work is done by lookupName. The rest is just validating
      // the result and making sure it's a type.
      LookupResults typeSyms;
      lookupName(typeSyms, ast, lookupOptions_);

      if (typeSyms.empty()) {
        diag.error(loc) << "Undefined type '" << ast << "'";
        lookupName(typeSyms, ast, lookupOptions_);
        return &BadType::instance;
      }

      QualifiedTypeList typeList;
      if (getLookupResultTypes(loc, typeSyms, typeList, defaultQualifiers)) {
        // TODO: Handle ambiguous type resolution.
        if (typeList.size() > 1) {
          diag.error(loc) << "Multiple definitions for '" << ast << "'";
          return &BadType::instance;
        }

        return typeList.front();
      }

      diag.error(loc) << "'" << ast << "' is not a type expression";
      for (LookupResults::iterator it = typeSyms.begin(); it != typeSyms.end(); ++it) {
        diag.info(it->location()) << Format_Verbose << *it;
      }

      return &BadType::instance;
    }

    case ASTNode::Array: {
      const ASTOper * arrayOp = static_cast<const ASTOper *>(ast);
      QualifiedType elementType = qualifiedTypeFromAST(arrayOp->arg(0));
      DASSERT(elementType);
      return const_cast<CompositeType *>(getArrayTypeForElement(elementType));
    }

    case ASTNode::BuiltIn: {
      Defn * def = static_cast<const ASTBuiltIn *>(ast)->value();
      if (TypeDefn * tdef = dyn_cast<TypeDefn>(def)) {
        return tdef->value();
      } else {
        diag.fatal(ast) << "'" << def->name() << "' is not a type";
        return &BadType::instance;
      }
    }

    case ASTNode::LogicalOr: {
      QualifiedTypeList unionTypes;
      if (getUnionTypes(ast, unionTypes)) {
        return UnionType::get(unionTypes);
      }

      return &BadType::instance;
    }

    case ASTNode::Tuple: {
      const ASTOper * tupleOp = static_cast<const ASTOper *>(ast);
      return tupleTypeFromASTNodeList(tupleOp->args());
    }

    case ASTNode::AnonFn: {
      const ASTFunctionDecl * fnDecl = static_cast<const ASTFunctionDecl *>(ast);
      FunctionType * ftype = functionTypeFromAST(fnDecl);
      if (isErrorResult(ftype)) {
        return ftype;
      }

      if (ftype->returnType().isNull()) {
        ftype->setReturnType(&VoidType::instance);
      }

      if (fnDecl->modifiers().flags & Static) {
        ftype->setIsStatic(true);
      } else {
        return const_cast<CompositeType *>(getFunctionInterfaceType(ftype));
      }

      return ftype;
    }

    case ASTNode::GetElement: {
      // Easiest way to handle this is to try and evaluate it as an expression, and see if
      // the result is a type literal.
      Expr * typeExpr = ExprAnalyzer(this, currentFunction()).reduceExpr(ast, NULL);
      if (isErrorResult(typeExpr)) {
        return &BadType::instance;
      } else if (TypeLiteralExpr * type = dyn_cast_or_null<TypeLiteralExpr>(typeExpr)) {
        return type->value();
      } else {
        diag.error(ast) << "Type name expected";
        return &BadType::instance;
      }
    }

    case ASTNode::TypeVar:
      return reduceTypeVariable(static_cast<const ASTTypeVariable *>(ast));

    case ASTNode::Address: {
      const ASTOper * op = static_cast<const ASTOper *>(ast);
      QualifiedType ty = qualifiedTypeFromAST(op->arg(0), defaultQualifiers);
      if (!isErrorResult(ty)) {
        if (defaultQualifiers) {
          //ty.addQualifiers(defaultQualifiers);
        }
        return AddressType::get(ty);
      }
      return &BadType::instance;
    }

    case ASTNode::FlexArray: {
       const ASTOper * op = static_cast<const ASTOper *>(ast);
       QualifiedType ty = qualifiedTypeFromAST(op->arg(0));
       if (!ty.isNull()) {
         return FlexibleArrayType::get(TupleType::get(ty));
       }
       return &BadType::instance;
     }

    case ASTNode::TypeAlias:
      DFAIL("Implement");
      break;

    case ASTNode::TypeModReadOnly:
    case ASTNode::TypeModMutable:
    case ASTNode::TypeModImmutable:
    case ASTNode::TypeModAdopted:
    case ASTNode::TypeModVolatile: {
      const ASTOper * op = static_cast<const ASTOper *>(ast);
      QualifiedType baseType = qualifiedTypeFromAST(op->arg(0));
      if (isErrorResult(baseType)) {
        return baseType;
      }

      switch (int(ast->nodeType())) {
        case ASTNode::TypeModReadOnly:  return baseType | QualifiedType::READONLY;
        case ASTNode::TypeModMutable:   return baseType | QualifiedType::MUTABLE;
        case ASTNode::TypeModImmutable: return baseType | QualifiedType::IMMUTABLE;
        case ASTNode::TypeModAdopted:   return baseType | QualifiedType::ADOPTED;
        case ASTNode::TypeModVolatile:  return baseType | QualifiedType::VOLATILE;
      }
      return &BadType::instance;
    }

    case ASTNode::Qualify: {
      const ASTOper * op = static_cast<const ASTOper *>(ast);
      const Type * fnVal = typeFromAST(op->arg(0));
      QualifiedType arg = qualifiedTypeFromAST(op->arg(1));
      if (isErrorResult(arg)) {
        return &BadType::instance;
      }
      const TupleType * args = TupleType::get(arg);
      if (const TypeFunction * tfn = dyn_cast<TypeFunction>(fnVal)) {
        return tfn->apply(args);
      }
      return new TypeFunctionCall(fnVal, args);
    }

    default:
      diag.fatal(ast) << "invalid node type " << nodeTypeName(ast->nodeType());
      break;
  }
  return NULL;
}

void TypeAnalyzer::undefinedType(const ASTNode * ast) {
  diag.fatal(ast) << "Undefined type '" << ast << "'";
  if (diag.enableVerboseErrors()) {
    diag.writeLnIndent("Scopes searched:");
    dumpScopeHierarchy();
  }
}

bool TypeAnalyzer::typeDefnListFromAST(const ASTNode * ast, DefnList & defns) {
  LookupResults results;
  lookupName(results, ast);
  const SourceLocation & loc = ast->location();
  for (LookupResults::iterator it = results.begin(); it != results.end();
      ++it) {
    if (TypeDefn * tdef = dyn_cast<TypeDefn>(it->defn())) {
      defns.push_back(tdef);
    } else {
      diag.fatal(loc) << "'" << *it << "' is not a type.";
    }
  }

  return !defns.empty();
}

FunctionType * TypeAnalyzer::functionTypeFromAST(const ASTFunctionDecl * ast) {
  QualifiedType returnType = qualifiedTypeFromAST(ast->returnType());
  const ASTParamList & astParams = ast->params();
  ParameterList params;
  for (ASTParamList::const_iterator it = astParams.begin(); it != astParams.end(); ++it) {
    ASTParameter * aparam = *it;

    if (aparam->flags() & Param_Star) {
      QualifiedType paramType = qualifiedTypeFromAST(aparam->type());
      if (const TupleType * paramTypes = dyn_cast<TupleType>(paramType.type())) {
        if (aparam->flags() & Param_Variadic) {
          diag.error(aparam) << "*Type params cannot be variadic";
        } else if (aparam->value() != NULL) {
          diag.error(aparam) << "*Type params cannot have default values";
        }

        for (TupleType::const_iterator it = paramTypes->begin(); it != paramTypes->end(); ++it) {
          // TODO - give each param a name.
          // TODO - make an alias for the params that can be used within the function body
          // accessible as an array.
          QualifiedType paramType = *it;
          ParameterDefn * param = new ParameterDefn(module_, "");
          setParamType(param, paramType);
          params.push_back(param);
          if (aparam->flags() & Param_KeywordOnly) {
            param->setFlag(ParameterDefn::KeywordOnly, true);
          }
        }
      } else if (paramType->typeClass() != Type::TypeVar) {
        diag.error(aparam) << "Type " << paramType << " cannot be expanded with '*'";
      }
    } else {
      // Note that type might be NULL if not specified. We'll pick it up
      // later from the default value.
      unsigned defaultQualifiers = 0;
      if (!(aparam->flags() & Param_Variadic)) {
        defaultQualifiers = QualifiedType::READONLY;
      }
      QualifiedType paramType = qualifiedTypeFromAST(aparam->type(), defaultQualifiers);
      ParameterDefn * param = new ParameterDefn(module_, aparam);
      if (aparam->flags() & Param_Variadic) {
        param->setFlag(ParameterDefn::Variadic, true);
      }

      if (aparam->flags() & Param_KeywordOnly) {
        param->setFlag(ParameterDefn::KeywordOnly, true);
      }
      setParamType(param, paramType);
      params.push_back(param);
    }
  }

  return new FunctionType(returnType, params);
}

void TypeAnalyzer::setParamType(ParameterDefn * param, QualifiedType paramType) {
//  if (paramType.isAdopted()) {
//    paramType.removeQualifiers(QualifiedType::MUTABILITY_MASK);
//    paramType.addQualifiers(QualifiedType::ADOPTED);
//  } else if (paramType.isMutable()) {
//    paramType.removeQualifiers(QualifiedType::MUTABILITY_MASK);
//  } else  if (!paramType.isImmutable()) {
//    paramType.removeQualifiers(QualifiedType::MUTABILITY_MASK);
//    if (!param->isVariadic() && isQualifiableType(paramType.unqualified())) {
//      paramType.addQualifiers(QualifiedType::READONLY);
//    }
//  }
  param->setType(paramType);
  param->setInternalType(paramType);
}

TupleType * TypeAnalyzer::tupleTypeFromASTNodeList(const ASTNodeList & args) {
  QualifiedTypeList fieldTypes;
  for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
    QualifiedType fieldType = qualifiedTypeFromAST(*it);
    if (isErrorResult(fieldType)) {
      return NULL;
    }

    fieldTypes.push_back(fieldType);
  }

  return TupleType::get(fieldTypes.begin(), fieldTypes.end());
}

Type * TypeAnalyzer::reduceTypeVariable(const ASTTypeVariable * ast) {
  diag.error(ast) << "Type variable used outside of pattern.";
  return &BadType::instance;
}

bool TypeAnalyzer::getUnionTypes(const ASTNode * ast, QualifiedTypeList & result) {
  if (ast->nodeType() == ASTNode::LogicalOr) {
    const ASTOper * unionOp = static_cast<const ASTOper *>(ast);
    const ASTNodeList & args = unionOp->args();
    for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
      if (!getUnionTypes(*it, result)) {
        return false;
      }
    }
  } else {
    QualifiedType elementType = qualifiedTypeFromAST(ast);
    if (isErrorResult(elementType)) {
      return false;
    }

    result.push_back(elementType);
  }

  return true;
}

} // namespace tart

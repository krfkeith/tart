/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/ASTDecl.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/Module.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"
#include "tart/Common/InternedString.h"
#include "tart/Objects/Builtins.h"

namespace tart {

Type * TypeAnalyzer::typeFromAST(const ASTNode * ast) {
  if (ast == NULL) {
    return NULL;
  }

  const SourceLocation & loc = ast->location();
  switch (ast->nodeType()) {
    case ASTNode::Id:
    case ASTNode::Member:
    case ASTNode::Specialize: {
      // Most of the work is done by lookupName. The rest is just validating
      // the result and making sure it's a type.
      ExprList typeExprs;
      lookupName(typeExprs, ast, lookupOptions_);

      if (typeExprs.empty()) {
        diag.error(loc) << "Undefined type '" << ast << "'";
        //lookupName(typeExprs, ast, lookupOptions_);
        return &BadType::instance;
      }

      TypeList typeList;
      if (getTypesFromExprs(loc, typeExprs, typeList)) {
        if (typeList.size() > 1) {
          diag.fatal(loc) << "Multiple definitions for '" << ast << "'";
          return &BadType::instance;
        }

        return typeList.front();
      }

      diag.error(loc) << "'" << ast << "' is not a type expression";
      for (ExprList::iterator it = typeExprs.begin(); it != typeExprs.end(); ++it) {
        diag.info(*it) << Format_Verbose << *it << " (" << (*it)->exprType() << ")";
      }

      return &BadType::instance;
    }

    case ASTNode::Array: {
      const ASTUnaryOp * arrayOp = static_cast<const ASTUnaryOp *>(ast);
      Type * elementType = typeFromAST(arrayOp->arg());
      DASSERT(elementType != NULL);
      return getArrayTypeForElement(elementType);
    }

    case ASTNode::BuiltIn: {
      Defn * def = static_cast<const ASTBuiltIn *>(ast)->value();
      if (TypeDefn * tdef = dyn_cast<TypeDefn>(def)) {
        return tdef->typeValue();
      } else {
        diag.fatal(ast) << "'" << def->name() << "' is not a type";
        return &BadType::instance;
      }
    }

    case ASTNode::LogicalOr: {
      ConstTypeList unionTypes;
      if (getUnionTypes(ast, unionTypes)) {
        return UnionType::get(unionTypes);
      }

      return &BadType::instance;
    }

    case ASTNode::Tuple: {
      const ASTOper * tupleOp = static_cast<const ASTOper *>(ast);
      const ASTNodeList & args = tupleOp->args();
      ConstTypeList fieldTypes;
      for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
        Type * fieldType = typeFromAST(*it);
        if (isErrorResult(fieldType)) {
          return false;
        }

        fieldTypes.push_back(fieldType);
      }

      return TupleType::get(fieldTypes.begin(), fieldTypes.end());
    }

    case ASTNode::AnonFn: {
      const ASTFunctionDecl * fnDecl = static_cast<const ASTFunctionDecl *>(ast);
      FunctionType * ftype = typeFromFunctionAST(fnDecl);
      if (isErrorResult(ftype)) {
        return ftype;
      }

      if (ftype->returnType() == NULL) {
        ftype->setReturnType(&VoidType::instance);
      }

      if (fnDecl->storageClass() == Storage_Static) {
        ftype->setIsStatic(true);
      } else {
        return getFunctionInterfaceType(ftype);
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
        return const_cast<Type *>(type->value());
      } else {
        diag.error(ast) << "Type name expected";
        return &BadType::instance;
      }
    }

    case ASTNode::TypeVar:
      return reduceTypeVariable(static_cast<const ASTTypeVariable *>(ast));

    case ASTNode::TypeAlias:
      DFAIL("Implement");

    default:
      diag.fatal(ast) << "invalid node type " << nodeTypeName(ast->nodeType());
      DFAIL("Unsupported node type");
  }
}

void TypeAnalyzer::undefinedType(const ASTNode * ast) {
  diag.fatal(ast) << "Undefined type '" << ast << "'";
  diag.writeLnIndent("Scopes searched:");
  dumpScopeHierarchy();
}

bool TypeAnalyzer::typeDefnListFromAST(const ASTNode * ast, DefnList & defns) {
  ExprList results;
  lookupName(results, ast);
  const SourceLocation & loc = ast->location();
  for (ExprList::iterator it = results.begin(); it != results.end();
      ++it) {
    if (TypeLiteralExpr * ctype = dyn_cast<TypeLiteralExpr>(*it)) {
      if (TypeDefn * tdef = ctype->value()->typeDefn()) {
        defns.push_back(tdef);
      } else {
        diag.fatal(loc) << "'" << ctype << "' is not a named type.";
      }
    } else {
      diag.fatal(loc) << "'" << *it << "' is not a type.";
    }
  }

  return !defns.empty();
}

FunctionType * TypeAnalyzer::typeFromFunctionAST(const ASTFunctionDecl * ast) {
  Type * returnType = typeFromAST(ast->returnType());
  const ASTParamList & astParams = ast->params();
  ParameterList params;
  for (ASTParamList::const_iterator it = astParams.begin(); it != astParams.end(); ++it) {
    ASTParameter * aparam = *it;

    if (aparam->flags() & Param_Star) {
      Type * paramType = typeFromAST(aparam->type());
      if (const TupleType * paramTypes = dyn_cast<TupleType>(paramType)) {
        if (aparam->flags() & Param_Variadic) {
          diag.error(aparam) << "*Type params cannot be variadic";
        } else if (aparam->value() != NULL) {
          diag.error(aparam) << "*Type params cannot have default values";
        }

        for (TupleType::const_iterator it = paramTypes->begin(); it != paramTypes->end(); ++it) {
          // TODO - give each param a name.
          // TODO - make an alias for the params that can be used within the function body
          // accessible as an array.
          const Type * ptype = *it;
          ParameterDefn * param = new ParameterDefn(module_, "");
          param->setType(ptype);
          param->setInternalType(ptype);
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
      Type * paramType = typeFromAST(aparam->type());
      ParameterDefn * param = new ParameterDefn(module_, aparam);
      param->setType(paramType);
      param->setInternalType(paramType);
      params.push_back(param);
      if (aparam->flags() & Param_Variadic) {
        param->setFlag(ParameterDefn::Variadic, true);
      }

      if (aparam->flags() & Param_KeywordOnly) {
        param->setFlag(ParameterDefn::KeywordOnly, true);
      }
    }
  }

  return new FunctionType(returnType, params);
}

Type * TypeAnalyzer::reduceTypeVariable(const ASTTypeVariable * ast) {
  diag.error(ast) << "Type variable used outside of pattern.";
  return &BadType::instance;
}

bool TypeAnalyzer::getUnionTypes(const ASTNode * ast, ConstTypeList & result) {
  if (ast->nodeType() == ASTNode::LogicalOr) {
    const ASTOper * unionOp = static_cast<const ASTOper *>(ast);
    const ASTNodeList & args = unionOp->args();
    for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
      if (!getUnionTypes(*it, result)) {
        return false;
      }
    }
  } else {
    Type * elementType = typeFromAST(ast);
    if (isErrorResult(elementType)) {
      return false;
    }

    result.push_back(elementType);
  }

  return true;
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/ASTDecl.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/Module.h"
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
      lookupName(typeExprs, ast);

      if (typeExprs.empty()) {
        diag.error(loc) << "Undefined type '" << ast << "'";
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
        return UnionType::get(ast->location(), unionTypes);
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
        // Non-static functions must have a self param, even though we don't know what type
        // it is.
        /*ParameterDefn * selfParam = new ParameterDefn(module, istrings.idSelf);
        selfParam->setType(Builtins::typeObject);
        selfParam->setInternalType(Builtins::typeObject);
        selfParam->addTrait(Defn::Singular);
        selfParam->setFlag(ParameterDefn::Reference, true);
        ftype->setSelfParam(selfParam);*/
      }

      return ftype;
    }

    case ASTNode::GetElement: {
      // Easiest way to handle this is to try and evaluate it as an expression, and see if
      // the result is a type literal.
      Expr * typeExpr = ExprAnalyzer(module, activeScope, subject(), currentFunction())
          .reduceExpr(ast, NULL);
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

    // Note that type might be NULL if not specified. We'll pick it up
    // later from the default value.
    Type * paramType = typeFromAST(aparam->type());
    ParameterDefn * param = new ParameterDefn(NULL, aparam);
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

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/ASTDecl.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/Module.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"

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
        diag.fatal(loc) << "Undefined type '" << ast << "'";
        return &BadType::instance;
      }

      DefnList typeList;
      if (getTypesFromExprs(loc, typeExprs, typeList)) {
        if (typeList.size() > 1) {
          diag.fatal(loc) << "Multiple definitions for '" << ast << "'";
          return &BadType::instance;
        }

        TypeDefn * tdef = static_cast<TypeDefn *>(typeList.front());
        Type * type = tdef->typeValue();
        if (type->typeClass() == Type::NativePointer) {
          AnalyzerBase::analyzeTypeDefn(tdef, Task_PrepCallOrUse);
          //analyzeLater(tdef);
          //DFAIL("Implement");
          //NativePointerTypeAnalyzer(
          //  static_cast<NativePointerType *>(type)).analyze(Task_InferType);
        } else /*if (tdef->defnType() != Defn::TypeParameter)*/ {
          analyzeLater(tdef);
        }

        return type;
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

      CompositeType * arrayType = getArrayTypeForElement(elementType);
      if (arrayType->isSingular()) {
        analyzeLater(arrayType->typeDefn());
      }

      return arrayType;
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
      const ASTOper * unionOp = static_cast<const ASTOper *>(ast);
      const ASTNodeList & args = unionOp->args();
      TypeList unionTypes;

      for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
        Type * elementType = typeFromAST(*it);
        if (isErrorResult(elementType)) {
          return elementType;
        }

        unionTypes.push_back(elementType);
      }

      return UnionType::create(ast->location(), unionTypes);
    }

    case ASTNode::AnonFn: {
      FunctionType * ftype = typeFromFunctionAST(static_cast<const ASTFunctionDecl *>(ast));
      if (isErrorResult(ftype)) {
        return ftype;
      }

      if (ftype->returnType() == NULL) {
        ftype->setReturnType(&VoidType::instance);
      }

      return ftype;
    }

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
    if (ConstantType * ctype = dyn_cast<ConstantType>(*it)) {
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

FunctionType * TypeAnalyzer::typeFromFunctionAST( const ASTFunctionDecl * ast) {
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
      param->setInternalType(getArrayTypeForElement(paramType));
    }

    if (aparam->flags() & Param_KeywordOnly) {
      param->setFlag(ParameterDefn::KeywordOnly, true);
    }
  }

  return new FunctionType(returnType, params);
}

bool TypeAnalyzer::analyzeTypeExpr(Type * type) {
  TypeDefn * de = type->typeDefn();
  if (de != NULL) {
    analyzeTypeDefn(de, Task_PrepMemberLookup);
    analyzeLater(de);
  } else {
    switch (type->typeClass()) {
      case Type::Function:
        return true;

      case Type::Constraint:
      case Type::Tuple:
      case Type::Pattern:
      //case Type::Binding:
      default:
        DFAIL("Implement");
        break;
    }
  }

  return true;
}

} // namespace tart

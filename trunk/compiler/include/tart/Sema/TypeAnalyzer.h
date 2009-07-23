/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_SEMA_TYPEANALYZER_H
#define TART_SEMA_TYPEANALYZER_H

#ifndef TART_SEMA_ANALYZERBASE_H
#include "tart/Sema/AnalyzerBase.h"
#endif

#ifndef TART_CFG_COMPOSITETYPE_H
#include "tart/CFG/CompositeType.h"
#endif

namespace tart {
  
class ASTNode;
class ASTFunctionDecl;
class FunctionType;

/// -------------------------------------------------------------------
/// Analyzer class for type expressions.
class TypeAnalyzer : public AnalyzerBase {
protected:
  void undefinedType(const ASTNode * ast);

public:
  /** Constructor. */
  TypeAnalyzer(Module * mod, Scope * parent)
    : AnalyzerBase(mod, parent)
  {}
  
  /** Construct a type from an AST. */
  Type * typeFromAST(const ASTNode * ast);

  /** Given an AST, find all type definitions. */
  bool typeDefnListFromAST(const ASTNode * ast, DefnList & defns);

  /** Construct a function type from an AST. */
  FunctionType * typeFromFunctionAST(const ASTFunctionDecl * ast);
  
  /** Given an element type, return the corresponding array type. The element
      type must already have been fully resolved. */
  static CompositeType * getArrayTypeForElement(Type * elementType);
  
  /** Queue any type definitions for analysis. */
  bool analyzeTypeExpr(Type * type);
};

}

#endif

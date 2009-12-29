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
public:
  /** Constructor. */
  TypeAnalyzer(Module * mod, Scope * parent)
    : AnalyzerBase(mod, parent)
  {}

  virtual ~TypeAnalyzer() {}

  /** Construct a type from an AST. */
  Type * typeFromAST(const ASTNode * ast);

  /** Given an AST, find all type definitions. */
  bool typeDefnListFromAST(const ASTNode * ast, DefnList & defns);

  /** Construct a function type from an AST. */
  FunctionType * typeFromFunctionAST(const ASTFunctionDecl * ast);

  virtual Type * reduceTypeVariable(const ASTTypeVariable * ast);

  bool getUnionTypes(const ASTNode * ast, ConstTypeList & result);

protected:
  void undefinedType(const ASTNode * ast);
};

}

#endif

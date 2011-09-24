/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_TYPEANALYZER_H
#define TART_SEMA_TYPEANALYZER_H

#ifndef TART_SEMA_ANALYZERBASE_H
#include "tart/Sema/AnalyzerBase.h"
#endif

#ifndef TART_TYPE_COMPOSITETYPE_H
#include "tart/Type/CompositeType.h"
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
  TypeAnalyzer(Module * mod, Scope * activeScope)
    : AnalyzerBase(mod, activeScope)
    , lookupOptions_(LOOKUP_DEFAULT)
  {}

  /** Constructor. */
  TypeAnalyzer(AnalyzerBase * parent, Scope * activeScope)
    : AnalyzerBase(parent->module(), activeScope, parent->subject(), parent->currentFunction())
    , lookupOptions_(LOOKUP_DEFAULT)
  {}

  virtual ~TypeAnalyzer() {}

  /** Set the options for looking up type names. */
  void setTypeLookupOptions(LookupOptions lookupOptions) {
    lookupOptions_ = lookupOptions;
  }

  /** Construct a type from an AST. */
  Type * typeFromAST(const ASTNode * ast);

  /** Construct a qualified type from an AST. */
  QualifiedType qualifiedTypeFromAST(const ASTNode * ast);

  /** Given an AST, find all type definitions. */
  bool typeDefnListFromAST(const ASTNode * ast, DefnList & defns);

  /** Construct a function type from an AST. */
  FunctionType * typeFromFunctionAST(const ASTFunctionDecl * ast);

  virtual Type * reduceTypeVariable(const ASTTypeVariable * ast);

  bool getUnionTypes(const ASTNode * ast, QualifiedTypeList & result);

protected:
  void undefinedType(const ASTNode * ast);
  LookupOptions lookupOptions_;
};

}

#endif

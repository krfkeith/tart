/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_DECLANALYZER_H
#define TART_SEMA_DECLANALYZER_H

#ifndef TART_SEMA_ANALYZERBASE_H
#include "tart/Sema/AnalyzerBase.h"
#endif

#ifndef TART_CFG_DECL_H
#include "tart/CFG/Defn.h"
#endif

#ifndef TART_CFG_COMPOSITETYPE_H
#include "tart/CFG/CompositeType.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Base class for analyzers that operate on declarations (class, function,
/// etc.)
class DefnAnalyzer : public AnalyzerBase {
public:
  /** Constructor. */
  DefnAnalyzer(Module * mod, Scope * parent, Defn * subject, FunctionDefn * currentFunction)
    : AnalyzerBase(mod, parent, subject, currentFunction)
  {}

  /** Do a full analysis of the target module. */
  bool analyzeModule();

  /** Run the specified passes. */
  bool analyze(Defn * in, DefnPasses & passes);

  /** Call the ScopeBuilder to create members of this defn. */
  bool createMembersFromAST(Defn * in);

  /** Resolve attributes explicitly called out in the AST. */
  bool resolveAttributes(Defn * in);

  /** Propagate attributes from a base type to its subtypes. */
  bool propagateSubtypeAttributes(Defn * baseDefn, Defn * target);

  /** Propagate attributes from an enclosing scope to it's members. */
  bool propagateMemberAttributes(Defn * scopeDefn, Defn * target);

  /** Add an additional attribute propagated from some other source. Does nothing if
      an attribute of the same type is already present. */
  bool propagateAttribute(Defn * in, Expr * attr);

  void applyAttributes(Defn * in);
  void applyAttribute(Defn * de, ConstantObjectRef * attrObj, FunctionDefn * applyMethod);
  void handleIntrinsicAttribute(Defn * de, Expr * attrCtor);
  void handleAttributeAttribute(Defn * de, ConstantObjectRef * attrObj);

  /** Resolve the target of an import statement, and add a definition
      to the target scope. */
  void importIntoScope(const ASTImport * import, Scope * targetScope);

  /** Analyze the template signature for this declaration */
  static void analyzeTemplateSignature(Defn * de);

  /** Add requested passes to the toRun set if not already run. */
  static void addPass(Defn * de, DefnPasses & toRun, const DefnPass requested);
  static void addPasses(Defn * de, DefnPasses & toRun, const DefnPasses & requested);

  /** Get the module in which a definition was defined - includes code to handle
      template instances which are defined in no module, but whose template definition
      is in a module. */
  Module * moduleForDefn(const Defn * def);
};

}

#endif

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
  DefnAnalyzer(Module * mod, Scope * parent)
    : AnalyzerBase(mod, parent)
  {}

  /** Do a full analysis of the target module. */
  bool analyzeModule();

  /** Run the specified passes. */
  bool analyze(Defn * in, DefnPasses & passes);

  /** Call the ScopeBuilder to create members of this defn. */
  bool createMembersFromAST(Defn * in);

  bool resolveAttributes(Defn * in);
  void applyAttributes(Defn * in);
  void applyAttribute(Defn * de, ConstantObjectRef * attrObj, FunctionDefn * applyMethod);
  void handleIntrinsicAttribute(Defn * de, Expr * attrCtor);
  void handleAttributeAttribute(Defn * de, ConstantObjectRef * attrObj);

  /** Resolve the target of an import statement, and add a definition
      to the target scope. */
  void importIntoScope(const ASTImport * import, Scope * targetScope);

  /** Given a list of definitions (the result of a lookup), find the one
      that best matches the template arguments, and return a specialization
      of that defn. */
  Defn * specialize(const SourceLocation & loc, DefnList & defs,
      const ASTNodeList & templateArgs);

  /** Analyze the template signature for this declaration */
  static void analyzeTemplateSignature(Defn * de);

  /** Add requested passes to the toRun set if not already run. */
  static void addPasses(Defn * de, DefnPasses & toRun, const DefnPasses & requested);

  /** Get the module in which a definition was defined - includes code to handle
      template instances which are defined in no module, but whose template definition
      is in a module. */
  Module * moduleForDefn(const Defn * def);
};

}

#endif

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

class SystemClass;

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
  void importIntoScope(const ASTImport * import, IterableScope * targetScope);

  /** Analyze the template signature for this declaration */
  static void analyzeTemplateSignature(Defn * de);

  /** Add 'in' to the set of reflected types, and import any types needed to store
      the reflection info. */
  void addReflectionInfo(Defn * in);

  /** Add a type to the set of types reflected within a module. */
  bool reflectType(const Type * type);
  void reflectTypeMembers(CompositeType * type);

  /** Import a system class into the module. */
  bool importSystemType(const SystemClass & sclass);

  /** Get the module in which a definition was defined - includes code to handle
      template instances which are defined in no module, but whose template definition
      is in a module. */
  Module * moduleForDefn(const Defn * def);
};

}

#endif

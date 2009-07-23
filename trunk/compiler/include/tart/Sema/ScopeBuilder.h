/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_SEMA_SCOPEBUILDER_H
#define TART_SEMA_SCOPEBUILDER_H

#ifndef TART_CFG_DECL_H
#include "tart/CFG/Defn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// ScopeBuilder - starting with a root-level definition, build entries
/// for all of the member definitions, and their members as well.
class ScopeBuilder {
public:
  /** Set up the scope represented by this definition, by populating it
      with all of the members. */
  static void createScopeMembers(Defn * parent);
  static void createScopeMembers(Defn * parent, const ASTDeclList & decs);
  static void createAccessors(PropertyDefn * prop);

  /** Create a new definition in the current scope from the given decl. */
  static Defn * createDefn(Scope * parent, Module * m, const ASTDecl * de);
  static Defn * createMemberDefn(
      Scope * parentScope, Defn * parentDefn, const ASTDecl * de);
  static Defn * createLocalDefn(
      Scope * parentScope, Defn * parentDefn, const ASTDecl * de);
  static Defn * createTemplateDefn(
      Scope * parent, Module * m, const ASTTemplate * tp);
  static Defn * mergeNamespace(Scope * parent, const ASTDecl * de);

  /** Ensure that it's OK to define a variable with the given name in
      this local scope. */
  static void checkVariableHiding(Scope * scope, const Defn * de);
  
  /** Ensure that it's OK to define a variable with the given name in
      this non-local scope. */
  static void checkNameConflict(Scope * scope, const Defn * de);
};

}

#endif

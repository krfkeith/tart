/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_NAMESPACEDEFN_H
#define TART_CFG_NAMESPACEDEFN_H

#ifndef TART_CFG_DEFN_H
#include "tart/CFG/Defn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A definition of a namespace
class NamespaceDefn : public Defn {
public:
  enum AnalysisPass {
    ImportPass,
    ScopeCreationPass,
    StaticInitializerPass,
    PassCount
  };

  typedef tart::PassMgr<AnalysisPass, PassCount> PassMgr;
  typedef PassMgr::PassSet PassSet;

  /** Constructor that takes a name */
  NamespaceDefn(Module * m, const char * name);

  /** Constructor that takes an AST declaration. */
  NamespaceDefn(Module * m, const ASTDecl * de);

  /** Get the scope containing the members of this namespace. */
  const IterableScope & memberScope() const { return members; }
  IterableScope & memberScope() { return members; }

  /** Which analysis passes are running / have run. */
  const PassMgr & passes() const { return passes_; }
  PassMgr & passes() { return passes_; }

  // Overrides

  Scope * definingScope() const { return members.parentScope(); }
  void setDefiningScope(Scope * scope) { members.setParentScope(scope); }
  void format(FormatStream & out) const;
  void trace() const;

  static inline bool classof(const NamespaceDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Namespace;
  }

private:
  IterableScope members;
  PassMgr passes_;
};

} // namespace tart

#endif

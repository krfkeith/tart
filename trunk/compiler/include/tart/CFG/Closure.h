/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_CLOSURE_H
#define TART_CFG_CLOSURE_H

#ifndef TART_CFG_EXPR_H
#include "tart/CFG/Expr.h"
#endif

#ifndef TART_CFG_SCOPE_H
#include "tart/CFG/Scope.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A reference to a closure environment.
class ClosureEnvExpr : public Expr, public Scope {
public:
  /** Constructor. */
  ClosureEnvExpr(const SourceLocation & loc, Scope * parentScope)
    : Expr(ClosureEnv, loc, NULL)
    , parentScope_(parentScope)
  {
  }

  // Expr Overrides

  void format(FormatStream & out) const;
  bool isSideEffectFree() const { return true; }
  bool isSingular() const { return true; }
  void trace() const;

  // Scope Overrides

  Scope * parentScope() const { return parentScope_; }
  void setParentScope(Scope * parent) { parentScope_ = parent; }
  const SymbolTable & members() const { return members_; }
  SymbolTable & members() { return members_; }
  void addMember(Defn * d);
  bool lookupMember(const char * ident, DefnList & defs, bool inherit) const;
  Defn * firstMember() const { return members_.first(); }
  const SymbolTable::Entry * findSymbol(const char * key) const {
    return members_.findSymbol(key);
  }

  bool allowOverloads() { return true; }
  size_t count() { return members_.count(); }
  void clear() { members_.clear(); }
  void dumpHierarchy(bool full) const;

  static inline bool classof(const ClosureEnvExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ClosureEnv;
  }

private:
  OrderedSymbolTable members_;
  Scope * parentScope_;
};

} // namespace tart

#endif // TART_CFG_CLOSURE_H

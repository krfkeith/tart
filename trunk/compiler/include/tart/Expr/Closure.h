/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_EXPR_CLOSURE_H
#define TART_EXPR_CLOSURE_H

#ifndef TART_EXPR_EXPR_H
#include "tart/Expr/Expr.h"
#endif

#ifndef TART_DEFN_SCOPE_H
#include "tart/Defn/Scope.h"
#endif

#ifndef TART_TYPE_COMPOSITETYPE_H
#include "tart/Type/CompositeType.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A reference to a closure environment.
class ClosureEnvExpr : public Expr, public Scope {
public:
  /** Constructor. */
  ClosureEnvExpr(const SourceLocation & loc, Scope * parentScope, Scope * finalScope,
      CompositeType * envType, Expr * baseExpr)
    : Expr(ClosureEnv, loc, NULL)
    , parentScope_(parentScope)
    , finalScope_(finalScope)
    , envType_(envType)
    , baseExpr_(baseExpr)
  {
  }

  // Expr Overrides

  void format(FormatStream & out) const;
  bool isSideEffectFree() const { return true; }
  bool isSingular() const { return true; }
  void trace() const;

  CompositeType * envType() const { return envType_; }

  // Scope Overrides

  Scope * parentScope() const { return parentScope_; }
  void setParentScope(Scope * parent) { parentScope_ = parent; }
  const SymbolTable & members() const { return envType_->members(); }
  SymbolTable & members() { return envType_->members(); }
  void addMember(Defn * d);
  bool lookupMember(llvm::StringRef ident, DefnList & defs, bool inherit) const;
  Defn * firstMember() const { return envType_->firstMember(); }
  const SymbolTable::Entry * findSymbol(const char * key) const {
    return envType_->findSymbol(key);
  }
  virtual Expr * baseExpr() { return baseExpr_; }

  bool allowOverloads() { return true; }
  size_t count() { return envType_->members().count(); }
  void clear() { envType_->members().clear(); }
  void dumpHierarchy(bool full) const;

  static inline bool classof(const ClosureEnvExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ClosureEnv;
  }

private:
  Scope * parentScope_;
  Scope * finalScope_;
  CompositeType * envType_;
  Expr * baseExpr_;
};

} // namespace tart

#endif // TART_EXPR_CLOSURE_H

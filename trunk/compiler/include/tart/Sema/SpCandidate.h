/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_SPECIALIZECANDIDATE_H
#define TART_SEMA_SPECIALIZECANDIDATE_H

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#ifndef TART_SEMA_BINDINGENV_H
#include "tart/Sema/BindingEnv.h"
#endif

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A candidate for template specialization
class SpCandidate : public GC {
private:
  //Expr * base;
  Defn * templateDefn_;
  Expr * base_;
  TypeVector * args_;
  BindingEnv env_;
  ConversionRank conversionRank_;

public:
  SpCandidate(Expr *base, Defn * tdef, TypeVector * args);

  /** The template. */
  Defn * templateDefn() const { return templateDefn_; }

  /** The type arguments. */
  TypeVector * args() const { return args_; }

  /** Base expression used, if any. */
  Expr * base() const { return base_; }

  /** The binding environment which maps pattern variables to values. */
  const BindingEnv & env() const { return env_; }
  BindingEnv & env() { return env_; }

  /** Perform unification on the candidate and its arguments. */
  bool unify(SourceContext * source);

  /** Return true if this specified call candidate has the same type as this one. */
  bool isEqual(const SpCandidate * other) const;

  /** Return true if this candidate is more specific than the one given. */
  bool isMoreSpecific(const SpCandidate * other) const;

  /** Update the compatibility score for this candidate. */
  ConversionRank updateConversionRank();

  // Overrides

  void trace() const;
};

FormatStream & operator<<(FormatStream & out, const SpCandidate & sp);

} // namespace tart

#endif

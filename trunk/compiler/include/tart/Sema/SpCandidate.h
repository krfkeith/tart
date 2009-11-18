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
public:
  SpCandidate(Expr *base, Defn * tdef, TupleType * args);

  /** The definition that has type arguments. */
  Defn * def() const { return def_; }

  /** The type arguments. */
  TupleType * args() const { return args_; }

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

  /** Return the compatibility score for this candidate. */
  ConversionRank conversionRank() const { return conversionRank_; }

  // Overrides

  void trace() const;

private:
  Defn * def_;
  Expr * base_;
  TupleType * args_;
  const TupleType * params_;
  BindingEnv env_;
  ConversionRank conversionRank_;
};

FormatStream & operator<<(FormatStream & out, const SpCandidate & sp);

} // namespace tart

#endif

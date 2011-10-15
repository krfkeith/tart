/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_SPECIALIZECANDIDATE_H
#define TART_SEMA_SPECIALIZECANDIDATE_H

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#ifndef TART_DEFN_TEMPLATE_H
#include "tart/Defn/Template.h"
#endif

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_SEMA_BINDINGENV_H
#include "tart/Sema/BindingEnv.h"
#endif

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

namespace tart {

class RelabelTransform;

/// -------------------------------------------------------------------
/// A candidate for template specialization
class SpCandidate : public GC {
public:
  SpCandidate(Expr *base, Defn * tdef, const TupleType * args);

  /** The definition that has type arguments. */
  Defn * def() const { return def_; }

  /** The input type arguments. */
  const TupleType * args() const { return args_; }

  /** The list of template parameters. */
  const TupleType * params() const { return params_; }
  void setParams(const TupleType * params) {
    params_ = params;
  }

  /** Base expression used, if any. */
  Expr * base() const { return base_; }

  /** Rename all type variables in the type signature, as preparation for unification. */
  void relabelTypeVars(BindingEnv & env);

  /** Rename all type variables, using an externally-supplied mapping. */
  void relabelTypeVars(RelabelTransform & rt);

  /** Perform unification on the candidate and its arguments. */
  bool unify(SourceContext * source, BindingEnv & env);

  /** Convert this specialization candidate to a type. */
  Type * toType(SourceContext * source, BindingEnv & env);

  /** Return true if this specified call candidate has the same type as this one. */
  bool isEqual(const SpCandidate * other) const;

  /** Return true if this candidate is more specific than the one given. */
  bool isMoreSpecific(const SpCandidate * other) const;

  /** Update the compatibility score for this candidate. */
  ConversionRank updateConversionRank();

  /** Report any conversion errors. */
  void reportConversionErrors();

  /** Return the compatibility score for this candidate. */
  ConversionRank conversionRank() const { return conversionRank_; }

  // Overrides

  void trace() const;

private:
  Defn * def_;
  Expr * base_;
  const TupleType * args_;
  const TupleType * params_;
  QualifiedTypeList typeParamDefaults_;
  TemplateConditionList conditions_;
  ConversionRank conversionRank_;
};

FormatStream & operator<<(FormatStream & out, const SpCandidate & sp);

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_AMBIGUOUSPARAMETERTYPE_H
#define TART_TYPE_AMBIGUOUSPARAMETERTYPE_H

#ifndef TART_TYPE_TYPECONSTRAINT_H
#include "tart/Type/TypeConstraint.h"
#endif

namespace tart {

// Forward declarations
class CallCandidate;

/// -------------------------------------------------------------------
/// A type constraint representing a value that will be passed as an
/// argument to a method, where we don't know exactly which method
/// will be chosen yet.
class AmbiguousParameterType : public TypeSetConstraint {
public:
  AmbiguousParameterType(CallExpr * call, unsigned argIndex)
    : TypeSetConstraint(AmbiguousParameter)
    , callExpr_(call)
    , argIndex_(argIndex)
  {}

  /** The call expression. */
  CallExpr * expr() const { return callExpr_; }

  /** The list of call candidates. */
  const Candidates & candidates() const;

  /** The argument index. */
  unsigned argIndex() const { return argIndex_; }

  /** The Nth param type for a given candidate. */
  const Type * candidateParamType(const CallCandidate * cc) const;

  // Overrides

  void expand(TypeExpansion & out) const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const AmbiguousParameterType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == AmbiguousParameter;
  }

private:
  CallExpr * callExpr_;
  unsigned argIndex_;
};

} // namespace tart

#endif // TART_TYPE_AMBIGUOUSPARAMETERTYPE_H

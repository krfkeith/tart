/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_AMBIGUOUSPARAMETERTYPE_H
#define TART_TYPE_AMBIGUOUSPARAMETERTYPE_H

#ifndef TART_TYPE_AMBIGUOUSTYPE_H
#include "tart/Type/AmbiguousType.h"
#endif

namespace tart {

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

  /** The argument index. */
  unsigned argIndex() const { return argIndex_; }

  // Overrides

  void listProspects(ProspectList & out, const ProvisionSet & add) const;
  void expandImpl(QualifiedTypeSet & out, unsigned qualifiers) const;
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

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_AMBIGUOUSPHITYPE_H
#define TART_TYPE_AMBIGUOUSPHITYPE_H

#ifndef TART_TYPE_TYPECONSTRAINT_H
#include "tart/Type/TypeConstraint.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A type constraint representing a set of possible alternative values
/// which could be the result of expression. The goal will be to find
/// a common type which fits all of them.

class AmbiguousPhiType : public TypeConstraint {
public:
  AmbiguousPhiType(const Type * expected)
    : TypeConstraint(AmbiguousPhi)
    , expected_(expected)
    , common_(NULL)
  {}

  // Add a possible type.
  void add(const Type * type);

  // The set of input types
  const ConstTypeList & types() const { return types_; }

  // The common type of the input types
  const Type * common() const { return common_; }
  void setCommon(const Type * ty) const { common_ = ty; }

  // The type we're attempting to assign to, which might be NULL.
  const Type * expected() const { return expected_; }

  // Overrides

  void expand(TypeExpansion & out) const;
  const Type * singularValue() const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isReferenceType() const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const AmbiguousPhiType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == AmbiguousPhi;
  }

private:
  ConstTypeList types_;
  const Type * expected_;
  mutable const Type * common_;
};

} // namespace tart

#endif // TART_TYPE_AMBIGUOUSPHITYPE_H

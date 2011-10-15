/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_AMBIGUOUSPHITYPE_H
#define TART_TYPE_AMBIGUOUSPHITYPE_H

#ifndef TART_TYPE_AMBIGUOUSTYPE_H
#include "tart/Type/AmbiguousType.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A type constraint representing a set of possible alternative values
/// which could be the result of expression. The goal will be to find
/// a common type which fits all of them.

class AmbiguousPhiType : public AmbiguousType {
public:
  AmbiguousPhiType(QualifiedType expected)
    : AmbiguousType(AmbiguousPhi)
    , expected_(expected)
    , common_(NULL)
  {}

  // Add a possible type.
  void add(QualifiedType type);

  // The set of input types
  const QualifiedTypeList & types() const { return types_; }

  // The common type of the input types
  QualifiedType common() const { return common_; }
  void setCommon(QualifiedType ty) const { common_ = ty; }

  // The type we're attempting to assign to, which might be NULL.
  QualifiedType expected() const { return expected_; }

  // Overrides

  void listProspects(ProspectList & out, const ProvisionSet & add) const;
  void expandImpl(QualifiedTypeSet & out, unsigned qualifiers) const;
  QualifiedType singularValue() const;
  bool isSingular() const;
  bool isReferenceType() const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const AmbiguousPhiType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == AmbiguousPhi;
  }

private:
  QualifiedTypeList types_;
  QualifiedType expected_;
  mutable QualifiedType common_;
};

} // namespace tart

#endif // TART_TYPE_AMBIGUOUSPHITYPE_H

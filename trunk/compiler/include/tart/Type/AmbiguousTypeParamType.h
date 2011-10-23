/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_AMBIGUOUSTYPEPARAMTYPE_H
#define TART_TYPE_AMBIGUOUSTYPEPARAMTYPE_H

#ifndef TART_TYPE_AMBIGUOUSTYPE_H
#include "tart/Type/AmbiguousType.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// An ambiguous type representing the Nth type param of the base
/// type, which may itself be ambiguous.
class AmbiguousTypeParamType : public TypeSetConstraint {
public:
  AmbiguousTypeParamType(QualifiedType base, const Type * match, unsigned paramIndex)
    : TypeSetConstraint(AmbiguousTypeParam)
    , base_(base)
    , match_(match)
    , paramIndex_(paramIndex)
  {}

  /** Static helper function which either returns the Nth type param,
      or returns an ambiguous (i.e. lazy) type param type.
   */
  static QualifiedType forType(QualifiedType base, const Type * match, unsigned paramIndex);

  /** The base type. */
  QualifiedType base() const { return base_; }

  /** The class or interface that originally defined the type parameter we want. */
  const Type * match() const { return match_; }

  /** The param index. */
  unsigned paramIndex() const { return paramIndex_; }

  // Overrides

  void listProspects(ProspectList & out, const ProvisionSet & add) const;
  void expandImpl(QualifiedTypeSet & out, unsigned qualifiers) const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const AmbiguousTypeParamType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == AmbiguousTypeParam;
  }

private:
  QualifiedType base_;
  const Type * match_;
  unsigned paramIndex_;
};

} // namespace tart

#endif // TART_TYPE_AMBIGUOUSTYPEPARAMTYPE_H

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TUPLETYPE_H
#define TART_CFG_TUPLETYPE_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

namespace tart {

typedef llvm::SmallVector<const llvm::Type *, 16> IRTypeList;

// TODO: Eliminate union members that are subclasses of other members.
// TODO: Sort union members into a canonical order.
// TODO: Allow comparisons between union types independent of member declaration order.
// TODO: Fold equivalent unions into a single representation.
// TODO: Finish the trace() function for this type.

/// -------------------------------------------------------------------
/// Represents a tuple of values which may have different types.
class TupleType : public TypeImpl {
public:
  typedef TypeRefList::iterator iterator;
  typedef TypeRefList::const_iterator const_iterator;

  /** Construct a tuple of the given member types. */
  static TupleType * get(const TypeRef singleTypeArg);
  static TupleType * get(const_iterator first, const_iterator last);
  static TupleType * get(const TypeRefList & members) {
    return get(members.begin(), members.end());
  }

  /** Return the list of possible types for this union. */
  const TypeRefList & members() const { return members_; }

  const_iterator begin() const { return members_.begin(); }
  const_iterator end() const { return members_.end(); }
  size_t size() const { return members_.size(); }

  const TypeRef & operator[](int index) const { return members_[index]; }

  // Overrides

  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isEqual(const Type * other) const;
  bool isSingular() const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  bool includes(const Type * other) const;
  void format(FormatStream & out) const;
  void trace() const;

  static inline bool classof(const TupleType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Tuple;
  }

protected:
  /** Construct a tuple type */
  TupleType(TypeRefList::const_iterator first, TypeRefList::const_iterator last);

  TypeRefList members_;
};

} // namespace tart

#endif // TART_CFG_TUPLETYPE_H

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
  typedef ConstTypeList::iterator iterator;
  typedef ConstTypeList::const_iterator const_iterator;

  /** Construct a tuple of the given member types. */
  static TupleType * get(const Type * singleTypeArg);
  static TupleType * get(const_iterator first, const_iterator last);
  static TupleType * get(const TypeList & members) {
    return get(members.begin(), members.end());
  }
  static TupleType * get(const ConstTypeList & members) {
    return get(members.begin(), members.end());
  }

  /** Return the list of possible types for this union. */
  const ConstTypeList & members() const { return members_; }

  const_iterator begin() const { return members_.begin(); }
  const_iterator end() const { return members_.end(); }
  size_t size() const { return members_.size(); }

  const Type * operator[](int index) const { return members_[index]; }
  const Type * member(int index) const { return members_[index]; }

  // Overrides

  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isEqual(const Type * other) const;
  bool isSingular() const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  bool includes(const Type * other) const;
  void formatMembers(FormatStream & out) const;
  void format(FormatStream & out) const;
  void trace() const;
  size_t numTypeParams() const { return members_.size(); }
  const Type * typeParam(int index) const { return members_[index]; }

  static inline bool classof(const TupleType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Tuple;
  }

protected:
  /** Construct a tuple type */
  TupleType(const_iterator first, const_iterator last);

  ConstTypeList members_;
};

} // namespace tart

#endif // TART_CFG_TUPLETYPE_H

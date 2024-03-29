/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TUPLETYPE_H
#define TART_TYPE_TUPLETYPE_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

namespace tart {

typedef llvm::SmallVector<llvm::Type *, 16> IRTypeList;

/// -------------------------------------------------------------------
/// Represents a tuple of values which may have different types.
class TupleType : public TypeImpl {
public:
  typedef QualifiedTypeList::iterator iterator;
  typedef QualifiedTypeList::const_iterator const_iterator;

  /** Construct a tuple of the given member types. */
  static TupleType * get(const_iterator first, const_iterator last);
  static TupleType * get(llvm::ArrayRef<QualifiedType> members) {
    return get(members.begin(), members.end());
  }

  /** Return the list of possible types for this union. */
  const QualifiedTypeList & members() const { return members_; }

  const_iterator begin() const { return members_.begin(); }
  const_iterator end() const { return members_.end(); }
  size_t size() const { return members_.size(); }

  QualifiedType operator[](int index) const { return members_[index]; }
  QualifiedType member(int index) const { return members_[index]; }

  /** True if this contains an error type. */
  bool containsBadType() const;

  // Overrides

  llvm::Type * createIRType() const;
  llvm::Type * irParameterType() const;
  bool isSingular() const;
  bool isReferenceType() const { return false; }
  TypeShape typeShape() const;
  void formatMembers(FormatStream & out) const;
  void format(FormatStream & out) const;
  void trace() const;
  size_t numTypeParams() const { return members_.size(); }
  QualifiedType typeParam(int index) const { return members_[index]; }
  Expr * nullInitValue() const;
  bool containsReferenceType() const { return containsReferenceType_; }

  static inline bool classof(const TupleType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Tuple;
  }

protected:
  /** Construct a tuple type */
  TupleType(const_iterator first, const_iterator last);

  QualifiedTypeList members_;
  bool containsReferenceType_;
};

} // namespace tart

#endif // TART_TYPE_TUPLETYPE_H

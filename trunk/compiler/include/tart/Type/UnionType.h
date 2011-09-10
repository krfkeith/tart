/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_UNIONTYPE_H
#define TART_TYPE_UNIONTYPE_H

#ifndef TART_TYPE_TUPLETYPE_H
#include "tart/Type/TupleType.h"
#endif

namespace tart {

#define LLVM_UNION_SUPPORT 0

typedef llvm::SmallVector<llvm::Type *, 16> IRTypeList;

// -------------------------------------------------------------------
// Disjoint or union type.
class UnionType : public TypeImpl {
public:
  typedef ConstTypeList::iterator iterator;
  typedef ConstTypeList::const_iterator const_iterator;

  /** Return a union of the given element types. */
  static UnionType * get(const ConstTypeList & members);

  /** Return the list of possible types for this union. */
  const TupleType & members() const { return *members_; }

  const_iterator begin() const { return members_->begin(); }
  const_iterator end() const { return members_->end(); }
  size_t size() const { return members_->size(); }

  /** Return the type arguments for this union. */
  const TupleType * typeArgs() const { return members_; }

  /** Return the number of type parameters of this type. */
  size_t numTypeParams() const;

  /** Return the number of type parameters of this type that are value types. */
  size_t numValueTypes() const { return numValueTypes_; }

  /** Return the number of type parameters of this type that are reference types. */
  size_t numReferenceTypes() const { return numReferenceTypes_; }

  /** Return the Nth type parameter. */
  const Type * typeParam(int index) const;

  /** Given a type, return the index of this type. */
  int getTypeIndex(const Type * type) const;

  /** Whether the 'void' type is included. */
  size_t hasVoidType() const { return hasVoidType_; }

  /** Whether the 'Null' type is included. */
  size_t hasNullType() const { return hasNullType_; }

  /** Return true if this union contains only reference types. (Including Null). This means
      that the type can be represented as a single pointer with no discriminator field. */
  bool hasRefTypesOnly() const;

  /** Return true if this type is a union of a single type with either null or void.
      (Null if it's a reference type, void if it's a value type.) The 'optional' keyword
      creates unions of this type.
   */
  bool isSingleOptionalType() const;

  /** Return true if this type is a union of a single reference type with null. */
  bool isSingleNullableType() const;

  /* Return the first member type that is neither null nor void. */
  const Type * getFirstNonVoidType() const;

  /** Create a typecast from this type to the desired type. */
  Expr * createDynamicCast(Expr * from, const Type * toType) const;

  /** Return true if the composite type 'toType' is a subclass of any of the member types. */
  bool isSubtypeOfOfAnyMembers(const CompositeType * toType) const;

  /** Return true if the composite type 'toType' is a subclass of all members not counting
      the null type. */
  bool isSupertypeOfAllMembers(const CompositeType * toType) const;

  // Overrides

  llvm::Type * createIRType() const;
  llvm::Type * irParameterType() const;
  llvm::Type * getDiscriminatorType() const;
  bool isSingular() const;
  bool isReferenceType() const { return false; }
  TypeShape typeShape() const;
  void format(FormatStream & out) const;
  void trace() const;
  Expr * nullInitValue() const;
  bool containsReferenceType() const;

  static inline bool classof(const UnionType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Union;
  }

protected:
  /** Construct a disjoint union type */
  UnionType(TupleType * members);

  // Given an IR type, return an estimate of the size of this type.
  static size_t estimateTypeSize(llvm::Type * type);

  TupleType * members_;
  size_t numValueTypes_;
  size_t numReferenceTypes_;
  bool hasVoidType_;
  bool hasNullType_;
  mutable IRTypeList irTypes_; // IR types corresponding to tart types.
};

}

#endif

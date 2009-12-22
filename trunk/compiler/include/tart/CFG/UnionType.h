/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_UNIONTYPE_H
#define TART_CFG_UNIONTYPE_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

namespace tart {

typedef llvm::SmallVector<const llvm::Type *, 16> IRTypeList;

// TODO: Finish the trace() function for this type.

// -------------------------------------------------------------------
// Disjoint or union type.
class UnionType : public TypeImpl, public Locatable {
public:
  /** Return a union of the given element types. */
  static UnionType * get(const SourceLocation & loc, const ConstTypeList & members);

  /** Return the list of possible types for this union. */
  const TupleType & members() const { return *members_; }

  /** Return the type arguments for this union. */
  const TupleType * typeArgs() const { return members_; }

  /** Return the number of type parameters of this type. */
  size_t numTypeParams() const;

  /** Return the Nth type parameter. */
  const Type * typeParam(int index) const;

  /** Where in the source file this expression comes from. */
  const SourceLocation & location() const { return loc_; }

  /** Given a type, return the index of this type. */
  int getTypeIndex(const Type * type) const;

  /** The number of value types in the union. */
  size_t numValueTypes() const { return numValueTypes_; }

  /** The number of reference types in the union. */
  size_t numRefTypes() const { return numReferenceTypes_; }

  /** Whether the 'void' type is included. */
  size_t hasVoidType() const { return hasVoidType_; }

  /** Create a typecast from this type to the desired type. */
  Expr * createDynamicCast(Expr * from, const Type * toType) const;

  // Overrides

  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isEqual(const Type * other) const;
  bool isSingular() const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  TypeShape typeShape() const;
  bool includes(const Type * other) const;
  void format(FormatStream & out) const;
  void trace() const;

  static inline bool classof(const UnionType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Union;
  }

protected:
  /** Construct a disjoint union type */
  UnionType(const SourceLocation & loc, const ConstTypeList & members);

  // Given an IR type, return an estimate of the size of this type.
  static size_t estimateTypeSize(const llvm::Type * type, size_t ptrSize);

  SourceLocation loc_;
  TupleType * members_;
  size_t numValueTypes_;
  size_t numReferenceTypes_;
  bool hasVoidType_;
  bool hasNullType_;
  mutable IRTypeList irTypes_; // IR types corresponding to tart types.
};

}

#endif

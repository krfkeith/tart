/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_NATIVETYPE_H
#define TART_CFG_NATIVETYPE_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#ifndef TART_CFG_DEFN_H
#include "tart/CFG/Defn.h"
#endif

#include <llvm/ADT/DenseMap.h>

namespace tart {

// -------------------------------------------------------------------
// Generic template for memory addresses. A memory address is similar
// to a pointer, except that it also allows indexing.
class AddressType : public TypeImpl {
public:

  /** Construct a native pointer type for the specified element type. */
  static AddressType * get(const Type * elemType);

  ~AddressType();

  /** Initialize the built-in template for this type. */
  static void initBuiltin();

  // Overrides

  size_t numTypeParams() const { return 1; }
  virtual const Type * typeParam(int index) const { return elementType_; }
  const llvm::Type * irType() const { return createIRType(); }
  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  void format(FormatStream & out) const;
  Expr * nullInitValue() const;
  unsigned getHashValue() const;

  static inline bool classof(const AddressType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == NAddress;
  }

  /** Singleton instance. */
  static TypeDefn typedefn;
  static AddressType prototype;
  static ASTBuiltIn biDef;

private:
  typedef llvm::DenseMap<const Type * , AddressType *, Type::KeyInfo> TypeMap;
  static TypeMap uniqueTypes_;

  AddressType(const Type * elemType);
  AddressType();

  const Type* elementType_;
};

// -------------------------------------------------------------------
// Generic template for native arrays
class NativeArrayType : public TypeImpl {
public:

  /** Construct a native array for the specified element type. */
  static NativeArrayType * get(const TupleType * typeArgs);

  /** Initialize the built-in template for this type. */
  static void initBuiltin();

  /** The element type of the array. */
  const Type * elementType() const;

  /** The fixed length of the array. */
  uint64_t size() const { return size_; }
  void setSize(uint64_t sz) { size_ = sz; }

  const TupleType * typeArgs() const { return typeArgs_; }

  // Overrides

  size_t numTypeParams() const { return 2; }
  virtual const Type * typeParam(int index) const;

  const llvm::Type * irType() const { return createIRType(); }
  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  void format(FormatStream & out) const;
  unsigned getHashValue() const;

  static inline bool classof(const NativeArrayType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == NArray;
  }

  static NativeArrayType prototype;
  static TypeDefn typedefn;

protected:
  typedef llvm::DenseMap<const Type *, NativeArrayType *, Type::KeyInfo> TypeMap;
  static TypeMap uniqueTypes_;

  /** Construct a native array type */
  NativeArrayType(const TupleType * typeArgs);
  NativeArrayType();

  const TupleType * typeArgs_;
  uint64_t size_;
};

// -------------------------------------------------------------------
// Generic template for "flexible" arrays - zero length arrays.
class FlexibleArrayType : public TypeImpl {
public:

  /** Construct a native array for the specified element type. */
  static FlexibleArrayType * get(const TupleType * typeArgs);

  /** Initialize the built-in template for this type. */
  static void initBuiltin();

  /** The element type of the array. */
  const Type * elementType() const;

  const TupleType * typeArgs() const { return typeArgs_; }

  // Overrides

  size_t numTypeParams() const { return 1; }
  virtual const Type * typeParam(int index) const;

  const llvm::Type * irType() const { return createIRType(); }
  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  void format(FormatStream & out) const;
  unsigned getHashValue() const;

  static inline bool classof(const FlexibleArrayType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == FlexibleArray;
  }

  static FlexibleArrayType prototype;
  static TypeDefn typedefn;

protected:
  typedef llvm::DenseMap<const Type *, FlexibleArrayType *, Type::KeyInfo> TypeMap;
  static TypeMap uniqueTypes_;

  /** Construct a native array type */
  FlexibleArrayType(const TupleType * typeArgs);
  FlexibleArrayType();

  const TupleType * typeArgs_;
  uint64_t size_;
};

}

#endif

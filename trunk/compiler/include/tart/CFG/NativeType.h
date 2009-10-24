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
  static AddressType * get(TypeRef elemType);

  ~AddressType();

  /** Initialize the built-in template for this type. */
  static void initBuiltin();

  // Overrides

  size_t numTypeParams() const { return 1; }
  virtual TypeRef typeParam(int index) const { return elementType_; }
  const llvm::Type * irType() const { return createIRType(); }
  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  void format(FormatStream & out) const;

  static inline bool classof(const AddressType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Address;
  }

  /** Singleton instance. */
  static TypeDefn typedefn;
  static AddressType prototype;

private:
  typedef llvm::DenseMap<TypeRef, AddressType *, TypeRef::KeyInfo> TypeMap;
  static TypeMap uniqueTypes_;

  AddressType(const TypeRef & elemType);
  AddressType();

  TypeRef elementType_;
};

// -------------------------------------------------------------------
// Generic template for native pointers
class PointerType : public TypeImpl {
public:

  /** Construct a native pointer type for the specified element type. */
  static PointerType * get(TypeRef elemType);

  /** Initialize the built-in template for this type. */
  static void initBuiltin();

  // Overrides

  size_t numTypeParams() const { return 1; }
  virtual TypeRef typeParam(int index) const { return elementType_; }
  const llvm::Type * irType() const { return createIRType(); }
  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  void format(FormatStream & out) const;

  static inline bool classof(const PointerType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Pointer;
  }

  /** Singleton instance. */
  static PointerType prototype;
  static TypeDefn typedefn;

protected:
  typedef llvm::DenseMap<TypeRef, PointerType *, TypeRef::KeyInfo> TypeMap;
  static TypeMap uniqueTypes_;

  PointerType(const TypeRef & elemType);
  PointerType();

  TypeRef elementType_;
};

// -------------------------------------------------------------------
// Generic template for native arrays
class NativeArrayType : public DeclaredType {
protected:
  Type * elementType_;
  uint64_t size_;

public:

  /** Construct a native array for the specified element type. */
  static NativeArrayType * create(Type * elemType, uint64_t sz);
  static NativeArrayType * get(const TypeRef & elemType, uint64_t sz);

  /** Construct a native array type */
  NativeArrayType(Type * elementType, uint64_t sz, TypeDefn * defn,
      Scope * parentScope);

  /** Initialize the built-in template for this type. */
  void initBuiltin();

  /** The fixed length of the array. */
  uint64_t size() const { return size_; }
  void setSize(uint64_t sz) { size_ = sz; }

  // Overrides

  const llvm::Type * irType() const { return createIRType(); }
  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  void format(FormatStream & out) const;

  static inline bool classof(const NativeArrayType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == NativeArray;
  }

  /** Singleton instance. */
  static NativeArrayType instance;
  static TypeDefn typedefn;
};

}

#endif

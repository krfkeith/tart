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

namespace tart {

// -------------------------------------------------------------------
// Generic template for native pointers
class NativePointerType : public DeclaredType {
protected:
  Type * elementType_;

public:

  /** Construct a native pointer type for the specified element type. */
  static NativePointerType * create(Type * elemType);

  /** Construct a native pointer type */
  NativePointerType(Type * elemType, TypeDefn * defn, Scope * parentScope);

  /** Initialize the built-in template for this type. */
  void initBuiltin();

  // Overrides

  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  void format(FormatStream & out) const;

  static inline bool classof(const NativePointerType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == NativePointer;
  }

  /** Singleton instance. */
  static NativePointerType instance;
  static TypeDefn typedefn;
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

  /** Construct a native array type */
  NativeArrayType(Type * elementType, uint64_t sz, TypeDefn * defn,
      Scope * parentScope);

  /** Initialize the built-in template for this type. */
  void initBuiltin();

  /** The fixed length of the array. */
  uint64_t size() const { return size_; }
  void setSize(uint64_t sz) { size_ = sz; }

  // Overrides

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

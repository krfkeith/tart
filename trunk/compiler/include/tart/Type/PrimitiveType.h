/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_PRIMITIVETYPE_H
#define TART_TYPE_PRIMITIVETYPE_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_DEFN_DEFN_H
#include "tart/Defn/Defn.h"
#endif

#ifndef TART_DEFN_TYPEDEFN_H
#include "tart/Defn/TypeDefn.h"
#endif

namespace tart {

class ASTBuiltIn;
class TypeAlias;

/// -------------------------------------------------------------------
/// Predicate functions for type ids.

inline bool isIntegerTypeId(TypeId id) {
  return id >= TypeId_Char && id <= TypeId_UInt64;
}

inline bool isUnsignedIntegerTypeId(TypeId id) {
  return (id >= TypeId_UInt8 && id <= TypeId_UInt64) || id == TypeId_Char;
}

inline bool isSignedIntegerTypeId(TypeId id) {
  return id >= TypeId_SInt8 && id <= TypeId_SInt64;
}

inline static bool isFloatingTypeId(TypeId id) {
  return id >= TypeId_Float && id <= TypeId_LongDouble;
}

// -------------------------------------------------------------------
// Base class for primitive types
class PrimitiveType : public DeclaredType {
public:
  /** Construct a primitive type */
  PrimitiveType(TypeDefn * de);

  /** Deferred initialization function, unique to each type. */
  virtual void initType() = 0;

  /** Deferred initialization function, unique to each type. */
  virtual void initMembers() = 0;

  /** Return the type id. */
  virtual TypeId typeId() const = 0;

  /** Return the number of bits of this primitive type. */
  virtual uint32_t numBits() const = 0;

  /** Define a constant member of this type. */
  void defineConstant(const char * name, ConstantExpr * value);

  PrimitiveType * nextType() const { return nextType_; }

  // Return an integer type that fits the given number of bits.
  static PrimitiveType * fitIntegerType(size_t nBits, bool isUnsigned);

  // If 'in' is an enum type, return its base type, otherwise just return 'in'.
  static const Type * derefEnumType(const Type * in);

  static void initPrimitiveTypes(Module * module);

  /** Return the type of 'int'. */
  static const Type * intType();

  /** Return the type of 'uint'. */
  static const Type * uintType();

  /** Return true if this primitive type is a subtype of 'base' */
  virtual bool isSubtypeOf(const PrimitiveType * base) const = 0;

  /** Left over from previous conversion implementation - move to TypeConversion.cpp. */
  virtual ConversionRank convertImpl(const Conversion & cn) const = 0;

    // Overrides
  llvm::Type * createIRType() const;
  virtual bool isSingular() const { return true; }

  static inline bool classof(const PrimitiveType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Type::Primitive;
  }

  // Used by the parser
  static ASTBuiltIn intDef;
  static ASTBuiltIn uintDef;

protected:
  ConversionRank convertToInteger(const Conversion & cn) const;
  ConversionRank convertConstantToInteger(const Conversion & cn) const;
  ConversionRank fromUnsizedIntToInt(const ConstantInteger * cint, Expr ** out) const;

  ConversionRank convertToFloat(const Conversion & cn) const;
  ConversionRank convertConstantToFloat(const Conversion & cn) const;
  ConversionRank fromUnsizedIntToFloat(const ConstantInteger * cint, Expr ** out) const;

  ConversionRank convertToBool(const Conversion & cn) const;
  ConversionRank convertConstantToBool(const Conversion & cn) const;

  ConversionRank convertFromObject(const Conversion & cn) const;

  PrimitiveType * nextType_;

  // Static list of all primitive types.
  static PrimitiveType * primitiveTypeList;
};

// -------------------------------------------------------------------
// Implementation class for primitive types
template<TypeId kTypeId>
class PrimitiveTypeImpl : public PrimitiveType {
public:
  static const TypeId id = kTypeId;

  /** Construct a primitive type */
  PrimitiveTypeImpl() : PrimitiveType(&typedefn) {}

  void initType();
  void initMembers();
  Expr * nullInitValue() const;
  bool isReferenceType() const { return kTypeId == TypeId_Null; }

  // Overrides

  TypeId typeId() const { return kTypeId; }
  uint32_t numBits() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  static inline bool classof(const PrimitiveTypeImpl<kTypeId> *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Type::Primitive &&
        static_cast<const PrimitiveType *>(t)->typeId() == kTypeId;
  }

  bool isSubtypeOf(const PrimitiveType * base) const {
    return base == this || MORE_GENERAL.contains(base->typeId());
  }

  /** Singleton instance. */
  static PrimitiveTypeImpl instance;
  static TypeDefn typedefn;
  static ASTBuiltIn biDef;
  static TypeIdSet MORE_GENERAL;
};

template<TypeId kTypeId>
ASTBuiltIn PrimitiveTypeImpl<kTypeId>::biDef(&typedefn);

template<TypeId kTypeId>
PrimitiveTypeImpl<kTypeId> PrimitiveTypeImpl<kTypeId>::instance;

// -------------------------------------------------------------------
// Specific primitive type implementations

typedef PrimitiveTypeImpl<TypeId_Void>    VoidType;
typedef PrimitiveTypeImpl<TypeId_Bool>    BoolType;
typedef PrimitiveTypeImpl<TypeId_Char>    CharType;
typedef PrimitiveTypeImpl<TypeId_SInt8>   Int8Type;
typedef PrimitiveTypeImpl<TypeId_SInt16>  Int16Type;
typedef PrimitiveTypeImpl<TypeId_SInt32>  Int32Type;
typedef PrimitiveTypeImpl<TypeId_SInt64>  Int64Type;
typedef PrimitiveTypeImpl<TypeId_UInt8>   UInt8Type;
typedef PrimitiveTypeImpl<TypeId_UInt16>  UInt16Type;
typedef PrimitiveTypeImpl<TypeId_UInt32>  UInt32Type;
typedef PrimitiveTypeImpl<TypeId_UInt64>  UInt64Type;
typedef PrimitiveTypeImpl<TypeId_Float>   FloatType;
typedef PrimitiveTypeImpl<TypeId_Double>  DoubleType;
typedef PrimitiveTypeImpl<TypeId_Null>    NullType;
typedef PrimitiveTypeImpl<TypeId_Any>     AnyType;
typedef PrimitiveTypeImpl<TypeId_Bad>     BadType;

// -------------------------------------------------------------------
// Special primitive class for unsized integers
class UnsizedIntType : public PrimitiveType {
public:

  /** Get an instance of UnsizedIntType for a specific integer value. */
  static UnsizedIntType * get(llvm::ConstantInt * intVal);

  /** Construct a primitive type */
  UnsizedIntType(llvm::ConstantInt *  intVal) : PrimitiveType(&typedefn), intVal_(intVal) {}

  /** The integer value. */
  const llvm::ConstantInt * intVal() const { return intVal_; }

  Expr * nullInitValue() const;
  bool isReferenceType() const { return false; }

  /** Number of bits required to hold this value. */
  unsigned signedBitsRequired() const;
  unsigned unsignedBitsRequired() const;

  /** Whether the value is negative. */
  bool isNegative() const { return intVal_->isNegative(); }

  /** Choose an integer size for this type. */
  PrimitiveType * fixIntSize(bool makeUnsigned) const;

  /** Replace all unsized integers in the type expression with appropriately sized ints. */
  const Type * assignIntSizes(const Type * in) const;

  // Overrides

  void initType();
  void initMembers() {}
  TypeId typeId() const { return TypeId_UnsizedInt; }
  uint32_t numBits() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  void format(FormatStream & out) const;
  bool isSubtypeOf(const PrimitiveType * base) const;

  // Casting support

  static inline bool classof(const UnsizedIntType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Type::Primitive &&
        static_cast<const PrimitiveType *>(t)->typeId() == TypeId_UnsizedInt;
  }

  // Generic unsized int instance.
  static UnsizedIntType instance;

private:
  static TypeDefn typedefn;
  const llvm::ConstantInt * intVal_;
};

}

#endif

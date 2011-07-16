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

  // Overrides
  const llvm::Type * createIRType() const;
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

  bool isSubtypeOf(const Type * other) const;
  bool includes(const Type * other) const;

  /** Singleton instance. */
  static PrimitiveTypeImpl instance;
  static TypeDefn typedefn;
  static ASTBuiltIn biDef;
  static TypeIdSet MORE_GENERAL;
  static TypeIdSet INCLUDES;
};

template<TypeId kTypeId> bool PrimitiveTypeImpl<kTypeId>::isSubtypeOf(const Type * other) const {
  if (other == this) {
    return true;
  }

  if (other->typeClass() == Type::Primitive) {
    const PrimitiveType * ptype = static_cast<const PrimitiveType *>(other);
    return MORE_GENERAL.contains(ptype->typeId());
  }

  if (other->typeClass() == Type::Protocol && supports(other)) {
    return true;
  }

  return false;
}

template<TypeId kTypeId> bool PrimitiveTypeImpl<kTypeId>::includes(const Type * other) const {
  other = derefEnumType(other);
  if (other == this) {
    return true;
  }

  if (other->typeClass() == Type::Primitive) {
    const PrimitiveType * ptype = static_cast<const PrimitiveType *>(other);
    return INCLUDES.contains(ptype->typeId());
  }

  return false;
}


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
typedef PrimitiveTypeImpl<TypeId_UnsizedInt> UnsizedIntType;
typedef PrimitiveTypeImpl<TypeId_Bad>     BadType;

}

#endif

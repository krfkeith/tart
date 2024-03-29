/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPE_H
#define TART_TYPE_TYPE_H

#ifndef TART_EXPR_CONSTANT_H
#include "tart/Expr/Constant.h"
#endif

#ifndef TART_COMMON_SMALLENUMSET_H
#include "tart/Common/SmallEnumSet.h"
#endif

#ifndef TART_DEFN_SCOPE_H
#include "tart/Defn/Scope.h"
#endif

#ifndef TART_TYPE_TYPECONVERSION_H
#include "tart/Type/TypeConversion.h"
#endif

#ifndef LLVM_ADT_DENSEMAP_H
#include "llvm/ADT/DenseMap.h"
#endif

#ifndef LLVM_TYPE_H
#include "llvm/Type.h"
#endif

#ifndef LLVM_ADT_STRINGREF_H
#include "llvm/ADT/StringRef.h"
#endif

namespace llvm {
  class Type;
}

namespace tart {

class TupleType;
class CompositeType;
class TypeDefn;

using llvm::StringRef;

/// -------------------------------------------------------------------
/// An enumeration of all of the fundamental types known to the compiler.
enum TypeId {
  TypeId_Void,
  TypeId_Bad,      // Used to signal errors

  // Scalar types
  TypeId_Bool,
  TypeId_Char,
  TypeId_UnsizedInt,
  TypeId_SInt8,
  TypeId_SInt16,
  TypeId_SInt32,
  TypeId_SInt64,
  TypeId_UInt8,
  TypeId_UInt16,
  TypeId_UInt32,
  TypeId_UInt64,
  TypeId_Float,
  TypeId_Double,
  TypeId_LongDouble,
  TypeId_Null,
  TypeId_Any,

  // Aggregate types - used only for static type declarations.
  TypeId_String,

  TypeId_Count,
};

/// -------------------------------------------------------------------
/// The term 'shape' is used here to mean the various ways that types
/// are stored and passed around - by copying, by reference, whether
/// the type can fit in registers, whether it's address can be taken,
/// and so on.
enum TypeShape {
  Shape_Unset = 0,          // Shape hasn't been determined yet
  Shape_None,               // Can't be instantiated
  Shape_ZeroSize,           // A type of size 0.
  Shape_Primitive,          // A primitive type - int, float, pointer.
  Shape_Small_RValue,       // A small value which cannot be addressed.
  Shape_Small_LValue,       // A small value which can be addressed.
  Shape_Large_Value,        // A large aggregate value
  Shape_Reference,          // A reference to an instance
};

/// -------------------------------------------------------------------
/// EnumSet of defn states.
typedef SmallEnumSet<TypeId, TypeId_Count> TypeIdSet;

/// -------------------------------------------------------------------
/// Interface for types.
class Type : public GC, public Formattable {
public:

  enum TypeClass {
    #define TYPE_CLASS(x) x,
    #define TYPE_RANGE(x, start, end) x ## Begin = start, x ## End = end,
    #include "TypeClass.def"
    #undef TYPE_CLASS
    #undef TYPE_RANGE

    KindCount,
  };

  static const char * typeClassName(TypeClass tc);

  /** Get the kind of type that this is. */
  TypeClass typeClass() const { return cls; }

  /** Get the LLVM IR type corresponding to this type. */
  virtual llvm::Type * irType() const = 0;

  /** Get the LLVM IR type corresponding to this type when embedded as a member within a
      larger type. */
  virtual llvm::Type * irEmbeddedType() const { return irType(); }

  /** Get the LLVM IR type corresponding to this type when passed as a parameter. */
  virtual llvm::Type * irParameterType() const { return irType(); }

  /** Get the LLVM IR type corresponding to this type when returned from a function. */
  virtual llvm::Type * irReturnType() const { return irParameterType(); }

  /** Get the LLVM IR type corresponding to this type. If this is a StructType, then make
      sure that the type body has been filled in. */
  virtual llvm::Type * irTypeComplete() const { return irType(); }

  /** Get the TypeDefn for this type, if any. */
  virtual TypeDefn * typeDefn() const { return NULL; }

  /** Return the scope containing the members of this type. */
  virtual const IterableScope * memberScope() const { return NULL; }
  virtual IterableScope * mutableMemberScope() const { return NULL; }

  /** Return the number of type parameters of this type. */
  virtual size_t numTypeParams() const { return 0; }

  /** Return the Nth type parameter. */
  virtual QualifiedType typeParam(int index) const;

  /** Return the list of all type parameters for this type. */
  void getTypeParams(QualifiedTypeList & out) const;

  /** Return true if this type supports the specified protocol. */
  bool supports(const Type * protocol) const;

  /** Return all the possible concrete types that this type could be. For regular types,
      this simply inserts the type into the output array unchanged. For type aliases
      and type variables, this recursively dereferences the variable. For ambiguous types, this
      inserts all of the possible types that the ambiguous type could be. The 'qualifiers'
      argument represents additional qualifiers to be applied to the type, which is used
      for calculating the expansion of qualified types.
   */
  virtual void expandImpl(QualifiedTypeSet & out, unsigned qualifiers) const {
    out.insert(QualifiedType(this, qualifiers));
  }

  /** Return whether this type is passed by value or by reference. */
  virtual bool isReferenceType() const = 0;

  /** A fully specified type is one in which there are no unbound type variables. */
  virtual bool isSingular() const = 0;

  /** True if this type is the 'void' type. */
  bool isVoidType() const;

  /** True if this type is the 'Null' type. */
  bool isNullType() const;

  /** True if this type is an integer type. */
  bool isIntType() const;

  /** True if this type is an unsigned integer type. */
  bool isUnsignedType() const;

  /** True if this type is a signed integer type. */
  bool isSignedType() const;

  /** True if this type is a floating point type. */
  bool isFPType() const;

  /** True if this type is the 'Error' type. */
  bool isErrorType() const;

  /** True if this type is the 'unsized int' type. */
  bool isUnsizedIntType() const;

  /** True if this type is a boolean type. */
  bool isBooleanType() const;

  /** Return true if this type can be boxed. */
  bool isBoxableType() const;

  /** Return true if this is a throwaway pattern type. */
  bool isScaffold() const;

  /** Return true if this type requires GC tracing. */
  virtual bool containsReferenceType() const { return false; }

  /** Return information about the representation of this type. */
  virtual TypeShape typeShape() const = 0;

  /** Determine if the specified 'fromType' can be converted to this
      type. Returns the degree of compatibility. */
  ConversionRank convert(const Conversion & conversion) const;

  /** Add an implicit cast. If no cast is needed, then simply return 'from'.
      As a side effect, emit appropriate warning messages if the cast wasn't
      possible or had problems. */
  Expr * implicitCast(const SourceLocation & loc, Expr * from, int options = 0) const;

  /** Add an explicit cast. If no cast is needed, then simply return 'from'.
      This suppresses warnings unless the cast is impossible. */
  Expr * explicitCast(const SourceLocation & loc, Expr * from, int options = 0) const;

  /** Get the default initialization value for this type, or NULL if
      this type cannot be null-initialized. */
  virtual Expr * nullInitValue() const { return NULL; }

  // Overrides

  void trace() const;
  static inline bool classof(const Type *) { return true; }

  // Static utility functions

  /** Return a type which is the common supertype of type1 and type2, or NULL if there
      is no such type.  */
  static QualifiedType commonBase(QualifiedType type1, QualifiedType type2);

  // Traits used when using type pointers as a key.
  struct KeyInfo {
    static inline const Type * getEmptyKey() { return reinterpret_cast<const Type *>(0); }
    static inline const Type * getTombstoneKey() { return reinterpret_cast<const Type *>(-1); }

    static unsigned getHashValue(const Type * val) {
      return (uintptr_t(val) >> 4) * 0x5bd1e995;
    }

    static bool isEqual(const Type * lhs, const Type * rhs) {
      return lhs == rhs;
    }

    static bool isPod() { return true; }
  };

protected:
  const TypeClass cls;

  // Protected constructor
  Type(TypeClass tc) : cls(tc) {}

  // Protected destructor
  virtual ~Type() {}
};

/// -------------------------------------------------------------------
/// Base class for most types.
class TypeImpl : public Type {
public:

  // Method to lazily create the IR type.
  llvm::Type * irType() const;
  TypeShape typeShape() const { return shape_; }

protected:
  mutable llvm::Type * irType_;
  mutable TypeShape shape_;

  TypeImpl(TypeClass cls, TypeShape shape)
    : Type(cls)
    , irType_(NULL)
    , shape_(shape)
  {}

  virtual llvm::Type * createIRType() const = 0;
};

/// -------------------------------------------------------------------
/// A type that can be declared. This can be a composite type,
/// an enum, or a primitive type.
///
/// A Nameable type is one that has the following characteristics:
///  -- It defines a scope containing members of the type.
///  -- It has a unique name.
class DeclaredType : public TypeImpl, public IterableScope {
protected:
  TypeDefn * defn_;

  DeclaredType(TypeClass cls, TypeDefn * de, Scope * parentScope, TypeShape shape);

public:

  /** Return the number of type parameters of this type. */
  size_t numTypeParams() const;

  /** Return the Nth type parameter. */
  QualifiedType typeParam(int index) const;

  // Overrides

  const IterableScope * memberScope() const { return this; }
  IterableScope * mutableMemberScope() const { return const_cast<DeclaredType *>(this); }
  TypeDefn * typeDefn() const { return defn_; }

  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const DeclaredType *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() <= Enum;
  }
};

/// -------------------------------------------------------------------
/// A pair of type refs - used as a map key.
class TypePair {
public:
  TypePair(const Type * first, const Type * second) : first_(first), second_(second) {}
  TypePair(const TypePair & src) : first_(src.first_), second_(src.second_) {}

  const Type * first() { return first_; }
  const Type * second() { return second_; }

  bool operator==(const TypePair & other) const {
    return first_ == other.first_ && second_ == other.second_;
  }

  bool operator!=(const TypePair & other) const {
    return !(*this == other);
  }

  // Structure used when using type ref as a key.
  struct KeyInfo {
    static inline TypePair getEmptyKey() {
      return TypePair(Type::KeyInfo::getEmptyKey(), Type::KeyInfo::getEmptyKey());
    }

    static inline TypePair getTombstoneKey() {
      return TypePair(Type::KeyInfo::getTombstoneKey(), Type::KeyInfo::getTombstoneKey());
    }

    static unsigned getHashValue(const TypePair & val) {
      return Type::KeyInfo::getHashValue(val.first_) ^
          (Type::KeyInfo::getHashValue(val.second_) << 1);
    }

    static bool isEqual(const TypePair & lhs, const TypePair & rhs) {
      return lhs.first_ == rhs.first_ && lhs.second_ == rhs.second_;
    }

    static bool isPod() { return true; }
  };

private:
  const Type * first_;
  const Type * second_;
};

// -------------------------------------------------------------------
// Utility functions

const char * compatibilityError(ConversionRank tc);

void compatibilityWarning(const SourceLocation & loc, ConversionRank tc,
    const Type * from, QualifiedType to);

void compatibilityWarning(const SourceLocation & loc, ConversionRank tc,
    const Expr * from, QualifiedType to);

// Given a type, append the linkage name of that type to the output buffer.
void typeLinkageName(llvm::SmallVectorImpl<char> & out, const Type * ty);
void typeLinkageName(llvm::SmallVectorImpl<char> & out, QualifiedType ty);

// Given a type, append the linkage name of that type to the stream.
void typeLinkageName(llvm::raw_ostream & out, const Type * ty);
void typeLinkageName(llvm::raw_ostream & out, QualifiedType ty);

/** Given two types, try and find the narrowest type that both
    can be converted to.
  */
const Type * findCommonType(const Type * t0, const Type * t1);

/** Given a pointer to a type, dereference any aliases and return the
    real underlying type. */
const Type * dealias(const Type * t);
Type * dealias(Type * t);
QualifiedType dealias(QualifiedType t);

/** Stream operator for type class names. */
inline FormatStream & operator<<(FormatStream & out, Type::TypeClass tc) {
  return out << Type::typeClassName(tc);
}

bool isLargeIRType(const Type * type);

/** Returns true if 'ty' is a type which can meaningfully have qualifiers. */
bool isQualifiableType(const Type * ty);

} // namespace tart

#endif // TART_TYPE_TYPE_H

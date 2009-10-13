/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TYPE_H
#define TART_CFG_TYPE_H

#ifndef TART_CFG_CONSTANT_H
#include "tart/CFG/Constant.h"
#endif

#ifndef TART_COMMON_SMALLENUMSET_H
#include "tart/Common/SmallEnumSet.h"
#endif

#ifndef TART_CFG_SCOPE_H
#include "tart/CFG/Scope.h"
#endif

namespace llvm {
  class Type;
}

namespace tart {

class BindingEnv;
class TypeRef;

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

  // Aggregate types - used only for static type declarations.
  TypeId_String,

  TypeId_Count,
};

/// -------------------------------------------------------------------
/// EnumSet of defn states.
typedef SmallEnumSet<TypeId, TypeId_Count> TypeIdSet;

/// -------------------------------------------------------------------
/// Represents the degrees of compatibility between two types. Higher
/// numerical values are considered 'more compatible' than lower ones.
/// Compatibility values within the same 'rank' are considered to be
/// equally good (or bad). So for example, PrecisionLoss is considered
/// as 'bad' as SignedUnsigned.
enum ConversionRank {
  // Rank 0: Impossible conversions
  Incompatible,      // let x:String = 1     No conversion possible

  // Rank 1: Lossy conversions (cause warning message to be emitted.)
  Truncation,        // let x:ubyte = 256    Value will be truncated
  SignedUnsigned,    // let x:uint = -1      Signed / unsigned mismatch
  PrecisionLoss,     // let x:int = 1.2      Loss of decimal precision
  IntegerToBool,     // let x:bool = 256     Compare with 0

  // Rank 2: Non-lossy conversions
  NonPreferred,      // let x:int = 1.0      Requires transformation

  // Rank 3-4: Trivial conversions
  ExactConversion,   // let x:byte = int(1)  Lossless conversion

  // Rank 5: Identity conversions
  IdenticalTypes,     // let x:int = int(1)   No conversion, same type
};

inline bool isConversionWarning(ConversionRank rank) {
  return rank < NonPreferred;
}

FormatStream & operator<<(FormatStream & out, ConversionRank tc);

/// -------------------------------------------------------------------
/// Input parameters for a type conversion operation.
struct Conversion {
  const Type * fromType;
  Expr * fromValue;
  Expr ** resultValue;
  BindingEnv * bindingEnv;
  bool matchPatterns;

  /** Test conversion from type to type. */
  Conversion(const Type * from);

  /** Test conversion from expression to type. */
  Conversion(Expr * from);

  /** Convert expression. */
  Conversion(Expr * from, Expr ** to);

  /** Returns the 'from' type, with aliases and type parameters resolved. */
  const Type * getFromType() const;
};

/// -------------------------------------------------------------------
/// Interface for types.
class Type : public GC, public Formattable {
public:

  enum TypeClass {
    #define TYPE_CLASS(x) x,
    #include "TypeClass.def"
    #undef TYPE_CLASS

    KindCount,
  };

  static const char * typeClassName(TypeClass tc);

  /** Get the kind of type that this is. */
  virtual TypeClass typeClass() const { return cls; }

  /** Get the LLVM IR type corresponding to this type. */
  virtual const llvm::Type * irType() const = 0;

  /** Get the LLVM IR type corresponding to this type when embedded as a member within a
      larger type. */
  virtual const llvm::Type * irEmbeddedType() const { return irType(); }

  /** Get the LLVM IR type corresponding to this type when passed as a parameter. */
  virtual const llvm::Type * irParameterType() const { return irType(); }

  /** Get the type of this type. */
  virtual Type * metaType() const { return NULL; }

  /** Get the TypeDefn for this type, if any. */
  virtual TypeDefn * typeDefn() const { return NULL; }

  /** Return the scope containing the members of this type. */
  virtual const IterableScope * memberScope() const { return NULL; }
  virtual IterableScope * memberScope() { return NULL; }

  /** Return the number of type parameters of this type. */
  virtual size_t numTypeParams() const { return 0; }

  /** Return the Nth type parameter. */
  virtual Type * typeParam(int index) const;

  /** Return true if two types are identical. */
  virtual bool isEqual(const Type * other) const;

  /** Return true if the specified type is more specific than 'other'. */
  virtual bool isSubtype(const Type * other) const = 0;

  /** A type is said to "include" another type if it can represent all possible values
      of that other type. So for example, 'int' includes 'short', since an int can
      contain all possible shorts. Note that the include relationship encompasses
      more than subtyping - a union type includes all its members, even though the
      members are not considered subtypes in the normal fashion.
    */
  virtual bool includes(const Type * other) const { return isEqual(other); }

  //virtual bool is

  /** Return whether this type is passed by value or by reference. */
  virtual bool isReferenceType() const = 0;

  /** A fully specified type is one in which there are no unbound type variables. */
  virtual bool isSingular() const = 0;

  /** True if this type is the 'void' type. */
  bool isVoidType() const;

  /** True if this type is the 'unsized int' type. */
  bool isUnsizedIntType() const;

  /** Determine if the specified 'fromType' can be converted to this
      type. Returns the degree of compatibility. */
  ConversionRank convert(const Conversion & conversion) const;

  /** Type-specific implementation of convert. */
  virtual ConversionRank convertImpl(const Conversion & conversion) const = 0;

  /** Some convenient wrappers around 'convert'. */
  ConversionRank canConvert(Expr * fromExpr) const;
  ConversionRank canConvert(const Type * fromType) const;

  /** Reverse conversion function, used when the source is a constraint. */
  virtual ConversionRank convertTo(const Type * toType) const {
    return Incompatible;
  }

  /** Add an implicit cast. If no cast is needed, then simply return 'from'.
      As a side effect, emit appropriate warning messages if the cast wasn't
      possible or had problems. */
  Expr * implicitCast(const SourceLocation & loc, Expr * from) const;

  /** Add an explicit cast. If no cast is needed, then simply return 'from'.
      This supresses warnings unless the cast is impossible. */
  Expr * explicitCast(const SourceLocation & loc, Expr * from) const;

  /** Get the default initialization value for this type, or NULL if
      this type cannot be null-initialized. */
  virtual Expr * nullInitValue() const { return NULL; }

  // Overrides

  void trace() const;
  static inline bool classof(const Type *) { return true; }

  // Static utility functions

  /** Given two types, return the one that is more general, the higher of the two. Returns NULL
      if neither type is a specialization of the other. */
  static Type * selectLessSpecificType(Type * type1, Type * type2);

  /** Return true if type1 and type2 are type expressions that, when finalized, will
      reduce to the same type. For example, List[T] is equivalent to List[S] if
      T is a pattern variable bound to S. */
  static bool equivalent(const Type * type1, const Type * type2);

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
protected:
  mutable const llvm::Type * irType_;

  TypeImpl(TypeClass cls)
    : Type(cls)
    , irType_(NULL)
  {}

  virtual const llvm::Type * createIRType() const = 0;

public:

  const llvm::Type * irType() const {
    if (irType_ == NULL) {
      irType_ = createIRType();
    }

    return irType_;
  }
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

  DeclaredType(TypeClass cls, TypeDefn * de, Scope * parentScope);

public:

  /** Return the number of type parameters of this type. */
  size_t numTypeParams() const;

  /** Return the Nth type parameter. */
  Type * typeParam(int index) const;

  // Overrides

  const IterableScope * memberScope() const { return this; }
  IterableScope * memberScope() { return this; }
  TypeDefn * typeDefn() const { return defn_; }

  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const DeclaredType *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() <= Enum;
  }
};

/// -------------------------------------------------------------------
/// A named alias of a type.
class TypeAlias : public Type {
public:
  /** Construct a new typealias. */
  TypeAlias(Type * val);

  /** The target of this alias. */
  Type * value() const { return value_; }
  void setValue(Type * value) { value_ = value; }

  // Overrides

  bool isSingular() const { return value_->isSingular(); }
  bool isEqual(const Type * other) const { return value_->isEqual(other); }
  bool isSubtype(const Type * other) const { return value_->isSubtype(other); }
  bool includes(const Type * other) const { return value_->includes(other); }
  bool isReferenceType() const { return value_->isReferenceType(); }
  const llvm::Type * irType() const;
  const llvm::Type * irEmbeddedType() const;
  const llvm::Type * irParameterType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  Expr * nullInitValue() const { return value_->nullInitValue(); }
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const TypeAlias *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() == Alias;
  }

private:
  Type * value_;
};

/// -------------------------------------------------------------------
/// A value which isn't a type, but which can be treated as one - used
/// for non-type template arguments.
class NonTypeConstant : public Type {
public:
  // Static factory function.
  static NonTypeConstant * get(ConstantExpr * value);

  // The constant value.
  ConstantExpr * value() const { return value_; }

  // Overrides

  bool isSingular() const { return value_->isSingular(); }
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const { return isEqual(other); }
  bool isReferenceType() const { return false; }
  const llvm::Type * irType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  Expr * nullInitValue() const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const NonTypeConstant *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() == NonType;
  }

private:
  ConstantExpr * value_;

  /** Construct a new typealias. */
  NonTypeConstant(ConstantExpr * value)
    : Type(NonType)
    , value_(value)
  {}
};

/// -------------------------------------------------------------------
/// A reference to a type, and the type modifiers.

class TypeRef {
public:
  enum Modifiers {
    Const = 1 << 0,
  };

  TypeRef() : type_(NULL), modifiers_(0) {}
  TypeRef(Type * type) : type_(type), modifiers_(0) {}
  TypeRef(Type * type, uint32_t modifiers) : type_(type), modifiers_(modifiers) {}
  TypeRef(const TypeRef & ref) : type_(ref.type_), modifiers_(ref.modifiers_) {}

  Type * type() const { return type_; }
  void setType(Type * type) { type_ = type; }

  /** Return the canonicalized version of the type. */
  const Type * dealias() const;
  Type * dealias();

  uint32_t modifiers() const { return modifiers_; }
  void setModifiers(uint32_t modifiers) { modifiers_ = modifiers; }

  TypeRef & operator=(const TypeRef & ref) {
    type_ = ref.type_;
    modifiers_ = ref.modifiers_;
  }

  Type::TypeClass typeClass() const { return type_->typeClass(); }

  bool isDefined() const { return type_ != NULL; }
  bool isVoidType() const { return isDefined() && type_->isVoidType(); }
  bool isNonVoidType() const { return isDefined() && !type_->isVoidType(); }
  bool isReferenceType() const { return isDefined() && type_->isReferenceType(); }
  bool isUnsizedIntType() const { return isDefined() && type_->isUnsizedIntType(); }
  bool isSingular() const { return isDefined() && type_->isSingular(); }

  bool isEqual(const TypeRef & other) const {
    return type_->isEqual(other.type_) && modifiers_ == other.modifiers_;
  }

  Expr * implicitCast(const SourceLocation & loc, Expr * from) const;
  Expr * explicitCast(const SourceLocation & loc, Expr * from) const;
  ConversionRank convert(const Conversion & conversion) const;

  TypeDefn * defn() const { return type_->typeDefn(); }
  const llvm::Type * irType() const { return type_->irType(); }
  const llvm::Type * irEmbeddedType() const { return type_->irEmbeddedType(); }
  const llvm::Type * irParameterType() const { return type_->irParameterType(); }

  void trace() const {
    if (type_) { type_->mark(); }
  }

private:
  Type * type_;
  uint32_t modifiers_;
};

FormatStream & operator<<(FormatStream & out, const TypeRef & ref);

typedef llvm::SmallVector<TypeRef, 8> TypeRefList;

inline void traceTypeRefList(const TypeRefList & refs) {
  for (TypeRefList::const_iterator it = refs.begin(); it != refs.end(); ++it) {
    it->trace();
  }
}

/// -------------------------------------------------------------------
/// Predicate functions for type ids.

inline bool isIntegerType(TypeId id) {
  return id >= TypeId_Char && id <= TypeId_UInt64;
}

inline bool isUnsignedIntegerType(TypeId id) {
  return (id >= TypeId_UInt8 && id <= TypeId_UInt64) || id == TypeId_Char;
}

inline bool isSignedIntegerType(TypeId id) {
  return id >= TypeId_SInt8 && id <= TypeId_SInt64;
}

inline static bool isFloatingType(TypeId id) {
  return id >= TypeId_Float && id <= TypeId_LongDouble;
}

// -------------------------------------------------------------------
// Utility functions

const char * compatibilityError(ConversionRank tc);

void compatibilityWarning(const SourceLocation & loc, ConversionRank tc,
    const Type * from, const Type * to);

void compatibilityWarning(const SourceLocation & loc, ConversionRank tc,
    const Expr * from, const Type * to);

// Given a type, append the linkage name of that type to the output buffer.
void typeLinkageName(std::string & out, const TypeRef & ty);
void typeLinkageName(std::string & out, const Type * ty);

/** Given two types, try and find the narrowest type that both
    can be converted to.
  */
Type * findCommonType(Type * t0, Type * t1);

/** Given a pointer to a type, dereference any aliases and return the
    real underlying type. */
const Type * dealias(const Type * t);
Type * dealias(Type * t);

/** Stream operator for pass names. */
inline FormatStream & operator<<(FormatStream & out, Type::TypeClass tc) {
  return out << Type::typeClassName(tc);
}

/** Type equality functor. */
class TypeEquals {
public:
  bool operator()(const Type * t0, const Type * t1) {
    return t0->isEqual(t1);
  }

  bool operator()(const TypeRef & t0, const TypeRef & t1) {
    return t0.isEqual(t1);
  }
};

/** Type 'less than' operator for sorting lists of types. */
class TypeLess {
public:
  bool operator()(const Type * t0, const Type * t1);
  bool operator()(const TypeRef & t0, const TypeRef & t1);
};

}

#endif

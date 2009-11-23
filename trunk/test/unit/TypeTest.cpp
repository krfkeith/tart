/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/CFG/Type.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/Module.h"
#include "tart/Common/Diagnostics.h"
#include "FakeSourceFile.h"
#include "TestHelpers.h"

using namespace tart;

TEST(TypeTest, FindCommonType) {
  EXPECT_EQ(&IntType::instance,
      findCommonType(&IntType::instance, &IntType::instance));

  EXPECT_EQ(&IntType::instance,
      findCommonType(&IntType::instance, &ShortType::instance));
}

TEST(TypeTest, TypeAttributes) {
}

#if 0
TEST(TypeTest, TypeRefIterPairTest) {
  typedef TypeTupleKeyInfo TRIPKI;

  TypeRef t0(&IntType::instance);
  TypeRef t1(&IntType::instance);
  TypeRef t2(&ShortType::instance);

  TypeTupleKey p0(&t0, &t0 + 1);
  TypeTupleKey p1(&t1, &t1 + 1);
  TypeTupleKey p2(&t2, &t2 + 1);

  ASSERT_TRUE(TRIPKI::getHashValue(p0) == TRIPKI::getHashValue(p1));
  ASSERT_FALSE(TRIPKI::getHashValue(p0) == TRIPKI::getHashValue(p2));
  ASSERT_TRUE(TRIPKI::isEqual(p0, p1));
  ASSERT_FALSE(TRIPKI::isEqual(p0, p2));
}
#endif

TEST(TypeTest, TupleTypeTest) {
  //typedef TypeRefIterPairKeyInfo TRIPKI;

  TupleType * t0 = TupleType::get(&IntType::instance);
  TupleType * t1 = TupleType::get(&IntType::instance);
  TupleType * t2 = TupleType::get(&ShortType::instance);

  //ASSERT_TRUE(TRIPKI::getHashValue(t0->iterPair()) == TRIPKI::getHashValue(t1->iterPair()));
  //ASSERT_FALSE(TRIPKI::getHashValue(t0->iterPair()) == TRIPKI::getHashValue(t2->iterPair()));
  //ASSERT_TRUE(TRIPKI::isEqual(t0->iterPair(), t1->iterPair()));
  //ASSERT_FALSE(TRIPKI::isEqual(t0->iterPair(), t2->iterPair()));

  ASSERT_TRUE(TupleType::KeyInfo::getHashValue(t0) == TupleType::KeyInfo::getHashValue(t1));
  ASSERT_FALSE(TupleType::KeyInfo::getHashValue(t0) == TupleType::KeyInfo::getHashValue(t2));
  ASSERT_TRUE(TupleType::KeyInfo::isEqual(t0, t1));
  ASSERT_FALSE(TupleType::KeyInfo::isEqual(t0, t2));

  ASSERT_TRUE((void *)t0 == (void *)t1);
  ASSERT_TRUE(t0 == t1);
  ASSERT_FALSE(t0 != t1);
  ASSERT_FALSE((void *)t0 == (void *)t2);
  ASSERT_FALSE(t0 == t2);
  ASSERT_TRUE(t0 != t2);

  ASSERT_EQ(1u, t0->size());
  ASSERT_TRUE(t0->begin()->isEqual(&IntType::instance));
  ASSERT_TRUE((*t0)[0].isEqual(&IntType::instance));

  ASSERT_TRUE(t0->isSingular());
  //static TupleType * get(const TypeRefList & trefs) {
  //  return get(&*trefs.begin(), &*trefs.end());
}

TEST(TypeTest, TupleTypeTest2) {
  SourceFile testSource("");
  Module testModule(&testSource, "test");
  TypeDefn * de = new TypeDefn(&testModule, "test");
  CompositeType * testType = new CompositeType(Type::Class, de, &testModule);
  de->addTrait(Defn::Singular);
  de->setTypeValue(testType);
  TypeRef objectTypeRef(testType);

  TupleType * t0 = TupleType::get(&objectTypeRef, &objectTypeRef + 1);
  TupleType * t1 = TupleType::get(&objectTypeRef, &objectTypeRef + 1);

  ASSERT_TRUE(TupleType::KeyInfo::getHashValue(t0) == TupleType::KeyInfo::getHashValue(t1));
  ASSERT_TRUE(TupleType::KeyInfo::isEqual(t0, t1));

  ASSERT_TRUE((void *)t0 == (void *)t1);
  ASSERT_TRUE(t0 == t1);
  ASSERT_FALSE(t0 != t1);
  ASSERT_TRUE((void *)t0 == (void *)t1);

  ASSERT_EQ(1u, t0->size());
  ASSERT_TRUE(t0->begin()->isEqual(testType));
  ASSERT_TRUE(t0->isSingular());
}

#if 0
// STUFF TO TEST...!

/// -------------------------------------------------------------------
/// Interface for types.
class Type : public GC, public Formattable {
public:

  virtual const IterableScope * memberScope() const { return NULL; }
  virtual size_t numTypeParams() const { return 0; }
  virtual TypeRef typeParam(int index) const;
  virtual bool isEqual(const Type * other) const;
  virtual bool isSubtype(const Type * other) const = 0;
  virtual bool includes(const Type * other) const { return isEqual(other); }

  //virtual bool is

  virtual bool isReferenceType() const = 0;
  virtual bool isSingular() const = 0;
  bool isVoidType() const;
  bool isUnsizedIntType() const;

  /** Determine if the specified 'fromType' can be converted to this
      type. Returns the degree of compatibility. */
  ConversionRank convert(const Conversion & conversion) const;

  /** Type-specific implementation of convert. */
  virtual ConversionRank convertImpl(const Conversion & conversion) const = 0;

  /** Some convenient wrappers around 'convert'. */
  ConversionRank canConvert(Expr * fromExpr, int options = 0) const;
  ConversionRank canConvert(const Type * fromType, int options = 0) const;

  /** Reverse conversion function, used when the source is a constraint. */
  virtual ConversionRank convertTo(const Type * toType) const {
    return Incompatible;
  }

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

  // Static utility functions

  /** Given two types, return the one that is more general, the higher of the two. Returns NULL
      if neither type is a specialization of the other. */
  static Type * selectLessSpecificType(Type * type1, Type * type2);

  /** Return true if type1 and type2 are type expressions that, when finalized, will
      reduce to the same type. For example, List[T] is equivalent to List[S] if
      T is a pattern variable bound to S. */
  static bool equivalent(const Type * type1, const Type * type2);

  // Structure used when using type as a key.
  struct KeyInfo {
    static inline Type * getEmptyKey() { return reinterpret_cast<Type *>(0); }
    static inline Type * getTombstoneKey() { return reinterpret_cast<Type *>(-1); }

    static unsigned getHashValue(const Type * val) {
      // TODO: Replace with hash of canonical type.
      return (uintptr_t(val) >> 4) ^ (uintptr_t(val) >> 9);
    }

    static bool isEqual(const Type * lhs, const Type * rhs) {
      // TODO: Replace with canonical comparison
      return lhs == rhs;
    }
  };
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
  TypeRef(const Type * type) : type_(const_cast<Type *>(type)), modifiers_(0) {}
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

  bool operator==(const TypeRef & other) const {
    return type_ == other.type_ && modifiers_ == other.modifiers_;
  }

  bool operator!=(const TypeRef & other) const {
    return !(*this == other);
  }

  Type::TypeClass typeClass() const { return type_->typeClass(); }

  bool isDefined() const { return type_ != NULL; }
  bool isUndefined() const { return type_ == NULL; }
  bool isVoidType() const { return isDefined() && type_->isVoidType(); }
  bool isNonVoidType() const { return isDefined() && !type_->isVoidType(); }
  bool isReferenceType() const { return isDefined() && type_->isReferenceType(); }
  bool isUnsizedIntType() const { return isDefined() && type_->isUnsizedIntType(); }
  bool isSingular() const { return isDefined() && type_->isSingular(); }
  bool isSubtype(const TypeRef & other) const;

  bool isEqual(const TypeRef & other) const {
    return type_->isEqual(other.type_) && modifiers_ == other.modifiers_;
  }

  Expr * implicitCast(const SourceLocation & loc, Expr * from, int options = 0) const;
  Expr * explicitCast(const SourceLocation & loc, Expr * from, int options = 0) const;
  ConversionRank convert(const Conversion & conversion) const;
  ConversionRank canConvert(Expr * fromExpr, int options = 0) const;
  ConversionRank canConvert(const Type * fromType, int options = 0) const;
  ConversionRank canConvert(const TypeRef & fromType, int options = 0) const;

  TypeDefn * defn() const { return type_->typeDefn(); }
  const llvm::Type * irType() const { return type_->irType(); }
  const llvm::Type * irEmbeddedType() const { return type_->irEmbeddedType(); }
  const llvm::Type * irParameterType() const { return type_->irParameterType(); }

  void trace() const {
    if (type_) { type_->mark(); }
  }

  // Structure used when using type ref as a key.
  struct KeyInfo {
    static inline TypeRef getEmptyKey() { return TypeRef(NULL, uint32_t(-1)); }
    static inline TypeRef getTombstoneKey() { return TypeRef(NULL, uint32_t(-2)); }

    static unsigned getHashValue(const TypeRef & val) {
      return Type::KeyInfo::getHashValue(val.type_) ^ val.modifiers_;
    }

    static bool isEqual(const TypeRef & lhs, const TypeRef & rhs) {
      return Type::KeyInfo::isEqual(lhs.type_, rhs.type_) && lhs.modifiers_ == rhs.modifiers_;
    }

    static bool isPod() { return true; }
  };
};

/// -------------------------------------------------------------------
/// A pair of type refs - used as a map key.
class TypePair {
public:
  TypePair(const TypeRef & first, const TypeRef & second) : first_(first), second_(second) {}
  TypePair(const TypePair & src) : first_(src.first_), second_(src.second_) {}

  const TypeRef & first() { return first_; }
  const TypeRef & second() { return second_; }

  bool operator==(const TypePair & other) const {
    return first_ == other.first_ && second_ == other.second_;
  }

  bool operator!=(const TypePair & other) const {
    return !(*this == other);
  }

  // Structure used when using type ref as a key.
  struct KeyInfo {
    static inline TypePair getEmptyKey() {
      return TypePair(TypeRef(NULL, uint32_t(-1)), TypeRef(NULL, uint32_t(-1)));
    }

    static inline TypePair getTombstoneKey() {
      return TypePair(TypeRef(NULL, uint32_t(-2)), TypeRef(NULL, uint32_t(-2)));
    }

    static unsigned getHashValue(const TypePair & val) {
      return TypeRef::KeyInfo::getHashValue(val.first_) ^
          (TypeRef::KeyInfo::getHashValue(val.second_) << 1);
    }

    static bool isEqual(const TypePair & lhs, const TypePair & rhs) {
      return TypeRef::KeyInfo::isEqual(lhs.first_, rhs.first_) &&
          TypeRef::KeyInfo::isEqual(lhs.second_, rhs.second_);
    }
  };
};

// -------------------------------------------------------------------
// Utility functions

// Given a type, append the linkage name of that type to the output buffer.
void typeLinkageName(std::string & out, const TypeRef & ty);
void typeLinkageName(std::string & out, const Type * ty);
void typeLinkageName(std::string & out, TupleType * tv);

/** Given two types, try and find the narrowest type that both
    can be converted to.
  */
Type * findCommonType(Type * t0, Type * t1);

/** Given a pointer to a type, dereference any aliases and return the
    real underlying type. */
const Type * dealias(const Type * t);
Type * dealias(Type * t);
TypeRef dealias(const TypeRef & tr);

/** Type equality functor. */
class TypeEquals {
public:
  bool operator()(const Type * t0, const Type * t1);
  bool operator()(const TypeRef & t0, const TypeRef & t1);
};

/** Type 'less than' operator for sorting lists of types. */
class TypeLess {
public:
  bool operator()(const Type * t0, const Type * t1);
  bool operator()(const TypeRef & t0, const TypeRef & t1);
};

}

#endif

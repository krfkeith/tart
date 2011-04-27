/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/Type/Type.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Defn/Module.h"
#include "tart/Common/Diagnostics.h"
#include "FakeSourceFile.h"
#include "TestHelpers.h"

using namespace tart;

class TupleTest : public testing::Test {
public:
  Module * testModule;
  TypeDefn * testTypeDefn;
  Type * testClass;

  Type * intTypes[4];
  Type * shortTypes[4];
  Type * testClassTypes[1];
  Type * intAndTestClass[4];

  TupleTest() {
    // Set up test class.
    testModule = new Module(new SourceFile("TypeTest"), "test");
    testTypeDefn = new TypeDefn(testModule, "test");
    testClass = new CompositeType(Type::Class, testTypeDefn, testModule);
    testTypeDefn->addTrait(Defn::Singular);
    testTypeDefn->setTypeValue(testClass);

    // Set up arrays of types
    intTypes[0] = intTypes[1] = intTypes[2] = intTypes[3] = &Int32Type::instance;
    shortTypes[0] = shortTypes[1] = shortTypes[2] = shortTypes[3] = &Int16Type::instance;
    intAndTestClass[0] = intAndTestClass[2] = &Int32Type::instance;
    intAndTestClass[1] = intAndTestClass[3] = testClassTypes[0] = testClass;
  }
};

TEST_F(TupleTest, FindCommonType) {
  EXPECT_EQ(&Int32Type::instance,
      findCommonType(&Int32Type::instance, &Int32Type::instance));

  EXPECT_EQ(&Int32Type::instance,
      findCommonType(&Int32Type::instance, &Int16Type::instance));
}

TEST_F(TupleTest, TypeAttributes) {
}

#if 0
TEST(TypeTest, TypePairTest) {
  typedef TypeTupleKeyInfo TRIPKI;

  TypeTupleKey p0(&Int32Type::instance, &Int32Type::instance + 1);
  TypeTupleKey p1(&Int32Type::instance, &Int32Type::instance + 1);
  TypeTupleKey p2(&Int16Type::instance, &Int16Type::instance + 1);

  ASSERT_TRUE(TRIPKI::getHashValue(p0) == TRIPKI::getHashValue(p1));
  ASSERT_FALSE(TRIPKI::getHashValue(p0) == TRIPKI::getHashValue(p2));
  ASSERT_TRUE(TRIPKI::isEqual(p0, p1));
  ASSERT_FALSE(TRIPKI::isEqual(p0, p2));
}
#endif

TEST_F(TupleTest, TupleTypeTest) {
  //typedef TypeRefIterPairKeyInfo TRIPKI;

  TupleType * t0 = TupleType::get(&Int32Type::instance);
  TupleType * t1 = TupleType::get(&Int32Type::instance);
  TupleType * t2 = TupleType::get(&Int16Type::instance);

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
  ASSERT_TRUE((*t0->begin())->isEqual(&Int32Type::instance));
  ASSERT_TRUE((*t0)[0]->isEqual(&Int32Type::instance));

  ASSERT_TRUE(t0->isSingular());
}

TEST_F(TupleTest, TupleTypeTest2) {
  TupleType * t0 = TupleType::get(&testClassTypes[0], &testClassTypes[1]);
  TupleType * t1 = TupleType::get(&testClassTypes[0], &testClassTypes[1]);

  ASSERT_TRUE(TupleType::KeyInfo::getHashValue(t0) == TupleType::KeyInfo::getHashValue(t1));
  ASSERT_TRUE(TupleType::KeyInfo::isEqual(t0, t1));

  ASSERT_TRUE((void *)t0 == (void *)t1);
  ASSERT_TRUE(t0 == t1);
  ASSERT_FALSE(t0 != t1);
  ASSERT_TRUE((void *)t0 == (void *)t1);

  ASSERT_EQ(1u, t0->size());
  ASSERT_TRUE(t0->member(0)->isEqual(testClass));
  ASSERT_TRUE(t0->isSingular());
}

/*TEST_F(TupleTest, LargeValue) {
  TupleType * t0 = TupleType::get(&testClassTypes[0], &testClassTypes[1]);
  TupleType * t1 = TupleType::get(&testClassTypes[0], &testClassTypes[2]);
  TupleType * t2 = TupleType::get(&testClassTypes[0], &testClassTypes[3]);
  TupleType * t3 = TupleType::get(&testClassTypes[0], &testClassTypes[4]);

  ASSERT_FALSE(t0->isLargeValueType());
  ASSERT_FALSE(t1->isLargeValueType());
  ASSERT_TRUE(t2->isLargeValueType());
  ASSERT_TRUE(t3->isLargeValueType());
}*/

TEST_F(TupleTest, TupleConversion) {
  TupleType * t0 = TupleType::get(&testClassTypes[0], &testClassTypes[2]);
  TupleType * t1 = TupleType::get(&testClassTypes[0], &testClassTypes[2]);
  TupleType * t2 = TupleType::get(&intTypes[0], &intTypes[2]);
  TupleType * t3 = TupleType::get(&shortTypes[0], &shortTypes[2]);

  ASSERT_EQ(IdenticalTypes, t0->canConvert(t1));
  ASSERT_EQ(IdenticalTypes, t1->canConvert(t0));
  ASSERT_EQ(ExactConversion, t2->canConvert(t3));
  ASSERT_EQ(Truncation, t3->canConvert(t2));
  ASSERT_EQ(Incompatible, t0->canConvert(t3));
  ASSERT_EQ(Incompatible, t3->canConvert(t0));
}


#if 0
// STUFF TO TEST...!

/// -------------------------------------------------------------------
/// Interface for types.
class Type : public GC, public Formattable {
public:

  virtual const IterableScope * memberScope() const { return NULL; }
  virtual size_t numTypeParams() const { return 0; }
  virtual const Type * typeParam(int index) const;
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
  virtual ConversionRank convertTo(const Type * toType, const Conversion & cn) const {
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
      return Type::KeyInfo::isEqual(lhs.first_, rhs.first_) &&
          Type::KeyInfo::isEqual(lhs.second_, rhs.second_);
    }

    static bool isPod() { return true; }
  };

private:
  const Type * first_;
  const Type * second_;
};

// -------------------------------------------------------------------
// Utility functions

// Given a type, append the linkage name of that type to the output buffer.
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

/** Type equality functor. */
class TypeEquals {
public:
  bool operator()(const Type * t0, const Type * t1);
};

/** Type 'less than' operator for sorting lists of types. */
class TypeLess {
public:
  bool operator()(const Type * t0, const Type * t1);
};

}

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>

#include "tart/CFG/CompositeType.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/UnionType.h"

#include "FakeSourceFile.h"
#include "TestHelpers.h"

namespace {
using namespace tart;

class UnionTest : public testing::Test {
protected:
  SourceLocation loc;
  SourceFile testSource;
  Module testModule;
  Type * testClass;

  UnionTest() : testSource(""), testModule(&testSource, "test") {
    // Set up test class.
    TypeDefn * testClassDef = new TypeDefn(&testModule, "Test");
    testClass = new CompositeType(Type::Class, testClassDef, &testModule);
    testClassDef->addTrait(Defn::Singular);
    testClassDef->setTypeValue(testClass);
  }
};

TEST_F(UnionTest, CreateUnion) {
  ConstTypeList memberTypes;
  memberTypes.push_back(&Int32Type::instance);
  memberTypes.push_back(&FloatType::instance);
  UnionType * utype = UnionType::get(loc, memberTypes);

  ASSERT_EQ(2u, utype->members().size());
  ASSERT_EQ(2u, utype->typeArgs()->size());
  ASSERT_EQ(2u, utype->numTypeParams());
  ASSERT_EQ(&Int32Type::instance, utype->typeParam(0));
  ASSERT_EQ(&FloatType::instance, utype->typeParam(1));
  ASSERT_FALSE(utype->hasVoidType());
  ASSERT_FALSE(utype->hasNullType());
  ASSERT_FALSE(utype->isSingleOptionalType());
  ASSERT_FALSE(utype->hasRefTypesOnly());
  ASSERT_EQ(&Int32Type::instance, utype->getFirstNonVoidType());
  ASSERT_TRUE(utype->isSingular());
}

TEST_F(UnionTest, CreateUnionVarargs) {
  UnionType * utype = UnionType::get(loc, &Int32Type::instance, &FloatType::instance, NULL);
  ASSERT_EQ(2u, utype->members().size());
  ASSERT_EQ(2u, utype->typeArgs()->size());
  ASSERT_EQ(2u, utype->numTypeParams());
  ASSERT_EQ(&Int32Type::instance, utype->typeParam(0));
  ASSERT_EQ(&FloatType::instance, utype->typeParam(1));
  ASSERT_FALSE(utype->hasVoidType());
  ASSERT_FALSE(utype->hasNullType());
  ASSERT_FALSE(utype->isSingleOptionalType());
  ASSERT_FALSE(utype->hasRefTypesOnly());
  ASSERT_EQ(&Int32Type::instance, utype->getFirstNonVoidType());
  ASSERT_TRUE(utype->isSingular());
}

TEST_F(UnionTest, GetTypeIndex) {
  UnionType * utype = UnionType::get(loc, &Int32Type::instance, &FloatType::instance, NULL);
  ASSERT_EQ(0, utype->getTypeIndex(&Int32Type::instance));
  ASSERT_EQ(1, utype->getTypeIndex(&FloatType::instance));
  ASSERT_EQ(&Int32Type::instance, utype->getFirstNonVoidType());
}

TEST_F(UnionTest, GetTypeSorting) {
  UnionType * utype = UnionType::get(loc, &FloatType::instance, &Int32Type::instance, NULL);
  ASSERT_EQ(0, utype->getTypeIndex(&Int32Type::instance));
  ASSERT_EQ(1, utype->getTypeIndex(&FloatType::instance));
  ASSERT_EQ(&Int32Type::instance, utype->getFirstNonVoidType());
}

TEST_F(UnionTest, IntOrVoidUnion) {
  UnionType * utype = UnionType::get(loc, &FloatType::instance, &VoidType::instance, NULL);
  ASSERT_TRUE(utype->hasVoidType());
  ASSERT_FALSE(utype->hasNullType());
  ASSERT_TRUE(utype->isSingleOptionalType());
  ASSERT_FALSE(utype->hasRefTypesOnly());
  ASSERT_EQ(&FloatType::instance, utype->getFirstNonVoidType());
  ASSERT_TRUE(utype->isSingular());
}

TEST_F(UnionTest, IntOrRefTypeUnion) {
  UnionType * utype = UnionType::get(loc, &Int32Type::instance, testClass, NULL);
  ASSERT_FALSE(utype->hasVoidType());
  ASSERT_FALSE(utype->hasNullType());
  ASSERT_FALSE(utype->isSingleOptionalType());
  ASSERT_FALSE(utype->hasRefTypesOnly());
  ASSERT_EQ(testClass, utype->getFirstNonVoidType());
  ASSERT_TRUE(utype->isSingular());
}

TEST_F(UnionTest, NullOrRefTypeUnion) {
  UnionType * utype = UnionType::get(loc, &NullType::instance, testClass, NULL);
  ASSERT_FALSE(utype->hasVoidType());
  ASSERT_TRUE(utype->hasNullType());
  ASSERT_TRUE(utype->isSingleOptionalType());
  ASSERT_TRUE(utype->hasRefTypesOnly());
  ASSERT_EQ(testClass, utype->getFirstNonVoidType());
  ASSERT_TRUE(utype->isSingular());
}

}

#if 0
  /** Return true if this union contains only reference types. (Including Null). This means
      that the type can be represented as a single pointer with no discriminator field. */
  bool hasRefTypesOnly() const;

  /** Create a typecast from this type to the desired type. */
  Expr * createDynamicCast(Expr * from, const Type * toType) const;

  // Overrides

  ConversionRank convertImpl(const Conversion & conversion) const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool includes(const Type * other) const;
#endif

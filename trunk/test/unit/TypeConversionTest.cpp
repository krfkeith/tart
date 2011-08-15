/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>

#include "tart/Defn/Module.h"
#include "tart/Defn/TypeDefn.h"

#include "tart/Type/EnumType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeRelation.h"
#include "tart/Type/TypeConversion.h"
#include "tart/Type/UnionType.h"

#include "tart/Common/Diagnostics.h"

#include "FakeSourceFile.h"
#include "TestHelpers.h"

using namespace tart;

class TypeConversionTest : public testing::Test {
protected:
  Module * testModule;

  virtual void SetUp() {
    testModule = new Module(NULL, "testModule");
  }

  virtual void TearDown() {}
};

TEST_F(TypeConversionTest, EnumConversion) {
  EnumType * eType1 = new EnumType(new TypeDefn(testModule, "e1"), testModule);
  eType1->setBaseType(&Int32Type::instance);
  EnumType * eType2 = new EnumType(new TypeDefn(testModule, "e2"), testModule);
  eType2->setBaseType(&UInt8Type::instance);

  EXPECT_EQ(IdenticalTypes, TypeConversion::check(eType1, eType1));
  EXPECT_EQ(Incompatible, TypeConversion::check(eType1, eType2));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, eType1));
  EXPECT_EQ(Incompatible, TypeConversion::check(&CharType::instance, eType1));
  EXPECT_EQ(Incompatible, TypeConversion::check(&Int8Type::instance, eType1));
  EXPECT_EQ(Incompatible, TypeConversion::check(&Int16Type::instance, eType1));
  EXPECT_EQ(Incompatible, TypeConversion::check(&Int32Type::instance, eType1));
  EXPECT_EQ(Incompatible, TypeConversion::check(&UInt8Type::instance, eType1));
  EXPECT_EQ(Incompatible, TypeConversion::check(&UInt8Type::instance, eType2));

  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, eType1,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&CharType::instance, eType1,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int8Type::instance, eType1,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int16Type::instance, eType1,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&Int32Type::instance, eType1,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt8Type::instance, eType1,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&UInt8Type::instance, eType2,
      TypeConversion::EXPLICIT));
}

TEST_F(TypeConversionTest, TupleConversion) {
  const Type * args1[2] = { &Int32Type::instance, &Int32Type::instance };
  const Type * args2[2] = { &Int32Type::instance, &Int16Type::instance };
  TupleType * tt1 = TupleType::get(args1);
  TupleType * tt2 = TupleType::get(args2);

  EXPECT_EQ(IdenticalTypes, TypeConversion::check(tt1, tt1));
  EXPECT_EQ(Truncation, TypeConversion::check(tt1, tt2));
}

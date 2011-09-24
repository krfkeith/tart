/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>

#include "tart/Defn/TypeDefn.h"

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TypeRelation.h"
#include "tart/Type/TypeConversion.h"

#include "tart/Common/Diagnostics.h"

#include "FakeSourceFile.h"
#include "TestHelpers.h"

using namespace tart;

TEST(TypeTest, Primitive) {
  ASSERT_EQ("void", VoidType::typedefn.name());
  ASSERT_EQ("bool", BoolType::typedefn.name());
  ASSERT_EQ("char", CharType::typedefn.name());
  ASSERT_EQ("int8", Int8Type::typedefn.name());
  ASSERT_EQ("int16",Int16Type::typedefn.name());
  ASSERT_EQ("int32",  Int32Type::typedefn.name());
  ASSERT_EQ("int64", Int64Type::typedefn.name());
  ASSERT_EQ("uint8",UInt8Type::typedefn.name());
  ASSERT_EQ("uint16",UInt16Type::typedefn.name());
  ASSERT_EQ("uint32", UInt32Type::typedefn.name());
  ASSERT_EQ("uint64",UInt64Type::typedefn.name());
  ASSERT_EQ("float",FloatType::typedefn.name());
  ASSERT_EQ("double",DoubleType::typedefn.name());
}

TEST(TypeTest, PrimitiveDef) {
  ASSERT_EQ(&VoidType::typedefn, VoidType::biDef.value());
  ASSERT_EQ(&BoolType::typedefn, BoolType::biDef.value());
  ASSERT_EQ(&CharType::typedefn, CharType::biDef.value());
  ASSERT_EQ(&Int8Type::typedefn, Int8Type::biDef.value());
  ASSERT_EQ(&Int16Type::typedefn, Int16Type::biDef.value());
  ASSERT_EQ(&Int32Type::typedefn, Int32Type::biDef.value());
  ASSERT_EQ(&Int64Type::typedefn, Int64Type::biDef.value());
  ASSERT_EQ(&UInt8Type::typedefn, UInt8Type::biDef.value());
  ASSERT_EQ(&UInt16Type::typedefn, UInt16Type::biDef.value());
  ASSERT_EQ(&UInt32Type::typedefn, UInt32Type::biDef.value());
  ASSERT_EQ(&UInt64Type::typedefn, UInt64Type::biDef.value());
  ASSERT_EQ(&FloatType::typedefn, FloatType::biDef.value());
  ASSERT_EQ(&DoubleType::typedefn, DoubleType::biDef.value());
}

TEST(TypeTest, DynCasting) {
  ASSERT_EQ(&DoubleType::instance,
      dyn_cast<PrimitiveType>(DoubleType::typedefn.typePtr()));
}

TEST(TypeTest, SpecificityTests) {
  EXPECT_TRUE(TypeRelation::isSubtype(&VoidType::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &Int16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &Int32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&VoidType::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &VoidType::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&BoolType::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &Int16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &Int32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&BoolType::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &BoolType::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&CharType::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &Int16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &Int32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&CharType::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &CharType::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int8Type::instance, &Int8Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int8Type::instance, &Int16Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int8Type::instance, &Int32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int8Type::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int8Type::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &Int8Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int16Type::instance, &Int16Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int16Type::instance, &Int32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int16Type::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int16Type::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &Int16Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int32Type::instance, &Int32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int32Type::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int32Type::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &Int16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &Int32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&Int64Type::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&Int64Type::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&UInt8Type::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt8Type::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt8Type::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt8Type::instance, &Int8Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt8Type::instance, &Int16Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt8Type::instance, &Int32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt8Type::instance, &Int64Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt8Type::instance, &UInt8Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt8Type::instance, &UInt16Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt8Type::instance, &UInt32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt8Type::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt8Type::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt8Type::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&UInt16Type::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt16Type::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt16Type::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt16Type::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt16Type::instance, &Int16Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt16Type::instance, &Int32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt16Type::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt16Type::instance, &UInt8Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt16Type::instance, &UInt16Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt16Type::instance, &UInt32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt16Type::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt16Type::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt16Type::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &Int16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &Int32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt32Type::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &UInt16Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt32Type::instance, &UInt32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt32Type::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt32Type::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &Int16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &Int32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &UInt32Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&UInt64Type::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &FloatType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&UInt64Type::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &Int16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &Int32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&FloatType::instance, &UInt64Type::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&FloatType::instance, &FloatType::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&FloatType::instance, &DoubleType::instance));

  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &VoidType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &BoolType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &CharType::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &Int8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &Int16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &Int32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &Int64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &UInt8Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &UInt16Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &UInt32Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &UInt64Type::instance));
  EXPECT_FALSE(TypeRelation::isSubtype(&DoubleType::instance, &FloatType::instance));
  EXPECT_TRUE(TypeRelation::isSubtype(&DoubleType::instance, &DoubleType::instance));
}

TEST(TypeTest, ConversionTests) {
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&VoidType::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&CharType::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&Int8Type::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&Int16Type::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&Int32Type::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&Int64Type::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&UInt8Type::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&UInt16Type::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&UInt32Type::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&UInt64Type::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &VoidType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &VoidType::instance));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &BoolType::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&BoolType::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&CharType::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&Int8Type::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&Int16Type::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&Int32Type::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&Int64Type::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&UInt8Type::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&UInt16Type::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&UInt32Type::instance, &BoolType::instance));
  EXPECT_EQ(IntegerToBool, TypeConversion::check(&UInt64Type::instance, &BoolType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &BoolType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &BoolType::instance));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &CharType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, &CharType::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&CharType::instance, &CharType::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int8Type::instance, &CharType::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int16Type::instance, &CharType::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int32Type::instance, &CharType::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int64Type::instance, &CharType::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt8Type::instance, &CharType::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt16Type::instance, &CharType::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt32Type::instance, &CharType::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt64Type::instance, &CharType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &CharType::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&FloatType::instance, &CharType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &CharType::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&DoubleType::instance, &CharType::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &Int8Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&BoolType::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&CharType::instance, &Int8Type::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&Int8Type::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&Int16Type::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&Int32Type::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&Int64Type::instance, &Int8Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&UInt8Type::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt16Type::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt32Type::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt64Type::instance, &Int8Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&FloatType::instance, &Int8Type::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &Int8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&DoubleType::instance, &Int8Type::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &Int16Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&BoolType::instance, &Int16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&CharType::instance, &Int16Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int8Type::instance, &Int16Type::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&Int16Type::instance, &Int16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&Int32Type::instance, &Int16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&Int64Type::instance, &Int16Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt8Type::instance, &Int16Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&UInt16Type::instance, &Int16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt32Type::instance, &Int16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt64Type::instance, &Int16Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &Int16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&FloatType::instance, &Int16Type::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &Int16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&DoubleType::instance, &Int16Type::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &Int32Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&BoolType::instance, &Int32Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&CharType::instance, &Int32Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int8Type::instance, &Int32Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int16Type::instance, &Int32Type::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&Int32Type::instance, &Int32Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&Int64Type::instance, &Int32Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt8Type::instance, &Int32Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt16Type::instance, &Int32Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&UInt32Type::instance, &Int32Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt64Type::instance, &Int32Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &Int32Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&FloatType::instance, &Int32Type::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &Int32Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&DoubleType::instance, &Int32Type::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &Int64Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&BoolType::instance, &Int64Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&CharType::instance, &Int64Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int8Type::instance, &Int64Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int16Type::instance, &Int64Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int32Type::instance, &Int64Type::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&Int64Type::instance, &Int64Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt8Type::instance, &Int64Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt16Type::instance, &Int64Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt32Type::instance, &Int64Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&UInt64Type::instance, &Int64Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &Int64Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&FloatType::instance, &Int64Type::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &Int64Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&DoubleType::instance, &Int64Type::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &UInt8Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, &UInt8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&CharType::instance, &UInt8Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int8Type::instance, &UInt8Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int16Type::instance, &UInt8Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int32Type::instance, &UInt8Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int64Type::instance, &UInt8Type::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&UInt8Type::instance, &UInt8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt16Type::instance, &UInt8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt32Type::instance, &UInt8Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt64Type::instance, &UInt8Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &UInt8Type::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&FloatType::instance, &UInt8Type::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &UInt8Type::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&DoubleType::instance, &UInt8Type::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &UInt16Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, &UInt16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&CharType::instance, &UInt16Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int8Type::instance, &UInt16Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int16Type::instance, &UInt16Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int32Type::instance, &UInt16Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int64Type::instance, &UInt16Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&UInt8Type::instance, &UInt16Type::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&UInt16Type::instance, &UInt16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt32Type::instance, &UInt16Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt64Type::instance, &UInt16Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &UInt16Type::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&FloatType::instance, &UInt16Type::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &UInt16Type::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&DoubleType::instance, &UInt16Type::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &UInt32Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, &UInt32Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&CharType::instance, &UInt32Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int8Type::instance, &UInt32Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int16Type::instance, &UInt32Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int32Type::instance, &UInt32Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int64Type::instance, &UInt32Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&UInt8Type::instance, &UInt32Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&UInt16Type::instance, &UInt32Type::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&UInt32Type::instance, &UInt32Type::instance));
  EXPECT_EQ(Truncation, TypeConversion::check(&UInt64Type::instance, &UInt32Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &UInt32Type::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&FloatType::instance, &UInt32Type::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &UInt32Type::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&DoubleType::instance, &UInt32Type::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &UInt64Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, &UInt64Type::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&CharType::instance, &UInt64Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int8Type::instance, &UInt64Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int16Type::instance, &UInt64Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int32Type::instance, &UInt64Type::instance));
  EXPECT_EQ(SignedUnsigned, TypeConversion::check(&Int64Type::instance, &UInt64Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&UInt8Type::instance, &UInt64Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&UInt16Type::instance, &UInt64Type::instance));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&UInt32Type::instance, &UInt64Type::instance));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&UInt64Type::instance, &UInt64Type::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&FloatType::instance, &UInt64Type::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&FloatType::instance, &UInt64Type::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Incompatible, TypeConversion::check(&DoubleType::instance, &UInt64Type::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&DoubleType::instance, &UInt64Type::instance,
      TypeConversion::EXPLICIT));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &FloatType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, &FloatType::instance));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&CharType::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&Int8Type::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&Int16Type::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&Int32Type::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&Int64Type::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt8Type::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt16Type::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&UInt32Type::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&UInt64Type::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&FloatType::instance, &FloatType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(Truncation, TypeConversion::check(&DoubleType::instance, &FloatType::instance));

  EXPECT_EQ(Incompatible, TypeConversion::check(&VoidType::instance, &DoubleType::instance));
  EXPECT_EQ(Incompatible, TypeConversion::check(&BoolType::instance, &DoubleType::instance));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&CharType::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&Int8Type::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&Int16Type::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&Int32Type::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&Int64Type::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt8Type::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt16Type::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(NonPreferred, TypeConversion::check(&UInt32Type::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(PrecisionLoss, TypeConversion::check(&UInt64Type::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&FloatType::instance, &DoubleType::instance,
      TypeConversion::EXPLICIT));
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&DoubleType::instance, &DoubleType::instance));
}

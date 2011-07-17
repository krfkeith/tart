/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>

#include "tart/Defn/TypeDefn.h"

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Common/Diagnostics.h"

#include "FakeSourceFile.h"
#include "TestHelpers.h"

using namespace tart;

TEST(TypeTest, Primitive) {
  ASSERT_STREQ("void", VoidType::typedefn.name());
  ASSERT_STREQ("bool", BoolType::typedefn.name());
  ASSERT_STREQ("char", CharType::typedefn.name());
  ASSERT_STREQ("int8", Int8Type::typedefn.name());
  ASSERT_STREQ("int16",Int16Type::typedefn.name());
  ASSERT_STREQ("int32",  Int32Type::typedefn.name());
  ASSERT_STREQ("int64", Int64Type::typedefn.name());
  ASSERT_STREQ("uint8",UInt8Type::typedefn.name());
  ASSERT_STREQ("uint16",UInt16Type::typedefn.name());
  ASSERT_STREQ("uint32", UInt32Type::typedefn.name());
  ASSERT_STREQ("uint64",UInt64Type::typedefn.name());
  ASSERT_STREQ("float",FloatType::typedefn.name());
  ASSERT_STREQ("double",DoubleType::typedefn.name());
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
      dyn_cast<PrimitiveType>(DoubleType::typedefn.typeValue()));
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
  EXPECT_EQ(IdenticalTypes, VoidType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, BoolType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(IdenticalTypes, BoolType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, BoolType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Incompatible, BoolType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, CharType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, CharType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(IdenticalTypes, CharType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, CharType::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(SignedUnsigned, CharType::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(SignedUnsigned, CharType::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(SignedUnsigned, CharType::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(NonPreferred, CharType::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(NonPreferred, CharType::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(NonPreferred, CharType::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(Truncation, CharType::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, CharType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, CharType::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, CharType::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(PrecisionLoss, CharType::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, Int8Type::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, Int8Type::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&CharType::instance));
  EXPECT_EQ(IdenticalTypes, Int8Type::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(SignedUnsigned, Int8Type::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, Int8Type::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, Int8Type::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(Truncation, Int8Type::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, Int16Type::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, Int16Type::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Truncation, Int16Type::instance.canConvert(&CharType::instance));
  EXPECT_EQ(ExactConversion, Int16Type::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(IdenticalTypes, Int16Type::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(Truncation, Int16Type::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(Truncation, Int16Type::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(NonPreferred, Int16Type::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(SignedUnsigned, Int16Type::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(Truncation, Int16Type::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(Truncation, Int16Type::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, Int16Type::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, Int16Type::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, Int16Type::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(Truncation, Int16Type::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, Int32Type::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, Int32Type::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(SignedUnsigned, Int32Type::instance.canConvert(&CharType::instance));
  EXPECT_EQ(ExactConversion, Int32Type::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(ExactConversion, Int32Type::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(IdenticalTypes, Int32Type::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(Truncation, Int32Type::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(NonPreferred, Int32Type::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(NonPreferred, Int32Type::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(SignedUnsigned, Int32Type::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(Truncation, Int32Type::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, Int32Type::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, Int32Type::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, Int32Type::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(Truncation, Int32Type::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, Int64Type::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, Int64Type::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, Int64Type::instance.canConvert(&CharType::instance));
  EXPECT_EQ(ExactConversion, Int64Type::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(ExactConversion, Int64Type::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(ExactConversion, Int64Type::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(IdenticalTypes, Int64Type::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(NonPreferred, Int64Type::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(NonPreferred, Int64Type::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(NonPreferred, Int64Type::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(SignedUnsigned, Int64Type::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, Int64Type::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, Int64Type::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, Int64Type::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(Truncation, Int64Type::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, UInt8Type::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, UInt8Type::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Truncation, UInt8Type::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, UInt8Type::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt8Type::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt8Type::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt8Type::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(IdenticalTypes, UInt8Type::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(Truncation, UInt8Type::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(Truncation, UInt8Type::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(Truncation, UInt8Type::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, UInt8Type::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, UInt8Type::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, UInt8Type::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(PrecisionLoss, UInt8Type::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, UInt16Type::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, UInt16Type::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Truncation, UInt16Type::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, UInt16Type::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt16Type::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt16Type::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt16Type::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(ExactConversion, UInt16Type::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(IdenticalTypes, UInt16Type::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(Truncation, UInt16Type::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(Truncation, UInt16Type::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, UInt16Type::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, UInt16Type::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, UInt16Type::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(PrecisionLoss, UInt16Type::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, UInt32Type::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, UInt32Type::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, UInt32Type::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, UInt32Type::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt32Type::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt32Type::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt32Type::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(ExactConversion, UInt32Type::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(ExactConversion, UInt32Type::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(IdenticalTypes, UInt32Type::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(Truncation, UInt32Type::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, UInt32Type::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, UInt32Type::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, UInt32Type::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(PrecisionLoss, UInt32Type::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, UInt64Type::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, UInt64Type::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, UInt64Type::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, UInt64Type::instance.canConvert(&Int8Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt64Type::instance.canConvert(&Int16Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt64Type::instance.canConvert(&Int32Type::instance));
  EXPECT_EQ(SignedUnsigned, UInt64Type::instance.canConvert(&Int64Type::instance));
  EXPECT_EQ(ExactConversion, UInt64Type::instance.canConvert(&UInt8Type::instance));
  EXPECT_EQ(ExactConversion, UInt64Type::instance.canConvert(&UInt16Type::instance));
  EXPECT_EQ(ExactConversion, UInt64Type::instance.canConvert(&UInt32Type::instance));
  EXPECT_EQ(IdenticalTypes, UInt64Type::instance.canConvert(&UInt64Type::instance));
  EXPECT_EQ(Incompatible, UInt64Type::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, UInt64Type::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Incompatible, UInt64Type::instance.canConvert(&DoubleType::instance));
  EXPECT_EQ(PrecisionLoss, UInt64Type::instance.canConvert(&DoubleType::instance, Conversion::Explicit));

  EXPECT_EQ(Incompatible, FloatType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, FloatType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(PrecisionLoss, FloatType::instance.canConvert(&CharType::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&Int8Type::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&Int16Type::instance, Conversion::Explicit));
  EXPECT_EQ(PrecisionLoss, FloatType::instance.canConvert(&Int32Type::instance, Conversion::Explicit));
  EXPECT_EQ(PrecisionLoss, FloatType::instance.canConvert(&Int64Type::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&UInt8Type::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&UInt16Type::instance, Conversion::Explicit));
  EXPECT_EQ(PrecisionLoss, FloatType::instance.canConvert(&UInt32Type::instance, Conversion::Explicit));
  EXPECT_EQ(PrecisionLoss, FloatType::instance.canConvert(&UInt64Type::instance, Conversion::Explicit));
  EXPECT_EQ(IdenticalTypes, FloatType::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(Truncation, FloatType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, DoubleType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, DoubleType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&CharType::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&Int8Type::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&Int16Type::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&Int32Type::instance, Conversion::Explicit));
  EXPECT_EQ(PrecisionLoss, DoubleType::instance.canConvert(&Int64Type::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&UInt8Type::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&UInt16Type::instance, Conversion::Explicit));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&UInt32Type::instance, Conversion::Explicit));
  EXPECT_EQ(PrecisionLoss, DoubleType::instance.canConvert(&UInt64Type::instance, Conversion::Explicit));
  EXPECT_EQ(ExactConversion, DoubleType::instance.canConvert(&FloatType::instance, Conversion::Explicit));
  EXPECT_EQ(IdenticalTypes, DoubleType::instance.canConvert(&DoubleType::instance));
}

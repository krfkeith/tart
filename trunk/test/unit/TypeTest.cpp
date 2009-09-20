/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/CFG/Type.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/Common/Diagnostics.h"
#include "FakeSourceFile.h"
#include "TestHelpers.h"

using namespace tart;

namespace tart {
  // Make node types streamable
  inline std::ostream & operator<<(std::ostream & out, const ConversionRank rank) {
    switch (rank) {
      case Incompatible: {
        out << "Incompatible";
        break;
      }

      case Truncation: {
        out << "Truncation";
        break;
      }

      case IntegerToBool: {
        out << "IntegerToBool";
        break;
      }

      case SignedUnsigned: {
        out << "SignedUnsigned";
        break;
      }

      case PrecisionLoss: {
        out << "PrecisionLoss";
        break;
      }

      case NonPreferred: {
        out << "NonPreferred";
        break;
      }

      case ExactConversion: {
        out << "ExactConversion";
        break;
      }

      case IdenticalTypes: {
        out << "IdenticalTypes";
        break;
      }
    }

    return out;
  }
}

TEST(TypeTest, Primitive) {
  ASSERT_EQ("void", VoidType::typedefn.name());
  ASSERT_EQ("bool", BoolType::typedefn.name());
  ASSERT_EQ("char", CharType::typedefn.name());
  ASSERT_EQ("byte", ByteType::typedefn.name());
  ASSERT_EQ("short",ShortType::typedefn.name());
  ASSERT_EQ("int",  IntType::typedefn.name());
  ASSERT_EQ("long", LongType::typedefn.name());
  ASSERT_EQ("ubyte",UByteType::typedefn.name());
  ASSERT_EQ("ushort",UShortType::typedefn.name());
  ASSERT_EQ("uint", UIntType::typedefn.name());
  ASSERT_EQ("ulong",ULongType::typedefn.name());
  ASSERT_EQ("float",FloatType::typedefn.name());
  ASSERT_EQ("double",DoubleType::typedefn.name());
}

TEST(TypeTest, PrimitiveDef) {
  ASSERT_EQ(&VoidType::typedefn, VoidType::biDef.value());
  ASSERT_EQ(&BoolType::typedefn, BoolType::biDef.value());
  ASSERT_EQ(&CharType::typedefn, CharType::biDef.value());
  ASSERT_EQ(&ByteType::typedefn, ByteType::biDef.value());
  ASSERT_EQ(&ShortType::typedefn, ShortType::biDef.value());
  ASSERT_EQ(&IntType::typedefn, IntType::biDef.value());
  ASSERT_EQ(&LongType::typedefn, LongType::biDef.value());
  ASSERT_EQ(&UByteType::typedefn, UByteType::biDef.value());
  ASSERT_EQ(&UShortType::typedefn, UShortType::biDef.value());
  ASSERT_EQ(&UIntType::typedefn, UIntType::biDef.value());
  ASSERT_EQ(&ULongType::typedefn, ULongType::biDef.value());
  ASSERT_EQ(&FloatType::typedefn, FloatType::biDef.value());
  ASSERT_EQ(&DoubleType::typedefn, DoubleType::biDef.value());
}

TEST(TypeTest, DynCasting) {
  ASSERT_EQ(&DoubleType::instance,
      dyn_cast<PrimitiveType>(DoubleType::typedefn.typeValue()));
}

TEST(TypeTest, SpecificityTests) {
  EXPECT_TRUE(VoidType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(VoidType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(BoolType::instance.isSubtype(&VoidType::instance));
  EXPECT_TRUE(BoolType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(BoolType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(CharType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&BoolType::instance));
  EXPECT_TRUE(CharType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(CharType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(ByteType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(ByteType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(ByteType::instance.isSubtype(&CharType::instance));
  EXPECT_TRUE(ByteType::instance.isSubtype(&ByteType::instance));
  EXPECT_TRUE(ByteType::instance.isSubtype(&ShortType::instance));
  EXPECT_TRUE(ByteType::instance.isSubtype(&IntType::instance));
  EXPECT_TRUE(ByteType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(ByteType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(ByteType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(ByteType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(ByteType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(ByteType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(ByteType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(ShortType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&ByteType::instance));
  EXPECT_TRUE(ShortType::instance.isSubtype(&ShortType::instance));
  EXPECT_TRUE(ShortType::instance.isSubtype(&IntType::instance));
  EXPECT_TRUE(ShortType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(ShortType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(IntType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&ShortType::instance));
  EXPECT_TRUE(IntType::instance.isSubtype(&IntType::instance));
  EXPECT_TRUE(IntType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(IntType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(LongType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&IntType::instance));
  EXPECT_TRUE(LongType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(LongType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(UByteType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(UByteType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(UByteType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(UByteType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(UByteType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(UByteType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(UByteType::instance.isSubtype(&LongType::instance));
  EXPECT_TRUE(UByteType::instance.isSubtype(&UByteType::instance));
  EXPECT_TRUE(UByteType::instance.isSubtype(&UShortType::instance));
  EXPECT_TRUE(UByteType::instance.isSubtype(&UIntType::instance));
  EXPECT_TRUE(UByteType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(UByteType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(UByteType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(UShortType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&UByteType::instance));
  EXPECT_TRUE(UShortType::instance.isSubtype(&UShortType::instance));
  EXPECT_TRUE(UShortType::instance.isSubtype(&UIntType::instance));
  EXPECT_TRUE(UShortType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(UShortType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(UIntType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&UShortType::instance));
  EXPECT_TRUE(UIntType::instance.isSubtype(&UIntType::instance));
  EXPECT_TRUE(UIntType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(UIntType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(ULongType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&UIntType::instance));
  EXPECT_TRUE(ULongType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&FloatType::instance));
  EXPECT_FALSE(ULongType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(FloatType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(FloatType::instance.isSubtype(&ULongType::instance));
  EXPECT_TRUE(FloatType::instance.isSubtype(&FloatType::instance));
  EXPECT_TRUE(FloatType::instance.isSubtype(&DoubleType::instance));

  EXPECT_FALSE(DoubleType::instance.isSubtype(&VoidType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&BoolType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&CharType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&ByteType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&ShortType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&IntType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&LongType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&UByteType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&UShortType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&UIntType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&ULongType::instance));
  EXPECT_FALSE(DoubleType::instance.isSubtype(&FloatType::instance));
  EXPECT_TRUE(DoubleType::instance.isSubtype(&DoubleType::instance));
}

TEST(TypeTest, TypeIncludes) {
  EXPECT_TRUE(VoidType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(CharType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(IntType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(LongType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&VoidType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&VoidType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&BoolType::instance));
  EXPECT_TRUE(BoolType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(CharType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(IntType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(LongType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&BoolType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&BoolType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&CharType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&CharType::instance));
  EXPECT_TRUE(CharType::instance.includes(&CharType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&CharType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&CharType::instance));
  EXPECT_FALSE(IntType::instance.includes(&CharType::instance));
  EXPECT_FALSE(LongType::instance.includes(&CharType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&CharType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&CharType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&CharType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&CharType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&CharType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&CharType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&ByteType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&ByteType::instance));
  EXPECT_FALSE(CharType::instance.includes(&ByteType::instance));
  EXPECT_TRUE(ByteType::instance.includes(&ByteType::instance));
  EXPECT_TRUE(ShortType::instance.includes(&ByteType::instance));
  EXPECT_TRUE(IntType::instance.includes(&ByteType::instance));
  EXPECT_TRUE(LongType::instance.includes(&ByteType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&ByteType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&ByteType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&ByteType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&ByteType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&ByteType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&ByteType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(CharType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&ShortType::instance));
  EXPECT_TRUE(ShortType::instance.includes(&ShortType::instance));
  EXPECT_TRUE(IntType::instance.includes(&ShortType::instance));
  EXPECT_TRUE(LongType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&ShortType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&ShortType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&IntType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&IntType::instance));
  EXPECT_FALSE(CharType::instance.includes(&IntType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&IntType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&IntType::instance));
  EXPECT_TRUE(IntType::instance.includes(&IntType::instance));
  EXPECT_TRUE(LongType::instance.includes(&IntType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&IntType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&IntType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&IntType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&IntType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&IntType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&IntType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&LongType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&LongType::instance));
  EXPECT_FALSE(CharType::instance.includes(&LongType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&LongType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&LongType::instance));
  EXPECT_FALSE(IntType::instance.includes(&LongType::instance));
  EXPECT_TRUE(LongType::instance.includes(&LongType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&LongType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&LongType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&LongType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&LongType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&LongType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&LongType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&UByteType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&UByteType::instance));
  EXPECT_FALSE(CharType::instance.includes(&UByteType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&UByteType::instance));
  EXPECT_TRUE(ShortType::instance.includes(&UByteType::instance));
  EXPECT_TRUE(IntType::instance.includes(&UByteType::instance));
  EXPECT_TRUE(LongType::instance.includes(&UByteType::instance));
  EXPECT_TRUE(UByteType::instance.includes(&UByteType::instance));
  EXPECT_TRUE(UShortType::instance.includes(&UByteType::instance));
  EXPECT_TRUE(UIntType::instance.includes(&UByteType::instance));
  EXPECT_TRUE(ULongType::instance.includes(&UByteType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&UByteType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&UByteType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&UShortType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&UShortType::instance));
  EXPECT_FALSE(CharType::instance.includes(&UShortType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&UShortType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&UShortType::instance));
  EXPECT_TRUE(IntType::instance.includes(&UShortType::instance));
  EXPECT_TRUE(LongType::instance.includes(&UShortType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&UShortType::instance));
  EXPECT_TRUE(UShortType::instance.includes(&UShortType::instance));
  EXPECT_TRUE(UIntType::instance.includes(&UShortType::instance));
  EXPECT_TRUE(ULongType::instance.includes(&UShortType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&UShortType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&UShortType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(CharType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(IntType::instance.includes(&UIntType::instance));
  EXPECT_TRUE(LongType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&UIntType::instance));
  EXPECT_TRUE(UIntType::instance.includes(&UIntType::instance));
  EXPECT_TRUE(ULongType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&UIntType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&UIntType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(CharType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(IntType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(LongType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&ULongType::instance));
  EXPECT_TRUE(ULongType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&ULongType::instance));
  EXPECT_FALSE(DoubleType::instance.includes(&ULongType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(CharType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(IntType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(LongType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&FloatType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&FloatType::instance));
  EXPECT_TRUE(FloatType::instance.includes(&FloatType::instance));
  EXPECT_TRUE(DoubleType::instance.includes(&FloatType::instance));

  EXPECT_FALSE(VoidType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(BoolType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(CharType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(ByteType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(ShortType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(IntType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(LongType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(UByteType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(UShortType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(UIntType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(ULongType::instance.includes(&DoubleType::instance));
  EXPECT_FALSE(FloatType::instance.includes(&DoubleType::instance));
  EXPECT_TRUE(DoubleType::instance.includes(&DoubleType::instance));
}

TEST(TypeTest, ConversionTests) {
  EXPECT_EQ(IdenticalTypes, VoidType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Incompatible, VoidType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, BoolType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(IdenticalTypes, BoolType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(IntegerToBool, BoolType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(Incompatible, BoolType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Incompatible, BoolType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, CharType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, CharType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(IdenticalTypes, CharType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, CharType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(SignedUnsigned, CharType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(SignedUnsigned, CharType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(SignedUnsigned, CharType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(NonPreferred, CharType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(NonPreferred, CharType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(NonPreferred, CharType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(Truncation, CharType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(PrecisionLoss, CharType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, CharType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, ByteType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, ByteType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(IdenticalTypes, ByteType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(SignedUnsigned, ByteType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, ByteType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, ShortType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, ShortType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Truncation, ShortType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(ExactConversion, ShortType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(IdenticalTypes, ShortType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(Truncation, ShortType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(Truncation, ShortType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(NonPreferred, ShortType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(SignedUnsigned, ShortType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(Truncation, ShortType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(Truncation, ShortType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(Truncation, ShortType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, ShortType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, IntType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, IntType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(SignedUnsigned, IntType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(ExactConversion, IntType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(ExactConversion, IntType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(IdenticalTypes, IntType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(Truncation, IntType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(NonPreferred, IntType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(NonPreferred, IntType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(SignedUnsigned, IntType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(Truncation, IntType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(Truncation, IntType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, IntType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, LongType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, LongType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, LongType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(ExactConversion, LongType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(ExactConversion, LongType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(ExactConversion, LongType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(IdenticalTypes, LongType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(NonPreferred, LongType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(NonPreferred, LongType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(NonPreferred, LongType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(SignedUnsigned, LongType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(Truncation, LongType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, LongType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, UByteType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, UByteType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Truncation, UByteType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, UByteType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(SignedUnsigned, UByteType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(SignedUnsigned, UByteType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(SignedUnsigned, UByteType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(IdenticalTypes, UByteType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(Truncation, UByteType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(Truncation, UByteType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(Truncation, UByteType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(PrecisionLoss, UByteType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, UByteType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, UShortType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, UShortType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(Truncation, UShortType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, UShortType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(SignedUnsigned, UShortType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(SignedUnsigned, UShortType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(SignedUnsigned, UShortType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(ExactConversion, UShortType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(IdenticalTypes, UShortType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(Truncation, UShortType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(Truncation, UShortType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(PrecisionLoss, UShortType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, UShortType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, UIntType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, UIntType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, UIntType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, UIntType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(SignedUnsigned, UIntType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(SignedUnsigned, UIntType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(SignedUnsigned, UIntType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(ExactConversion, UIntType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(ExactConversion, UIntType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(IdenticalTypes, UIntType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(Truncation, UIntType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(PrecisionLoss, UIntType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, UIntType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, ULongType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(ExactConversion, ULongType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, ULongType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(SignedUnsigned, ULongType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(SignedUnsigned, ULongType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(SignedUnsigned, ULongType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(SignedUnsigned, ULongType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(ExactConversion, ULongType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(ExactConversion, ULongType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(ExactConversion, ULongType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(IdenticalTypes, ULongType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(PrecisionLoss, ULongType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(PrecisionLoss, ULongType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, FloatType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, FloatType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(NonPreferred, FloatType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(IdenticalTypes, FloatType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(Truncation, FloatType::instance.canConvert(&DoubleType::instance));

  EXPECT_EQ(Incompatible, DoubleType::instance.canConvert(&VoidType::instance));
  EXPECT_EQ(Incompatible, DoubleType::instance.canConvert(&BoolType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&CharType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&ByteType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&ShortType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&IntType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&LongType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&UByteType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&UShortType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&UIntType::instance));
  EXPECT_EQ(NonPreferred, DoubleType::instance.canConvert(&ULongType::instance));
  EXPECT_EQ(ExactConversion, DoubleType::instance.canConvert(&FloatType::instance));
  EXPECT_EQ(IdenticalTypes, DoubleType::instance.canConvert(&DoubleType::instance));
}

TEST(TypeTest, FindCommonType) {
  EXPECT_EQ(&IntType::instance,
      findCommonType(&IntType::instance, &IntType::instance));

  EXPECT_EQ(&IntType::instance,
      findCommonType(&IntType::instance, &ShortType::instance));
}

#if 0
#include "tart/Parse/Parser.h"
#include "tart/Analyze/Analyzer.h"

void TestAssignable() {
    using namespace tart;

    Type * type;

    // Check specializations
    const Type * intType = ParseTypeLiteral("int", 0);
    const Type * shortType = ParseTypeLiteral("short", 0);
    const Type * floatType = ParseTypeLiteral("float", 0);
    const Type * f1Type = ParseTypeLiteral("function :int -> int", 0);
    const Type * f2Type = ParseTypeLiteral("function :short -> int", 0);
    const Type * f3Type = ParseTypeLiteral("function :int -> short", 0);
    const Type * ff1Type = ParseTypeLiteral("function (:function :int -> int) -> int", 0);
    const Type * ff2Type = ParseTypeLiteral("function (:function :short -> int) -> int", 0);

    TEST_ASSERT(intType->isAssignableFrom(shortType));
    TEST_ASSERT(!shortType->isAssignableFrom(intType));
    TEST_ASSERT(!floatType->isAssignableFrom(intType));
    TEST_ASSERT(!intType->isAssignableFrom(floatType));

    TEST_ASSERT(f1Type->isAssignableFrom(f1Type));
    TEST_ASSERT(!f1Type->isAssignableFrom(f2Type));
    TEST_ASSERT(f1Type->isAssignableFrom(f3Type));
    TEST_ASSERT(f2Type->isAssignableFrom(f1Type));
    TEST_ASSERT(f2Type->isAssignableFrom(f2Type));
    TEST_ASSERT(f2Type->isAssignableFrom(f3Type));
    TEST_ASSERT(!f3Type->isAssignableFrom(f1Type));
    TEST_ASSERT(!f3Type->isAssignableFrom(f2Type));
    TEST_ASSERT(f3Type->isAssignableFrom(f3Type));

    //TEST_ASSERT(shortType->isSubtype(intType));
    //TEST_ASSERT(!intType->isSubtype(shortType));

    TEST_ASSERT(ff1Type->isAssignableFrom(ff1Type));
    TEST_ASSERT(ff2Type->isAssignableFrom(ff2Type));
    TEST_ASSERT(ff1Type->isAssignableFrom(ff2Type));
    TEST_ASSERT(!ff2Type->isAssignableFrom(ff1Type));

/*
    {
        Type * type1 = ParseTypeLiteral("function (x:int) -> int", 0);
        Type * type2 = ParseTypeLiteral("function (x:short) -> int", 0);

        TEST_ASSERT_EQUAL(true, type2->isMoreSpecializedThan(type1));
        TEST_ASSERT_EQUAL(false, type1->isMoreSpecializedThan(type2));
    } */
}

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/Sema/ParameterAssignments.h"
#include "tart/Type/StaticType.h"
#include "tart/Defn/FunctionDefn.h"

using namespace tart;

TEST(ParameterAssigmentsTest, TestNoArgs) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target,
      &StaticFnType0<Int32Type>::value);

  ASSERT_TRUE(builder.check());
  ASSERT_TRUE(builder.isValid());
  ASSERT_EQ(size_t(0), target.size());
}

TEST(ParameterAssigmentsTest, TestPositionalAssignment) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target,
    &StaticFnType3<
      Int32Type, Int32Type, Int32Type, Int32Type>::value);

  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.check());
  ASSERT_TRUE(builder.isValid());
  ASSERT_EQ(size_t(3), target.size());
  ASSERT_EQ(0, target[0]);
  ASSERT_EQ(1, target[1]);
}

TEST(ParameterAssigmentsTest, TestKeywordAssignment) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target,
    &StaticFnType3<
      Int32Type, Int32Type, Int32Type, Int32Type>::value);

  ASSERT_TRUE(builder.addKeywordArg("a1"));
  ASSERT_TRUE(builder.addKeywordArg("a0"));
  ASSERT_TRUE(builder.addKeywordArg("a2"));
  ASSERT_TRUE(builder.check());
  ASSERT_TRUE(builder.isValid());
  ASSERT_EQ(size_t(3), target.size());
  ASSERT_EQ(1, target[0]);
  ASSERT_EQ(0, target[1]);
  ASSERT_EQ(2, target[2]);
}

TEST(ParameterAssigmentsTest, TestPositionalAndKeywordAssignment) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target,
    &StaticFnType3<
      Int32Type, Int32Type, Int32Type, Int32Type>::value);

  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addKeywordArg("a2"));
  ASSERT_TRUE(builder.check());
  ASSERT_TRUE(builder.isValid());
  ASSERT_EQ(size_t(3), target.size());
  ASSERT_EQ(0, target[0]);
  ASSERT_EQ(1, target[1]);
  ASSERT_EQ(2, target[2]);
}

TEST(ParameterAssigmentsTest, TestMissingArg) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target,
    &StaticFnType3<
      Int32Type, Int32Type, Int32Type, Int32Type>::value);

  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_FALSE(builder.check());
  ASSERT_FALSE(builder.isValid());
}

TEST(ParameterAssigmentsTest, TestReusedArg) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target,
    &StaticFnType3<
      Int32Type, Int32Type, Int32Type, Int32Type>::value);

  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_FALSE(builder.addKeywordArg("a0"));
  ASSERT_FALSE(builder.check());
  ASSERT_FALSE(builder.isValid());
}

TEST(ParameterAssigmentsTest, TestBadKeywordAssignment) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target,
    &StaticFnType3<
      Int32Type, Int32Type, Int32Type, Int32Type>::value);

  ASSERT_FALSE(builder.addKeywordArg("a4"));
  ASSERT_FALSE(builder.check());
  ASSERT_FALSE(builder.isValid());
}

TEST(ParameterAssigmentsTest, TestVariadicArg) {
  static ParameterDefn * variadicArgs[] = {
      &StaticParamDefn<Int32Type, 0>::value,
      &StaticParamDefn<Int32Type, 1, ParameterDefn::Variadic>::value,
      &StaticParamDefn<Int32Type, 2, ParameterDefn::KeywordOnly>::value,
  };
  static FunctionType variadicFunction(&Int32Type::instance, variadicArgs, 3);
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target, &variadicFunction);

  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addKeywordArg("a2"));

  ASSERT_TRUE(builder.check());
  ASSERT_TRUE(builder.isValid());

  ASSERT_EQ(size_t(6), target.size());
  ASSERT_EQ(0, target[0]);
  ASSERT_EQ(1, target[1]);
  ASSERT_EQ(1, target[2]);
  ASSERT_EQ(1, target[3]);
  ASSERT_EQ(1, target[4]);
  ASSERT_EQ(2, target[5]);
}

TEST(ParameterAssigmentsTest, TestEmptyVariadicArg) {
  static ParameterDefn * variadicArgs[] = {
      &StaticParamDefn<Int32Type, 0>::value,
      &StaticParamDefn<Int32Type, 1, ParameterDefn::Variadic>::value,
      &StaticParamDefn<Int32Type, 2, ParameterDefn::KeywordOnly>::value,
  };
  static FunctionType variadicFunction(&Int32Type::instance, variadicArgs, 3);
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target, &variadicFunction);

  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addKeywordArg("a2"));
  ASSERT_TRUE(builder.check());
  ASSERT_TRUE(builder.isValid());

  ASSERT_EQ(size_t(2), target.size());
  ASSERT_EQ(0, target[0]);
  ASSERT_EQ(2, target[1]);
}

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include <gtest/gtest.h>
#include "tart/Sema/ParameterAssignments.h"
#include "tart/CFG/StaticType.h"
#include "tart/CFG/FunctionDefn.h"

using namespace tart;

TEST(ParameterAssigmentsTest, TestNoArgs) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target, 
    &StaticFnType0<TypeId_SInt32>::value);
      
  ASSERT_TRUE(builder.check());
  ASSERT_TRUE(builder.isValid());
  ASSERT_EQ(size_t(0), target.size());
}

TEST(ParameterAssigmentsTest, TestPositionalAssignment) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target, 
    &StaticFnType3<
      TypeId_SInt32, TypeId_SInt32, TypeId_SInt32, TypeId_SInt32>::value);
      
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
      TypeId_SInt32, TypeId_SInt32, TypeId_SInt32, TypeId_SInt32>::value);
      
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

TEST(ParameterAssigmentsTest, TestMixedAssignment) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target, 
    &StaticFnType3<
      TypeId_SInt32, TypeId_SInt32, TypeId_SInt32, TypeId_SInt32>::value);
      
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
      TypeId_SInt32, TypeId_SInt32, TypeId_SInt32, TypeId_SInt32>::value);
      
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_FALSE(builder.check());
  ASSERT_FALSE(builder.isValid());
}

TEST(ParameterAssigmentsTest, TestReusedArg) {
  ParameterAssignments target;
  ParameterAssignmentsBuilder builder(target, 
    &StaticFnType3<
      TypeId_SInt32, TypeId_SInt32, TypeId_SInt32, TypeId_SInt32>::value);
      
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_TRUE(builder.addPositionalArg());
  ASSERT_FALSE(builder.addKeywordArg("a0"));
  ASSERT_FALSE(builder.check());
  ASSERT_FALSE(builder.isValid());
}

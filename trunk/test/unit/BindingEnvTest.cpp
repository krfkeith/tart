/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "tart/Defn/Template.h"
#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/Infer/TypeAssignment.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TypeConversion.h"

#include "MockProvision.h"
#include "TestHelpers.h"

namespace {
using namespace tart;
using namespace testing;

class BindingEnvTest : public testing::Test {
protected:
  void TearDown() {
    testing::Test::TearDown();
  }

  BindingEnvTest() {}
};

TEST_F(BindingEnvTest, EmptyEnv) {
  BindingEnv env;
  EXPECT_TRUE(env.empty());
  EXPECT_EQ(0l, env.stateCount());
}

TEST_F(BindingEnvTest, Assign) {
  BindingEnv env;
  TypeVariable * tv = new TypeVariable(SourceLocation(), "T");
  TypeAssignment * ta = env.assign(tv, NULL, NULL);
  EXPECT_FALSE(env.empty());
  ASSERT_TRUE(ta != NULL);
  EXPECT_TRUE(ta->next() == NULL);
  EXPECT_EQ(tv, ta->target());
}

TEST_F(BindingEnvTest, TypeAssignment) {
  BindingEnv env;
  TypeVariable * tv = new TypeVariable(SourceLocation(), "T");
  TypeAssignment * ta = env.assign(tv, NULL, NULL);

  Constraint * s = ta->constraints().insert(
      SourceLocation(), &Int32Type::instance, 0, Constraint::EXACT);
  ASSERT_TRUE(s != NULL);
  EXPECT_TRUE(s->value() == &Int32Type::instance);
  EXPECT_TRUE(s->checkProvisions());

  // Since ta == int, should convert OK
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&Int32Type::instance, ta));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int16Type::instance, ta));

  // Add another constraint
//  s = ta->add(SourceLocation(), &Int8Type::instance, Constraint::EXACT);
//  ASSERT_TRUE(s != NULL);
//  EXPECT_EQ(Truncation, TypeConversion::check(&Int32Type::instance, ta));
//  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int8Type::instance, ta));
}

TEST_F(BindingEnvTest, AssignmentProvisions) {
  BindingEnv env;
  MockProvision trueProvision;
  MockProvision falseProvision;
  TypeVariable * tv = new TypeVariable(SourceLocation(), "T");
  TypeAssignment * ta = env.assign(tv, NULL, NULL);
  ProvisionSet psTrue;
  ProvisionSet psFalse;
  psTrue.insert(&trueProvision);
  psFalse.insert(&falseProvision);

  Constraint * s = ta->constraints().insert(
      SourceLocation(), &Int32Type::instance, 0, Constraint::EXACT, psTrue);
  EXPECT_CALL(trueProvision, check()).WillRepeatedly(Return(true));
  EXPECT_TRUE(s->checkProvisions());
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&Int32Type::instance, ta));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int16Type::instance, ta));

  s = ta->constraints().insert(
      SourceLocation(), &Int8Type::instance, 0, Constraint::EXACT, psFalse);
  EXPECT_CALL(falseProvision, check()).WillRepeatedly(Return(false));
  EXPECT_FALSE(s->checkProvisions());

  // Since the second constraint is ignored, results should not change.
  EXPECT_EQ(IdenticalTypes, TypeConversion::check(&Int32Type::instance, ta));
  EXPECT_EQ(ExactConversion, TypeConversion::check(&Int16Type::instance, ta));
}

}  // namespace

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/Sema/BindingEnv.h"
#include "tart/Type/StaticType.h"
#include "TestHelpers.h"

namespace {
using namespace tart;

class UnifyTest : public testing::Test {
protected:
  ConstantString * str;

  virtual void SetUp() {
    str = new ConstantString(SourceLocation(), "test");
  }

  virtual void TearDown() {}
};

TEST_F(UnifyTest, UnifyBasicExpr) {
  BindingEnv env;

  //ASSERT_EQ(Unify_Yes, env.unify(str, str, Invariant));
}

TEST_F(UnifyTest, UnifyBasicType) {
  BindingEnv env;
  SourceContext source(SourceLocation(), NULL);
  ASSERT_TRUE(env.unify(&source, &Int16Type::instance, &Int16Type::instance, Invariant));
}

}

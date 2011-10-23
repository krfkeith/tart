/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "TestCompiler.h"

using namespace tart;

TEST(StmtTest, SwitchStmtDuplicateCase) {
  EXPECT_COMPILE_FAILURE(
      "def func() { var a:int32 = 1; switch a { case 1 {} case 1 {} } }",
      "Duplicate case");
}

TEST(StmtTest, SwitchStmtInvalidCaseType) {
  EXPECT_COMPILE_FAILURE(
      "def func() { var a:int32 = 1; switch a { case \"Hey\" {} } }",
      "incompatible");
}

TEST(StmtTest, SwitchStmtTruncatedCaseValue) {
  EXPECT_COMPILE_FAILURE(
      "def func() { var a:int8 = 1; switch a { case 1000 {} } }",
      "can never equal");
}

TEST(StmtTest, SwitchStmtInvalidExpressionType) {
  EXPECT_COMPILE_FAILURE(
      "def func() { var a:float = 1.0; switch a { case 1.0 {} } }",
      "Invalid expression type");
}

TEST(StmtTest, SwitchStmtOnlyOneDefault) {
  EXPECT_COMPILE_FAILURE(
      "def func() { var a:int8 = 1; switch a { case 1 {} case * {} case * {} } }",
      "Only one");
}

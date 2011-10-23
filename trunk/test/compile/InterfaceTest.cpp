/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "TestCompiler.h"

using namespace tart;

TEST(InterfaceTest, Methods) {
  EXPECT_COMPILE_SUCCESS("interface Test { def func(); }");
  EXPECT_COMPILE_SUCCESS("interface Test { def func -> int; }");
}

TEST(InterfaceTest, Properties) {
  EXPECT_COMPILE_SUCCESS("interface Test { def prop:int { get; } }");
  EXPECT_COMPILE_SUCCESS("interface Test { def prop:int { set; } }");
  EXPECT_COMPILE_SUCCESS("interface Test { def prop:int { get; set; } }");
}

TEST(InterfaceTest, DataMemberNotAllowed) {
  EXPECT_COMPILE_FAILURE("interface Test { var x:int32; }", "Data member not allowed");
}

TEST(InterfaceTest, MethodBodyNotAllowed) {
  EXPECT_COMPILE_FAILURE("interface Test { def func() {} }", "body not allowed");
  EXPECT_COMPILE_FAILURE(
      "interface Test { def prop:int { get { return 0; } } }",
      "body not allowed");
}

TEST(InterfaceTest, NoAccessors) {
  EXPECT_COMPILE_FAILURE("interface Test { def prop:int {} }", "must contain");
}

TEST(InterfaceTest, Visbility) {
  EXPECT_COMPILE_SUCCESS("interface Test { public def func(); }");
  EXPECT_COMPILE_FAILURE("interface Test { internal def func(); }", "cannot be non-public");
  EXPECT_COMPILE_FAILURE("interface Test { private def func(); }", "cannot be non-public");
  EXPECT_COMPILE_FAILURE("interface Test { protected def func(); }", "cannot be non-public");
}

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "TestCompiler.h"

using namespace tart;

TEST(MethodDefTest, RequireMethodBody) {
  EXPECT_COMPILE_SUCCESS("class Test { def func() -> void {} }");
  EXPECT_COMPILE_FAILURE("class Test { def func() -> void; }", "body required");
}

TEST(MethodDefTest, AbstractMethodInNonAbstractClass) {
  EXPECT_COMPILE_SUCCESS("abstract class Test { abstract def func() -> void; }");
  EXPECT_COMPILE_FAILURE("class Test { abstract def func() -> void; }", "declared abstract");
}

TEST(MethodDefTest, AbstractMethodCannotHaveBody) {
  EXPECT_COMPILE_SUCCESS("abstract class Test { abstract def func() -> void; }");
  EXPECT_COMPILE_FAILURE(
      "abstract class Test { abstract def func() -> void {} }", "cannot have a body");
}

TEST(MethodDefTest, SignatureConflict) {
  EXPECT_COMPILE_SUCCESS("class Test { def func1() {} def func2() {} }");
  EXPECT_COMPILE_FAILURE("class Test { def func() {} def func() {} }", "signature conflict");
}

TEST(MethodDefTest, ExternWithBody) {
  EXPECT_COMPILE_SUCCESS(
      "abstract class Test { @Extern(\"foo\") def func() -> void; }");
  EXPECT_COMPILE_FAILURE(
      "abstract class Test { @Extern(\"foo\") def func() -> void {} }", "cannot have a body");
}

TEST(MethodDefTest, UndefMethodWithBody) {
  EXPECT_COMPILE_FAILURE("class Test { undef func() -> void {} }", "cannot have a body");
}

TEST(MethodDefTest, UndefMethodWithNoOverride) {
  EXPECT_COMPILE_SUCCESS("class Base { def func() {} } class Test : Base { undef func(); }");
  EXPECT_COMPILE_FAILURE("class Test { undef func() -> void; }", "does not override");
}

TEST(MethodDefTest, Override) {
  EXPECT_COMPILE_SUCCESS("class Base { def func() {} } class Test : Base { override func() {} }");
  EXPECT_COMPILE_FAILURE(
      "class Base { def func() {} } class Test : Base { def func() {} }",
      "override");
  EXPECT_COMPILE_FAILURE(
      "class Base { def func() {} } class Test : Base { override func() -> int { return 0; } }",
      "hidden");
}

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "TestCompiler.h"

using namespace tart;

TEST(TypeModifierTest, ModifierConflict) {
  EXPECT_COMPILE_FAILURE("readonly immutable class Test {}", "Conflicting");
  EXPECT_COMPILE_FAILURE("readonly mutable class Test {}", "Conflicting");
  EXPECT_COMPILE_FAILURE("mutable immutable class Test {}", "Conflicting");
}

TEST(TypeModifierTest, ClassDefnModifiers) {
  EXPECT_COMPILE_FAILURE("adopted class Test {}", "adopted");
  EXPECT_COMPILE_SUCCESS("readonly class Test {}");
  EXPECT_COMPILE_FAILURE("mutable class Test {}", "mutable");
  EXPECT_COMPILE_SUCCESS("immutable class Test {}");
  EXPECT_COMPILE_FAILURE("adopted interface Test {}", "adopted");
  EXPECT_COMPILE_SUCCESS("readonly interface Test {}");
  EXPECT_COMPILE_FAILURE("mutable interface Test {}", "mutable");
  EXPECT_COMPILE_SUCCESS("immutable interface Test {}");
  EXPECT_COMPILE_FAILURE("adopted struct Test {}", "adopted");
  EXPECT_COMPILE_SUCCESS("readonly struct Test {}");
  EXPECT_COMPILE_FAILURE("mutable struct Test {}", "mutable");
  EXPECT_COMPILE_SUCCESS("immutable struct Test {}");
  EXPECT_COMPILE_FAILURE("adopted protocol Test {}", "adopted");
  EXPECT_COMPILE_SUCCESS("readonly protocol Test {}");
  EXPECT_COMPILE_FAILURE("mutable protocol Test {}", "mutable");
  EXPECT_COMPILE_SUCCESS("immutable protocol Test {}");
}

TEST(TypeModifierTest, EnumDefnModifiers) {
  EXPECT_COMPILE_FAILURE("abstract enum Test {}", "abstract");
  EXPECT_COMPILE_FAILURE("final enum Test {}", "final");
  EXPECT_COMPILE_FAILURE("adopted enum Test {}", "cannot be used as a declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly enum Test {}", "modifiers");
  EXPECT_COMPILE_FAILURE("mutable enum Test {}", "modifiers");
  EXPECT_COMPILE_FAILURE("immutable enum Test {}", "modifiers");
}

TEST(TypeModifierTest, NamespaceDefnModifiers) {
  EXPECT_COMPILE_FAILURE("abstract namespace Test {}", "abstract");
  EXPECT_COMPILE_FAILURE("final namespace Test {}", "final");
  EXPECT_COMPILE_FAILURE("adopted namespace Test {}", "cannot be used as a declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly namespace Test {}", "modifiers");
  EXPECT_COMPILE_FAILURE("mutable namespace Test {}", "modifiers");
  EXPECT_COMPILE_FAILURE("immutable namespace Test {}", "modifiers");
}

TEST(TypeModifierTest, GlobalValueDefnModifiers) {
  EXPECT_COMPILE_FAILURE("adopted var test:int;", "cannot be used as a declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly var test:int;", "Use 'let'");
  EXPECT_COMPILE_FAILURE("mutable var test:int;", "instance");
  EXPECT_COMPILE_FAILURE("immutable var test:int;", "Use 'let'");
  EXPECT_COMPILE_FAILURE("adopted let test:int;", "cannot be used as a declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly let test:int;", "are always");
  EXPECT_COMPILE_FAILURE("mutable let test:int;", "cannot be declared");
  EXPECT_COMPILE_FAILURE("immutable let test:int;", "are always");
  EXPECT_COMPILE_FAILURE("adopted def test:int { get { return 0; } }", "declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly def test:int { get { return 0; } }", "modifiers");
  EXPECT_COMPILE_FAILURE("mutable def test:int { get { return 0; } }", "modifiers");
  EXPECT_COMPILE_FAILURE("immutable def test:int { get { return 0; } }", "modifiers");
}

TEST(TypeModifierTest, MemberValueDefnModifiers) {
  EXPECT_COMPILE_FAILURE("class Test { adopted var test:int; }", "declaration modifier");
  EXPECT_COMPILE_FAILURE("class Test { readonly var test:int; }", "Use 'let'");
  EXPECT_COMPILE_SUCCESS("class Test { mutable var test:int; }");
  EXPECT_COMPILE_FAILURE("class Test { immutable var test:int; }", "Use 'let'");
  EXPECT_COMPILE_FAILURE("class Test { adopted let test:int; }", "declaration modifier");
  EXPECT_COMPILE_WARNING("class Test { readonly let test:int; }", "are always");
  EXPECT_COMPILE_FAILURE("class Test { mutable let test:int; }", "cannot be declared");
  EXPECT_COMPILE_WARNING("class Test { immutable let test:int; }", "are always");
  EXPECT_COMPILE_FAILURE(
      "class Test { adopted def test:int {  { return 0; } } }", "declaration modifier");
  EXPECT_COMPILE_FAILURE("class Test { readonly def test:int { get { return 0; } } }", "modifiers");
  EXPECT_COMPILE_FAILURE("class Test { mutable def test:int { get { return 0; } } }", "modifiers");
  EXPECT_COMPILE_FAILURE(
      "class Test { immutable def test:int { get { return 0; } } }", "modifiers");
}

TEST(TypeModifierTest, GlobalFunctionDefnModifiers) {
  EXPECT_COMPILE_FAILURE("adopted def test -> void {}", "declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly def test -> void {}", "Only instance");
  EXPECT_COMPILE_FAILURE("mutable def test -> void {}", "invalid type modifier");
  EXPECT_COMPILE_FAILURE("immutable def test -> void {}", "invalid type modifier");
}

TEST(TypeModifierTest, MemberFunctionDefnModifiers) {
  EXPECT_COMPILE_FAILURE("class Test { adopted def test -> void {} }", "declaration modifier");
  EXPECT_COMPILE_SUCCESS("class Test { readonly def test -> void {} }");
  EXPECT_COMPILE_FAILURE("class Test { mutable def test -> void {} }", "invalid type modifier");
  EXPECT_COMPILE_FAILURE("class Test { immutable def test -> void {} }", "invalid type modifier");
}

TEST(TypeModifierTest, ReadOnlyMethod) {
  EXPECT_COMPILE_FAILURE(
      "class Test { var m:int; readonly def test -> void { m = 0; } }",
      "write to read-only");
}

#if 0
TEST(TypeModifierTest, ReadonlyParam) {
  EXPECT_COMPILE_FAILURE(
      "def test(a:int[]) -> void { a[0] = 1; }",
      "write to read-only");
}

TEST(TypeModifierTest, ReadonlyField) {
  EXPECT_COMPILE_FAILURE(
      "class Test { var m:int; } def test(t:Test) -> void { t.m = 1; }",
      "write to read-only");
  EXPECT_COMPILE_SUCCESS("class Test { mutable var m:int; } def test(t:Test) -> void { t.m = 1; }");
}
#endif

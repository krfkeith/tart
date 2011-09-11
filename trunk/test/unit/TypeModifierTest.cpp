/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "TestCompiler.h"

#include "Tart/Common/PackageMgr.h"

using namespace tart;

class TypeModifierTest : public testing::Test {
public:
  testing::AssertionResult expectSuccess(const char * src) {
    TestCompiler tc("TypeModifierTest");
    tc.compile(src);
    if (tc.errorCount() > 0) {
      return testing::AssertionFailure() << "Compilation error: '" << tc.errorLog() << "'.";
    } else {
      return testing::AssertionSuccess();
    }
  }

  testing::AssertionResult expectError(const char * src, llvm::StringRef errStr) {
    TestCompiler tc("TypeModifierTest");
    tc.compile(src);
    if (tc.errorCount() == 0) {
      return testing::AssertionFailure() <<
          "Expected error [" << errStr << "] for input '" << src << "'";
    } else if (!tc.matchError(errStr)) {
      return testing::AssertionFailure() <<
          "Expected error [" << errStr << "], actual error was '" << tc.errorLog() << "'.";
    } else {
      return testing::AssertionSuccess();
    }
  }

  testing::AssertionResult expectWarning(const char * src, llvm::StringRef errStr) {
    TestCompiler tc("TypeModifierTest");
    tc.compile(src);
    if (tc.errorCount() != 0) {
      return testing::AssertionFailure() <<
          "Expected warning [" << errStr << "] for input '" << src << "'";
    } else if (!tc.matchError(errStr)) {
      return testing::AssertionFailure() <<
          "Expected warning [" << errStr << "], actual error was '" << tc.errorLog() << "'.";
    } else {
      return testing::AssertionSuccess();
    }
  }
};

TEST_F(TypeModifierTest, ModifierConflict) {
  EXPECT_COMPILE_FAILURE("readonly immutable class Test {}", "Conflicting");
  EXPECT_COMPILE_FAILURE("readonly mutable class Test {}", "Conflicting");
  EXPECT_COMPILE_FAILURE("mutable immutable class Test {}", "Conflicting");
}

TEST_F(TypeModifierTest, ClassDefnModifiers) {
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

TEST_F(TypeModifierTest, EnumDefnModifiers) {
  EXPECT_COMPILE_FAILURE("abstract enum Test {}", "abstract");
  EXPECT_COMPILE_FAILURE("final enum Test {}", "final");
  EXPECT_COMPILE_FAILURE("adopted enum Test {}", "cannot be used as a declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly enum Test {}", "modifiers");
  EXPECT_COMPILE_FAILURE("mutable enum Test {}", "modifiers");
  EXPECT_COMPILE_FAILURE("immutable enum Test {}", "modifiers");
}

TEST_F(TypeModifierTest, NamespaceDefnModifiers) {
  EXPECT_COMPILE_FAILURE("abstract namespace Test {}", "abstract");
  EXPECT_COMPILE_FAILURE("final namespace Test {}", "final");
  EXPECT_COMPILE_FAILURE("adopted namespace Test {}", "cannot be used as a declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly namespace Test {}", "modifiers");
  EXPECT_COMPILE_FAILURE("mutable namespace Test {}", "modifiers");
  EXPECT_COMPILE_FAILURE("immutable namespace Test {}", "modifiers");
}

TEST_F(TypeModifierTest, GlobalValueDefnModifiers) {
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

TEST_F(TypeModifierTest, MemberValueDefnModifiers) {
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

TEST_F(TypeModifierTest, GlobalFunctionDefnModifiers) {
  EXPECT_COMPILE_FAILURE("adopted def test -> void {}", "declaration modifier");
  EXPECT_COMPILE_FAILURE("readonly def test -> void {}", "Only instance");
  EXPECT_COMPILE_FAILURE("mutable def test -> void {}", "invalid type modifier");
  EXPECT_COMPILE_FAILURE("immutable def test -> void {}", "invalid type modifier");
}

TEST_F(TypeModifierTest, MemberFunctionDefnModifiers) {
  EXPECT_COMPILE_FAILURE("class Test { adopted def test -> void {} }", "declaration modifier");
  EXPECT_COMPILE_SUCCESS("class Test { readonly def test -> void {} }");
  EXPECT_COMPILE_FAILURE("class Test { mutable def test -> void {} }", "invalid type modifier");
  EXPECT_COMPILE_FAILURE("class Test { immutable def test -> void {} }", "invalid type modifier");
}

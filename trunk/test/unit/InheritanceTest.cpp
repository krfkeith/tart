/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "TestCompiler.h"

#include "Tart/Common/PackageMgr.h"

using namespace tart;

class InheritanceTest : public testing::Test {
public:
  void expectSuccess(const char * src) {
    TestCompiler tc("InheritanceTest");
    tc.compile(src);
    EXPECT_EQ(0, tc.errorCount());
  }

  void expectError(const char * src, llvm::StringRef errStr) {
    TestCompiler tc("InheritanceTest");
    tc.compile(src);
    ASSERT_NE(0, tc.errorCount()) << "Expected failure for input '" << src << "'";
    EXPECT_TRUE(tc.matchError(errStr)) << "Expected error message '" << errStr <<
        "', actual '" << tc.errorLog() << "'.";
  }
};

TEST_F(InheritanceTest, RequiredMethod) {
  expectSuccess(
      "public abstract class Test : Iterable[String] {\n"
      "  abstract def iterate -> Iterator[String]; }");
}

TEST_F(InheritanceTest, MissingMethod) {
  expectError(
      "public final class Test : Iterable[String] {}",
      "lacks implementation");
}

TEST_F(InheritanceTest, BadOverrideType) {
  expectError(
      "public final class Test : Iterable[String] { def iterate -> void {} }",
      "lacks implementation");
  expectError(
      "public final class Test : Iterable[String] { def iterate (i:int32) -> void {} }",
      "lacks implementation");
}

TEST_F(InheritanceTest, CannotInheritFromFinalType) {
  expectError(
      "public final class Test : String { def construct() {} }",
      "is final");
}

TEST_F(InheritanceTest, BadBaseForStruct) {
  expectError(
      "public final struct Test : Object {}",
      "struct can only derive from a struct");
}

TEST_F(InheritanceTest, BadBaseForInterface) {
  expectError(
      "public interface Test : Object {}",
      "interface can only inherit");
}

TEST_F(InheritanceTest, NonSingularBase) {
  expectError(
      "public final class Test : Array {}",
      "is a template");
}

TEST_F(InheritanceTest, PrimitiveBase) {
  expectError(
      "public final class Test : int32 {}",
      "Cannot inherit from int32");
}

TEST_F(InheritanceTest, CircularInheritance) {
  expectError(
      "public class Test : Test {}",
      "Circular");
}

TEST_F(InheritanceTest, MultipleInheritance) {
  expectError(
      "public class Test : Exception, Exception {}",
      "single concrete");
}

TEST_F(InheritanceTest, FinalInterface) {
  expectError(
      "public final interface Test {}",
      "Interface type cannot be final");
}

TEST_F(InheritanceTest, FinalProtocol) {
  expectError(
      "public final protocol Test {}",
      "Protocol type cannot be final");
}

TEST_F(InheritanceTest, FinalInterfaceMethod) {
  expectError(
      "public interface Test { final def iterate -> void {} }",
      "cannot be final");
}

TEST_F(InheritanceTest, PrivateInterfaceMethod) {
  expectError(
      "public interface Test { private def iterate -> void; }",
      "cannot be non-public");
}

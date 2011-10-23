/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "TestCompiler.h"

using namespace tart;

TEST(InheritanceTest, RequiredMethod) {
  EXPECT_COMPILE_SUCCESS(
      "public abstract class Test : Iterable[String] {\n"
      "  abstract def iterate -> Iterator[String]; }");
}

TEST(InheritanceTest, MissingMethod) {
  EXPECT_COMPILE_FAILURE(
      "public final class Test : Iterable[String] {}",
      "lacks implementation");
}

TEST(InheritanceTest, BadOverrideType) {
  EXPECT_COMPILE_FAILURE(
      "public final class Test : Iterable[String] { def iterate -> void {} }",
      "lacks implementation");
  EXPECT_COMPILE_FAILURE(
      "public final class Test : Iterable[String] { def iterate (i:int32) -> void {} }",
      "lacks implementation");
}

TEST(InheritanceTest, CannotInheritFromFinalType) {
  EXPECT_COMPILE_FAILURE(
      "public final class Test : String { def construct() {} }",
      "is final");
}

TEST(InheritanceTest, BadBaseForStruct) {
  EXPECT_COMPILE_FAILURE(
      "public final struct Test : Object {}",
      "struct can only derive from a struct");
}

TEST(InheritanceTest, BadBaseForInterface) {
  EXPECT_COMPILE_FAILURE(
      "public interface Test : Object {}",
      "interface can only inherit");
}

TEST(InheritanceTest, NonSingularBase) {
  EXPECT_COMPILE_FAILURE(
      "public final class Test : Array {}",
      "is a template");
}

TEST(InheritanceTest, PrimitiveBase) {
  EXPECT_COMPILE_FAILURE(
      "public final class Test : int32 {}",
      "Cannot inherit from int32");
}

TEST(InheritanceTest, CircularInheritance) {
  EXPECT_COMPILE_FAILURE(
      "public class Test : Test {}",
      "Circular");
}

TEST(InheritanceTest, MultipleInheritance) {
  EXPECT_COMPILE_FAILURE(
      "public class Test : Exception, Exception {}",
      "single concrete");
}

TEST(InheritanceTest, FinalInterface) {
  EXPECT_COMPILE_FAILURE(
      "public final interface Test {}",
      "Interface type cannot be final");
}

TEST(InheritanceTest, FinalProtocol) {
  EXPECT_COMPILE_FAILURE(
      "public final protocol Test {}",
      "Protocol type cannot be final");
}

TEST(InheritanceTest, FinalInterfaceMethod) {
  EXPECT_COMPILE_FAILURE(
      "public interface Test { final def iterate -> void {} }",
      "cannot be final");
}

TEST(InheritanceTest, PrivateInterfaceMethod) {
  EXPECT_COMPILE_FAILURE(
      "public interface Test { private def iterate -> void; }",
      "cannot be non-public");
}

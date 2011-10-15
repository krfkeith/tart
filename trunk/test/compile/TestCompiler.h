/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/Defn/Module.h"

#include "tart/Type/Type.h"

#include "tart/Parse/Parser.h"

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ScopeBuilder.h"

#include "tart/Common/Diagnostics.h"

#include "FakeSourceFile.h"
#include "TestHelpers.h"

using namespace tart;

class TestCompiler {
public:
  TestCompiler(const char * testName) {
    module_ = new Module(new FakeSourceFile(testName), "test");
  }

  void compile(const char * source) {
    Parser parser(new FakeSourceFile(source), module_);
    writer_.clear();
    Diagnostics::Writer * saveWriter = diag.setWriter(&writer_);
    if (parser.parse()) {
      ScopeBuilder::createScopeMembers(module_);
    }

    if (diag.getErrorCount() == 0) {
      AnalyzerBase::analyzeModule(module_);
    }

    errorCount_ = diag.getErrorCount();
    diag.setWriter(saveWriter);
    diag.reset();
  }

  int errorCount() const {
    return errorCount_;
  }

  llvm::StringRef errorLog() const {
    return writer_.str();
  }

  bool matchError(llvm::StringRef matchStr) {
    return writer_.str().find(matchStr) != llvm::StringRef::npos;
  }

  static testing::AssertionResult expectSuccess(const char * src) {
    TestCompiler tc("TypeModifierTest");
    tc.compile(src);
    if (tc.errorCount() > 0) {
      return testing::AssertionFailure() << "Compilation error: '" << tc.errorLog() << "'.";
    } else {
      return testing::AssertionSuccess();
    }
  }

  static testing::AssertionResult expectError(const char * src, llvm::StringRef errStr) {
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

  static testing::AssertionResult expectWarning(const char * src, llvm::StringRef errStr) {
    TestCompiler tc("TypeModifierTest");
    tc.compile(src);
    if (tc.errorCount() != 0) {
      return testing::AssertionFailure() <<
          "Expected warning [" << errStr << "] for input '" << src << "', but got error '" <<
          tc.errorLog() << "'.";
    } else if (!tc.matchError(errStr)) {
      return testing::AssertionFailure() <<
          "Expected warning [" << errStr << "], actual error was '" << tc.errorLog() << "'.";
    } else {
      return testing::AssertionSuccess();
    }
  }

private:
  Module * module_;
  Diagnostics::StringWriter writer_;
  int errorCount_;
};

#define EXPECT_COMPILE_FAILURE(source, error) \
    EXPECT_TRUE(TestCompiler::expectError(source, error))
#define EXPECT_COMPILE_WARNING(source, error) \
    EXPECT_TRUE(TestCompiler::expectWarning(source, error))
#define EXPECT_COMPILE_SUCCESS(source) \
    EXPECT_TRUE(TestCompiler::expectSuccess(source))

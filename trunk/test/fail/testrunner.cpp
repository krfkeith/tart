/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/GC.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"
#include "tart/Parse/Parser.h"
#include "tart/Objects/Builtins.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ScopeBuilder.h"
#include <llvm/Support/CommandLine.h>
#include <llvm/System/Signals.h>
#include <llvm/System/Path.h>

using namespace tart;

// Global options

static llvm::cl::list<std::string>
ModulePaths("i", llvm::cl::Prefix, llvm::cl::desc("Module search path"));

static llvm::cl::list<std::string>
InputFilenames(llvm::cl::Positional, llvm::cl::desc("<input files>"));

static llvm::cl::opt<std::string>
SourcePath("sourcepath", llvm::cl::desc("Where to find input files"));

Diagnostics::StringWriter errors;

void skipSpace(const std::string & str, size_t & pos) {
  while (pos < str.size() && isspace(str[pos])) {
    ++pos;
  }
}

void trim(std::string & str) {
  size_t pos = str.size();
  while (pos > 1 && isspace(str[pos - 1])) {
    --pos;
  }

  str.resize(pos);
}

bool match(const std::string & str, size_t & pos, const char * text) {
  size_t i = pos;
  while (*text != 0 && i < str.size()) {
    if (str[i++] != *text++) {
      return false;
    }
  }

  pos = i;
  return true;
}

int main(int argc, char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::cl::ParseCommandLineOptions(argc, argv, " tart\n");

  // Requires at least one input file
  if (InputFilenames.empty()) {
    fprintf(stderr, "No input files specified\n");
    return -1;
  }

  GC::init();

  // Add the module search paths.
  for (unsigned i = 0, e = ModulePaths.size(); i != e; ++i) {
    const std::string &modPath = ModulePaths[i];
    PackageMgr::get().addImportPath(modPath);
  }

  // Now get the system classes we will need.
  Builtins::init();
  Builtins::loadSystemClasses();

  // Process the input files.
  int failureCount = 0;
  diag.setWriter(&errors);
  for (unsigned i = 0, e = InputFilenames.size(); i != e; ++i) {
    bool running = true;
    llvm::sys::Path filePath(SourcePath);
    filePath.appendComponent(InputFilenames[i]);
    SourceFile source(filePath.str());
    Module module(&source, filePath.getBasename(), &Builtins::module);
    Parser parser(&source, &module);
    if (!parser.parseImports(module.imports())) {
      break;
    }

    while (!parser.finished() && running) {
      std::string testName(filePath.getBasename());
      std::string testMsg;
      if (!parser.docComment().empty()) {
        const std::string & doc = parser.docComment();
        size_t pos = 0;
        skipSpace(doc, pos);
        if (match(doc, pos, "TEST ")) {
          skipSpace(doc, pos);
          size_t endPos = doc.find(':', pos);
          if (endPos != doc.npos) {
            testName.append(".");
            testName.append(doc, pos, endPos - pos);
            trim(testName);
            pos = endPos + 1;
            skipSpace(doc, pos);
            testMsg.assign(doc, pos, doc.npos);
            trim(testMsg);
          }
        }
      }

      diag.reset();
      errors.str().clear();
      module.clearDefns();

      // Attempt to parse and analyze the declaration.
      if (parser.declaration(module.astMembers(), DeclModifiers())) {
        while (!parser.finished() && parser.docComment().empty() &&
            parser.declaration(module.astMembers(), DeclModifiers())) {}
      } else {
        running = false;
      }

      SourceLocation loc;
      if (!module.astMembers().empty()) {
        loc = module.astMembers().front()->location();
        ScopeBuilder::createScopeMembers(&module);
        if (diag.getErrorCount() == 0) {
          AnalyzerBase::analyzeModule(&module);
        }
      }

      std::string errorMsg;
      errorMsg.swap(errors.str());
      trim(errorMsg);

      if (diag.getMessageCount(Diagnostics::Fatal) != 0) {
        diag.reset();
        diag.setWriter(&Diagnostics::StdErrWriter::instance);
        diag.error(loc) << "ERROR: Test runner encountered a fatal error during test " <<
            testName << ".";
        diag.info(loc) << "Expected error message [" << testMsg << "]";
        diag.info(loc) << "Actual error message [" << errorMsg << "]";
        diag.setWriter(&errors);
        ++failureCount;
      } else if (diag.getErrorCount() != 0) {
        // A parsing error.
        if (errorMsg.find(testMsg) == errorMsg.npos) {
          diag.reset();
          diag.setWriter(&Diagnostics::StdErrWriter::instance);
          diag.error(loc) << "ERROR: Incorrect failure message for " << testName << ".";
          diag.info(loc) << "Expected error message [" << testMsg << "]";
          diag.info(loc) << "Actual error message [" << errorMsg << "]";
          diag.setWriter(&errors);
          ++failureCount;
        }
      } else {
        diag.reset();
        diag.setWriter(&Diagnostics::StdErrWriter::instance);
        diag.error(loc) << "ERROR: " << testName << " should have failed, but it did not.";
        diag.info(loc) << "Expected error message [" << testMsg << "]";
        if (!errorMsg.empty()) {
          diag.info(loc) << "Compiler output [" << errorMsg << "]";
        }
        diag.setWriter(&errors);
        ++failureCount;
      }
    }
  }

  GC::uninit();
  return failureCount != 0;
}

/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Compiler.h"
#include "tart/Common/SourceFile.h"
#include "tart/Common/PackageMgr.h"

#include "tart/CFG/Module.h"

#include "tart/Objects/Builtins.h"

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ScopeBuilder.h"

#include "tart/Gen/CodeGenerator.h"

#include <llvm/Support/CommandLine.h>
#include <llvm/System/Path.h>

namespace tart {

static llvm::cl::opt<std::string>
SourcePath("sourcepath", llvm::cl::desc("Where to find input files"));

Compiler::Compiler()
  : generateBitcode_(true)
  , generateDependencies_(false)
{
}

void Compiler::processInputFile(const std::string & inFile) {
  llvm::sys::Path filePath(SourcePath);
  filePath.appendComponent(inFile);
  std::string moduleName;

  // Add extension if needed.
  if (filePath.getSuffix().empty()) {
    filePath.appendSuffix("tart");
  }

  // And remove extension from module name.
  moduleName.reserve(inFile.size());
  std::string::const_iterator it = inFile.begin(), itEnd = inFile.end();
  if (inFile.rfind(".tart") == inFile.size() - 5) {
    itEnd -= 5;
  }

  // Convert module path to dotted form.
  char lastCh = 0;
  while (it != itEnd) {
    char ch = *it++;
    if (ch == '/' || ch == '\\') {
      if (lastCh == '.') {
        continue;
      }
      moduleName.push_back('.');
      lastCh = '.';
    } else {
      moduleName.push_back(ch);
      lastCh = ch;
    }
  }

  // Check if the file is good.
  if (!filePath.exists()) {
    // Try just the raw input file.
    filePath.set(inFile);

    // Add extension if needed.
    if (filePath.getSuffix().empty()) {
      filePath.appendSuffix("tart");
    }

    if (!filePath.exists()) {
      fprintf(stderr, "Input file '%s' not found\n", inFile.c_str());
      exit(-1);
    }

    // Adjust the module name
    std::string modulePrefix;
    for (std::string::iterator it = SourcePath.begin(); it != SourcePath.end(); ++it) {
      char ch = *it;
      if (ch == '/' || ch == '\\')
        ch = '.';
      modulePrefix.push_back(ch);
    }

    if (modulePrefix[modulePrefix.size() - 1] != '.')
      modulePrefix.push_back('.');

    if (moduleName.size() <= modulePrefix.size() || moduleName.find(modulePrefix) != 0) {
      fprintf(stderr, "Input file '%s' not found on source path\n", inFile.c_str());
      exit(-1);
    }

    moduleName.erase(0, modulePrefix.size());
  }

  if (!filePath.canRead()) {
    fprintf(stderr, "Error reading input file '%s'\n", filePath.c_str());
    exit(-1);
  }

  SourceFile  src(filePath.c_str());
  Module * mod = PackageMgr::get().getCachedModule(moduleName);
  if (mod == NULL) {
    mod = new Module(&src, moduleName, &Builtins::module);
    Parser parser(&src, mod);
    if (parser.parse()) {
      ScopeBuilder::createScopeMembers(mod);
      mod->findPrimaryDefn();
      PackageMgr::get().addModule(mod);
    }
  }

  if (diag.getErrorCount() == 0) {
    AnalyzerBase::analyzeModule(mod);
  }

  if (generateBitcode_ && diag.getErrorCount() == 0) {
    CodeGenerator codeGen(mod);
    codeGen.generate();
  }

  if (generateDependencies_ && diag.getErrorCount() == 0) {
  }

  mod->trace();
  PackageMgr::get().trace();
  GC::sweep();
}

}

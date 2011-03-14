/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Compiler.h"
#include "tart/Common/SourceFile.h"
#include "tart/Common/PackageMgr.h"

#include "tart/Defn/Module.h"

#include "tart/Objects/Builtins.h"

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ScopeBuilder.h"

#include "tart/Gen/CodeGenerator.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/system_error.h"

namespace tart {

using namespace llvm::sys;
using llvm::errs;

static llvm::cl::opt<std::string>
SourcePath("sourcepath", llvm::cl::desc("Root directory of source package"));

Compiler::Compiler()
  : generateBitcode_(true)
  , generateDependencies_(false)
{
}

void Compiler::processInputFile(llvm::StringRef inFile) {
  llvm::SmallString<128> filePath(SourcePath);
  path::append(filePath, inFile);

  // Add extension if needed.
  if (!path::has_extension(filePath.str())) {
    path::replace_extension(filePath, "tart");
  }

  // And remove extension from module name.
  std::string moduleName;
  moduleName.reserve(inFile.size());
  llvm::StringRef::const_iterator it = inFile.begin(), itEnd = inFile.end();
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
  bool exists;
  if (llvm::errc::success != fs::exists(filePath.str(), exists) || exists == false) {
    // Try just the raw input file.
    filePath = inFile;

    // Add extension if needed.
    if (!path::has_extension(filePath.str())) {
      path::replace_extension(filePath, "tart");
    }

    if (llvm::errc::success != fs::exists(filePath.str(), exists) || exists == false) {
      errs() << "Input file '" << inFile << "' not found\n";
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
      errs() << "Input file '" << inFile  << "' not found on source path\n";
      exit(-1);
    }

    moduleName.erase(0, modulePrefix.size());
  }

  bool result;
  if (llvm::errc::success != fs::exists(filePath.str(), result) || result == false) {
    errs() << "Error reading input file '" << filePath << "'\n";
    exit(-1);
  }

  SourceFile  src(filePath.c_str());
  Module * mod = PackageMgr::get().getCachedModule(moduleName);
  if (mod == NULL) {
    mod = new Module(moduleName, &Builtins::module);
    mod->setModuleSource(&src);
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

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Common/GC.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/Compiler.h"
#include "tart/Common/PackageMgr.h"
#include "tart/Objects/Builtins.h"
#include <llvm/Support/CommandLine.h>
#include <llvm/System/Signals.h>

using namespace tart;

// Global options

static llvm::cl::opt<bool>
Stats("showstats", llvm::cl::desc("Print performance metrics and statistics"));

static llvm::cl::list<std::string>
ModulePaths("i", llvm::cl::Prefix, llvm::cl::desc("Module search path"));

static llvm::cl::list<std::string>
InputFilenames(llvm::cl::Positional, llvm::cl::desc("<input files>"));

static llvm::cl::opt<std::string>
Depends("depends", llvm::cl::desc("Name of file to write dependency information to"));

int main(int argc, char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::cl::ParseCommandLineOptions(argc, argv, " tart\n");

  // Requires at least one input file
  if (InputFilenames.empty()) {
    fprintf(stderr, "No input files specified\n");
    return -1;
  }

  GC::init();
  //GC::setDebugLevel(1);
  
  // Add the module search paths.
  for (unsigned i = 0, e = ModulePaths.size(); i != e; ++i) {
    const std::string &modPath = ModulePaths[i];
    PackageMgr::get().addImportPath(modPath);
  }

  // Now get the system classes we will need.
  Builtins::init();
  Builtins::loadSystemClasses();
  
  // Process the input files.
  Compiler compiler;
  if (!Depends.empty()) {
    compiler.setGenerateDependencies(true);
    compiler.setGenerateBitcode(false);
  }

  for (unsigned i = 0, e = InputFilenames.size(); i != e; ++i) {
    const std::string &inFile = InputFilenames[i];
    compiler.processInputFile(inFile);
  }    
  
  if (!Depends.empty()) {
    llvm::sys::Path dependsPath(Depends);
    fprintf(stderr, "Depends path: %s\n", dependsPath.toString().c_str());
    std::ofstream dependsOut(dependsPath.c_str());
    dependsOut.close();
    return 0;
  }

#if 0
  if (Stats) {
      // Printed from high-to-low level.
      //SourceMgr.PrintStats();
      //compiler.GetFileManager().PrintStats();
      //fprintf(stderr, "\n");
  }
#endif

  GC::uninit();
  return diag.getErrorCount() != 0;
}

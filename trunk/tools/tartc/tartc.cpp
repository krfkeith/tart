/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/GC.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/Compiler.h"
#include "tart/Common/PackageMgr.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/TargetSelection.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"

#include "config_paths.h"

using namespace tart;
using namespace llvm;
using namespace llvm::sys;

// Global options

//static cl::opt<bool>
//Stats("showstats", cl::desc("Print performance metrics and statistics"));

static cl::list<std::string>
ModulePaths("i", cl::Prefix, cl::desc("Module search path"));

static cl::list<std::string>
InputFilenames(cl::Positional, cl::desc("<input files>"));

static cl::opt<bool>
NoStdInc("nostdlib", cl::desc("Don't add the standard libraries to the module import path list"));

int main(int argc, char **argv) {
  PrintStackTraceOnErrorSignal();
  cl::ParseCommandLineOptions(argc, argv, " tart\n");
  PrettyStackTraceProgram X(argc, argv);
  //llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.

  InitializeAllTargets();
  InitializeAllTargetMCs();

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

  // Standard library goes last.
  if (!NoStdInc) {
    bool exists = false;
    if (fs::exists(TART_INSTALL_DIR_LIB_STD_BC, exists) == errc::success && exists) {
      PackageMgr::get().addImportPath(TART_INSTALL_DIR_LIB_STD_BC);
    }
  }

  // Now get the system classes we will need.
  TargetSelection::init();
  Builtins::init();
  Builtins::loadSystemClasses();

  // Process the input files.
  Compiler compiler;
  for (unsigned i = 0, e = InputFilenames.size(); i != e; ++i) {
    const std::string &inFile = InputFilenames[i];
    compiler.processInputFile(inFile);
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

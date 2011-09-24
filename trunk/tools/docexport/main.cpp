/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/GC.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/TargetSelection.h"

#include "tart/Common/GC.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Target/TargetSelect.h"

#include "DocExporter.h"

using namespace tart;
using namespace llvm;

// Global options

static llvm::cl::list<std::string>
ModulePaths("i", llvm::cl::Prefix, llvm::cl::desc("Module search path"));

static llvm::cl::list<std::string>
InputFilenames(llvm::cl::Positional, llvm::cl::desc("<input files>"));

static cl::opt<std::string> optOutputFilename("o",
    cl::desc("output filename"),
    cl::value_desc("filename"));

int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal();
  cl::ParseCommandLineOptions(argc, argv, "docexport\n");
  PrettyStackTraceProgram X(argc, argv);

  // Requires at least one input file
  if (InputFilenames.empty()) {
    errs() << "No input files specified\n";
    return -1;
  }

  // Requires an output file
  if (optOutputFilename.empty()) {
    errs() << "No output file specified\n";
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

  // Open the output file.
  std::string errorInfo;
  raw_fd_ostream out(optOutputFilename.c_str(), errorInfo, raw_fd_ostream::F_Binary);
  if (!errorInfo.empty()) {
    errs() << errorInfo << "\n";
    return -1;
  }

  // Process the input files.
  DocExporter exporter(out);
  exporter.begin();
  for (unsigned i = 0, e = InputFilenames.size(); i != e; ++i) {
    outs() << "Extracting: " << InputFilenames[i] << "\n";
    exporter.processInputFile(InputFilenames[i]);
    out.flush();
  }
  exporter.end();
  out.close();

  GC::uninit();
  return diag.getErrorCount() != 0;
}

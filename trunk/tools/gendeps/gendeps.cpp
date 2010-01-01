/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/IRReader.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/System/Signals.h"

#include <algorithm>
#include <sstream>

using namespace llvm;

static cl::list<std::string> optInputFilenames(cl::Positional, cl::OneOrMore,
    cl::desc("<input bitcode files>"));

static cl::opt<std::string> outputFilename("o",
    cl::desc("Output filename"),
    cl::value_desc("filename"));

namespace {
  int xform(int c) {
    if (isalnum(c)) {
      return toupper(c);
    } else {
      return '_';
    }
  }
}

int main(int argc, char **argv, char **envp) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  LLVMContext & context = getGlobalContext();
  llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.

  // Parse the command line options
  cl::ParseCommandLineOptions(argc, argv, "gendeps\n");

  typedef std::vector<sys::Path> Paths;
  Paths filePaths;
  for (unsigned i = 0; i < optInputFilenames.size(); ++i) {
    filePaths.push_back(sys::Path(optInputFilenames[i]));
  }

  // Sort the paths
  std::sort(filePaths.begin(), filePaths.end());

  // Output string stream
  std::stringstream ss;

  SMDiagnostic smErr;
  std::auto_ptr<Module> module;
  Paths depPaths;
  for (Paths::iterator path = filePaths.begin(); path != filePaths.end(); ++path) {
    module.reset(ParseIRFile(path->str(), smErr, context));
    depPaths.clear();
    NamedMDNode * deps = module->getNamedMetadata("tart.module_deps");
    if (deps != NULL) {
      MDNode * node = cast<MDNode>(deps->getOperand(0));
      size_t nodeCt = node->getNumOperands();
      for (size_t i = 0; i < nodeCt; ++i) {
        if (const MDString * ms = dyn_cast<MDString>(node->getOperand(i))) {
          depPaths.push_back(sys::Path(ms->getString()));
        }
      }

      if (!depPaths.empty()) {
        // Sort the dependencies
        std::sort(depPaths.begin(), depPaths.end());

        // Print out the deps.
        path->eraseSuffix();
        std::string depVar(path->str());
        std::transform(depVar.begin(), depVar.end(), depVar.begin(), xform);
        ss << "set(" << depVar << "_DEPS\n";
        for (Paths::iterator it = depPaths.begin(); it != depPaths.end(); ++it) {
          ss << "    \"" << it->str() << "\"\n";
        }

        ss << ")\n\n";
      }
    }
  }

  if (!outputFilename.getValue().empty()) {
    // First, attempt to read the file and see if it changed.
    sys::PathWithStatus outPath(outputFilename.getValue());
    if (outPath.canRead()) {
      const sys::FileStatus * fstat = outPath.getFileStatus();
      if (fstat != NULL) {
        size_t bufSize = fstat->fileSize;
      }
    }

    std::string depErr;
    raw_fd_ostream depOut(outPath.c_str(), depErr);
    if (depErr.empty()) {
      depOut << ss.str();
    } else {
      errs() << depErr;
    }
    depOut.close();
  } else {
    outs() << ss.str();
  }
  return 0;
}

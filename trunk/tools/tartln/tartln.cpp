/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "llvm/LinkAllVMCore.h"
#include "llvm/Linker.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/ModuleProvider.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/CodeGen/FileWriters.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileUtilities.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PluginLoader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/StandardPasses.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/System/DynamicLibrary.h"
#include "llvm/System/Host.h"
#include "llvm/System/Signals.h"
#include "llvm/System/Program.h"
#include "llvm/Target/SubtargetFeature.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegistry.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Config/config.h"

#include "ReflectionSizePass.h"
#include "ReflectorPass.h"

#include <memory>
#include <cstring>

using namespace llvm;

enum OutputType {
  Unset = 0,
  BitcodeFile,
  AssemblyFile,
  ObjectFile,
  ExecutableFile,
  // native dynamic lib
};

enum OptLevel {
  O0, O1, O2, O3,
};

// Input/Output Options

static cl::list<std::string> optInputFilenames(cl::Positional, cl::OneOrMore,
    cl::desc("<input bitcode files>"));

static cl::opt<std::string> optOutputFilename("o",
    cl::desc("Override output filename"),
    cl::value_desc("filename"));

static cl::list<std::string> optModulePaths("i", cl::Prefix,
    cl::desc("Specify a module search path"),
    cl::value_desc("directory"));

static cl::opt<bool> optVerbose("v",
    cl::desc("Verbose output"));

static cl::list<std::string> optLibPaths("L", cl::Prefix,
    cl::desc("Specify a library search path"),
    cl::value_desc("directory"));

static cl::list<std::string> optFrameworkPaths("F", cl::Prefix,
    cl::desc("Specify a framework search path"),
    cl::value_desc("directory"));

static cl::list<std::string> optLibraries("l", cl::Prefix,
    cl::desc("Specify libraries to link to"),
    cl::value_desc("library prefix"));

static cl::list<std::string> optFrameworks("framework",
    cl::desc("Specify frameworks to link to"),
    cl::value_desc("framework"));

static cl::opt<bool> optDumpAsm("dump-asm",
    cl::desc("Print resulting IR"));

static cl::opt<bool> optShowSizes("show-size",
    cl::desc("Print size of debugging and reflection data"));

// Options to control the linking, optimization, and code gen processes

cl::opt<OutputType> optOutputType("filetype", cl::init(Unset),
    cl::desc("Choose a file type (not all types are supported by all targets):"),
    cl::values(
        clEnumValN(BitcodeFile, "bc", "  Emit a bitcode ('.bc') file"),
        clEnumValN(ObjectFile, "obj", "Emit a native object ('.o') file [experimental]"),
        clEnumValN(AssemblyFile, "asm","  Emit an assembly ('.s') file"),
        clEnumValN(ExecutableFile, "exec","  Emit a native executable file"),
        clEnumValEnd));

static cl::opt<bool> optLinkAsLibrary("link-as-library",
    cl::desc("Link the .bc files together as a library, not an executable"));

//Don't verify at the end
static cl::opt<bool> optDontVerify("disable-verify", cl::ReallyHidden);

static cl::opt<OptLevel> optOptimizationLevel(cl::init(O0),
    cl::desc("Choose optimization level:"),
    cl::values(
        clEnumVal(O0, "No optimizations"),
        clEnumVal(O1, "Enable trivial optimizations"),
        clEnumVal(O2, "Enable default optimizations"),
        clEnumVal(O3, "Enable expensive optimizations"),
        clEnumValEnd));

static cl::opt<bool> optDisableInline("disable-inlining",
    cl::desc("Do not run the inliner pass"));

static cl::opt<bool> optInternalize("internalize",
    cl::desc("Mark all symbols as internal except for 'main'"));

static cl::opt<bool> optVerifyEach("verify-each",
    cl::desc("Verify intermediate results of all passes"));

static cl::opt<bool> optStrip("strip-all",
    cl::desc("Strip all symbol info from executable"));

static cl::opt<bool> optStripDebug("strip-debug",
    cl::desc("Strip debugger symbol info from executable"));

// Code generation options

static cl::opt<std::string> optTargetTriple("mtriple",
    cl::desc("Override target triple for module"));

static cl::opt<std::string> optMArch("march",
    cl::desc("Architecture to generate code for (see --version)"));

static cl::opt<std::string> optMCPU("mcpu",
  cl::desc("Target a specific cpu type (-mcpu=help for details)"),
  cl::value_desc("cpu-name"),
  cl::init(""));

static cl::list<std::string> optMAttrs("mattr",
  cl::CommaSeparated,
  cl::desc("Target specific attributes (-mattr=help for details)"),
  cl::value_desc("a1,+a2,-a3,..."));

static cl::opt<bool> optDisableRedZone("disable-red-zone",
  cl::desc("Do not emit code that uses the red zone."),
  cl::init(false));

static cl::opt<bool> optNoImplicitFloats("no-implicit-float",
  cl::desc("Don't generate implicit floating point instructions (x86-only)"),
  cl::init(false));

/// printAndExit - Prints a message to standard error and exits with error code
///
/// Inputs:
///  Message  - The message to print to standard error.
///
static void printAndExit(const std::string &Message, int errcode = 1) {
  errs() << "tartln: " << Message << "\n";
  llvm_shutdown();
  exit(errcode);
}

static void printCommand(const std::vector<const char*> &args) {
  for (std::vector<const char*>::const_iterator it = args.begin(); it != args.end(); ++it) {
    if (*it) outs() << "'" << *it << "'" << " ";
  }

  outs() << "\n";
  outs().flush();
}

// A utility function that adds a pass to the pass manager but will also add
// a verifier pass after if we're supposed to verify.
static inline void addPass(PassManager & pm, Pass * pass) {
  // Add the pass to the pass manager...
  pm.add(pass);

  // If we are verifying all of the intermediate steps, add the verifier...
  if (optVerifyEach) {
    pm.add(createVerifierPass());
  }
}

/// Optimize - Perform link time optimizations. This will run the scalar
/// optimizations, any loaded plugin-optimization modules, and then the
/// inter-procedural optimizations if applicable.
void optimize(Module * module, const TargetData * targetData) {
  // Add an appropriate TargetData instance for this module...
  FunctionPassManager *FPasses = NULL;
  if (optOptimizationLevel >= O1) {
    FunctionPassManager fpm(new ExistingModuleProvider(module));
    if (targetData) {
      fpm.add(new TargetData(*targetData));
      fpm.doInitialization();
      for (Module::iterator it = module->begin(); it != module->end(); ++it) {
        fpm.run(*it);
      }
    }
  }

  // Instantiate the pass manager to organize the passes.
  PassManager passes;

  // If we're verifying, start off with a verification pass.
  if (optVerifyEach) {
    passes.add(createVerifierPass());
  }

  // Add an appropriate TargetData instance for this module...
  addPass(passes, new TargetData(*targetData));
  if (!optLinkAsLibrary) {
    addPass(passes, new tart::ReflectorPass());
    if (optInternalize) {
      std::vector<const char *> externs;
      externs.push_back("main");
      externs.push_back("String_create");
      passes.add(createInternalizePass(externs)); // Internalize all but exported API symbols.
    }
  }

  if (optOptimizationLevel > O0) {
    createStandardModulePasses(
        &passes,
        int(optOptimizationLevel),
        true /* OptimizeSize */,
        true /* UnitAtATime */,
        true /* UnrollLoops */,
        true /* SimplifyLibCalls */,
        true /* HaveExceptions */,
        NULL /* *InliningPass */);
  }

  if (optOptimizationLevel > O0) {
    createStandardLTOPasses(
        &passes,
        false,              // Internalize
        !optDisableInline,  // Run inliner
        optVerifyEach);
  }

  // If the -s or -S command line options were specified, strip the symbols out
  // of the resulting program to make it smaller.  -s and -S are GNU ld options
  // that we are supporting; they alias -strip-all and -strip-debug.
  if (optStrip || optStripDebug) {
    addPass(passes, createStripSymbolsPass(optStripDebug && !optStrip));
  }

  // The user's passes may leave cruft around. Clean up after them them but
  // only if we haven't got DisableOptimizations set
  if (optOptimizationLevel > O0) {
    addPass(passes, createInstructionCombiningPass());
    addPass(passes, createCFGSimplificationPass());
    addPass(passes, createAggressiveDCEPass());
    addPass(passes, createGlobalDCEPass());
  } else if (optInternalize) {
    addPass(passes, createGlobalDCEPass());
  }

  // Make sure everything is still good.
  if (!optDontVerify) {
    passes.add(createVerifierPass());
  }

  tart::ReflectionSizePass * rsPass = NULL;
  if (optShowSizes) {
    rsPass = new tart::ReflectionSizePass();
    passes.add(rsPass);
  }

  // Run our queue of passes all at once now, efficiently.
  passes.run(*module);

  if (rsPass != NULL) {
    rsPass->report();
  }
}

/// copyEnv - This function takes an array of environment variables and makes a
/// copy of it.  This copy can then be manipulated any way the caller likes
/// without affecting the process's real environment.
///
/// Inputs:
///  envp - An array of C strings containing an environment.
///
/// Return value:
///  NULL - An error occurred.
///
///  Otherwise, a pointer to a new array of C strings is returned.  Every string
///  in the array is a duplicate of the one in the original array (i.e. we do
///  not copy the char *'s from one array to another).
///
static char ** copyEnv(char ** const envp) {
  // Count the number of entries in the old list;
  unsigned entries; // The number of entries in the old environment list
  for (entries = 0; envp[entries] != NULL; entries++) {}

  // Add one more entry for the NULL pointer that ends the list.
  ++entries;

  // If there are no entries at all, just return NULL.
  if (entries == 0) {
    return NULL;
  }

  // Allocate a new environment list.
  char **newenv = new char*[entries];
  if ((newenv = new char*[entries]) == NULL) {
    return NULL;
  }

  // Make a copy of the list.  Don't forget the NULL that ends the list.
  entries = 0;
  while (envp[entries] != NULL) {
    newenv[entries] = new char[strlen(envp[entries]) + 1];
    strcpy(newenv[entries], envp[entries]);
    ++entries;
  }

  newenv[entries] = NULL;
  return newenv;
}

/// removeEnv - Remove the specified environment variable from the environment
/// array.
///
/// Inputs:
///  name - The name of the variable to remove.  It cannot be NULL.
///  envp - The array of environment variables.  It cannot be NULL.
///
/// Notes:
///  This is mainly done because functions to remove items from the environment
///  are not available across all platforms.  In particular, Solaris does not
///  seem to have an unsetenv() function or a setenv() function (or they are
///  undocumented if they do exist).
///
static void removeEnv(const char * name, char ** const envp) {
  for (unsigned index = 0; envp[index] != NULL; index++) {
    // Find the first equals sign in the array and make it an EOS character.
    char *p = strchr(envp[index], '=');
    if (p == NULL) {
      continue;
    } else {
      *p = '\0';
    }

    // Compare the two strings.  If they are equal, zap this string.
    // Otherwise, restore it.
    if (!strcmp(name, envp[index])) {
      *envp[index] = '\0';
    } else {
      *p = '=';
    }
  }

  return;
}

std::auto_ptr<TargetMachine> selectTarget(Module & mod) {
  // If we are supposed to override the target triple, do so now.
  if (!optTargetTriple.empty()) {
    mod.setTargetTriple(optTargetTriple);
  }

  Triple theTriple(mod.getTargetTriple());
  if (theTriple.getTriple().empty()) {
    theTriple.setTriple(sys::getHostTriple());
  }

  // Allocate target machine.  First, check whether the user has explicitly
  // specified an architecture to compile for. If so we have to look it up by
  // name, because it might be a backend that has no mapping to a target triple.
  const Target * theTarget = 0;
  if (!optMArch.empty()) {
    for (TargetRegistry::iterator it = TargetRegistry::begin(); it != TargetRegistry::end(); ++it) {
      if (optMArch == it->getName()) {
        theTarget = &*it;
        break;
      }
    }

    if (!theTarget) {
      errs() << "tartln: error: invalid target '" << optMArch << "'.\n";
      llvm_shutdown();
      exit(1);
    }

    // Adjust the triple to match (if known), otherwise stick with the
    // module/host triple.
    Triple::ArchType archType = Triple::getArchTypeForLLVMName(optMArch);
    if (archType != Triple::UnknownArch) {
      theTriple.setArch(archType);
    }
  } else {
    std::string errMsg;
    theTarget = TargetRegistry::lookupTarget(theTriple.getTriple(), errMsg);
    if (theTarget == 0) {
      errs() << "tartln: error auto-selecting target for module '"
             << errMsg << "'.  Please use the -march option to explicitly "
             << "pick a target.\n";
      llvm_shutdown();
      exit(1);
    }
  }

  // Package up features to be passed to target/subtarget
  std::string featuresStr;
  if (optMCPU.size() || optMAttrs.size()) {
    SubtargetFeatures features;
    features.setCPU(optMCPU);
    for (unsigned i = 0; i != optMAttrs.size(); ++i) {
      features.AddFeature(optMAttrs[i]);
    }

    featuresStr = features.getString();
  }

  return std::auto_ptr<TargetMachine>(
      theTarget->createTargetMachine(theTriple.getTriple(), featuresStr));
}

/// GenerateBitcode - generates a bitcode file from the module provided
void generateBitcode(Module * module, const sys::Path & outputFilePath) {

  if (optVerbose) {
    outs() << "Generating bitcode to " << outputFilePath.str() << '\n';
  }

  // Create the output file.
  std::string errorInfo;
  raw_fd_ostream bcOut(outputFilePath.c_str(), errorInfo, raw_fd_ostream::F_Binary);
  if (!errorInfo.empty()) {
    printAndExit(errorInfo);
  }

  // Ensure that the bitcode file gets removed from the disk if we get a
  // terminating signal.
  sys::RemoveFileOnSignal(outputFilePath);

  // Write it out
  WriteBitcodeToFile(module, bcOut);

  // Close the bitcode file.
  bcOut.close();
}

static void generateAssembly(std::auto_ptr<Module> & mod, const sys::Path & assemblyFile,
    TargetMachine & target, TargetMachine::CodeGenFileType codeGenType) {
  std::string errMsg;

  if (optVerbose) {
    outs() << "Generating assembly to " << assemblyFile.str() << '\n';
  }

  // Figure out where we are going to send the output...
  sys::RemoveFileOnSignal(assemblyFile);

  std::auto_ptr<formatted_raw_ostream> asOut;
  formatted_raw_ostream * pOut;

  if (assemblyFile.str() == "-") {
    outs() << "Generating assembly to stdout\n";
    pOut = &fouts();
  } else {
    raw_fd_ostream * fdOut =
        new raw_fd_ostream(assemblyFile.c_str(), errMsg, raw_fd_ostream::F_Binary);
    if (!errMsg.empty()) {
      errs() << errMsg << '\n';
      delete fdOut;
      sys::Path(assemblyFile).eraseFromDisk();
      llvm_shutdown();
      exit(1);
    }

    asOut.reset(new formatted_raw_ostream(*fdOut, formatted_raw_ostream::DELETE_STREAM));
    pOut = asOut.get();
  }

  if (!asOut.get()) {
    sys::Path(assemblyFile).eraseFromDisk();
    llvm_shutdown();
    exit(1);
  }

  CodeGenOpt::Level optLevel = CodeGenOpt::Default;
  switch (optOptimizationLevel) {
    case O0:
      optLevel = CodeGenOpt::None;
      break;

    case O1:
    case O2:
      optLevel = CodeGenOpt::Default;
      break;

    case O3:
      optLevel = CodeGenOpt::Aggressive;
      break;
  }

  // Build up all of the passes that we want to do to the module.
  ExistingModuleProvider moduleProvider(mod.get());
  FunctionPassManager passes(&moduleProvider);

  // Add the target data from the target machine, if it exists, or the module.
  if (const TargetData * targetData = target.getTargetData()) {
    passes.add(new TargetData(*targetData));
  } else {
    passes.add(new TargetData(mod.get()));
  }

  if (!optDontVerify) {
    passes.add(createVerifierPass());
  }

  // Ask the target to add backend passes as necessary.
  ObjectCodeEmitter * emitter = 0;

  // Override default to generate verbose assembly.
  target.setAsmVerbosityDefault(true);

  switch (target.addPassesToEmitFile(passes, *asOut.get(), codeGenType, optLevel)) {
    default:
      assert(0 && "Invalid file model!");
      goto fail;

    case FileModel::Error:
      errs() << "tartln: target does not support generation of this file type!\n";
      goto fail;

    case FileModel::AsmFile:
      break;

    case FileModel::MachOFile:
      emitter = AddMachOWriter(passes, *asOut.get(), target);
      break;

    case FileModel::ElfFile:
      emitter = AddELFWriter(passes, *asOut.get(), target);
      break;
  }

  if (target.addPassesToEmitFileFinish(passes, emitter, optLevel)) {
    errs() << "tartln: target does not support generation of this file type!\n";
    goto fail;
  }

  passes.doInitialization();

  // Run our queue of passes all at once now, efficiently.
  // TODO: this could lazily stream functions out of the module.
  for (Module::iterator it = mod.get()->begin(); it != mod.get()->end(); ++it) {
    if (!it->isDeclaration()) {
      if (optDisableRedZone) {
        it->addFnAttr(Attribute::NoRedZone);
      }

      if (optNoImplicitFloats) {
        it->addFnAttr(Attribute::NoImplicitFloat);
      }

      passes.run(*it);
    }
  }

  passes.doFinalization();
  moduleProvider.releaseModule(&errMsg);
  return;

fail:
  sys::Path(assemblyFile).eraseFromDisk();
  llvm_shutdown();
  exit(1);
}

// BuildLinkItems -- This function generates a LinkItemList for the LinkItems
// linker function by combining the Files and Libraries in the order they were
// declared on the command line.
static void buildLinkItems(Linker::ItemList & items, const cl::list<std::string> & files,
    const cl::list<std::string> & libraries) {

  // Build the list of linkage items for LinkItems.

  cl::list<std::string>::const_iterator fileIt = files.begin();
  cl::list<std::string>::const_iterator libIt = libraries.begin();

  int libPos = -1, filePos = -1;
  while (libIt != libraries.end() || fileIt != files.end()) {
    if (libIt != libraries.end()) {
      libPos = libraries.getPosition(libIt - libraries.begin());
    } else {
      libPos = -1;
    }

    if (fileIt != files.end()) {
      filePos = files.getPosition(fileIt - files.begin());
    } else {
      filePos = -1;
    }

    if (filePos != -1 && (libPos == -1 || filePos < libPos)) {
      // Add a source file
      items.push_back(std::make_pair(*fileIt++, false));
    } else if (libPos != -1 && (filePos == -1 || libPos < filePos)) {
      // Add a library
      items.push_back(std::make_pair(*libIt++, true));
    }
  }
}

int main(int argc, char **argv, char **envp) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  LLVMContext &context = getGlobalContext();
  llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.

  // Initialize targets first, so that --version shows registered targets.
  InitializeAllTargets();
  InitializeAllAsmPrinters();

  try {
    // Parse the command line options
    cl::ParseCommandLineOptions(argc, argv, "tartln\n");

    // Construct a Linker (now that Verbose is set)
    Linker linker("tartln", optOutputFilename, context, optVerbose);

    // Keep track of the native link items (versus the bitcode items)
    Linker::ItemList nativeLinkItems;

    // Add library paths to the linker
    linker.addPaths(optModulePaths);
    linker.addSystemPaths();

    // Remove any consecutive duplicates of the same library...
    optLibraries.erase(std::unique(optLibraries.begin(), optLibraries.end()), optLibraries.end());

    if (optLinkAsLibrary) {
      std::vector<sys::Path> filePaths;
      for (unsigned i = 0; i < optInputFilenames.size(); ++i) {
        filePaths.push_back(sys::Path(optInputFilenames[i]));
      }

      if (linker.LinkInFiles(filePaths)) {
        return 1; // Error already printed
      }

      // The libraries aren't linked in but are noted as "dependent" in the module.
      for (cl::list<std::string>::const_iterator it = optLibraries.begin();
          it != optLibraries.end(); ++it) {
        linker.getModule()->addLibrary(*it);
      }
    } else {
      // Build a list of the items from our command line
      Linker::ItemList items;
      buildLinkItems(items, optInputFilenames, optLibraries);

      // Link all the items together
      if (linker.LinkInItems(items, nativeLinkItems)) {
        return 1; // Error already printed
      }
    }

    std::auto_ptr<Module> composite(linker.releaseModule());
    std::auto_ptr<TargetMachine> targetMachine = selectTarget(*composite.get());

/*    TargetData * TD = 0;
    const std::string &ModuleDataLayout = M.get()->getDataLayout();
    if (!ModuleDataLayout.empty())
      TD = new TargetData(ModuleDataLayout);
    else if (!DefaultDataLayout.empty())
      TD = new TargetData(DefaultDataLayout);

    if (TD)
      Passes.add(TD);*/

    // Optimize the module
    optimize(composite.get(), targetMachine->getTargetData());

    if (optDumpAsm) {
      errs() << "-------------------------------------------------------------\n";
      errs() << composite.get();
      errs() << "-------------------------------------------------------------\n";
    }

    // Determine output file name - and possibly deduce file type
    sys::Path outputFilename;
    bool outputToStdout = (optOutputFilename == "-");
    if (!outputToStdout) {
      outputFilename = optOutputFilename;
      std::string suffix = outputFilename.getSuffix(); // Note: empty if no name set

      if (optOutputType == Unset) {
        if (suffix.empty()) {
          errs() << "tartln: output type not specified.\n";
          return 1;
        } else if (suffix == "bc") {
          optOutputType = BitcodeFile;
        } else if (suffix == "s") {
          optOutputType = AssemblyFile;
        } else {
          errs() << "tartln: unknown output file type suffix '" <<
          suffix << "'.\n";
          return 1;
        }
      }

      // If no output file name was set, use the first input name
      if (outputFilename.empty()) {
        outputFilename = optInputFilenames[0];
      }

      outputFilename.eraseSuffix();
      switch (optOutputType) {
        case AssemblyFile:
          outputFilename.appendSuffix("s");
          break;

        case BitcodeFile:
          outputFilename.appendSuffix("bc");
          break;

        case ExecutableFile:
          #if defined(_WIN32) || defined(__CYGWIN__)
            if (!optLinkAsLibrary) {
              outputFilename.appendSuffix("exe");
            } else {
              outputFilename.appendSuffix("lib");
            }
          #endif
          break;

        default:
          break;
      }
    }

    if (optOutputType == BitcodeFile) {
      generateBitcode(composite.get(), outputFilename);
    } else if (optOutputType == AssemblyFile) {
      generateAssembly(composite, outputFilename, *targetMachine.get(),
          TargetMachine::AssemblyFile);
    } else {
      printAndExit("Unsupported output type");
    }

    // If we are not linking a library, generate either a native executable
    // or a JIT shell script, depending upon what the user wants.
    if (!optLinkAsLibrary) {
#if 0
      // If the user wants to run a post-link optimization, run it now.
      if (!optPostLinkOpts.empty()) {
        std::vector<std::string> opts = optPostLinkOpts;
        for (std::vector<std::string>::iterator it = opts.begin(), E = opts.end(); it != E; ++it) {
          sys::Path prog(*it);
          if (!prog.canExecute()) {
            prog = sys::Program::FindProgramByName(*it);
            if (prog.isEmpty()) {
              printAndExit(std::string("Optimization program '") + *it
                  + "' is not found or not executable.");
            }
          }
          // Get the program arguments
          sys::Path tmpOutput("opt_result");
          std::string errMsg;
          if (tmpOutput.createTemporaryFileOnDisk(true, &errMsg)) printAndExit(errMsg);

          const char* args[4];
          args[0] = it->c_str();
          args[1] = optBitcodeOutputFilename.c_str();
          args[2] = tmpOutput.c_str();
          args[3] = 0;
          if (0 == sys::Program::ExecuteAndWait(prog, args, 0, 0, 0, 0, &errMsg)) {
            if (tmpOutput.isBitcodeFile() || tmpOutput.isBitcodeFile()) {
              sys::Path target(optBitcodeOutputFilename);
              target.eraseFromDisk();
              if (tmpOutput.renamePathOnDisk(target, &errMsg)) printAndExit(errMsg, 2);
            } else
              printAndExit("Post-link optimization output is not bitcode");
          } else {
            printAndExit(errMsg);
          }
        }
      }
#endif
#if 0

      // If the user wants to generate a native executable, compile it from the
      // bitcode file.
      //
      // Otherwise, create a script that will run the bitcode through the JIT.
      // Name of the Assembly Language output file
      sys::Path assemblyFile(optOutputFilename);
      assemblyFile.appendSuffix("s");

      // Mark the output files for removal if we get an interrupt.
      sys::RemoveFileOnSignal(assemblyFile);
      sys::RemoveFileOnSignal(sys::Path(optOutputFilename));

      // Determine the locations of the llc and gcc programs.
      sys::Path llc = FindExecutable("llc", argv[0], (void *) (intptr_t) &optimize);
      if (llc.isEmpty()) printAndExit("Failed to find llc");

      sys::Path gcc = sys::Program::FindProgramByName("gcc");
      if (gcc.isEmpty()) printAndExit("Failed to find gcc");

      // Generate an assembly language file for the bitcode.
      std::string errMsg;
      if (0 != generateAssembly(assemblyFile.str(), optBitcodeOutputFilename, llc, errMsg)) {
        printAndExit(errMsg);
      }

      if (0 != generateNative(optOutputFilename, assemblyFile.str(), nativeLinkItems, gcc, envp,
          errMsg)) {
        printAndExit(errMsg);
      }

      // Remove the assembly language file.
      assemblyFile.eraseFromDisk();



      // Make the script executable...
      if (sys::Path(optOutputFilename).makeExecutableOnDisk(&errMsg)) printAndExit(errMsg);

      // Make the bitcode file readable and directly executable in LLEE as well
      if (sys::Path(optBitcodeOutputFilename).makeExecutableOnDisk(&errMsg)) printAndExit(errMsg);

      if (sys::Path(optBitcodeOutputFilename).makeReadableOnDisk(&errMsg)) printAndExit(errMsg);
#endif
    }
  } catch (const std::string& msg) {
    printAndExit(msg, 2);
  } catch (...) {
    printAndExit("Unexpected unknown exception occurred.", 2);
  }

  // Graceful exit
  return 0;
}

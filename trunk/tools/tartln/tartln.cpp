/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"
#include "llvm/Linker.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/ModuleProvider.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Config/config.h"
#include "llvm/CodeGen/FileWriters.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/CodeGen/LinkAllAsmWriterComponents.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegistry.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Target/SubtargetFeature.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/RegistryParser.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/System/Signals.h"
#include "llvm/System/Host.h"
#include "llvm/LinkAllVMCore.h"

#include <fstream>
#include <iostream>
#include <memory>

using namespace llvm;

// -------------------------------------------------------------------
// Output type

enum OutputType {
  Unset = 0,
  BitcodeFile,
  AssemblyFile,
  ObjectFile,
  // native object
  // native dynamic lib
  // native executable
};

enum OptLevel {
  O0, O1, O2, O3,
};

// -------------------------------------------------------------------
// Output file

class OutputFile {
private:
  sys::Path path;
  raw_ostream * strm;

public:
  OutputFile() : strm(NULL) {}

  ~OutputFile() {
    if (strm != &outs()) {
      delete strm;
    }

    if (!path.empty()) {
      path.eraseFromDisk();
    }
  }

  void open(raw_ostream * s) {
    assert(path.empty());
    assert(strm == NULL);
    strm = s;
  }

  void open(const std::string & in) {
    assert(path.empty());
    assert(strm == NULL);
    open(sys::Path(in));
  }

  void open(const sys::Path & in) {
    assert(path.empty());
    assert(strm == NULL);

    path = in;
    sys::RemoveFileOnSignal(path);
    std::string error;
    strm = new raw_fd_ostream(path.c_str(), true, true, error);
    if (!error.empty()) {
      std::cerr << error << '\n';
      delete strm;
      strm = NULL;
    }
  }

  void close() {
    path.clear();
  }

  bool isValid() const {
    return strm != NULL;
  }

  raw_ostream * getStream() const {
    assert(strm != NULL);
    return strm;
  }
};

// -------------------------------------------------------------------
// Command-line options

static cl::list<std::string>
InputFilenames(cl::Positional, cl::OneOrMore, cl::desc("<input files>"));

static cl::opt<std::string>
OutputFilename("o", cl::desc("Output filename"), cl::value_desc("filename"));

static cl::list<std::string>
ModulePaths("i", cl::Prefix, cl::desc("Module search path"));

static cl::opt<bool>
Verbose("v", cl::desc("Print information about actions taken"));

static cl::opt<bool>
DumpAsm("dump-asm", cl::desc("Print resulting IR"));

static cl::opt<bool>
NoExtern("no-extern", cl::desc("Shrink executable size by removing externally visible symbols"));

cl::opt<OptLevel> OptimizationLevel(cl::desc("Choose optimization level:"),
  cl::values(
    clEnumVal(O0, "No optimizations"),
    clEnumVal(O1, "Enable trivial optimizations"),
    clEnumVal(O2, "Enable default optimizations"),
    clEnumVal(O3, "Enable expensive optimizations"),
   clEnumValEnd));

static cl::opt<std::string>
TargetTriple("mtriple", cl::desc("Override target triple for module"));

static cl::opt<std::string>
MArch("march", cl::desc("Architecture to generate code for (see --version)"));

static cl::opt<std::string>
MCPU("mcpu",
  cl::desc("Target a specific cpu type (-mcpu=help for details)"),
  cl::value_desc("cpu-name"),
  cl::init(""));

static cl::list<std::string>
MAttrs("mattr",
  cl::CommaSeparated,
  cl::desc("Target specific attributes (-mattr=help for details)"),
  cl::value_desc("a1,+a2,-a3,..."));

cl::opt<OutputType>
FileType("filetype", cl::init(Unset),
  cl::desc("Choose a file type (not all types are supported by all targets):"),
  cl::values(
     clEnumValN(BitcodeFile,  "bc", "  Emit a bitcode ('.bc') file"),
     clEnumValN(ObjectFile, "obj", "Emit a native object ('.o') file [experimental]"),
     clEnumValN(AssemblyFile, "asm","  Emit an assembly ('.s') file"),
     clEnumValEnd));

#if 0
cl::opt<TargetMachine::CodeGenFileType>
FileType("filetype", cl::init(TargetMachine::AssemblyFile),
  cl::desc("Choose a file type (not all types are supported by all targets):"),
  cl::values(
       clEnumValN(TargetMachine::AssemblyFile,    "asm",
                  "  Emit an assembly ('.s') file"),
       clEnumValN(TargetMachine::ObjectFile,    "obj",
                  "  Emit a native object ('.o') file [experimental]"),
       clEnumValN(TargetMachine::DynamicLibrary, "dynlib",
                  "  Emit a native dynamic library ('.so') file"
                  " [experimental]"),
       clEnumValEnd));
#endif

// -------------------------------------------------------------------
// loadModule - Read the specified bitcode file in and return it.  This routine
// searches the link path for the specified file to try to find it...
static std::auto_ptr<Module> loadModule(const std::string & inputName) {

  sys::Path fileName;
  if (!fileName.set(inputName)) {
    cerr << "Invalid file name: '" << inputName << "'\n";
    return std::auto_ptr<Module>();
  }

  std::string ErrorMessage;
  if (fileName.exists()) {
    if (Verbose) cerr << "Loading '" << fileName.c_str() << "'\n";
    Module* Result = 0;

    const std::string & FNStr = fileName.toString();
    if (MemoryBuffer *Buffer = MemoryBuffer::getFileOrSTDIN(FNStr,
                                                            &ErrorMessage)) {
      Result = ParseBitcodeFile(Buffer, llvm::getGlobalContext(), &ErrorMessage);
      delete Buffer;
    }
    if (Result) return std::auto_ptr<Module>(Result);   // Load successful!

    if (Verbose) {
      cerr << "Error opening bitcode file: '" << fileName.c_str() << "'";
      if (ErrorMessage.size()) cerr << ": " << ErrorMessage;
      cerr << "\n";
    }
  } else {
    cerr << "Bitcode file: '" << fileName.c_str() << "' does not exist.\n";
  }

  return std::auto_ptr<Module>();
}

static void addOptimizerPasses(PassManager & pm) {

  if (NoExtern) {
    std::vector<const char *> externs;
    externs.push_back("main");
    externs.push_back("String_create");
    //externs.push_back("_String_create");
    pm.add(createInternalizePass(externs));   // Internalize all but exported API symbols.
  }

  if (OptimizationLevel >= O1) {
    pm.add(createRaiseAllocationsPass());     // call %malloc -> malloc inst
    pm.add(createCFGSimplificationPass());    // Clean up disgusting code
    pm.add(createPromoteMemoryToRegisterPass());// Kill useless allocas
    pm.add(createGlobalOptimizerPass());      // Optimize out global vars
    pm.add(createGlobalDCEPass());            // Remove unused fns and globs
  }

  if (OptimizationLevel >= O2) {
    pm.add(createIPConstantPropagationPass());// IP Constant Propagation
    pm.add(createDeadArgEliminationPass());   // Dead argument elimination
    pm.add(createInstructionCombiningPass()); // Clean up after IPCP & DAE
    pm.add(createCFGSimplificationPass());    // Clean up after IPCP & DAE

    pm.add(createPruneEHPass());              // Remove dead EH info

    //if (!DisableInline)
    pm.add(createFunctionInliningPass());   // Inline small functions
    pm.add(createArgumentPromotionPass());    // Scalarize uninlined fn args

    pm.add(createTailDuplicationPass());      // Simplify cfg by copying code
    pm.add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
    pm.add(createCFGSimplificationPass());    // Merge & remove BBs
    pm.add(createScalarReplAggregatesPass()); // Break up aggregate allocas
    pm.add(createInstructionCombiningPass()); // Combine silly seq's
    pm.add(createCondPropagationPass());      // Propagate conditionals

    pm.add(createTailCallEliminationPass());  // Eliminate tail calls
    pm.add(createCFGSimplificationPass());    // Merge & remove BBs
    pm.add(createReassociatePass());          // Reassociate expressions
    pm.add(createLoopRotatePass());
    pm.add(createLICMPass());                 // Hoist loop invariants
    pm.add(createLoopUnswitchPass());         // Unswitch loops.
    pm.add(createLoopIndexSplitPass());       // Index split loops.
    pm.add(createInstructionCombiningPass()); // Clean up after LICM/reassoc
    pm.add(createIndVarSimplifyPass());       // Canonicalize indvars
    pm.add(createLoopUnrollPass());           // Unroll small loops
    pm.add(createInstructionCombiningPass()); // Clean up after the unroller
    pm.add(createGVNPass());                  // Remove redundancies
    pm.add(createSCCPPass());                 // Constant prop with SCCP

    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    pm.add(createInstructionCombiningPass());
    pm.add(createCondPropagationPass());      // Propagate conditionals

    pm.add(createDeadStoreEliminationPass()); // Delete dead stores
    pm.add(createAggressiveDCEPass());        // SSA based 'Aggressive DCE'
    pm.add(createCFGSimplificationPass());    // Merge & remove BBs
    pm.add(createSimplifyLibCallsPass());     // Library Call Optimizations
    pm.add(createDeadTypeEliminationPass());  // Eliminate dead types
    pm.add(createConstantMergePass());        // Merge dup global constants
  }
}

static void optimize(Module & mod, TargetMachine & target) {
  PassManager pm;

  // If the -strip-debug command line option was specified, do it.
  //if (StripDebug)
  //pm.add(createStripSymbolsPass(true));

  pm.add(new TargetData(*target.getTargetData()));
  addOptimizerPasses(pm);
  pm.run(mod);
}

/// -------------------------------------------------------------------
/// main
int main(int argc, char **argv) {
  // Initialize targets first.
  InitializeAllTargets();
  InitializeAllAsmPrinters();

  cl::ParseCommandLineOptions(argc, argv, " tart\n");
  sys::PrintStackTraceOnErrorSignal();

  unsigned baseArg = 0;
  std::auto_ptr<Module> module(loadModule(InputFilenames[baseArg]));
  if (module.get() == 0) {
    cerr << argv[0] << ": error loading file '"
         << InputFilenames[baseArg] << "'\n";
    return 1;
  }

  std::string errorMsg;
  for (unsigned i = baseArg+1; i < InputFilenames.size(); ++i) {
    std::auto_ptr<Module> M(loadModule(InputFilenames[i]));
    if (M.get() == 0) {
      cerr << argv[0] << ": error loading file '" <<InputFilenames[i]<< "'\n";
      return 1;
    }

    if (Verbose) cerr << "Linking in '" << InputFilenames[i] << "'\n";

    if (Linker::LinkModules(module.get(), M.get(), &errorMsg)) {
      cerr << argv[0] << ": link error in '" << InputFilenames[i]
           << "': " << errorMsg << "\n";
      return 1;
    }
  }

  Module & mod = *module.get();
  if (!OutputFilename.empty() && OutputFilename != "-") {
    mod.setModuleIdentifier(OutputFilename);
  }

  // If we are supposed to override the target triple, do so now.
  if (!TargetTriple.empty())
    mod.setTargetTriple(TargetTriple);

  Triple TheTriple(mod.getTargetTriple());
  if (TheTriple.getTriple().empty())
    TheTriple.setTriple(sys::getHostTriple());

  // Allocate target machine.  First, check whether the user has explicitly
  // specified an architecture to compile for. If so we have to look it up by
  // name, because it might be a backend that has no mapping to a target triple.
  const Target *TheTarget = 0;
  if (!MArch.empty()) {
    for (TargetRegistry::iterator it = TargetRegistry::begin(),
           ie = TargetRegistry::end(); it != ie; ++it) {
      if (MArch == it->getName()) {
        TheTarget = &*it;
        break;
      }
    }

    if (!TheTarget) {
      errs() << argv[0] << ": error: invalid target '" << MArch << "'.\n";
      return 1;
    }

    // Adjust the triple to match (if known), otherwise stick with the
    // module/host triple.
    Triple::ArchType Type = Triple::getArchTypeForLLVMName(MArch);
    if (Type != Triple::UnknownArch)
      TheTriple.setArch(Type);
  } else {
    std::string Err;
    TheTarget = TargetRegistry::lookupTarget(TheTriple.getTriple(), Err);
    if (TheTarget == 0) {
      errs() << argv[0] << ": error auto-selecting target for module '"
             << Err << "'.  Please use the -march option to explicitly "
             << "pick a target.\n";
      return 1;
    }
  }

  // Package up features to be passed to target/subtarget
  std::string FeaturesStr;
  if (MCPU.size() || MAttrs.size()) {
    SubtargetFeatures Features;
    Features.setCPU(MCPU);
    for (unsigned i = 0; i != MAttrs.size(); ++i)
      Features.AddFeature(MAttrs[i]);
    FeaturesStr = Features.getString();
  }

  std::auto_ptr<TargetMachine>
    target(TheTarget->createTargetMachine(TheTriple.getTriple(), FeaturesStr));
  assert(target.get() && "Could not allocate target machine!");
  TargetMachine &Target = *target.get();

  optimize(*module.get(), Target);

  if (DumpAsm) {
    cerr << "-------------------------------------------------------------\n";
    cerr << * module.get();
    cerr << "-------------------------------------------------------------\n";
  }

  // Determine output file name - and possibly deduce file type
  sys::Path outputName;
  bool outputToStdout = (OutputFilename == "-");
  if (!outputToStdout) {
    outputName = OutputFilename;
    std::string suffix = outputName.getSuffix();  // Note: empty if no name set

    if (FileType == Unset) {
      if (suffix.empty()) {
        cerr << argv[0] << ": output type not specified.\n";
        return 1;
      } else if (suffix == "bc") {
        FileType = BitcodeFile;
      } else if (suffix == "s") {
        FileType = AssemblyFile;
      } else {
        cerr << argv[0] << ": unknown output file type suffix '" <<
            suffix << "'.\n";
        return 1;
      }
    }

    // If no output file name was set, use the first input name
    if (outputName.empty()) {
      outputName = InputFilenames[baseArg];
    }

    outputName.eraseSuffix();
    switch (int(FileType)) {
      case AssemblyFile:
        outputName.appendSuffix("s");
        break;
      case BitcodeFile:
        outputName.appendSuffix("bc");
        break;
    }
  }

  // If writing bitcode, then use a regular ostream.
  if (FileType == BitcodeFile) {
    if (outputToStdout) {
      std::cerr << argv[0] << ": can't write binary bitcode to stdout\n";
      return 1;
    }

    //std::cout << "Writing '" << outputName << "'.\n";
    std::ostream * outs = new std::ofstream(
        outputName.c_str(), std::ios::out | std::ios::trunc | std::ios::binary);
    WriteBitcodeToFile(module.get(), *outs);
    delete outs;
    return 0;
  }

  // Otherwise, we need a raw_ostream (grrr...)
  OutputFile outFile;
  if (outputToStdout) {
    outFile.open(&outs());
  } else {
    //std::cout << "Writing '" << outputName << "'.\n";
    outFile.open(outputName);
    if (!outFile.isValid()) {
      return 1;
    }
  }

  // Figure out where we are going to send the output...

  raw_ostream * rawStream = outFile.getStream();
  if (!rawStream) {
    return 1;
  }

  formatted_raw_ostream * outStream = new formatted_raw_ostream(*rawStream,
      formatted_raw_ostream::DELETE_STREAM);

  //formatted_raw_ostream * outStream = GetOutputStream(TheTarget->getName(), argv[0]);
  //if (outStream == 0) return 1;

  //raw_ostream * outStream = outFile.getStream();
  TargetMachine::CodeGenFileType outFmt = TargetMachine::AssemblyFile;

  // If this target requires addPassesToEmitWholeFile, do it now.  This is
  // used by strange things like the C backend.
  if (Target.WantsWholeFile()) {
    PassManager passMgr;
    passMgr.add(new TargetData(*Target.getTargetData()));
    passMgr.add(createVerifierPass());

    // Ask the target to add backend passes as necessary.
    if (Target.addPassesToEmitWholeFile(passMgr, *outStream, outFmt, CodeGenOpt::Default)) {
      std::cerr << argv[0] << ": target does not support generation of this"
                << " file type!\n";
      return 1;
    }

    passMgr.run(mod);
  } else {
    // Build up all of the passes that we want to do to the module.
    ExistingModuleProvider Provider(module.release());
    FunctionPassManager Passes(&Provider);
    Passes.add(new TargetData(*Target.getTargetData()));
    Passes.add(createVerifierPass());

    // Ask the target to add backend passes as necessary.
    ObjectCodeEmitter * MCE = 0;

    switch (Target.addPassesToEmitFile(Passes, *outStream, outFmt, CodeGenOpt::Default)) {
    default:
      assert(0 && "Invalid file model!");
      return 1;
    case FileModel::Error:
      std::cerr << argv[0] << ": target does not support generation of this"
                << " file type!\n";
      return 1;
    case FileModel::AsmFile:
      break;
    case FileModel::MachOFile:
      MCE = AddMachOWriter(Passes, *outStream, Target);
      break;
    case FileModel::ElfFile:
      MCE = AddELFWriter(Passes, *outStream, Target);
      break;
    }

    if (Target.addPassesToEmitFileFinish(Passes, MCE, CodeGenOpt::Default)) {
      std::cerr << argv[0] << ": target does not support generation of this"
                << " file type!\n";
      return 1;
    }

    Passes.doInitialization();

    // Run our queue of passes all at once now, efficiently.
    // TODO: this could lazily stream functions out of the module.
    for (Module::iterator it = mod.begin(); it != mod.end(); ++it) {
      if (!it->isDeclaration()) {
        Passes.run(*it);
      }
    }

    Passes.doFinalization();
  }

  outFile.close();
  return 0;
}

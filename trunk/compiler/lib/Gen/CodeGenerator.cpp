/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
#include "tart/Common/InternedString.h"

#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"

#include "tart/Objects/Builtins.h"

#include <llvm/Config/config.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Target/TargetData.h>
#include <llvm/PassManager.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/LinkAllVMCore.h>

namespace tart {
    
using llvm::Function;
using llvm::BasicBlock;
using llvm::Value;

static llvm::cl::opt<std::string>
outputDir("d", llvm::cl::desc("Output directory"),
    llvm::cl::value_desc("dir"), llvm::cl::init(""));

static llvm::cl::opt<bool>
Dump("dump", llvm::cl::desc("Print generated IR to stderr"));

static llvm::cl::opt<bool>
ShowGen("show-generated", llvm::cl::desc("Display generated symbols"));

static llvm::cl::opt<bool>
Debug("g", llvm::cl::desc("Generate source-level debugging information"));

CodeGenerator::CodeGenerator(Module * mod)
    : module(mod)
    , entryPoint_(NULL)
    , irModule_(mod->getIRModule())
#if 0
    , doExpansions(false)
#endif
    , currentFunction_(NULL)
    , dbgFactory_(*mod->getIRModule())
#if 0
    , moduleInitFunc(NULL)
    , moduleInitBlock(NULL)
#endif
    , unwindTarget_(NULL)
    , unwindRaiseException_(NULL)
    , unwindResume_(NULL)
    , exceptionPersonality_(NULL)
    , debug(Debug)
{
  methodPtrType = llvm::PointerType::getUnqual(llvm::OpaqueType::get());
#if 0
  std::vector<const llvm::Type *> args;
  moduleInitFuncType = FunctionType::get(llvm::Type::VoidTy, args, false);
#endif
}

void CodeGenerator::generate() {
  using namespace llvm;

  // Generate debugging information
  if (debug) {
    getCompileUnit(module);
  }

  irModule_->addTypeName("tart.core.Object", Builtins::typeObject->getIRType());
  irModule_->addTypeName("tart.reflect.Type", Builtins::typeType->getIRType());

  // Generate all declarations.
  DefnSet & xdefs = module->getXDefs();
  for (DefnSet::iterator it = xdefs.begin(); it != xdefs.end(); ++it) {
    Defn * de = *it;
    /*if (de->getModule() != module) {
        diag.debug("Generating external reference for %s",
            de->getQualifiedName().c_str());
    }*/

    if (diag.inRecovery()) {
      diag.recovered();
    }
    
    if (!de->isSingular()) {
      continue;
    }

    //if (ShowGen) {
    //  diag.debug() << "Generating " << de->getQualifiedName();
    //}

    genXDef(de);
  }

  DefnSet & xrefs = module->getXRefs();
  for (DefnSet::iterator it = xrefs.begin(); it != xrefs.end(); ++it) {
    Defn * de = *it;
    if (xdefs.count(de)) {
      continue;
    }

    if (diag.inRecovery()) {
      diag.recovered();
    }

    if (!de->isSingular()) {
      continue;
    }
    
    //diag.debug(de) << "XRef: " << de;

    if (const TypeDefn * tdef = dyn_cast<TypeDefn>(de)) {
      if (const CompositeType * ctype = dyn_cast<CompositeType>(tdef->getTypeValue())) {
        if (irModule_->getTypeByName(tdef->getLinkageName()) == NULL) {
          irModule_->addTypeName(tdef->getLinkageName(), ctype->getIRType());
        }
      }
    }
    
    if (de->isSynthetic()) {
      genXDef(de);
    }
  }

#if 0
  // Finish up static constructors.
  if (moduleInitFunc) {
    // See if any actual code was added to the init block.
    if (moduleInitBlock->empty()) {
      moduleInitBlock->eraseFromParent();
      moduleInitFunc->eraseFromParent();
    } else {
      using namespace llvm;
      builder_.SetInsertPoint(moduleInitBlock);
      builder_.CreateRet(NULL);
      builder_.ClearInsertionPoint();

      std::vector<Constant *> ctorMembers;
      ctorMembers.push_back(ConstantInt::get(llvm::Type::Int32Ty, 65536));
      ctorMembers.push_back(moduleInitFunc);

      Constant * ctorStruct = ConstantStruct::get(ctorMembers);
      Constant * ctorArray = ConstantArray::get(
            ArrayType::get(ctorStruct->getType(), 1),
            &ctorStruct, 1);
      Constant * initVar = new GlobalVariable(
        ctorArray->getType(), true,
        GlobalValue::AppendingLinkage,
        ctorArray, "llvm.global_ctors", irModule_);
    }
  }
#endif

  if (diag.getErrorCount() == 0 && entryPoint_ != NULL) {
    genEntryPoint();
  }

#if 0
  if (Dump) {
    fprintf(stderr, "<code style='whitespace:pre-wrap'>");
    if (diag.getErrorCount() == 0) {
      fprintf(stderr, "------------------------------------------------\n");
      irModule_->dump();
      fprintf(stderr, "------------------------------------------------\n");
    }
    fprintf(stderr, "</code>");
  }
#endif

  if (diag.getErrorCount() == 0) {
    // File handle for output bitcode
    llvm::sys::Path binPath(outputDir);
    const std::string & moduleName = module->getLinkageName();
    size_t pos = 0;
    for (;;) {
      size_t dot = moduleName.find('.', pos);
      size_t len = dot == moduleName.npos ? moduleName.npos : dot - pos;
      if (binPath.isEmpty()) {
        binPath.set(moduleName.substr(pos, len));
      } else {
        binPath.appendComponent(moduleName.substr(pos, len));
      }
      
      if (dot == moduleName.npos) {
        break;
      }

      pos = dot + 1;
    }
    
    binPath.appendSuffix("bc");
    
    llvm::sys::Path binDir(binPath);
    binDir.eraseComponent();
    if (!binDir.isEmpty()) {
      std::string err;
      if (binDir.createDirectoryOnDisk(true, &err)) {
        diag.fatal() << "Cannot create output directory '" << binDir.c_str() << "': " << err;
        return;
      }
    }

    std::ofstream binOut(binPath.c_str());
    if (!binOut.good()) {
      diag.fatal() << "Cannot write output file '" << binPath.c_str() << "'";
      return;
    }
    
    llvm::PassManager passManager;
    passManager.add(new llvm::TargetData(irModule_));
    passManager.add(llvm::createVerifierPass()); // Verify that input is correct
    passManager.add(llvm::CreateBitcodeWriterPass(binOut));
    passManager.run(*irModule_);
    binOut.close();

#if 0
    // File handle for output metadata
    llvm::sys::Path metaPath(outputDir);
    metaPath.appendComponent(srcPath.getBasename());
    metaPath.appendSuffix("md");

    // Generate the module metadata
    std::ofstream metaOut(metaPath.toString().c_str());
    genModuleMetadata(metaOut);
    metaOut.close();
#endif
  }
}

llvm::DICompileUnit CodeGenerator::getCompileUnit(const ProgramSource * source) {
  using namespace llvm;
  DICompileUnit & compileUnit = dbgCompileUnits_[source];
  if (compileUnit.isNull()) {
    if (source != NULL) {
      llvm::sys::Path srcPath(source->getFilePath());
      compileUnit = dbgFactory_.CreateCompileUnit(
        0xABBA, // Take a chance on me...
        srcPath.getLast(),
        srcPath.getDirname() + "/",
        "0.1 tartc");
    }
  }

  return compileUnit;
}

llvm::DICompileUnit CodeGenerator::getCompileUnit(Defn * defn) {
  return getCompileUnit(defn->module()->moduleSource());
}

unsigned CodeGenerator::getSourceLineNumber(const SourceLocation & loc) {
  TokenPosition pos = getTokenPosition(loc);
  return pos.beginLine;
}

void CodeGenerator::genEntryPoint() {
  using namespace llvm;

  // Generate the main method
  std::vector<const llvm::Type *> mainArgs;
  mainArgs.push_back(llvm::Type::Int32Ty);
  mainArgs.push_back(
    PointerType::get(
      PointerType::get(
        llvm::Type::Int8Ty, 0), 0));

  // Create the function type
  llvm::FunctionType * functype = llvm::FunctionType::get(llvm::Type::Int32Ty, mainArgs, false);
  Function * mainFunc = Function::Create(functype, Function::ExternalLinkage, "main", irModule_);

  // Create the entry block
  builder_.SetInsertPoint(BasicBlock::Create("entry", mainFunc));

  // Create the exception handler block
  BasicBlock * blkSuccess = BasicBlock::Create("success", mainFunc);
  BasicBlock * blkFailure = BasicBlock::Create("failure", mainFunc);

  // Check the type signature of the entry point function
  llvm::Function * entryFunc = entryPoint_->irFunction();
  const llvm::FunctionType * entryType = entryFunc->getFunctionType();
  if (entryType->getNumParams() > 1) {
    diag.fatal(entryPoint_) << "EntryPoint function must have either 0 or 1 parameters";
    return;
  }

  std::vector<Value*> argv;
  if (entryType->getNumParams() != 0) {
    const llvm::Type * argType = entryType->getParamType(0);
    argv.push_back(llvm::Constant::getNullValue(argType));
    //const llvm::Type * arrayOfStrings =
    //    llvm::ArrayType::get(StringType::get().getIRType(), 0)
  }

  // Create the call to the entry point function
  Value * returnVal = builder_.CreateInvoke(
        entryFunc, blkSuccess, blkFailure, argv.begin(), argv.end());
  if (entryType->getReturnType() == llvm::Type::VoidTy) {
    // void entry point, return 0
    returnVal = llvm::ConstantInt::get(llvm::Type::Int32Ty, 0);
  } else if (entryType->getReturnType() != llvm::Type::Int32Ty) {
    diag.fatal(entryPoint_) << "EntryPoint function must have either void or int32 return type";
    return;
  }

  builder_.SetInsertPoint(blkSuccess);
  builder_.CreateRet(returnVal);

  builder_.SetInsertPoint(blkFailure);
  builder_.CreateRet(llvm::ConstantInt::get(llvm::Type::Int32Ty, (uint64_t)-1, true));

  llvm::verifyFunction(*mainFunc);
}

llvm::Function * CodeGenerator::getUnwindRaiseException() {
  using namespace llvm;

  if (unwindRaiseException_ == NULL) {
    const llvm::Type * unwindExceptionType = Builtins::typeUnwindException->getIRType();
    std::vector<const llvm::Type *> parameterTypes;
    parameterTypes.push_back(PointerType::getUnqual(unwindExceptionType));
    const llvm::FunctionType * ftype =
        llvm::FunctionType::get(llvm::Type::Int32Ty, parameterTypes, false);
    unwindRaiseException_ = cast<Function>(
        irModule_->getOrInsertFunction("_Unwind_RaiseException", ftype));
    unwindRaiseException_->addFnAttr(Attribute::NoReturn);
  }
  
  return unwindRaiseException_;
}

llvm::Function * CodeGenerator::getUnwindResume() {
  using namespace llvm;

  if (unwindResume_ == NULL) {
    const llvm::Type * unwindExceptionType = Builtins::typeUnwindException->getIRType();
    std::vector<const llvm::Type *> parameterTypes;
    parameterTypes.push_back(PointerType::getUnqual(unwindExceptionType));
    const llvm::FunctionType * ftype =
        llvm::FunctionType::get(llvm::Type::Int32Ty, parameterTypes, false);
    unwindResume_ = cast<Function>(
        irModule_->getOrInsertFunction("_Unwind_Resume", ftype));
    unwindResume_->addFnAttr(Attribute::NoReturn);
  }
  
  return unwindResume_;
}

llvm::Function * CodeGenerator::getExceptionPersonality() {
  using namespace llvm;
  using llvm::Type;
  using llvm::FunctionType;

  if (exceptionPersonality_ == NULL) {
    std::vector<const Type *> parameterTypes;
    parameterTypes.push_back(Type::Int32Ty);
    parameterTypes.push_back(Type::Int32Ty);
    parameterTypes.push_back(Type::Int64Ty);
    parameterTypes.push_back(PointerType::get(Type::Int8Ty, 0));
    parameterTypes.push_back(PointerType::get(Type::Int8Ty, 0));
    const FunctionType * ftype = FunctionType::get(llvm::Type::Int32Ty, parameterTypes, false);

    exceptionPersonality_ = cast<Function>(
        irModule_->getOrInsertFunction("__tart_eh_personality", ftype));
    exceptionPersonality_->addFnAttr(Attribute::NoUnwind);
  }
  
  return exceptionPersonality_;
}

Function * CodeGenerator::findMethod(const CompositeType * type, const char * methodName) {
  DASSERT(type->isSingular());
  DASSERT(type->typeDefn()->getAST() != NULL);
  DefnList defs;

  if (!type->lookupMember(methodName, defs, false)) {
    DFAIL("Couldn't find system definition");
  }

  if (defs.size() > 1) {
    DFAIL("Couldn't find system definition");
  }

  FunctionDefn * fn = cast<FunctionDefn>(defs.front());
  return genFunctionValue(fn);
}

}

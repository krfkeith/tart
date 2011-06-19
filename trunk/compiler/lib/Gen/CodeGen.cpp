/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
#include "tart/Common/InternedString.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/UnionType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Objects/TargetSelection.h"

#include "llvm/Config/config.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Intrinsics.h"

#include <memory>

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

llvm::cl::opt<bool>
NoGC("nogc", llvm::cl::desc("Don't generate garbage-collection intrinsics"));

extern SystemNamespaceMember<FunctionDefn> gc_alloc;

CodeGenerator::CodeGenerator(Module * mod)
  : context_(llvm::getGlobalContext())
  , builder_(llvm::getGlobalContext())
  , module_(mod)
  , irModule_(mod->irModule())
  , currentFn_(NULL)
  , invokeFnType_(NULL)
  , structRet_(NULL)
  , moduleInitFunc_(NULL)
  , moduleInitBlock_(NULL)
  , reflector_(*this)
  , gcEnabled_(!NoGC)
  , diBuilder_(*mod->irModule())
  , blockExits_(NULL)
  , isUnwindBlock_(false)
  , unwindRaiseException_(NULL)
  , unwindResume_(NULL)
  , exceptionPersonality_(NULL)
  , exceptionTracePersonality_(NULL)
  , globalAlloc_(NULL)
  , gcAlloc_(NULL)
  , gcAllocContext_(NULL)
  , debug_(Debug)
{
  // Turn on reflection if (a) it's enabled on the command-line, and (b) there were
  // any reflectable definitions within the module.
  reflector_.setEnabled(mod->isReflectionEnabled());
  methodPtrType_ = llvm::OpaqueType::get(context_)->getPointerTo();

  voidValue_ = llvm::UndefValue::get(builder_.getVoidTy());
}

void CodeGenerator::generate() {
  using namespace llvm;

  // Add target selection info
  TargetSelection::instance.addToModule(irModule_);
  targetData_ = TargetSelection::instance.targetData();
  intPtrType_ = targetData_->getIntPtrType(context_);

  // Generate debugging information
  if (debug_) {
    genDICompileUnit();
    dbgFile_ = genDIFile(module_->moduleSource());
    DASSERT(dbgFile_.Verify());
  }

  addTypeName(Builtins::typeObject);
  addTypeName(Builtins::typeTypeInfoBlock);
  if (reflector_.enabled() && Builtins::typeModule.peek() != NULL) {
    addTypeName(Builtins::typeModule);
    addTypeName(Builtins::typeNameTable);
    addTypeName(Builtins::typeCompositeType);
    addTypeName(Builtins::typeDerivedType);
    addTypeName(Builtins::typeEnumType);
  }

  // Write out a list of all modules this one depends on.
  addModuleDependencies();

  // Generate all declarations.
  DefnSet & xdefs = module_->exportDefs();
  for (DefnSet::iterator it = xdefs.begin(); it != xdefs.end(); ++it) {
    Defn * de = *it;
    if (diag.inRecovery()) {
      diag.recovered();
    }

    DASSERT_OBJ(de->isSingular(), de);
    genXDef(de);
  }

  if (reflector_.enabled() &&
      Builtins::typeModule->passes().isFinished(CompositeType::FieldPass)) {
    reflector_.emitModule(module_);
  }

  // Emit tables for static GC roots.
  emitStaticRoots();

  // Finish up static constructors.
  if (moduleInitFunc_) {
    // See if any actual code was added to the init block.
    if (moduleInitBlock_->empty()) {
      moduleInitBlock_->eraseFromParent();
      moduleInitFunc_->eraseFromParent();
    } else {
      using namespace llvm;
      builder_.SetInsertPoint(moduleInitBlock_);
      builder_.CreateRet(NULL);
      builder_.ClearInsertionPoint();

      std::vector<Constant *> ctorMembers;
      ctorMembers.push_back(getInt32Val(65536));
      ctorMembers.push_back(moduleInitFunc_);

      Constant * ctorStruct = ConstantStruct::get(context_, ctorMembers, false);
      Constant * ctorArray = ConstantArray::get(
            ArrayType::get(ctorStruct->getType(), 1),
            &ctorStruct, 1);
      Constant * initVar = new GlobalVariable(
          *irModule_, ctorArray->getType(), true,
          GlobalValue::AppendingLinkage,
          ctorArray, "llvm.global_ctors");
      (void)initVar;
    }
  }

  if (diag.getErrorCount() == 0 && module_->entryPoint() != NULL) {
    genEntryPoint();
  }

  genModuleMetadata();

  if (Dump) {
    if (diag.getErrorCount() == 0) {
      fprintf(stderr, "------------------------------------------------\n");
      irModule_->dump();
      fprintf(stderr, "------------------------------------------------\n");
    }
  }

  if (diag.getErrorCount() == 0) {
    verifyModule();
    outputModule();
  }
}

llvm::ConstantInt * CodeGenerator::getIntVal(int value) {
  using namespace llvm;
  return ConstantInt::get(intPtrType_, value, true);
}

llvm::ConstantInt * CodeGenerator::getInt16Val(int value) {
  using namespace llvm;
  return ConstantInt::get(static_cast<const IntegerType *>(builder_.getInt16Ty()), value, true);
}

llvm::ConstantInt * CodeGenerator::getInt32Val(int value) {
  using namespace llvm;
  return ConstantInt::get(static_cast<const IntegerType *>(builder_.getInt32Ty()), value, true);
}

llvm::ConstantInt * CodeGenerator::getInt64Val(int64_t value) {
  using namespace llvm;
  return ConstantInt::get(static_cast<const IntegerType *>(builder_.getInt64Ty()), value, true);
}

void CodeGenerator::verifyModule() {
  llvm::PassManager passManager;
  //passManager.add(new llvm::TargetData(irModule_));
  passManager.add(llvm::createVerifierPass()); // Verify that input is correct
  passManager.run(*irModule_);
}

void CodeGenerator::outputModule() {
  // File handle for output bitcode
  llvm::sys::Path binPath(outputDir);
  llvm::StringRef moduleName = module_->linkageName();
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
  if (binDir.eraseComponent()) {
    if (!binDir.isEmpty()) {
      std::string err;
      if (binDir.createDirectoryOnDisk(true, &err)) {
        diag.fatal() << "Cannot create output directory '" << binDir.c_str() << "': " << err;
        return;
      }
    }
  }

  std::string errorInfo;
  std::auto_ptr<llvm::raw_ostream> binOut(
      new llvm::raw_fd_ostream(binPath.c_str(), errorInfo, llvm::raw_fd_ostream::F_Binary));
  if (!errorInfo.empty()) {
    diag.fatal() << errorInfo << '\n';
    return;
  }

  llvm::WriteBitcodeToFile(irModule_, *binOut);
}

void CodeGenerator::genModuleInitFunc() {
  using namespace llvm;

  if (moduleInitFunc_ == NULL) {
    llvm::FunctionType * ftype = llvm::FunctionType::get(llvm::Type::getVoidTy(context_), false);
    moduleInitFunc_ = Function::Create(ftype, Function::InternalLinkage, ".module.init", irModule_);
    moduleInitBlock_ = BasicBlock::Create(context_, "entry", moduleInitFunc_);
  }
}

void CodeGenerator::genEntryPoint() {
  using namespace llvm;

  FunctionDefn * entryPoint = module_->entryPoint();

  // Generate the main method
  std::vector<const llvm::Type *> mainArgs;
  mainArgs.push_back(builder_.getInt32Ty());
  mainArgs.push_back(builder_.getInt8Ty()->getPointerTo()->getPointerTo());

  // Create the function type
  llvm::FunctionType * functype = llvm::FunctionType::get(builder_.getInt32Ty(), mainArgs, false);
  DASSERT((MDNode *)dbgContext_ == NULL);
  Function * mainFunc = Function::Create(functype, Function::ExternalLinkage, "main", irModule_);

  Function::arg_iterator args = mainFunc->arg_begin();
  Value * argc = args++;
  Value * argv = args++;

  // Create the entry block
  builder_.SetInsertPoint(BasicBlock::Create(context_, "main_entry", mainFunc));

  // Check the type signature of the entry point function
  llvm::Function * entryFunc = genFunctionValue(entryPoint);
  const llvm::FunctionType * entryType = entryFunc->getFunctionType();
  if (entryType->getNumParams() != 1) {
    diag.error(entryPoint) << "EntryPoint function must have 1 parameter";
    return;
  }

  Function * programStart = genFunctionValue(module_->programStartup());
  Value * returnVal = builder_.CreateCall3(programStart, argc, argv, entryFunc, "returnVal");

  // Create the call to the entry point function
  if (entryType->getReturnType() == builder_.getVoidTy()) {
    // void entry point, return 0
    returnVal = getInt32Val(0);
  } else if (entryType->getReturnType() != builder_.getInt32Ty()) {
    diag.fatal(entryPoint) << "EntryPoint function must have either void or int32 return type";
    return;
  }

  builder_.CreateRet(returnVal);
  llvm::verifyFunction(*mainFunc);
}

llvm::Function * CodeGenerator::getUnwindRaiseException() {
  using namespace llvm;

  if (unwindRaiseException_ == NULL) {
    const llvm::Type * unwindExceptionType = Builtins::typeUnwindException->irType();
    std::vector<const llvm::Type *> parameterTypes;
    parameterTypes.push_back(unwindExceptionType->getPointerTo());
    const llvm::FunctionType * ftype =
        llvm::FunctionType::get(builder_.getInt32Ty(), parameterTypes, false);
    unwindRaiseException_ = cast<Function>(
        irModule_->getOrInsertFunction("_Unwind_RaiseException", ftype));
    unwindRaiseException_->addFnAttr(Attribute::NoReturn);
  }

  return unwindRaiseException_;
}

llvm::Function * CodeGenerator::getUnwindResume() {
  using namespace llvm;

  if (unwindResume_ == NULL) {
    const llvm::Type * unwindExceptionType = Builtins::typeUnwindException->irType();
    std::vector<const llvm::Type *> parameterTypes;
    parameterTypes.push_back(unwindExceptionType->getPointerTo());
    const llvm::FunctionType * ftype =
        llvm::FunctionType::get(builder_.getInt32Ty(), parameterTypes, false);
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
    parameterTypes.push_back(builder_.getInt32Ty());
    parameterTypes.push_back(builder_.getInt32Ty());
    parameterTypes.push_back(builder_.getInt64Ty());
    parameterTypes.push_back(builder_.getInt8Ty()->getPointerTo());
    parameterTypes.push_back(builder_.getInt8Ty()->getPointerTo());
    const FunctionType * ftype = FunctionType::get(builder_.getInt32Ty(), parameterTypes, false);

    exceptionPersonality_ = cast<Function>(
        irModule_->getOrInsertFunction("__tart_eh_personality", ftype));
    exceptionPersonality_->addFnAttr(Attribute::NoUnwind);
  }

  return exceptionPersonality_;
}

llvm::Function * CodeGenerator::getExceptionTracePersonality() {
  using namespace llvm;
  using llvm::Type;
  using llvm::FunctionType;

  if (exceptionTracePersonality_ == NULL) {
    std::vector<const Type *> parameterTypes;
    parameterTypes.push_back(builder_.getInt32Ty());
    parameterTypes.push_back(builder_.getInt32Ty());
    parameterTypes.push_back(builder_.getInt64Ty());
    parameterTypes.push_back(builder_.getInt8Ty()->getPointerTo());
    parameterTypes.push_back(builder_.getInt8Ty()->getPointerTo());
    const FunctionType * ftype = FunctionType::get(builder_.getInt32Ty(), parameterTypes, false);

    exceptionTracePersonality_ = cast<Function>(
        irModule_->getOrInsertFunction("__tart_eh_trace_personality", ftype));
    exceptionTracePersonality_->addFnAttr(Attribute::NoUnwind);
  }

  return exceptionTracePersonality_;
}

llvm::Function * CodeGenerator::getGlobalAlloc() {
  using namespace llvm;
  using llvm::Type;
  using llvm::FunctionType;

  if (globalAlloc_ == NULL) {
    std::vector<const Type *> parameterTypes;
    parameterTypes.push_back(builder_.getInt64Ty());
    const FunctionType * ftype = FunctionType::get(
        builder_.getInt8Ty()->getPointerTo(),
        parameterTypes,
        false);

    globalAlloc_ = cast<Function>(irModule_->getOrInsertFunction("malloc", ftype));
    globalAlloc_->addFnAttr(Attribute::NoUnwind);
  }

  return globalAlloc_;
}

llvm::Function * CodeGenerator::getGcAlloc() {
  using namespace llvm;

  DASSERT(gcAllocContext_ != NULL);
  if (gcAlloc_ == NULL) {
    gcAlloc_ = genFunctionValue(gc_alloc);
  }

  return gcAlloc_;
}

Function * CodeGenerator::findMethod(const CompositeType * type, const char * methodName) {
  DASSERT(type->isSingular());
  DASSERT(type->typeDefn()->ast() != NULL);
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

llvm::GlobalVariable * CodeGenerator::createModuleObjectPtr() {
  return reflector_.getModulePtr(module_);
}

llvm::GlobalVariable * CodeGenerator::createModuleObjectPtr(Module * module) {
  return reflector_.getModulePtr(module);
}

llvm::GlobalVariable * CodeGenerator::createPackageObjectPtr() {
  return reflector_.getPackagePtr(module_);
}

llvm::GlobalVariable * CodeGenerator::createPackageObjectPtr(Module * module) {
  return reflector_.getPackagePtr(module);
}

void CodeGenerator::addModuleDependencies() {
  using namespace llvm;
  const ModuleSet & modules = module_->importModules();
  if (!modules.empty()) {
    llvm::StringMap<char> paths;

    for (ModuleSet::const_iterator it = modules.begin(); it != modules.end(); ++it) {
      Module * m = *it;
      ProgramSource * source = m->moduleSource();
      if (source->container() != NULL) {
        source = source->container();
      }

      if (!source->filePath().empty()) {
        paths[source->filePath()] = 1;
      }
    }

    ValueList deps;
    for (llvm::StringMap<char>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
      deps.push_back(MDString::get(context_, it->first()));
    }

    irModule_->getOrInsertNamedMetadata("tart.module_deps")->addOperand(
        MDNode::get(context_, deps));
  }
}

void CodeGenerator::addTypeName(const CompositeType * type) {
  if (type != NULL && type->typeDefn() != NULL &&
      type->passes().isFinished(CompositeType::BaseTypesPass) &&
      type->passes().isFinished(CompositeType::FieldPass)) {
    irModule_->addTypeName(type->typeDefn()->qualifiedName(), type->irType());
  }
}

bool CodeGenerator::hasAddress(const Expr * expr) {
  switch (expr->exprType()) {
    case Expr::LValue: {
      const LValueExpr * lval = static_cast<const LValueExpr *>(expr);
      if (lval->base() != NULL) {
        if (lval->base()->type()->isReferenceType()) {
          return true;
        }

        // It has an address if it's base does.
        return hasAddress(lval->base());
      }

      if (lval->value()->defnType() == Defn::Var) {
        return true;
      }

      if (lval->value()->defnType() == Defn::Parameter) {
        const ParameterDefn * param = static_cast<const ParameterDefn *>(lval->value());
        if (param->isLValue() || param->isReference()) {
          return true;
        }
      }

      TypeShape shape = lval->type()->typeShape();
      return shape == Shape_Reference || shape == Shape_Large_Value;
    }

    case Expr::ElementRef: {
      const BinaryExpr * op = static_cast<const BinaryExpr *>(expr);
      return hasAddress(op->first());
      break;
    }

    default:
      return false;
  }
}

void CodeGenerator::ensureLValue(const Expr * expr, const llvm::Type * type) {
#if !NDEBUG
  if (!isa<llvm::PointerType>(type)) {
    if (expr != NULL) {
      diag.error(expr) << "Not an lvalue: " << expr;
    }
    type->dump(irModule_);
    DFAIL("Expecting an lvalue");
  }
#endif
}

llvm::Value * CodeGenerator::loadValue(llvm::Value * value, const Expr * expr,
    llvm::StringRef name ) {
#if !NDEBUG
  const llvm::Type * type = value->getType();
  if (!isa<llvm::PointerType>(type)) {
    if (expr != NULL) {
      diag.error(expr) << Format_Type << "Not an lvalue: " << expr;
    }

    type->dump(irModule_);
    DFAIL("Expecting an lvalue");
  }
#endif
  return builder_.CreateLoad(value, name);
}

#if 0
void CodeGenerator::compareTypes(llvm::Type * expected, llvm::Type * actual) {

}
#endif

}

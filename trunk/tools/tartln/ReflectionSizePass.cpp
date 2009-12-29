/** LLVM pass to measure the size of reflection data. */

#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"

#include "ReflectionSizePass.h"

namespace tart {
using namespace llvm;

char ReflectionSizePass::ID = 0;

namespace {

RegisterPass<ReflectionSizePass> X(
    "reflection-size", "Reflection Size Pass",
    false /* Only looks at CFG */,
    false /* Analysis Pass */);

const size_t PTR_SIZE = sizeof(void *);

size_t ptrAlign(size_t size) {
  return size + ((PTR_SIZE - 1) & ~PTR_SIZE);
}

}

bool ReflectionSizePass::runOnModule(llvm::Module & module) {
  using llvm::Module;
  Module::GlobalListType & globals = module.getGlobalList();
  for (Module::GlobalListType::const_iterator it = globals.begin(); it != globals.end(); ++it) {
    measureGlobal(it);
  }

  return false;
}

void ReflectionSizePass::measureGlobal(const GlobalValue * val) {
  if (val->getName().startswith(".type") ||
      val->getName().startswith(".method") ||
      val->getName().startswith(".tuple") ||
      val->getName().startswith(".data")) {
    size_t size = getSizeofGlobalValue(val);
    errs() << "Size of '" << val->getName() << "' is " << size << "\n";
    globalSize_ += size;
  }
}

size_t ReflectionSizePass::getSizeofGlobalValue(const GlobalValue * val) {
  if (globals_.count(val)) {
    return 0;
  }

  globals_.insert(val);

  const GlobalVariable * var = cast<GlobalVariable>(val);
  if (var->getInitializer() != NULL) {
    return getSizeofConstant(var->getInitializer());
  }

  return 0;
}

size_t ReflectionSizePass::getSizeofConstant(const Constant * c) {
  size_t size = 0;
  //if (ConstantInt * csize = dyn_cast<ConstantInt>(ConstantExpr::getSizeOf(c->getType()))) {
  //  size += csize->getValue().getZExtValue();
  //}

  if (const ConstantStruct * cs = dyn_cast<ConstantStruct>(c)) {
    //errs() << "CStruct: " << val->getName() << "\n";
    //size += cs->getType()->getScalarSizeInBits();
    for (ConstantStruct::const_op_iterator it = cs->op_begin(); it != cs->op_end(); ++it) {
      if (const Constant * op = dyn_cast<Constant>(it)) {
        size += getSizeofConstant(op);
      } else {
        errs() << "Other op: " << it << "\n";
      }
    }
  } else if (const ConstantArray * ca = dyn_cast<ConstantArray>(c)) {
    for (ConstantStruct::const_op_iterator it = ca->op_begin(); it != ca->op_end(); ++it) {
      if (const Constant * op = dyn_cast<Constant>(it)) {
        size += getSizeofConstant(op);
      } else {
        errs() << "Other op: " << it << "\n";
      }
    }
    //errs() << "CArray: " << val->getName() << "\n";
  } else if (const ConstantInt * ci = dyn_cast<ConstantInt>(c)) {
    return ci->getType()->getPrimitiveSizeInBits() / 8;
  } else if (const ConstantFP * cf = dyn_cast<ConstantFP>(c)) {
    return cf->getType()->getPrimitiveSizeInBits() / 8;
  } else if (const ConstantExpr * ce = dyn_cast<ConstantExpr>(c)) {
    return ce->getType()->getPrimitiveSizeInBits() / 8;
  } else if (const ConstantPointerNull * cpn = dyn_cast<ConstantPointerNull>(c)) {
    return sizeof(void *);
  } else if (const ConstantExpr * ce = dyn_cast<ConstantExpr>(c)) {
    return sizeof(void *);
  } else if (const ConstantAggregateZero * caz = dyn_cast<ConstantAggregateZero>(c)) {
    getSizeofType(caz->getType(), size);
  } else if (const GlobalValue * val = dyn_cast<GlobalValue>(c)) {
    return sizeof(void *);
    //return getSizeofGlobalValue(val) + sizeof(void *);
  } else {
    errs() << "Other: ";
    c->print(errs());
    errs() << "\n";
  }

  return size;
}

void ReflectionSizePass::getSizeofType(const llvm::Type * ty, size_t & size) {
  switch (ty->getTypeID()) {
    case Type::FloatTyID:
    case Type::DoubleTyID:
    case Type::X86_FP80TyID:
    case Type::FP128TyID:
    case Type::PPC_FP128TyID:
    case Type::IntegerTyID:
      size += ty->getPrimitiveSizeInBits() / 8;
      break;

    case Type::PointerTyID:
      size = ptrAlign(size) + PTR_SIZE;
      break;

    case Type::StructTyID:
    case Type::ArrayTyID:
    case Type::VectorTyID:
      break;

    default:
      break;
  }
}

void ReflectionSizePass::report() const {
  errs() << "Reflection size: globals=" << globalSize_ << " methodCount=" << methodCount_ << "\n";

}

}

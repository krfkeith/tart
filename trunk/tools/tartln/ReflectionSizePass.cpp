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

size_t align(size_t size, size_t align) {
  if (align >= 8) {
    align = 8;
  } else if (align >= 3) {
    align = 4;
  } else if (align >= 2) {
    align = 2;
  } else {
    return size;
  }

  return (size + align - 1) & ~(align - 1);
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
    globalSize_ += getSizeofGlobalValue(val);
  } else if (val->getName().startswith(".string")) {
    stringSize_ += getSizeofGlobalValue(val);
  }
}

size_t ReflectionSizePass::getSizeofGlobalValue(const GlobalValue * val) {
  if (!globals_.insert(val)) {
    return 0;
  }

  if (val->getName().endswith(".tib")) {
    return 0;
  }

  const GlobalVariable * var = cast<GlobalVariable>(val);
  if (var->hasInitializer()) {
    size_t size = getSizeofConstant(var->getInitializer());
    //errs() << "Size of '" << val->getName() << "' is " << size << "\n";
    return size;
  }

  return 0;
}

size_t ReflectionSizePass::getSizeofConstant(const Constant * c) {
  size_t size = getSizeofType(c->getType(), 0);
  if (const ConstantStruct * cs = dyn_cast<ConstantStruct>(c)) {
    for (ConstantStruct::const_op_iterator it = cs->op_begin(); it != cs->op_end(); ++it) {
      if (GlobalVariable * gv = dyn_cast<GlobalVariable>(it)) {
        //size += getSizeofGlobalValue(gv);
      }
    }
  } else if (const ConstantArray * ca = dyn_cast<ConstantArray>(c)) {
    for (ConstantStruct::const_op_iterator it = ca->op_begin(); it != ca->op_end(); ++it) {
      if (GlobalVariable * gv = dyn_cast<GlobalVariable>(it)) {
        //size += getSizeofGlobalValue(gv);
      }
    }
  }

  return size;
}

size_t ReflectionSizePass::getSizeofType(const llvm::Type * ty, size_t prevSize) {
  switch (ty->getTypeID()) {
    case Type::FloatTyID:
    case Type::DoubleTyID:
    case Type::X86_FP80TyID:
    case Type::FP128TyID:
    case Type::PPC_FP128TyID:
    case Type::IntegerTyID: {
      size_t sz = ty->getPrimitiveSizeInBits() / 8;
      return align(prevSize, sz) + sz;
    }

    case Type::PointerTyID:
      return align(prevSize, PTR_SIZE) + PTR_SIZE;

    case Type::StructTyID: {
      size_t size = prevSize;
      size_t fieldCount = ty->getNumContainedTypes();
      for (size_t i = 0; i < fieldCount; ++i) {
        size = getSizeofType(ty->getContainedType(i), size);
      }

      return size;
    }

    case Type::ArrayTyID: {
      const llvm::ArrayType * aty = cast<llvm::ArrayType>(ty);
      size_t elemSize = getSizeofType(ty->getContainedType(0));
      return align(elemSize, prevSize) + elemSize * aty->getNumElements();
    }

    case Type::VectorTyID: {
      const llvm::VectorType * vty = cast<llvm::VectorType>(ty);
      size_t elemSize = getSizeofType(ty->getContainedType(0));
      return align(elemSize, prevSize) + elemSize * vty->getNumElements();
    }

    default:
      return 0;
  }
}

void ReflectionSizePass::report() const {
  errs() << "Reflection size: globalSize = " << globalSize_ <<
      " stringSize = " << stringSize_ <<
      " methodCount = " << methodCount_ << "\n";
}

}

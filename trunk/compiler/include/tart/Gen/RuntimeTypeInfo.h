/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_RUNTIMETYPEINFO_H
#define TART_GEN_RUNTIMETYPEINFO_H

#include <llvm/Function.h>
#include <llvm/Constant.h>

namespace tart {

class Module;
class CompositeType;

/// -------------------------------------------------------------------
/// Runtime structures for a type.
class RuntimeTypeInfo {
private:
  const CompositeType * type;
  bool external;
  llvm::GlobalValue::LinkageTypes linkageType;
  llvm::GlobalVariable * typeDescriptor_;
  llvm::GlobalVariable * typeInfoBlock;
  llvm::PATypeHolder typeInfoBlockType;
  llvm::Constant * typeInfoPtr;
  llvm::Function * typeAllocator;

public:
  RuntimeTypeInfo(const CompositeType * ty, Module * m);

  /** The type we're generating. */
  const CompositeType * getType() const {
    return type;
  }

  /** True if the type is defined external to the module. */
  bool isExternal() const { return external; }

  /** Linkage type to use for this type. */
  llvm::GlobalValue::LinkageTypes getLinkageType() const {
    return linkageType;
  }

  void setTypeDescriptor(llvm::GlobalVariable * value) {
    typeDescriptor_ = value;
  }

  /** The TypeInfoBlock for this type. */
  llvm::GlobalVariable * getTypeInfoBlock() const {
    return typeInfoBlock;
  }

  void setTypeInfoBlock(llvm::GlobalVariable * value) {
    typeInfoBlock = value;
  }

  /** The PATypeHolder for the TypeInfoBlock type. */
  llvm::PATypeHolder & getTypeInfoBlockType() {
    return typeInfoBlockType;
  }

  /** The TypeInfoBlock pointer for this type. */
  llvm::Constant * getTypeInfoPtr() const {
    return typeInfoPtr;
  }

  void setTypeInfoPtr(llvm::Constant * value) {
    typeInfoPtr = value;
  }

  /** The allocator function for this type. */
  llvm::Function * getTypeAllocator() const {
    return typeAllocator;
  }

  void setTypeAllocator(llvm::Function * value) {
    typeAllocator = value;
  }
};

}

#endif

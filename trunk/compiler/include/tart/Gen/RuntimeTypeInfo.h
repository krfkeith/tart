/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_RUNTIMETYPEINFO_H
#define TART_GEN_RUNTIMETYPEINFO_H

#ifndef LLVM_FUNCTION_H
#include "llvm/Function.h"
#endif

#ifndef LLVM_CONSTANT_H
#include "llvm/Constant.h"
#endif

#ifndef LLVM_DERIVED_TYPES_H
#include "llvm/DerivedTypes.h"
#endif

namespace tart {

class Module;
class CompositeType;

/// -------------------------------------------------------------------
/// Runtime structures for a type.
class RuntimeTypeInfo {
private:
  const CompositeType * type;
  bool external_;
  llvm::GlobalValue::LinkageTypes linkageType_;
  llvm::GlobalVariable * typeInfoBlock_;
  llvm::Function * typeAllocator_;

public:
  RuntimeTypeInfo(const CompositeType * ty, Module * m);

  /** The type we're generating. */
  const CompositeType * getType() const {
    return type;
  }

  /** True if the type is defined external to the module. */
  bool isExternal() const { return external_; }

  /** Linkage type to use for this type. */
  llvm::GlobalValue::LinkageTypes getLinkageType() const {
    return linkageType_;
  }

  /** The TypeInfoBlock for this type. */
  llvm::GlobalVariable * getTypeInfoBlock() const {
    return typeInfoBlock_;
  }

  void setTypeInfoBlock(llvm::GlobalVariable * value) {
    typeInfoBlock_ = value;
  }

  void setTypeAllocator(llvm::Function * value) {
    typeAllocator_ = value;
  }
};

}

#endif

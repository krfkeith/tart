/** Similar to StructBuilder, but works purely with LLVM types. */

#include "tart/Common/ConstantBuilder.h"

#include "llvm/Target/TargetData.h"

namespace tart {

ConstantBuilder & ConstantBuilder::addField(llvm::Constant * value) {
  assert(value->getType() != NULL);
  members_.push_back(value);
  return *this;
}

ConstantBuilder & ConstantBuilder::addObjectHeader(llvm::Constant * typeInfo) {
  ConstantBuilder objectHeader(module_);
  objectHeader.addField(llvm::ConstantExpr::getPointerCast(typeInfo, tibType()->getPointerTo()));
  objectHeader.addInt(0);
  members_.push_back(objectHeader.buildStruct());
  return *this;
}

ConstantBuilder & ConstantBuilder::addInt(int i) {
  TargetData targetData(&module_);
  const IntegerType * intptrType = targetData.getIntPtrType(module_.getContext());
  addField(ConstantInt::get(intptrType, i, false));
  return *this;
}

Constant * ConstantBuilder::buildStruct(const llvm::Type * type) {
  return ConstantStruct::get(cast<StructType>(type), members_);
}

Constant * ConstantBuilder::buildStruct() {
  return ConstantStruct::get(context_, members_, false);
}

const Type * ConstantBuilder::tibType() {
  if (tibType_ == NULL) {
    tibType_ = module_.getTypeByName("tart.core.TypeInfoBlock");
    assert(tibType_ != NULL);
  }

  return tibType_;
}

}

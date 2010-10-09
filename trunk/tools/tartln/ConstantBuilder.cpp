/** Similar to StructBuilder, but works purely with LLVM types. */

#include "ConstantBuilder.h"

namespace tart {

ConstantBuilder & ConstantBuilder::addField(llvm::Constant * value) {
  assert(value->getType() != NULL);
  members_.push_back(value);
  return *this;
}

Constant * ConstantBuilder::buildStruct(const llvm::Type * type) {
  return ConstantStruct::get(cast<StructType>(type), members_);
}

Constant * ConstantBuilder::buildStruct() {
  return ConstantStruct::get(context_, members_, false);
}

}

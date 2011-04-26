/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Meta/MDNodeBuilder.h"
#include "llvm/Metadata.h"

namespace tart {

using namespace llvm;

// -------------------------------------------------------------------
// MDNodeBuilder

MDNode * MDNodeBuilder::build() {
  return MDNode::get(context_, args_);
}

MDNodeBuilder & MDNodeBuilder::put(llvm::StringRef str) {
  args_.push_back(MDString::get(context_, str));
  return *this;
}

} // namespace tart

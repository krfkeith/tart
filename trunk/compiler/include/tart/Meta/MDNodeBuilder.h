/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_MDNODEBUILDER_H
#define TART_META_MDNODEBUILDER_H

#ifndef LLVM_ADT_SMALLVECTOR_H
#include "llvm/ADT/SmallVector.h"
#endif

#ifndef LLVM_ADT_STRINGREF_H
#include "llvm/ADT/StringRef.h"
#endif

namespace llvm {
class NamedMDNode;
class MDNode;
class MDString;
class Value;
class LLVMContext;
}

namespace tart {

typedef llvm::SmallVector<llvm::Value *, 16> ValueList;

/// -------------------------------------------------------------------
/// Metadata node builder.

class MDNodeBuilder {
public:
  MDNodeBuilder(llvm::LLVMContext & context) : context_(context) {}

  /** Add a value to the operands of the MDNode. */
  MDNodeBuilder & put(llvm::Value * value) {
    args_.push_back(value);
    return *this;
  }

  /** Add an MDString value to the operands of the MDNode. */
  MDNodeBuilder & put(llvm::StringRef str);

  /** Build the MDNode. */
  llvm::MDNode * build();

private:

  llvm::LLVMContext & context_;
  ValueList args_;
};

} // namespace tart

#endif

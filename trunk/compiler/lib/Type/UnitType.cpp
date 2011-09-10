/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/UnitType.h"
#include "tart/Expr/Constant.h"

#include "tart/Common/Diagnostics.h"

#ifndef LLVM_ADT_FOLDINGSET_H
#include "llvm/ADT/FoldingSet.h"
#endif

namespace tart {

namespace {
  /** Code to clear the cached type maps during a collection. */
  class UniqueTypesRoot : public GCRootBase {
  public:
    UniqueTypesRoot(UnitType::UniqueTypes & map) : map_(map) {}

  private:
    void trace() const {
      for (UnitType::UniqueTypes::iterator it = map_.begin(); it != map_.end(); ++it) {
        it->getValue()->mark();
      }
    }

    UnitType::UniqueTypes & map_;
  };
}

// -------------------------------------------------------------------
// UnitType

UnitType::UniqueTypes UnitType::uniqueTypes_;

UnitType * UnitType::get(ConstantExpr * value) {
  // Make the type map a garbage collection root
  static UniqueTypesRoot root(uniqueTypes_);

  llvm::FoldingSetNodeID id;
  id.Add(value);
  void * insertPoint;
  UniqueTypeNode * node = uniqueTypes_.FindNodeOrInsertPos(id, insertPoint);
  if (node != NULL) {
    return node->getValue();
  }

  UnitType * result = new UnitType(value);
  uniqueTypes_.InsertNode(new UniqueTypeNode(result), insertPoint);
  return result;
}

llvm::Type * UnitType::irType() const {
  DFAIL("IllegalState");
}

Expr * UnitType::nullInitValue() const {
  DFAIL("IllegalState");
}

void UnitType::trace() const {
  value_->mark();
}

void UnitType::format(FormatStream & out) const {
  out << value_;
}

} // namespace tart

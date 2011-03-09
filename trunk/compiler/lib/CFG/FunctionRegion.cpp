/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/FunctionRegion.h"
#include "tart/Defn/FunctionDefn.h"

namespace tart {

// -------------------------------------------------------------------
// FunctionRegion

void FunctionRegion::trace() const {
  safeMark(function_);
  safeMark(parentRegion_);
}

void FunctionRegion::dump() const {
  diag.debug() << function_->linkageName() << " in:";
  parentRegion_->dump();
}

} // namespace tart

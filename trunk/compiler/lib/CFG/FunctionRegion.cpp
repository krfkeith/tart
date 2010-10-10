/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/FunctionRegion.h"
#include "tart/CFG/FunctionDefn.h"

namespace tart {

// -------------------------------------------------------------------
// FunctionRegion

SLC FunctionRegion::create(FunctionDefn * fn, SLC & parentLoc) {
  if (parentLoc.region != NULL) {
    FunctionRegion * region = new FunctionRegion(fn, parentLoc.region);
    return SourceLocation(region, parentLoc.begin, parentLoc.end);
  } else {
    return parentLoc;
  }
}

void FunctionRegion::trace() const {
  safeMark(function_);
  safeMark(parentRegion_);
}

} // namespace tart

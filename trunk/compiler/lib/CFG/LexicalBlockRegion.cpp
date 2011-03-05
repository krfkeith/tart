/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/LexicalBlockRegion.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// LexicalBlockRegion

void LexicalBlockRegion::trace() const {
  safeMark(location_.region);
}

void LexicalBlockRegion::dump() const {
  diag.debug() << "lexical block in:";
  location_.region->dump();
}

} // namespace tart

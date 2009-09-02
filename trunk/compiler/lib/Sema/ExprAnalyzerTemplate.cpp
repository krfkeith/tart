/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Module.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include <llvm/DerivedTypes.h>

namespace tart {

Expr * ExprAnalyzer::reduceSpecialize(const ASTSpecialize * ast,
    Type * expected) {
  DFAIL("Obsolete, to be deleted");
}

} // namespace tart

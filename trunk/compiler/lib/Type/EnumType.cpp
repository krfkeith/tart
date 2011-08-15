/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/EnumType.h"
#include "tart/Expr/Exprs.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// EnumType

llvm::Type * EnumType::createIRType() const {
  return baseType_->irType();
}

} // namespace tart

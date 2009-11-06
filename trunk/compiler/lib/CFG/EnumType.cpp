/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/EnumType.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// EnumType

const llvm::Type * EnumType::createIRType() const {
  return baseType_->irType();
}

bool EnumType::isSubtype(const Type * other) const {
  return this == other /*|| baseType_->isSubtype(other)*/;
}

bool EnumType::includes(const Type * other) const {
  if (this->isEqual(other)) {
    return true;
  }

  // TODO: Only if other is an enum type and inherits from this?
  return false;
}

ConversionRank EnumType::convertImpl(const Conversion & cn) const {
  if (cn.getFromType() == this) {
    if (cn.resultValue != NULL) {
      *cn.resultValue = cn.fromValue;
    }

    return IdenticalTypes;
  }

  if ((cn.options & Conversion::Dynamic) && cn.getFromType()->isReferenceType()) {
    if (cn.resultValue != NULL) {
      *cn.resultValue = new CastExpr(Expr::UnboxCast, cn.fromValue->location(),
          const_cast<EnumType *>(this), cn.fromValue);
    }

    return NonPreferred;
  }

  return Incompatible;
  //diag.error() << "Failed conversion from " << cn.fromType << " to " << this;
  //DFAIL("Implement");
}

} // namespace tart

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

ConversionRank EnumType::convertImpl(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (fromType == this) {
    if (cn.resultValue != NULL) {
      *cn.resultValue = cn.fromValue;
    }

    return IdenticalTypes;
  }

  // An integer 0 can be converted to a flags enum.
  if (isFlags() && fromType->isIntType() && cn.fromValue != NULL &&
      cn.fromValue->isConstant()) {
    if (ConstantInteger * cint = dyn_cast<ConstantInteger>(cn.fromValue)) {
      if (cint->value()->isNullValue()) {
        return baseType_->convertImpl(cn);
      }
    }
  }

  // An integer can be coerced to an enum.
  if (fromType->isIntType() && (cn.options & Conversion::Coerce)) {
    ConversionRank rank = baseType_->convertImpl(cn);
    if (rank != Incompatible) {
      if (cn.resultValue != NULL) {
        *cn.resultValue = new CastExpr(Expr::BitCast, cn.fromValue->location(), this,
            cn.fromValue);
      }
      return rank;
    }
  }

  // Unboxing
  if ((cn.options & Conversion::Checked) && fromType->isReferenceType()) {
    if (cn.resultValue != NULL) {
      *cn.resultValue = new CastExpr(Expr::UnboxCast, cn.fromValue->location(),
          this, cn.fromValue);
    }

    return NonPreferred;
  }

  return Incompatible;
  //diag.error() << "Failed conversion from " << cn.fromType << " to " << this;
  //DFAIL("Implement");
}

} // namespace tart

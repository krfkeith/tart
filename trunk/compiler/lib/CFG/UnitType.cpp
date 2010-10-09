/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/UnitType.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// UnitType

UnitType * UnitType::get(ConstantExpr * value) {
  // TODO: Fold unique values.
  return new UnitType(value);
}

bool UnitType::isEqual(const Type * other) const {
  if (const UnitType * ntc = dyn_cast<UnitType>(other)) {
    return value_->isEqual(ntc->value());
  }

  return false;
}

const llvm::Type * UnitType::irType() const {
  DFAIL("IllegalState");
}

ConversionRank UnitType::convertImpl(const Conversion & conversion) const {
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

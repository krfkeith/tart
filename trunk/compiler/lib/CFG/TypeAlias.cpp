/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/TypeAlias.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// TypeAlias

TypeAlias::TypeAlias(const Type * val)
  : Type(Alias)
  , value_(val)
{
}

const llvm::Type * TypeAlias::irType() const {
  DASSERT(value_ != NULL);
  return value_->irType();
}

const llvm::Type * TypeAlias::irEmbeddedType() const {
  return value_->irEmbeddedType();
}

const llvm::Type * TypeAlias::irParameterType() const {
  return value_->irParameterType();
}

const llvm::Type * TypeAlias::irReturnType() const {
  return value_->irReturnType();
}

ConversionRank TypeAlias::convertImpl(const Conversion & conversion) const {
  DASSERT(value_ != NULL);
  return value_->convertImpl(conversion);
}

void TypeAlias::format(FormatStream & out) const {
  DASSERT(value_ != NULL);
  return value_->format(out);
}

void TypeAlias::trace() const {
  value_->mark();
}

} // namespace tart

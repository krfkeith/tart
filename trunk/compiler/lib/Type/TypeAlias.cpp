/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/TypeAlias.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// TypeAlias

TypeAlias::TypeAlias(const Type * val, TypeDefn * defn)
  : Type(Alias)
  , value_(val)
  , defn_(defn)
{
}

llvm::Type * TypeAlias::irType() const {
  DASSERT(!value_.isNull());
  return value_->irType();
}

llvm::Type * TypeAlias::irEmbeddedType() const {
  return value_->irEmbeddedType();
}

llvm::Type * TypeAlias::irParameterType() const {
  return value_->irParameterType();
}

llvm::Type * TypeAlias::irReturnType() const {
  return value_->irReturnType();
}

void TypeAlias::format(FormatStream & out) const {
  out << value_;
}

void TypeAlias::trace() const {
  safeMark(value_.unqualified());
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/UnitType.h"
#include "tart/Type/TypeVariable.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// TypeVariable

TypeVariable::TypeVariable(const SourceLocation & location, StringRef name, Target target,
    QualifiedType metaType)
  : TypeImpl(TypeVar, Shape_Unset)
  , location_(location)
  , target_(target)
  , metaType_(metaType)
  , upperBound_(NULL)
  , name_(name)
  , isVariadic_(false)
{}

llvm::Type * TypeVariable::createIRType() const {
  DFAIL("Invalid");
}

bool TypeVariable::canBindTo(QualifiedType value) const {
  if (Qualified<UnitType> nt = value.dyn_cast<UnitType>()) {
    if (!metaType_) {
      return false;
    }

    ConstantExpr * expr = nt->value();
    return TypeConversion::check(expr, metaType_);
  } else if (value->typeClass() == Type::Assignment) {
    return true;
  } else if (!metaType_) {
    return true;
  } else {
    return false;
  }
}

bool TypeVariable::isReferenceType() const {
  return false;
}

bool TypeVariable::isSingular() const {
  return false;
}

void TypeVariable::trace() const {
  TypeImpl::trace();
  location_.trace();
}

void TypeVariable::format(FormatStream & out) const {
  out << "%" << name_;
  if (isVariadic_) {
    out << "...";
  }
}

} // namespace Tart

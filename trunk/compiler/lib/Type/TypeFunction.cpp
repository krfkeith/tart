/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/TupleType.h"
#include "tart/Type/TypeFunction.h"
#include "tart/Type/TypeVariable.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// TypeFunction

llvm::Type * TypeFunction::irType() const {
  DFAIL("Unimplemented Method");
}

// -------------------------------------------------------------------
// QualifyingTypeFunction

const QualifiedType QualifyingTypeFunction::apply(const TupleType * args) const {
  DASSERT(args->size() == 1);
  return args->member(0) | qualifiers_;
}

void QualifyingTypeFunction::format(FormatStream & out) const {
  formatQualifiedTypePrefix(out, qualifiers_);
  out << "?";
  formatQualifiedTypeSuffix(out, qualifiers_);
}

void QualifyingTypeFunction::formatArgs(FormatStream & out, const TupleType * args) const {
  if (qualifiers_) {
    formatQualifiedTypePrefix(out, qualifiers_);
    out << args->member(0);
    formatQualifiedTypeSuffix(out, qualifiers_);
  } else {
    out << "identity(" << args->member(0) << ")";
  }
}

// -------------------------------------------------------------------
// TypeFunctionCall

llvm::Type * TypeFunctionCall::irType() const {
  DFAIL("Unimplemented Method");
}

void TypeFunctionCall::trace() const {
  fnVal_->mark();
  args_->mark();
}

void TypeFunctionCall::format(FormatStream & out) const {
  if (const TypeFunction * tf = dyn_cast_or_null<TypeFunction>(fnVal_)) {
    tf->formatArgs(out, args_);
  } else {
    out << fnVal_ << args_;
  }
}

} // namespace tart

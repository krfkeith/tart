/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"
#include "tart/Defn/TemplateConditions.h"

#include "tart/Sema/TypeTransform.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// TypeComparisonCondition

void TypeComparisonCondition::trace() const {
  first_->mark();
  second_->mark();
}

// -------------------------------------------------------------------
// IsSubclassCondition

bool IsSubtypeCondition::eval() const {
  return first_->isSubtype(second_);
}

TemplateCondition * IsSubtypeCondition::transform(TypeTransform & transform) {
  const Type * first = transform.transform(first_);
  const Type * second = transform.transform(second_);

  if (first == first_ && second == second_) {
    return this;
  }

  return new IsSubtypeCondition(first, second);
}

void IsSubtypeCondition::format(FormatStream & out) const {
  out << first_ << " <: " << second_;
}

} //  namespace tart

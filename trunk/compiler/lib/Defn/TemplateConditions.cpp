/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"
#include "tart/Defn/TemplateConditions.h"

#include "tart/Type/TypeRelation.h"

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
  if (first_.qualifiers() == 0 || second_.qualifiers() == 0) {
    // If either side lacks explicit qualifiers, then don't use qualifiers at all.
    return TypeRelation::isSubtype(first_.unqualified(), second_.unqualified());
  } else {
    return TypeRelation::isSubtype(first_, second_);
  }
}

TemplateCondition * IsSubtypeCondition::transform(TypeTransform & transform) {
  QualifiedType first = transform(first_);
  QualifiedType second = transform(second_);

  if (first == first_ && second == second_) {
    return this;
  }

  return new IsSubtypeCondition(first, second);
}

void IsSubtypeCondition::format(FormatStream & out) const {
  out << first_ << " <: " << second_;
}

} //  namespace tart

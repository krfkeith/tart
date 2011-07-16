/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/Infer/Constraint.h"

#include "tart/Type/Type.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/EnumType.h"

#include "tart/Common/Formattable.h"

namespace tart {

// -------------------------------------------------------------------
// ConstraintFunction

FormatStream & operator<<(FormatStream & out, Constraint::Kind kind) {
  switch (kind) {
    case Constraint::EXACT: out << "=="; break;
    case Constraint::UPPER_BOUND: out << "<="; break;
    case Constraint::LOWER_BOUND: out << ">="; break;
  }
  return out;
}

// -------------------------------------------------------------------
// Constraint

bool Constraint::checkProvisions() const {
  if (!provisions_.empty()) {
    return provisions_.check();
  }
  return true;
}

void Constraint::trace() const {
  safeMark(value_);
  for (ProvisionSet::const_iterator it = provisions_.begin(); it != provisions_.end(); ++it) {
    (*it)->mark();
  }
}

bool Constraint::accepts(const Type * ty) const {
  switch (kind_) {
    case EXACT:
      return ty == value_;

    case LOWER_BOUND: {
      if (Type::isSubtype(value_, ty)) {
        return true;
      }

      if (const EnumType * et = dyn_cast<EnumType>(value_)) {
        // TODO: For some reason putting this code where it belongs - in EnumType - breaks things.
        return Type::isSubtype(et->baseType(), ty);
      }

      return false;
    }

    case UPPER_BOUND:
      return Type::isSubtype(ty, value_);
  }
}

bool Constraint::equals(const Constraint * cst) const {
  return cst->value_ == value_ &&
      cst->kind_ == kind_ &&
      cst->stateCount_ == stateCount_ &&
      cst->provisions_.equals(provisions_);
}

Constraint * Constraint::intersect(Constraint * c0, Constraint * c1) {
  const Type * t0 = PrimitiveType::derefEnumType(c0->value());
  const Type * t1 = PrimitiveType::derefEnumType(c1->value());

  bool isMoreLenient0 = c1->provisions().implies(c0->provisions());
  bool isMoreLenient1 = c0->provisions().implies(c1->provisions());
  bool isEqualProvisions = isMoreLenient0 && isMoreLenient1;

  // If the provision are disjoint, then no joining is possible.
  if (!isMoreLenient0 && !isMoreLenient1) {
    return NULL;
  }

  if (t0 == t1) {
    if (c0->kind() == c1->kind()) {
      return isMoreLenient0 ? c0 : c1;
    } else if (c0->kind() == EXACT && isMoreLenient0) {
      return c0;
    } else if (c1->kind() == EXACT && isMoreLenient1) {
      return c1;
    } else if (isEqualProvisions &&
        ((c0->kind() == LOWER_BOUND && c1->kind() == UPPER_BOUND) ||
         (c0->kind() == UPPER_BOUND && c1->kind() == LOWER_BOUND))) {
      return new Constraint(c0->location(), t0, EXACT, c0->provisions());
    } else {
      return NULL;
    }
  }

  bool isSubtypeOf0 = false;
  bool isSubtypeOf1 = false;

  if (const CompositeType * ct0 = dyn_cast<CompositeType>(t0)) {
    if (const CompositeType * ct1 = dyn_cast<CompositeType>(t1)) {
      isSubtypeOf0 = ct0->isSubclassOf(ct1);
      isSubtypeOf1 = ct1->isSubclassOf(ct0);
    }
  } else if (t0->isIntType() && t1->isIntType()) {
    isSubtypeOf0 = Type::isSubtype(t0, t1);
    isSubtypeOf1 = Type::isSubtype(t1, t0);
  } else if (t0->isFPType() && t1->isFPType()) {
    isSubtypeOf0 = Type::isSubtype(t0, t1);
    isSubtypeOf1 = Type::isSubtype(t1, t0);
  }

  if (isSubtypeOf0) { // c0 <= c1
    DASSERT(!isSubtypeOf1);
    if ((c0->kind() == UPPER_BOUND || c0->kind() == EXACT) &&
        c1->kind() == UPPER_BOUND &&
        isMoreLenient0) {
      // (T <= c0 < c1) or (T == c0 < c1)
      return c0;
    }

    if (c0->kind() == LOWER_BOUND &&
        (c1->kind() == LOWER_BOUND || c1->kind() == EXACT) &&
        isMoreLenient1) {
      // (c0 < c1 <= T) or (c0 < c1 == T)
      return c1;
    }

  } else if (isSubtypeOf1) {
    if (c1->kind() == LOWER_BOUND &&
        (c0->kind() == LOWER_BOUND || c0->kind() == EXACT) &&
        isMoreLenient0) {
      // (c1 < c0 <= T) or (c1 < c0 == T)
      return c0;
    }

    if ((c1->kind() == UPPER_BOUND || c1->kind() == EXACT) &&
        c0->kind() == UPPER_BOUND &&
        isMoreLenient1) {
      // (T <= c1 < c0) or (T == c1 < c0)
      return c1;
    }
  }

  if (c0->kind() == EXACT && c0->value()->isSingular() &&
      c1->value()->typeClass() == Type::Assignment && isMoreLenient0) {
    return c0;
  }

  if (c1->kind() == EXACT && c1->value()->isSingular() &&
      c0->value()->typeClass() == Type::Assignment && isMoreLenient1) {
    return c1;
  }

  return NULL;
}

bool Constraint::contradicts(const Constraint * c0, const Constraint * c1) {
  const Type * t0 = PrimitiveType::derefEnumType(c0->value());
  const Type * t1 = PrimitiveType::derefEnumType(c1->value());

  if (t0 == t1) {
    return false;
  }

  // If the types are not singular then we don't know if there's a contradiction.
  // TODO: Could likely allow some tests.
  if (!t0->isSingular() || !t1->isSingular()) {
    return false;
  }

  bool isSubtypeOf0 = t0->isSubtypeOf(t1);
  bool isSubtypeOf1 = t1->isSubtypeOf(t0);

  if (isSubtypeOf0 && isSubtypeOf1) {
    return false;
  }

  switch (c0->kind()) {
    case EXACT:
      return c1->kind() == EXACT
          || (c1->kind() == UPPER_BOUND && !isSubtypeOf0)
          || (c1->kind() == LOWER_BOUND && !isSubtypeOf1);
    case UPPER_BOUND:
      return !isSubtypeOf1 && c1->kind() != UPPER_BOUND;
    case LOWER_BOUND:
      return !isSubtypeOf0 && c1->kind() != LOWER_BOUND;
  }
}

} // namespace tart

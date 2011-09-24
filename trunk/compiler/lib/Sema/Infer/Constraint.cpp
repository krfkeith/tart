/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/Infer/Constraint.h"

#include "tart/Type/Type.h"
#include "tart/Type/TypeRelation.h"
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
  safeMark(value_.unqualified());
  for (ProvisionSet::const_iterator it = provisions_.begin(); it != provisions_.end(); ++it) {
    (*it)->mark();
  }
}

bool Constraint::accepts(QualifiedType ty) const {
  switch (kind_) {
    case EXACT:
      return TypeRelation::isEqual(ty, value_);

    case LOWER_BOUND:
      return TypeRelation::isSubtype(value_, ty);

    case UPPER_BOUND:
      return TypeRelation::isSubtype(ty, value_);
  }
  return false;
}

bool Constraint::equals(const Constraint * cst) const {
  return cst->value_ == value_ &&
      cst->kind_ == kind_ &&
      cst->stateCount_ == stateCount_ &&
      cst->provisions_.equals(provisions_);
}

Constraint * Constraint::intersect(Constraint * cl, Constraint * cr) {
  QualifiedType tl = cl->value();
  QualifiedType tr = cr->value();

  bool isMoreLenient0 = cr->provisions().implies(cl->provisions());
  bool isMoreLenient1 = cl->provisions().implies(cr->provisions());
  bool isEqualProvisions = isMoreLenient0 && isMoreLenient1;

  // If the provision are disjoint, then no joining is possible.
  if (!isMoreLenient0 && !isMoreLenient1) {
    return NULL;
  }

  // Note we don't use isEqual here because it can infinitely recurse.
  if (tl == tr) {
    if (cl->kind() == cr->kind()) {
      return isMoreLenient0 ? cl : cr;
    } else if (cl->kind() == EXACT && isMoreLenient0) {
      return cl;
    } else if (cr->kind() == EXACT && isMoreLenient1) {
      return cr;
    } else if (isEqualProvisions &&
        ((cl->kind() == LOWER_BOUND && cr->kind() == UPPER_BOUND) ||
         (cl->kind() == UPPER_BOUND && cr->kind() == LOWER_BOUND))) {
      return new Constraint(cl->location(), tl, EXACT, cl->provisions());
    } else {
      return NULL;
    }
  }

  bool isSubtypeL = false;
  bool isSubtypeR = false;

  if (tl.isa<CompositeType>()) {
    if (tr.isa<CompositeType>()) {
      isSubtypeL = TypeRelation::isSubclass(tl, tr);
      isSubtypeR = TypeRelation::isSubclass(tr, tl);
    }
  } else if (tl->isIntType() && tr->isIntType()) {
    isSubtypeL = TypeRelation::isSubtype(tl, tr);
    isSubtypeR = TypeRelation::isSubtype(tr, tl);
  } else if (tl->isFPType() && tr->isFPType()) {
    isSubtypeL = TypeRelation::isSubtype(tl, tr);
    isSubtypeR = TypeRelation::isSubtype(tr, tl);
  }

  if (isSubtypeL) { // c0 <= c1
    // TODO: Handle cases of more complex equality.
    if (isSubtypeR) {
      return NULL;
    }
    if ((cl->kind() == UPPER_BOUND || cl->kind() == EXACT) &&
        cr->kind() == UPPER_BOUND &&
        isMoreLenient0) {
      // (T <= c0 < c1) or (T == c0 < c1)
      return cl;
    }

    if (cl->kind() == LOWER_BOUND &&
        (cr->kind() == LOWER_BOUND || cr->kind() == EXACT) &&
        isMoreLenient1) {
      // (c0 < c1 <= T) or (c0 < c1 == T)
      return cr;
    }

  } else if (isSubtypeR) {
    if (cr->kind() == LOWER_BOUND &&
        (cl->kind() == LOWER_BOUND || cl->kind() == EXACT) &&
        isMoreLenient0) {
      // (c1 < c0 <= T) or (c1 < c0 == T)
      return cl;
    }

    if ((cr->kind() == UPPER_BOUND || cr->kind() == EXACT) &&
        cl->kind() == UPPER_BOUND &&
        isMoreLenient1) {
      // (T <= c1 < c0) or (T == c1 < c0)
      return cr;
    }
  }

  if (cl->kind() == EXACT && cl->value()->isSingular() &&
      cr->value()->typeClass() == Type::Assignment && isMoreLenient0) {
    return cl;
  }

  if (cr->kind() == EXACT && cr->value()->isSingular() &&
      cl->value()->typeClass() == Type::Assignment && isMoreLenient1) {
    return cr;
  }

  return NULL;
}

bool Constraint::contradicts(const Constraint * cl, const Constraint * cr) {
  const Type * tl = cl->value().type();
  const Type * tr = cr->value().type();

  if (tl == tr) {
    return false;
  }

  // If the types are not singular then we don't know if there's a contradiction.
  // TODO: Could likely allow some tests.
  if (!tl->isSingular() || !tr->isSingular()) {
    return false;
  }

  bool isSubtypeL = TypeRelation::isSubtype(tl, tr);
  bool isSubtypeR = TypeRelation::isSubtype(tr, tl);

  if (isSubtypeL && isSubtypeR) {
    return false;
  }

  switch (cl->kind()) {
    case EXACT:
      return cr->kind() == EXACT
          || (cr->kind() == UPPER_BOUND && !isSubtypeL)
          || (cr->kind() == LOWER_BOUND && !isSubtypeR);
    case UPPER_BOUND:
      return !isSubtypeR && cr->kind() != UPPER_BOUND;
    case LOWER_BOUND:
      return !isSubtypeL && cr->kind() != LOWER_BOUND;
  }
  return false;
}

} // namespace tart

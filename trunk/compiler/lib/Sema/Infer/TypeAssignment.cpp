/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"

#include "tart/Type/TypeAlias.h"
#include "tart/Type/TypeConversion.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/ADT/DenseSet.h"

namespace tart {

typedef llvm::DenseSet<const Type *, Type::KeyInfo> TypeSet;

// -------------------------------------------------------------------
// TypeAssignment

TypeAssignment::TypeAssignment(const TypeVariable * target, GC * scope)
  : Type(Assignment)
  , next_(NULL)
  , scope_(scope)
  , target_(target)
  , primaryProvision_(NULL)
  , sequenceNum_(0)
  , value_(NULL)
{
}

void TypeAssignment::setValue(QualifiedType value) {
  value_ = value;
}

void TypeAssignment::remove(ConstraintSet::iterator si) {
  constraints_.erase(si);
}

bool TypeAssignment::isSingular() const {
  if (value_) {
    return value_->isSingular();
  }

  return false;
}

bool TypeAssignment::isReferenceType() const {
  if (value_) {
    return value_->isReferenceType();
  }

  return false;
}

void TypeAssignment::expand(QualifiedTypeSet & out) const {
  if (value_) {
    value_->expand(out);
  } else {
    for (ConstraintSet::const_iterator si = begin(), sEnd = end(); si != sEnd; ++si) {
      Constraint * s = *si;
      if (!s->visited() && s->checkProvisions()) {
        s->setVisited(true);
        s->value()->expand(out);
        s->setVisited(false);
      }
    }
  }
}

QualifiedType TypeAssignment::findSingularSolution() {
  ConstraintSet::const_iterator ciEnd = end();

  // First check all the EXACT constraints
  value_ = NULL;
  for (ConstraintSet::const_iterator ci = begin(); ci != ciEnd; ++ci) {
    Constraint * c = *ci;
    if (c->checkProvisions()) {
      if (c->kind() == Constraint::EXACT) {
        QualifiedType ty = TypeAssignment::deref(c->value());
        if (!value_) {
          value_ = ty;
        } else if (!TypeRelation::isEqual(value_, ty)) {
          value_ = NULL;
          return NULL;
        }
      }
    }
  }

  // Now check all the other constraints to insure that it satisfies them all.
  if (value_) {
    for (ConstraintSet::const_iterator ci = begin(); ci != ciEnd; ++ci) {
      Constraint * c = *ci;
      if (c->checkProvisions() && c->kind() != Constraint::EXACT && !c->accepts(value_)) {
        value_ = NULL;
        return NULL;
      }
    }
    return value_;
  }

  // There was no EXACT solution, so next try LOWER_BOUND constraints
  for (ConstraintSet::const_iterator ci = begin(); ci != ciEnd; ++ci) {
    Constraint * c = *ci;
    if (c->checkProvisions()) {
      if (c->kind() == Constraint::LOWER_BOUND) {
        QualifiedType ty = TypeAssignment::deref(c->value());
        if (!value_) {
          value_ = ty;
        } else if (TypeRelation::isSubtype(ty, value_)) {
          continue;
        } else if (TypeRelation::isSubtype(value_, ty)) {
          value_ = ty;
        } else {
          // Attempt to find a common base of value_ and the lower bound.
          value_ = Type::commonBase(value_, ty);
          if (!value_) {
            return NULL;
          }
        }
      }
    }
  }

  // Now check all the other constraints to insure that it satisfies them all.
  if (value_) {
    for (ConstraintSet::const_iterator ci = begin(); ci != ciEnd; ++ci) {
      Constraint * c = *ci;
      if (c->checkProvisions() && c->kind() == Constraint::UPPER_BOUND && !c->accepts(value_) &&
          c->value()->typeClass() != Type::Assignment) {
        value_ = NULL;
        return NULL;
      }
    }
    return value_;
  }

  // There were no LOWER_BOUND constraints, so try UPPER_BOUND constraints:
  if (!value_) {
    for (ConstraintSet::const_iterator ci = begin(); ci != ciEnd; ++ci) {
      Constraint * c = *ci;
      if (c->checkProvisions()) {
        if (c->kind() == Constraint::UPPER_BOUND) {
          QualifiedType ty = TypeAssignment::deref(c->value());
          if (!value_) {
            value_ = ty;
          } else if (TypeRelation::isSubtype(ty, value_)) {
            value_ = ty;
          } else if (TypeRelation::isSubtype(value_, ty)) {
            continue;
          } else {
            value_ = NULL;
            return NULL;
          }
        }
      }
    }
  }

  return value_;
}

Expr * TypeAssignment::nullInitValue() const {
  DFAIL("IllegalState");
}

void TypeAssignment::trace() const {
  safeMark(next_);
  safeMark(target_);
  safeMark(value_.unqualified());
  for (ConstraintSet::const_iterator si = begin(), sEnd = end(); si != sEnd; ++si) {
    (*si)->mark();
  }
}

void TypeAssignment::format(FormatStream & out) const {
  out << target_ << "." << sequenceNum_;
  if (out.isVerbose()) {
    if (value_) {
      out << "==";
      value_->format(out);
    } else if (!constraints_.empty()) {
      for (ConstraintSet::const_iterator si = begin(), sEnd = end(); si != sEnd; ++si) {
        Constraint * s = *si;
        if (!s->visited() && s->checkProvisions()) {
          s->setVisited(true);
          out << "==" << s->value();
          s->setVisited(false);
        }
      }
    }
  }
}

llvm::Type * TypeAssignment::irType() const {
  DFAIL("IllegalState");
}

QualifiedType TypeAssignment::deref(QualifiedType in) {
  while (Qualified<TypeAssignment> ta = in.dyn_cast<TypeAssignment>()) {
    if (ta->value()) {
      in = ta->value();
    } else {
      break;
    }
  }

  return in;
}

} // namespace tart

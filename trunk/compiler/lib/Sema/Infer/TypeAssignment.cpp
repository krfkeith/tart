/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"

#include "tart/Type/TypeAlias.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/ADT/DenseSet.h"

namespace tart {

typedef llvm::DenseSet<const Type *, Type::KeyInfo> TypeSet;

extern bool unifyVerbose;

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

void TypeAssignment::remove(ConstraintSet::iterator si) {
  constraints_.erase(si);
}

bool TypeAssignment::isSingular() const {
  if (const Type * val = value()) {
    return val->isSingular();
  }

  return false;
}

bool TypeAssignment::isReferenceType() const {
  if (value_) {
    return value_->isReferenceType();
  }

  return false;
}

bool TypeAssignment::isEqual(const Type * other) const {
  return compare(&Type::isEqual, other);
}

bool TypeAssignment::isSubtypeOf(const Type * other) const {
  if (value_ != NULL) {
    return TypeRelation::isSubtype(value_, other);
  } else {
    bool any = false;
    for (ConstraintSet::const_iterator si = begin(), sEnd = end(); si != sEnd; ++si) {
      Constraint * cst = *si;
      if (!cst->visited() && cst->checkProvisions()) {
        if (cst->kind() == Constraint::LOWER_BOUND) {
          // There's no way to determine if this is true, so return false.
          return false;
        }

        cst->setVisited(true);
        if (!TypeRelation::isSubtype(cst->value(), other)) {
          cst->setVisited(false);
          return false;
        }
        any = true;
        cst->setVisited(false);
      }
    }
    return any;
  }
}

void TypeAssignment::expand(TypeExpansion & out) const {
  if (value_ != NULL) {
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

const Type * TypeAssignment::findSingularSolution() {
  ConstraintSet::const_iterator ciEnd = end();

  // First check all the EXACT constraints
  value_ = NULL;
  for (ConstraintSet::const_iterator ci = begin(); ci != ciEnd; ++ci) {
    Constraint * c = *ci;
    if (c->checkProvisions()) {
      if (c->kind() == Constraint::EXACT) {
        const Type * ty = TypeAssignment::deref(c->value());
        if (value_ == NULL) {
          value_ = ty;
        } else if (!TypeRelation::isEqual(value_, ty)) {
          value_ = NULL;
          return NULL;
        }
      }
    }
  }

  // Now check all the other constraints to insure that it satisfies them all.
  if (value_ != NULL) {
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
        const Type * ty = TypeAssignment::deref(c->value());
        if (value_ == NULL) {
          value_ = ty;
        } else if (TypeRelation::isSubtype(ty, value_)) {
          continue;
        } else if (TypeRelation::isSubtype(value_, ty)) {
          value_ = ty;
        } else {
          // Attempt to find a common base of value_ and the lower bound.
          value_ = Type::commonBase(value_, ty);
          if (value_ == NULL) {
            return NULL;
          }
        }
      }
    }
  }

  // Now check all the other constraints to insure that it satisfies them all.
  if (value_ != NULL) {
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
  if (value_ == NULL) {
    for (ConstraintSet::const_iterator ci = begin(); ci != ciEnd; ++ci) {
      Constraint * c = *ci;
      if (c->checkProvisions()) {
        if (c->kind() == Constraint::UPPER_BOUND) {
          const Type * ty = TypeAssignment::deref(c->value());
          if (value_ == NULL) {
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

ConversionRank TypeAssignment::convertImpl(const Conversion & cn) const {
  if (value_ != NULL) {
    return value_->convert(cn);
  } else {
    ConversionRank rank = Incompatible;
    for (ConstraintSet::const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
      Constraint * c = *ci;
      if (!c->visited() && c->checkProvisions()) {
        c->setVisited(true);
        switch (c->kind()) {
          case Constraint::EXACT:
            rank = std::max(rank, c->value()->convert(cn));
            break;

          case Constraint::LOWER_BOUND:
            // Means T == value or is a supertype of value.
            // Can we convert from 'conversion.fromType' to a supertype of 'value'?
            // Should in general be the same as converting to 'value'.
            rank = std::max(rank, c->value()->convert(cn));
            //DFAIL("Implement");
            break;

          case Constraint::UPPER_BOUND:
            // In general, the answer to this case is unknowable.
            // Means T == value or is a subtype of value.
            rank = std::max(rank, std::min(NonPreferred, c->value()->convert(cn)));
            break;
        }
        c->setVisited(false);
      }
    }
    return rank;
  }
}

ConversionRank TypeAssignment::convertTo(const Type * toType, const Conversion & cn) const {
  if (value_ != NULL) {
    return toType->canConvert(value_, cn.options);
  }

  if (cn.resultValue != NULL) {
    return Incompatible;
  }

  ConversionRank rank = Incompatible;
  for (ConstraintSet::const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    Constraint * c = *ci;
    if (!c->visited() && c->checkProvisions()) {
      c->setVisited(true);
      switch (c->kind()) {
        case Constraint::EXACT:
          rank = std::max(rank, toType->canConvert(c->value(), cn.options));
          break;

        case Constraint::LOWER_BOUND:
          // In general, the answer to this case is unknowable.
          // For the moment, we'll say 'yes', but with a lower ranking.
          rank = std::max(rank, std::min(NonPreferred, toType->canConvert(c->value(), cn.options)));
          break;

        case Constraint::UPPER_BOUND:
          rank = std::max(rank, toType->canConvert(c->value(), cn.options));
          break;
      }
      c->setVisited(false);
    }
  }

  return rank;
}

Expr * TypeAssignment::nullInitValue() const {
  DFAIL("IllegalState");
}

void TypeAssignment::trace() const {
  safeMark(next_);
  safeMark(target_);
  safeMark(value_);
  for (ConstraintSet::const_iterator si = begin(), sEnd = end(); si != sEnd; ++si) {
    (*si)->mark();
  }
}

void TypeAssignment::format(FormatStream & out) const {
  out << target_ << "." << sequenceNum_;
  if (out.isVerbose()) {
    if (value_ != NULL) {
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

const llvm::Type * TypeAssignment::irType() const {
  DFAIL("IllegalState");
}

const Type * TypeAssignment::deref(const Type * in) {
  while (const TypeAssignment * ta = dyn_cast<TypeAssignment>(in)) {
    if (ta->value() != NULL) {
      in = ta->value();
    } else {
      break;
    }
  }

  return in;
}

} // namespace tart

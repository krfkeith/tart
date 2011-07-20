/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/Infer/ConstraintSet.h"
#include "tart/Sema/Infer/TypeAssignment.h"

namespace tart {

// -------------------------------------------------------------------
// ConstraintSet

Constraint * ConstraintSet::insert(
    SLC & loc, const Type * ty, unsigned state, Constraint::Kind kind,
    const ProvisionSet & provisions) {
  for (const_iterator ci = begin(), itEnd = end(); ci != itEnd; ++ci) {
    Constraint * s = *ci;
    if (s->value() == ty &&
        s->stateCount() == state &&
        s->kind() == kind &&
        s->provisions().equals(provisions)) {
      return s;
    }
  }

  Constraint * cst = new Constraint(loc, ty, state, kind, provisions);
  insertImpl(cst);
  return cst;
}

Constraint * ConstraintSet::insert(
    SLC & loc, const Type * ty, Constraint::Kind kind, const ProvisionSet & provisions) {
  for (const_iterator ci = begin(), itEnd = end(); ci != itEnd; ++ci) {
    Constraint * s = *ci;
    if (s->value() == ty && s->kind() == kind && s->provisions().equals(provisions)) {
      return s;
    }
  }

  Constraint * cst = new Constraint(loc, ty, kind, provisions);
  insertImpl(cst);
  return cst;
}

Constraint * ConstraintSet::insert(Constraint * in) {
  for (const_iterator ci = begin(), itEnd = end(); ci != itEnd; ++ci) {
    if (in->equals(*ci)) {
      return *ci;
    }
  }

  insertImpl(in);
  return in;
}

Constraint * ConstraintSet::insertAndOptimize(
    SLC & loc, const Type * ty, Constraint::Kind kind, const ProvisionSet & provisions) {
  return insertAndOptimize(new Constraint(loc, ty, kind, provisions));
}

Constraint * ConstraintSet::insertAndOptimize(Constraint * in) {
  for (iterator ci = begin(); ci != end();) {
    Constraint * cst = *ci;
    Constraint * opt = Constraint::intersect(cst, in);
    if (opt != NULL) {
      ci = erase(ci);
      in = opt;
    } else {
      ++ci;
    }
  }

  insertImpl(in);
  return in;
}

bool ConstraintSet::contains(const Constraint * s0) const {
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    const Constraint * s1 = *ci;
    if (s0->value() == s1->value() &&
        s0->kind() == s1->kind() &&
        s0->provisions().equals(s1->provisions())) {
      return true;
    }
  }

  return false;
}

bool ConstraintSet::containsAll(const ConstraintSet & other) const {
  for (const_iterator ci = other.begin(), ciEnd = other.end(); ci != ciEnd; ++ci) {
    if (!contains(*ci)) {
      return false;
    }
  }

  return true;
}

bool ConstraintSet::equals(ConstraintSet & other) const {
  return size() == other.size() && containsAll(other) && other.containsAll(*this);
}

bool ConstraintSet::accepts(const Type * ty) const {
  bool any = false;
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    Constraint * cst = *ci;
    if (cst->visited()) {
      any = true;
    } else if (cst->checkProvisions()) {
      cst->setVisited(true);
      if (!cst->accepts(ty)) {
        cst->setVisited(false);
        return false;
      }
      cst->setVisited(false);
      any = true;
    }
  }
  return any;
}

void ConstraintSet::minimize() {
  ConstraintSet saved;
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    Constraint * c = *ci;
    for (iterator si = saved.begin(); si != saved.end();) {
      Constraint * combined = Constraint::intersect(c, *si);
      if (combined != NULL) {
        si = saved.erase(si);
        c = combined;
      } else {
        ++si;
      }
    }
    saved.push_back(c);
  }

  std::swap(saved, *this);
}

} // namespace tart

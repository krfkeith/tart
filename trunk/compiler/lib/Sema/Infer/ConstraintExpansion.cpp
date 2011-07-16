/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Type/AmbiguousParameterType.h"
#include "tart/Type/AmbiguousResultType.h"

#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/Infer/ConstraintExpansion.h"
#include "tart/Sema/Infer/TypeAssignment.h"

namespace tart {

// -------------------------------------------------------------------
// ConstraintExpansion

bool ConstraintExpansion::expand(Constraint * s) {
  return expand(s->value(), s->kind(), s->provisions());
}

bool ConstraintExpansion::expand(const Type * ty, Constraint::Kind kind,
    const ProvisionSet & provisions) {
  switch (ty->typeClass()) {
    case Type::Assignment: {
      const TypeAssignment * ta = static_cast<const TypeAssignment *>(ty);

      // If the TA is unconditionally set to a value, then don't bother with
      // the constraints.
      if (ta->value() != NULL) {
        if (ta->value() != exclude_) {
          result_.insertAndOptimize(SourceLocation(), ta->value(), kind, provisions);
        }
        return true;
      }

      // Dereference type vars that are on the right side of a type assignment.
      const ConstraintSet & sl = ta->constraints();
      ConstraintSet tempResult;
      bool preserveOriginal = true;
      for (ConstraintSet::const_iterator si = sl.begin(), sEnd = sl.end(); si != sEnd; ++si) {
        Constraint * c = *si;
        if (c->value() != exclude_) { // Eliminate cyclic constraints
          Constraint::Kind combinedKind;
          if (kind == Constraint::EXACT && c->kind() == Constraint::EXACT) {
            combinedKind = Constraint::EXACT;
            preserveOriginal = false;
          } else if (kind == Constraint::EXACT) {
            combinedKind = c->kind();
            preserveOriginal = false; // Note: This may be logically unsound in some cases
          } else if (c->kind() == Constraint::EXACT) {
            combinedKind = kind;
            preserveOriginal = false; // Note: This may be logically unsound in some cases
          } else if (kind == c->kind()) {
            combinedKind = kind;
          } else {
            return false;
          }
          // Add the dereferenced assignment with the union of both sets of provisions.
          ProvisionSet unionProvisions(provisions);
          unionProvisions.insert(c->provisions().begin(), c->provisions().end());
          if (unionProvisions.isConsistent()) {
            tempResult.insert(c->location(), c->value(), combinedKind,
                unionProvisions);
          }
        }
      }

      for (ConstraintSet::const_iterator si = tempResult.begin(), sEnd = tempResult.end();
          si != sEnd; ++si) {
        result_.insertAndOptimize(*si);
      }

      // We no longer need the original if there was an EXACT->EXACT constraint chain
      return !preserveOriginal;
    }

    case Type::AmbiguousParameter: {
      const AmbiguousParameterType * poc = static_cast<const AmbiguousParameterType *>(ty);
      const Candidates & cd = poc->expr()->candidates();
      for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
        CallCandidate * cc = *it;
        const Type * paramType = cc->paramType(poc->argIndex());
        ProvisionSet ccProvisions(provisions);
        ccProvisions.insertIfValid(cc->primaryProvision());
        if (ccProvisions.isConsistent()) {
          result_.insertAndOptimize(poc->expr()->location(), paramType, kind, ccProvisions);
        }
      }
      return true;
    }

    case Type::AmbiguousResult: {
      const AmbiguousResultType * roc = static_cast<const AmbiguousResultType *>(ty);
      const Candidates & cd = roc->expr()->candidates();
      for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
        CallCandidate * cc = *it;
        const Type * resultType = roc->candidateResultType(cc);
        ProvisionSet ccProvisions(provisions);
        ccProvisions.insertIfValid(cc->primaryProvision());
        if (ccProvisions.isConsistent()) {
          result_.insertAndOptimize(roc->expr()->location(), resultType, kind, ccProvisions);
        }
      }
      return true;
    }

    default:
      return false;
  }
}

void ConstraintExpansion::expandAll(const ConstraintSet & cset) {
  ConstraintSet unsizedIntConstraints;
  for (ConstraintSet::const_iterator ci = cset.begin(), sEnd = cset.end(); ci != sEnd; ++ci) {
    if (!expand(*ci)) {
      // Reinsert the original unchanged constraint.
      result_.insertAndOptimize(*ci);
    }
  }
}

} // namespace tart

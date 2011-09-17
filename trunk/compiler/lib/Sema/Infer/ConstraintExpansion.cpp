/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Type/AmbiguousParameterType.h"
#include "tart/Type/AmbiguousResultType.h"
#include "tart/Type/AmbiguousTypeParamType.h"

#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/Infer/ConstraintExpansion.h"
#include "tart/Sema/Infer/TypeAssignment.h"

namespace tart {

// -------------------------------------------------------------------
// ConstraintExpansion

bool ConstraintExpansion::expand(Constraint * s) {
  return expand(s->value(), s->kind(), s->provisions());
}

bool ConstraintExpansion::expand(QualifiedType ty, Constraint::Kind kind,
    const ProvisionSet & provisions) {
  switch (ty->typeClass()) {
    case Type::Assignment: {
      Qualified<TypeAssignment> ta = ty.as<TypeAssignment>();

      // If the TA is unconditionally set to a value, then don't bother with
      // the constraints.
      if (ta->value()) {
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
            tempResult.insert(new Constraint(c->location(), c->value(), combinedKind,
                unionProvisions));
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
      Qualified<AmbiguousParameterType> apt = ty.as<AmbiguousParameterType>();
      const Candidates & cd = apt->expr()->candidates();
      for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
        CallCandidate * cc = *it;
        QualifiedType paramType = cc->paramType(apt->argIndex());
        ProvisionSet ccProvisions(provisions);
        ccProvisions.insertIfValid(cc->primaryProvision());
        if (ccProvisions.isConsistent()) {
          result_.insertAndOptimize(apt->expr()->location(), paramType.type(), kind, ccProvisions);
        }
      }
      return true;
    }

    case Type::AmbiguousResult: {
      Qualified<AmbiguousResultType> art = ty.as<AmbiguousResultType>();
      const Candidates & cd = art->expr()->candidates();
      for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
        CallCandidate * cc = *it;
        QualifiedType resultType = art->candidateResultType(cc);
        ProvisionSet ccProvisions(provisions);
        ccProvisions.insertIfValid(cc->primaryProvision());
        if (ccProvisions.isConsistent()) {
          result_.insertAndOptimize(art->expr()->location(), resultType.type(), kind, ccProvisions);
        }
      }
      return true;
    }

    case Type::AmbiguousTypeParam: {
      Qualified<AmbiguousTypeParamType> tpt = ty.as<AmbiguousTypeParamType>();
      // Expand the base type.
      ConstraintExpansion expander(exclude_);
      if (!expander.expand(tpt->base(), kind, provisions)) {
        return false;
      }

      // If the expansion that came back was just 'base' and nothing more, then return 'unchanged'.
      const ConstraintSet & cs = expander.result();
      if (cs.size() == 1 && cs.front()->value() == tpt->base()) {
        return false;
      }

      for (ConstraintSet::const_iterator si = cs.begin(), sEnd = cs.end(); si != sEnd; ++si) {
        Constraint * cst = *si;
        QualifiedType st = AmbiguousTypeParamType::forType(
            cst->value(), tpt->match(), tpt->paramIndex());
        if (st) {
          result_.insertAndOptimize(
              cst->location(), st, Constraint::combine(kind, cst->kind()), cst->provisions());
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

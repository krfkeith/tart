/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_CONSTRAINTSET_H
#define TART_SEMA_INFER_CONSTRAINTSET_H

#ifndef TART_SEMA_INFER_CONSTRAINT_H
#include "tart/Sema/Infer/Constraint.h"
#endif

namespace tart {

typedef llvm::SmallPtrSet<const Type *, 4> TypeSolutionSet;

/// -------------------------------------------------------------------
/// A list of Constraints.

class ConstraintSet : public llvm::SmallVector<Constraint *, 4> {
public:
  /** Add a new constraint, or return an existing one if it matches. */
  Constraint * insert(SLC & loc, const Type * ty, unsigned state, Constraint::Kind kind,
      const ProvisionSet & provisions = ProvisionSet());

  /** Add a new constraint, or return an existing one if it matches. */
  Constraint * insert(SLC & loc, const Type * ty, Constraint::Kind kind,
      const ProvisionSet & provisions = ProvisionSet());

  /** Add a new constraint, or return an existing one if it matches. */
  Constraint * insert(Constraint * in);

  /** Insert a constraint, merging with existing constraints if possible. */
  Constraint * insertAndOptimize(
      SLC & loc, const Type * ty, Constraint::Kind kind, const ProvisionSet & provisions);

  /** Insert a constraint, merging with existing constraints if possible. */
  Constraint * insertAndOptimize(Constraint * in);

  /** Return true if this list contains a constraint whose value, function, and
      provisions match the input. */
  bool contains(const Constraint * s) const;

  /** Return true if this list contains all of the constraints in 'other'. */
  bool containsAll(const ConstraintSet & other) const;

  /** Return true if both lists have equivalent constraints. */
  bool equals(ConstraintSet & other) const;

  /** Attempt to optimize this set of constraints. */
  void minimize();

  /** Return if all active constraints in this constraint set accept the give type. */
  bool accepts(const Type * ty) const;

private:
  /** Wrap actual insertion so we can set a breakpoint on insert. */
  void insertImpl(Constraint * cst) {
    push_back(cst);
  }
};

} // namespace tart

#endif // TART_SEMA_INFER_CONSTRAINTSET_H

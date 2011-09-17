/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_CONSTRAINTEXPANSION_H
#define TART_SEMA_INFER_CONSTRAINTEXPANSION_H

#ifndef TART_SEMA_INFER_CONSTRAINTSET_H
#include "tart/Sema/Infer/ConstraintSet.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A helper class that attempts to expand a single input constraint
/// into multiple output constraints by
class ConstraintExpansion {
public:
  ConstraintExpansion(const Type * exclude = NULL) : exclude_(exclude) {}

  /** Given an input constraint, attempt to expand it dereferencing
      type variables or decomposing type constraints. Note that this
      only dereferences a *single level* of type variables - it does not
      recurse.

      Returns false if the constraint could not be expanded. No changes
      to the result list will be made.
   */
  bool expand(Constraint * c);

  /** Similar to expand above, except that it takes the individual properties
      of a constraint. */
  bool expand(QualifiedType ty, Constraint::Kind kind, const ProvisionSet & provisions);

  /** Expand all of the constraints in the list 'sl'. Substitutions which cannot
      be expanded are added to the result list unchanged. */
  void expandAll(const ConstraintSet & sl);

  /** The output constraint list. */
  const ConstraintSet & result() const { return result_; }
  ConstraintSet & result() { return result_; }

private:
  ConstraintSet result_;
  const Type * exclude_;
};

} // namespace tart

#endif // TART_SEMA_INFER_CONSTRAINTEXPANSION_H

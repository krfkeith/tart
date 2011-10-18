/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_TYPEASSIGNMENT_H
#define TART_SEMA_INFER_TYPEASSIGNMENT_H

#ifndef TART_SEMA_INFER_CONSTRAINTSET_H
#include "tart/Sema/Infer/ConstraintSet.h"
#endif

namespace tart {

class BindingEnv;
class TypeVariable;

/// -------------------------------------------------------------------
/// A TypeAssignment represents the assignment of a type variable to a
/// value within an environment.
///
/// There are three levels of multiplicity that have to be handled.
/// First, the same template can occur multiple times in an expression,
/// and all such instances are distinct - so a template such as
/// List[T] has to be renamed to List[T1], List[T2] and so on.
///
/// Second, a given template variable may have more than one value
/// assigned to it. You can think of assignments as assertions of
/// equivalence - so if an variable T has assignments for both A
/// and B, it means that T is equivalent to both A and B (which
/// must be equivalent to each other.) Part of the operation
/// of the type inference algorithm is to reconcile the definitions
/// of A, B, and T so that they are in agreement.
///
/// Finally, each assignment may represent a collection of
/// alternate types, only one of which can be in the final
/// solution. This can happen when the substituted value is
/// the return type of an overloaded method, for example.
class TypeAssignment : public Type {
public:
  /** The next type assignment defined in the environment. */
  TypeAssignment * next() const { return next_; }

  /** The expression node which generated this type assignment. This will either
      be a call to a templated function, or an explicit template specialization. */
  GC * scope() const { return scope_; }

  /** The target of the assignment - the type variable to which a value is
      being assigned. */
  const TypeVariable * target() const { return target_; }

  /** The primary precondition which must me true in order for this type
      assignment to be considered. For type assignments that are
      generated from overloaded methods, this will be a NotCulledProvision
      on the specific method. */
  const Provision * primaryProvision() const { return primaryProvision_; }
  void setPrimaryProvision(const Provision * provision) { primaryProvision_ = provision; }

  /** Return true if the primary provision is true. */
  bool checkPrimaryProvision() const {
    return primaryProvision_ == NULL || primaryProvision_->check();
  }

  /** A number used to distinguish multiple instances of the same type variable
      within a single environment. An expression may have multiple instances
      of the same template. Before unification, we rename all of the type bindings
      so that each one is unique. So a type variable 'T' will have type bindings
      T1, T2, T3 ... etc.
   */
  int sequenceNum() const { return sequenceNum_; }

  /** The final value of the type, after all constraints have been solved. */
  QualifiedType value() const { return value_; }
  void setValue(QualifiedType value);

  /** The list of constraints. You can think of a constraint as an assertion
      of equivalence. */
  const ConstraintSet & constraints() const { return constraints_; }
  ConstraintSet & constraints() { return constraints_; }
  ConstraintSet & mutableConstraints() const {
    return const_cast<TypeAssignment *>(this)->constraints_;
  }
  ConstraintSet::iterator begin() { return constraints_.begin(); }
  ConstraintSet::const_iterator begin() const { return constraints_.begin(); }
  ConstraintSet::iterator end() { return constraints_.end(); }
  ConstraintSet::const_iterator end() const { return constraints_.end(); }

  /** Remove an existing constraint. */
  void remove(ConstraintSet::iterator si);

  /** Set the value of this assignment to a type that satisfies all active constraints,
      if such as type can be found. Sets value to NULL if there is no such type. */
  QualifiedType findSingularSolution();

  // Statics

  /** If 'in' is a type assignment with a value, return the value, otherwise return 'in'
      unchanged. */
  static QualifiedType deref(QualifiedType in);

  // Overrides

  void expandImpl(QualifiedTypeSet & out, unsigned qualifiers) const;
  bool isSingular() const;
  bool isReferenceType() const;
  TypeShape typeShape() const {
    return value_ ? value_->typeShape() : Shape_Unset;
  }
  Expr * nullInitValue() const;
  void trace() const;
  void format(FormatStream & out) const;
  llvm::Type * irType() const;

  static inline bool classof(const TypeAssignment *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == Assignment;
  }

  TypeAssignment(const TypeVariable * target, GC * scope);

private:
  friend class BindingEnv;

  TypeAssignment * next_;
  GC * scope_;
  const TypeVariable * target_;
  const Provision * primaryProvision_;
  int sequenceNum_;
  QualifiedType value_;
  ConstraintSet constraints_;
};

} // namespace tart

#endif

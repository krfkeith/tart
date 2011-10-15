/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_CONSTRAINT_H
#define TART_SEMA_INFER_CONSTRAINT_H

#ifndef TART_COMMON_SOURCELOCATION_H
#include "tart/Common/SourceLocation.h"
#endif

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_SEMA_INFER_PROVISION_H
#include "tart/Sema/Infer/Provision.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A Constraint is owned by a type assignment, and limits the set
/// of possible types that can be assigned.
class Constraint : public GC, public Locatable {
public:
  /** The different kinds of constraints. */
  enum Kind {
    EXACT = 0,          ///< Type must match exactly
    UPPER_BOUND,        ///< Type must match or be a subtype of constraint
    LOWER_BOUND,        ///< Type must match or be a supertype of constraint
  };

  Constraint(SourceLocation loc, QualifiedType value, unsigned state, Kind kind,
      const ProvisionSet & provisions)
    : loc_(loc)
    , value_(value)
    , kind_(kind)
    , stateCount_(state)
    , visited_(false)
    , provisions_(provisions)
  {
  }

  Constraint(SourceLocation loc, QualifiedType value, Kind kind, const ProvisionSet & provisions)
    : loc_(loc)
    , value_(value)
    , kind_(kind)
    , stateCount_(0)
    , visited_(false)
    , provisions_(provisions)
  {
  }

  /** Location of where this constraint came from. */
  const SourceLocation & location() const { return loc_; }

  /** The right side of the constraint. */
  QualifiedType value() const { return value_; }
  void setValue(QualifiedType value) { value_ = value; }

  /** What type of constraint this is - upper bound, lower bound, etc. */
  Kind kind() const { return kind_; }
  void setFunction(Kind kind) { kind_ = kind; }

  /** State counter value at the time this constraint was added. Used for backtracking. */
  unsigned stateCount() const { return stateCount_; }

  /** Whether this constraint has been visited (used in traversing the graph.) */
  bool visited() const { return visited_; }
  void setVisited(bool visited) const { visited_ = visited; }

  /** The set of conditions which must all be true, otherwise this constraint is ignored. */
  const ProvisionSet & provisions() const { return provisions_; }
  ProvisionSet & provisions() { return provisions_; }

  /** Check whether all of the provisions are true. */
  bool checkProvisions() const;

  /** Return true if this constraint accepts 'ty' */
  bool accepts(QualifiedType ty) const;

  /** Compare two constraints for equality. */
  bool equals(const Constraint * cst) const;

  // Statics

  /** Attempt to calculate a constraint representing the
      intersection of the two input constraints. Returns
      NULL if the constraints are disjoint. May return either
      of the input constraints if one is a strict subset
      of the other, or it may return a new constraint which
      is a combination of both of the inputs.

      Note that for purposes of this function, the provisions
      of a constraint are contravariant - meaning that a
      constraint having more lenient provisions is overall
      considered more strict, because its restriction applies
      to more situations.
   */
  static Constraint * intersect(Constraint * c0, Constraint * c1);

  /** Return true if the two constraints can bever be satisfied simultaneously.
      Note that provisions are not considered. */
  static bool contradicts(const Constraint * c0, const Constraint * c1);

  /** For a given constraint type, return the inverse type. */
  static inline Kind reverse(Kind kind) {
    switch (kind) {
      case EXACT: return EXACT;
      case UPPER_BOUND: return LOWER_BOUND;
      case LOWER_BOUND: return UPPER_BOUND;
    }
    return EXACT;
  }

  /** Produce a constraint type that is the intersection of two types. */
  static inline Kind combine(Kind cf0, Kind cf1) {
    return cf0 == cf1 ? cf0 : EXACT;
  }

  // Overrides

  void trace() const;

private:
  friend class TypeAssignment;

  SourceLocation loc_;
  QualifiedType value_;
  Kind kind_;
  unsigned stateCount_;
  mutable bool visited_;
  ProvisionSet provisions_;
};

FormatStream & operator<<(FormatStream & out, Constraint::Kind kind);

} // namespace tart

#endif // TART_SEMA_INFER_CONSTRAINT_H

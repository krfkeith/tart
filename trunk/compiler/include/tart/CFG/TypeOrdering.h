/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TYPEORDERING_H
#define TART_CFG_TYPEORDERING_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

namespace tart {

enum ComparisonResult {
  EQUAL,        // Types are identical.
  LEFT_FIRST,   // The left side precedes the right side.
  RIGHT_FIRST,  // The left side succedes the right side.
  UNORDERED,    // Types have no unambiguous ordering.
};

/// -------------------------------------------------------------------
/// Compare two types by specificity:
///   * A subclass is considered more specific than its bases.
///   * A small integer type is more specific than a big one.
///   * A member of a union is more specific than the union.
///   * A pattern variable is more specific than a concrete type, if
///      the concrete type can meet the constraints of the pattern.
///   * A pattern variable with constraints is more specific than one
///      with no constraints.
///
///  A result of LEFT_FIRST means that the left-side argument is the
///  more specific type.

ComparisonResult compareSpecificity(const TupleType * t1, const TupleType * t2);
ComparisonResult compareSpecificity(const TypeRef & t1, const TypeRef & t2);
ComparisonResult compareSpecificity(const Type * t1, const Type * t2);

ComparisonResult compareLexical(const TupleType * t1, const TupleType * t2);
ComparisonResult compareLexical(const TypeRef & t1, const TypeRef & t2);
ComparisonResult compareLexical(const Type * t1, const Type * t2);

}

#endif // TART_CFG_TYPEORDERING_H

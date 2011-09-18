/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPERELATION_H
#define TART_TYPE_TYPERELATION_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Various binary relations on types. All of these operators do
/// maximal dereferencing on their arguments, meaning that if the
/// argument is an alias, an assignment, or an ambiguous type, the
/// operator will attempt to "drill down" into the definition as
/// deeply as possible.
namespace TypeRelation {

enum Options {
  VERBOSE = (1<<0),                         // Report why we didn't match
  DISTINGUISH_VARIADIC_PARAMS = (1<<1),     // Functions different based on variadic flag
  DISTINGUISH_SELF_PARAM = (1<<2),          // Functions different based on self param
  MATCH_ALL_AMBIG = (1<<3),                 // Ambiguous types must match *all* possibles
  EXACT_ASSIGNMENT = (1<<4),                // Type assignment matches must be strict

  DEFAULT = MATCH_ALL_AMBIG,
};

/** Returns true if the type on the left is equal to the type on
    the right. */
bool isEqual(QualifiedType lhs, QualifiedType rhs);

/** Returns true if the type on the left is either the same as,
    or is a subtype of, the type on the right. Note that in the case of
    ambiguous types, all possibilities must pass the subclass test
    in order for the whole to be considered a subtype. */
bool isSubtype(QualifiedType ty, QualifiedType base);

/** Is similar to isSubtype, but more restrictive - only returns true for class/interface types,
    not other types. As a result of this restriction, we always know that when this function
    returns true, a value of type 'ty' can be stored directly into a variable of type 'base' without
    conversion or transformation. */
bool isSubclass(QualifiedType ty, QualifiedType base);

/** Type equality functor. */
class Equal {
public:
  bool operator()(const Type * t0, const Type * t1) {
    return isEqual(t0, t1);
  }
  bool operator()(QualifiedType t0, QualifiedType t1) {
    return isEqual(t0, t1);
  }
};

} // namespace TypeRelation
} // namespace tart

#endif // TART_TYPE_TYPERELATION_H

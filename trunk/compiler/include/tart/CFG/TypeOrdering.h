/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TYPEORDERING_H
#define TART_CFG_TYPEORDERING_H

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

namespace tart {

class Type;
class PrimitiveType;
class CompositeType;
class EnumType;
class FunctionType;
class BoundMethodType;
class UnionType;
class TupleType;
class AddressType;
class PointerType;
class NativeArrayType;
class UnitType;
class PatternVar;
class PatternValue;
class TypeConstraint;

enum ComparisonResult {
  EQUAL,        // Types are identical.
  LEFT_FIRST,   // The left side precedes the right side.
  RIGHT_FIRST,  // The left side succedes the right side.
  UNORDERED,    // Types have no unambiguous ordering.
};

ComparisonResult operator+(ComparisonResult r0, ComparisonResult r1);

// Flip the order from left to right and right to left.
ComparisonResult operator-(ComparisonResult cr);

/// -------------------------------------------------------------------
/// Abstract class that defines a comparison order between type
/// expressions.

class TypeOrdering {
  virtual ComparisonResult compare(const Type * t1, const Type * t2);
  virtual ComparisonResult compare(const PrimitiveType * t1, const PrimitiveType * t2);
  virtual ComparisonResult compare(const CompositeType * t1, const CompositeType * t2);
  virtual ComparisonResult compare(const EnumType * t1, const EnumType * t2);
  virtual ComparisonResult compare(const FunctionType * t1, const FunctionType * t2);
  virtual ComparisonResult compare(const BoundMethodType * t1, const BoundMethodType * t2);
  virtual ComparisonResult compare(const UnionType * t1, const UnionType * t2);
  virtual ComparisonResult compare(const TupleType * t1, const TupleType * t2);
  virtual ComparisonResult compare(const AddressType * t1, const AddressType * t2);
  virtual ComparisonResult compare(const PointerType * t1, const PointerType * t2);
  virtual ComparisonResult compare(const NativeArrayType * t1, const NativeArrayType * t2);
  virtual ComparisonResult compare(const UnitType * t1, const UnitType * t2);

  virtual ComparisonResult compareDissimilar(const Type * t1, const Type * t2);
  virtual ComparisonResult compareWithPattern(const PatternVar * t1, const Type * t2);
  virtual ComparisonResult compareWithPatternValue(const PatternValue * t1, const Type * t2);
  virtual ComparisonResult compareWithConstraint(const TypeConstraint * t1, const Type * t2);

  //    case Type::Alias:
};

/// -------------------------------------------------------------------
/// Defines a total ordering of all unique types. The ordering is
/// somewhat arbitrary, but it's enough to sort type expressions
/// in a deterministic way.
class LexicalTypeOrdering {
public:
  bool operator()(const Type * t0, const Type * t1) const;

  static int compare(const Type * t0, const Type * t1);

  template <class T>
  static int compare(T t0, T t1) {
    if (t0 < t1) { return -1; }
    if (t1 < t0) { return 1; }
    return 0;
  }
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

class SpecificityTypeOrder : public TypeOrdering {

};

/** Stream operator for type comparison result. */
FormatStream & operator<<(FormatStream & out, ComparisonResult cr);

} // namespace tart

#endif // TART_CFG_TYPEORDERING_H

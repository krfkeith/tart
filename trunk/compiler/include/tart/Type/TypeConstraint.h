/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPECONSTRAINT_H
#define TART_TYPE_TYPECONSTRAINT_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef LLVM_ADT_SMALLPTRSET_H
#include "llvm/ADT/SmallPtrSet.h"
#endif

namespace tart {

// Forward declarations
class CallExpr;
class CallCandidate;
class TupleCtorExpr;

/// -------------------------------------------------------------------
/// Abstract base class for type constraints. A type constraint represents
/// some subset of possible types.
class TypeConstraint : public Type {
public:

  virtual const Type * singularValue() const = 0;

  // Overrides

  llvm::Type * irType() const;
  TypeShape typeShape() const { return Shape_Unset; }

  static inline bool classof(const TypeConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() >= AmbiguousResult;
  }

protected:
  TypeConstraint(TypeClass tcls) : Type(tcls) {}
};

/// -------------------------------------------------------------------
/// A constraint that represents a set of types that is the result of
/// some type expression. The set of types may grow or shrink during
/// type inferencing as a result of partial solutions.
class TypeSetConstraint : public TypeConstraint {
public:

  const Type * singularValue() const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isEqual(const Type * other) const;
  bool isSingular() const;
  bool isReferenceType() const;
  void format(FormatStream & out) const;

protected:
  TypeSetConstraint(TypeClass tcls) : TypeConstraint(tcls) {}
};

/// -------------------------------------------------------------------
/// A type constraint representing the possible integer types which
/// an unsized integer constant can be converted to. Mainly this is
/// just a holder for the integer constant so that we can calculate
/// the conversion rankings knowing how large an integer we'll need.

class SizingOfConstraint : public TypeConstraint {
public:
  SizingOfConstraint(ConstantInteger * intVal)
    : TypeConstraint(SizingOf)
    , intVal_(intVal)
    , isNegative_(intVal->isNegative())
  {}

  /** The integer value. */
  ConstantInteger * intVal() const { return intVal_; }

  /** Whether the value is negative. */
  bool isNegative() const { return isNegative_; }

  /** Number of bits required to hold this value. */
  unsigned signedBitsRequired() const;
  unsigned unsignedBitsRequired() const;

  bool isSubtypeOf(const Type * other) const;

  // Overrides

  const Type * singularValue() const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const { return false; }
  bool isEqual(const Type * other) const;
  bool isIntType() const { return true; }
  bool isReferenceType() const { return false; }
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const SizingOfConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == SizingOf;
  }

private:
  ConstantInteger * intVal_;
  bool isNegative_;
};

} // namespace tart

#endif

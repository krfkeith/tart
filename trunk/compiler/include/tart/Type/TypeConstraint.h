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

  /** Return true if the predicate is true for all active members, and there is at least
      one active. */
  //virtual bool all() const = 0;

  // Overrides

  const llvm::Type * irType() const;
  TypeShape typeShape() const { return Shape_Unset; }

  static inline bool classof(const TypeConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() >= ResultOf;
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
  bool includes(const Type * other) const;
  bool isEqual(const Type * other) const;
  bool isSubtypeOf(const Type * other) const;
  bool isSingular() const;
  bool isReferenceType() const;
  void format(FormatStream & out) const;

protected:
  TypeSetConstraint(TypeClass tcls) : TypeConstraint(tcls) {}
};

/// -------------------------------------------------------------------
/// A type constraint representing the result of a method, where we don't
/// know exactly which method will be chosen yet.
class ResultOfConstraint : public TypeSetConstraint {
public:
  ResultOfConstraint(CallExpr * call)
    : TypeSetConstraint(ResultOf)
    , callExpr_(call)
  {}

  /** The call expression. */
  CallExpr * expr() const { return callExpr_; }

  /** The list of call candidates. */
  const Candidates & candidates() const;

  /** The result type for a given candidate. */
  const Type * candidateResultType(const CallCandidate * cc) const;

  // Overrides

  void expand(TypeExpansion & out) const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const ResultOfConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == ResultOf;
  }

private:
  CallExpr * callExpr_;
};

/// -------------------------------------------------------------------
/// A type constraint representing a value that will be passed as an
/// argument to a method, where we don't know exactly which method
/// will be chosen yet.
class ParameterOfConstraint : public TypeSetConstraint {
public:
  ParameterOfConstraint(CallExpr * call, unsigned argIndex)
    : TypeSetConstraint(ParameterOf)
    , callExpr_(call)
    , argIndex_(argIndex)
  {}

  /** The call expression. */
  CallExpr * expr() const { return callExpr_; }

  /** The list of call candidates. */
  const Candidates & candidates() const;

  /** The argument index. */
  unsigned argIndex() const { return argIndex_; }

  /** The Nth param type for a given candidate. */
  const Type * candidateParamType(const CallCandidate * cc) const;

  // Overrides

  void expand(TypeExpansion & out) const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const ParameterOfConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == ParameterOf;
  }

private:
  CallExpr * callExpr_;
  unsigned argIndex_;
};

/// -------------------------------------------------------------------
/// A type constraint representing a type parameter to a type which
/// in turn may be a constraint.
class TypeParamOfConstraint : public TypeSetConstraint {
public:
  TypeParamOfConstraint(const TypeConstraint * base, Type::TypeClass cls, unsigned paramIndex)
    : TypeSetConstraint(TypeParamOf)
    , base_(base)
    , cls_(cls)
    , paramIndex_(paramIndex)
  {}

  /** The type that we want to get a type parameter of. */
  const TypeConstraint * base() const { return base_; }

  /** Given a type, return the value of the Nth type parameter. */
  const Type * forType(const Type * ty) const;

  // Overrides

  void expand(TypeExpansion & out) const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const TypeParamOfConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == TypeParamOf;
  }

private:
  const TypeConstraint * base_;
  Type::TypeClass cls_;
  unsigned paramIndex_;
};

/// -------------------------------------------------------------------
/// A type constraint representing a tuple whose members may themselves
/// be constraints.
class TupleOfConstraint : public TypeConstraint {
public:
  TupleOfConstraint(TupleCtorExpr * tuple)
    : TypeConstraint(TupleOf)
    , tuple_(tuple)
  {}

  TupleCtorExpr * tuple() const { return tuple_; }

  // Overrides

  const Type * singularValue() const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool includes(const Type * other) const;
  bool isSubtypeOf(const Type * other) const;
  bool isSingular() const;
  bool isReferenceType() const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const TupleOfConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == TupleOf;
  }

private:
  TupleCtorExpr * tuple_;
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

  // Overrides

  const Type * singularValue() const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool includes(const Type * other) const;
  bool isSubtypeOf(const Type * other) const;
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

/// -------------------------------------------------------------------
/// A type constraint representing a set of possible alternative values
/// which could be the result of expression. The goal will be to find
/// a common type which fits all of them.

class PHIConstraint : public TypeConstraint {
public:
  PHIConstraint(const Type * expected)
    : TypeConstraint(PhiType)
    , expected_(expected)
    , common_(NULL)
  {}

  // Add a possible type.
  void add(const Type * type);

  // The set of input types
  const ConstTypeList & types() const { return types_; }

  // The common type of the input types
  const Type * common() const { return common_; }
  void setCommon(const Type * ty) const { common_ = ty; }

  // The type we're attempting to assign to, which might be NULL.
  const Type * expected() const { return expected_; }

  // Overrides

  void expand(TypeExpansion & out) const;
  const Type * singularValue() const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool includes(const Type * other) const;
  bool isSubtypeOf(const Type * other) const;
  bool isSingular() const;
  bool isReferenceType() const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const PHIConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == PhiType;
  }

private:
  ConstTypeList types_;
  const Type * expected_;
  mutable const Type * common_;
};

} // namespace tart

#endif

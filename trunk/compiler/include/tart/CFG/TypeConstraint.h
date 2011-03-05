/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TYPECONSTRAINT_H
#define TART_CFG_TYPECONSTRAINT_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#include "llvm/ADT/SmallPtrSet.h"

namespace tart {

// Forward declarations
class CallExpr;
class BindingEnv;
class CallCandidate;
class TupleCtorExpr;

// Represents the expansion of a constraint into all of the types that match
// that constraint.
typedef llvm::SmallPtrSet<const Type *, 32> TypeExpansion;

/// -------------------------------------------------------------------
/// Abstract base class for type constraints. A type constraint represents
/// some subset of possible types.
class TypeConstraint : public Type {
public:

  // Unify with the input pattern
  virtual bool unifyWithPattern(BindingEnv &env, const Type * pattern) const = 0;
  virtual const Type * singularValue() const = 0;

  /** Expand this type constraint into all of the possible types that match
      the constraint. */
  virtual void expand(TypeExpansion & out) const = 0;

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
protected:
  TypeSetConstraint(TypeClass tcls) : TypeConstraint(tcls) {}

public:

  // Unify with the input pattern
  bool unifyWithPattern(BindingEnv &env, const Type * pattern) const {
    return false;
  }
  const Type * singularValue() const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool includes(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isSingular() const;
  bool isReferenceType() const;
  void format(FormatStream & out) const;
};

/// -------------------------------------------------------------------
/// A type constraint representing the result of a method, where we don't
/// know exactly which method will be chosen yet.
class ResultOfConstraint : public TypeSetConstraint {
public:
  ResultOfConstraint(CallExpr * call)
    : TypeSetConstraint(ResultOf)
    , callExpr(call)
  {}

  // Overrides

  void expand(TypeExpansion & out) const;
  bool unifyWithPattern(BindingEnv &env, const Type * pattern) const;
  void trace() const;
  void format(FormatStream & out) const;

private:
  CallExpr * callExpr;

  const Type * candidateResultType(const CallCandidate * cc) const;
};

/// -------------------------------------------------------------------
/// A type constraint representing a value that will be passed as an
/// argument to a method, where we don't know exactly which method
/// will be chosen yet.
class ParameterOfConstraint : public TypeSetConstraint {
public:
  ParameterOfConstraint(CallExpr * call, int index)
    : TypeSetConstraint(ParameterOf)
    , callExpr(call)
    , argIndex(index)
  {}

  // Overrides

  void expand(TypeExpansion & out) const;
  bool unifyWithPattern(BindingEnv &env, const Type * pattern) const;
  void trace() const;
  void format(FormatStream & out) const;

private:
  CallExpr * callExpr;
  int argIndex;
};


/// -------------------------------------------------------------------
/// A type constraint representing a type parameter to a type which
/// in turn may be a constraint. This version only handles types
/// which have a single type parameter - Address, NativeArray, etc.
class SingleTypeParamOfConstraint : public TypeSetConstraint {
public:
  SingleTypeParamOfConstraint(const TypeConstraint * base, Type::TypeClass cls)
    : TypeSetConstraint(SingleTypeParamOf)
    , base_(base)
    , cls_(cls)
  {}

  // Overrides

  bool unifyWithPattern(BindingEnv &env, const Type * pattern) const;
  void expand(TypeExpansion & out) const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const SingleTypeParamOfConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == SingleTypeParamOf;
  }

private:
  const TypeConstraint * base_;
  Type::TypeClass cls_;
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

  void expand(TypeExpansion & out) const;
  const Type * singularValue() const;
  bool unifyWithPattern(BindingEnv &env, const Type * pattern) const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool includes(const Type * other) const;
  bool isSubtype(const Type * other) const;
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
} // namespace tart

#endif

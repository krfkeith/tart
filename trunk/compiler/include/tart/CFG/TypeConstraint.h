/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TYPECONSTRAINT_H
#define TART_CFG_TYPECONSTRAINT_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

namespace tart {

// Forward declarations
class CallExpr;
class BindingEnv;
class CallCandidate;

/// -------------------------------------------------------------------
/// Abstract base class for type constraints. A type constraint represents
/// some subset of possible types.
class TypeConstraint : public Type {
protected:
  TypeConstraint(TypeClass tcls) : Type(tcls) {}

public:

  // Unify with the input pattern
  virtual bool unifyWithPattern(BindingEnv &env, const Type * pattern) const = 0;
  virtual const Type * singularValue() const = 0;

  // Overrides

  const llvm::Type * irType() const;
  TypeShape typeShape() const { return Shape_Unset; }

  static inline bool classof(const TypeConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() >= ResultOf;
  }
};

/// -------------------------------------------------------------------
/// A type constraint representing the result of a method, where we don't
/// know exactly which method will be chosen yet.
class ResultOfConstraint : public TypeConstraint {
public:
  ResultOfConstraint(CallExpr * call)
    : TypeConstraint(ResultOf)
    , callExpr(call)
  {}

  // Overrides

  const Type * singularValue() const;
  bool unifyWithPattern(BindingEnv &env, const Type * pattern) const;
  ConversionRank convertTo(const Type * toType, const Conversion & cn) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool includes(const Type * other) const;
  bool isSubtype(const Type * other) const;
  void trace() const;
  void format(FormatStream & out) const;
  bool isSingular() const;
  bool isReferenceType() const;

private:
  CallExpr * callExpr;

  const Type * candidateResultType(const CallCandidate * cc) const;
};

/// -------------------------------------------------------------------
/// A type constraint representing a value that will be passed as an
/// argument to a method, where we don't know exactly which method
/// will be chosen yet.
class ParameterOfConstraint : public TypeConstraint {
public:
  ParameterOfConstraint(CallExpr * call, int index)
    : TypeConstraint(ParameterOf)
    , callExpr(call)
    , argIndex(index)
  {}

  // Overrides

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

private:
  CallExpr * callExpr;
  int argIndex;
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

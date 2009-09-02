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
  TypeConstraint() : Type(Constraint) {}

public:

  // Unify with the input pattern
  virtual bool unifyWithPattern(BindingEnv &env, Type * pattern) = 0;

  // Overrides

  const llvm::Type * irType() const;
  static inline bool classof(const TypeConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == Constraint;
  }
};

/// -------------------------------------------------------------------
/// A type constraint representing the result of a method, where we don't
/// know exactly which method will be chosen yet.
class ResultOfConstraint : public TypeConstraint {
private:
  CallExpr * callExpr;
public:
  ResultOfConstraint(CallExpr * call)
    : callExpr(call)
  {}

  // Overrides

  bool unifyWithPattern(BindingEnv &env, Type * pattern);
  ConversionRank convertTo(const Type * toType) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool includes(const Type * other) const;
  bool isSubtype(const Type * other) const;
  void trace() const;
  void format(FormatStream & out) const;
  bool isSingular() const;
  bool isReferenceType() const;
};

/// -------------------------------------------------------------------
/// A type constraint representing a value that will be passed as an
/// argument to a method, where we don't know exactly which method
/// will be chosen yet.
class ParameterOfConstraint : public TypeConstraint {
private:
  CallExpr * callExpr;
  int argIndex;
public:
  ParameterOfConstraint(CallExpr * call, int index)
    : callExpr(call)
    , argIndex(index)
  {}

  // Overrides

  bool unifyWithPattern(BindingEnv &env, Type * pattern);
  ConversionRank convertTo(const Type * toType) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool includes(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isSingular() const;
  bool isReferenceType() const;
  void trace() const;
  void format(FormatStream & out) const;
};

} // namespace tart

#endif

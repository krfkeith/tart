/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_BINDINGENV_H
#define TART_SEMA_BINDINGENV_H

#ifndef TART_AST_ASTDECL_H
#include "tart/AST/ASTDecl.h"
#endif

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#ifndef TART_TYPE_TYPECONSTRAINT_H
#include "tart/Type/TypeConstraint.h"
#endif

namespace tart {

class TemplateSignature;
class TemplateCondition;
class TypeVariable;
class AddressType;
class TypeLiteralType;
class NativeArrayType;
class FlexibleArrayType;
class UnionType;

typedef llvm::SmallVector<TemplateCondition *, 2> TemplateConditionList;

// -------------------------------------------------------------------
// A Substitution is a proposal that two type expressions are equivalent
// within a given context.
class Substitution : public GC {
public:
  Substitution(const Type * left, const Type * right, Substitution * prev = NULL)
    : left_(left)
    , right_(right)
    , lowerBound_(right)
    , upperBound_(right)
    , prev_(prev)
  {
  }

  Substitution(const Type * left, const Type * upper, const Type * lower,
      Substitution * prev = NULL)
    : left_(left)
    , right_(lower)
    , lowerBound_(lower)
    , upperBound_(upper)
    , prev_(prev)
  {
  }

  /** The left side of the substitution. */
  const Type * left() const { return left_; }
  void setLeft(const Type * value) { left_ = value; }

  /** The right side of the substitution. */
  const Type * right() const { return right_; }
  void setRight(Type * value) { right_ = value; }

  /** The upper bound of the right side. */
  const Type * upperBound() const { return upperBound_; }
  void setUpperBound(Type * value) { upperBound_ = value; }

  /** The lower bound of the right side. */
  const Type * lowerBound() const { return lowerBound_; }
  void setLowerBound(Type * value) { lowerBound_ = value; }

  /** Previous substitution in the environment. */
  Substitution * prev() const { return prev_; }

  // Overrides

  void trace() const;

private:
  const Type * left_;
  const Type * right_;
  const Type * lowerBound_;
  const Type * upperBound_;

  Substitution * prev_;
};

/// -------------------------------------------------------------------
/// A TypeBinding represents a (environment, typevar) pair, which when
/// dereferenced causes a lookup of that typevar within that environment.
class TypeBinding : public Type {
public:
  TypeBinding(BindingEnv * env, const TypeVariable * var)
    : Type(Binding)
    , env_(env)
    , var_(var)
  {}

  const BindingEnv * env() const { return env_; }
  const TypeVariable * var() const { return var_; }

  // The value that is currently bound to var in env. Can return null if there's no value
  // bound to var.
  Type * value() const;

  // Overrides

  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const;
  TypeShape typeShape() const {
    Type * ty = value();
    return ty != NULL ? ty->typeShape() : Shape_Unset;
  }
  bool includes(const Type * other) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  Expr * nullInitValue() const;
  void trace() const;
  void format(FormatStream & out) const;
  const llvm::Type * irType() const;

  static inline bool classof(const TypeBinding *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == Binding;
  }

private:
  BindingEnv * env_;
  const TypeVariable * var_;
};

/// -------------------------------------------------------------------
/// Performs unification between types and produces a set of type
/// bindings.
class BindingEnv {
public:
  BindingEnv() : substitutions_(NULL) { index_ = nextIndex_++; }
  BindingEnv(const BindingEnv & env) : substitutions_(env.substitutions()) { index_ = nextIndex_++; }

  /** Return true if there are no variable bindings. */
  bool empty() const { return substitutions_ == NULL; }

  /** Reset all bindings. */
  void reset();

  /** Perform unification from a pattern type to a value type. */
  bool unify(SourceContext * source, const Type * pattern, const Type * value, Variance variance);

  /** Get the value for the specified type variable. */
  Type * get(const TypeVariable * type) const;

  /** Get the value for the specified type binding. */
  Type * get(const TypeBinding * type) const;

  /** Get the value for the specified type variable. If the binding is to another
      variable, dereference that as well. */
  Type * dereference(Type * type) const;

  /** Given a type expression, return the equivalent expression where all
      type variables have been replaced with the corresponding type
      bindings for this environment.

      This function attempts to avoid creating new type objects when
      the input expression contains no type variables.
   */
  const Type * subst(const Type * in) const;

  /** Return a list of substitutions for this environment. */
  Substitution * substitutions() const {
    return substitutions_;
  }

  /** Set the list of substitutions for this environment. */
  void setSubstitutions(Substitution * s) {
    substitutions_ = s;
  }

  /** Add a new substitution into this environment. */
  Substitution * addSubstitution(const Type * left, const Type * right);

  /** Add a new substitution into this environment (upper and lower bounds). */
  Substitution * addSubstitution(const Type * left, const Type * upper, const Type * lower);

  /** Given the left-hand side of a substitition, return the substitution. */
  Substitution * getSubstitutionFor(const Type * left) const {
    for (Substitution * s = substitutions_; s != NULL; s = s->prev()) {
      if (s->left() == left) {
        return s;
      }
    }

    return NULL;
  }

  // Used for displaying in debugger only, return value is ephemeral.
  const char * str() const;

    // Overrides

  void trace() const;

  int index_;
private:
  friend FormatStream & operator<<(FormatStream & out, const BindingEnv & env);

  Substitution * substitutions_;
  static int nextIndex_;

  bool unifyPattern(SourceContext * source, const TypeVariable * pattern, const Type * value,
      Variance variance);
  bool unifyAddressType(SourceContext * source, const AddressType * pattern, const Type * value);
  bool unifyNativeArrayType(SourceContext * source, const NativeArrayType * pattern,
      const Type * value);
  bool unifyFlexibleArrayType(SourceContext * source, const FlexibleArrayType * pattern,
      const Type * value);
  bool unifyTypeLiteralType(SourceContext * source, const TypeLiteralType * pattern,
      const Type * value);
  bool unifyCompositeType(SourceContext * source, const CompositeType * pattern,
      const CompositeType * value, Variance variance);
  bool unifyUnionType(SourceContext * source, const UnionType * pattern,
      const UnionType * value, Variance variance);
  bool unifyToUnionType(SourceContext * source, const Type * pattern, const UnionType * value,
      Variance variance);
  bool unifyImpl(SourceContext * source, const Type * pattern, const Type * value,
      Variance variance);
  bool unifyWithBoundValue(
      SourceContext * source, const Type * prevValue, const Type * newValue, Variance variance);

  /** Given two types, return the one that is more general, the higher of the two. Returns NULL
      if neither type is a specialization of the other. */
  const Type * selectLessSpecificType(SourceContext * source, const Type * type1,
      const Type * type2);

  bool hasVar(const TypeVariable * var) const;
};

FormatStream & operator<<(FormatStream & out, const BindingEnv & env);

} // namespace tart

#endif

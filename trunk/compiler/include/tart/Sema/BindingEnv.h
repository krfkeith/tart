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

#ifndef TART_CFG_TYPECONSTRAINT_H
#include "tart/CFG/TypeConstraint.h"
#endif

#include <llvm/ADT/DenseMap.h>

namespace tart {

class TemplateSignature;
class PatternVar;
class AddressType;
class PointerType;
class NativeArrayType;

// -------------------------------------------------------------------
// A Subsitution is a proposal that two type expressions are equivalent
// within a given context.
class Substitution : public GC {
public:
  Substitution(const Type * left, Type * right, Substitution * prev = NULL)
    : left_(left)
    , right_(right)
    , upperBound_(right)
    , lowerBound_(right)
    , prev_(prev)
  {
  }

  Substitution(const Type * left, Type * upper, Type * lower, Substitution * prev = NULL)
    : left_(left)
    , right_(lower)
    , upperBound_(upper)
    , lowerBound_(lower)
    , prev_(prev)
  {
  }

  /** The left side of the substitution. */
  const Type * left() const { return left_; }
  void setLeft(const Type * value) { left_ = value; }

  /** The right side of the substitution. */
  Type * right() const { return right_; }
  void setRight(Type * value) { right_ = value; }

  /** The upper bound of the right side. */
  Type * upperBound() const { return upperBound_; }
  void setUpperBound(Type * value) { upperBound_ = value; }

  /** The lower bound of the right side. */
  Type * lowerBound() const { return lowerBound_; }
  void setLowerBound(Type * value) { lowerBound_ = value; }

  /** Previous substitution in the environment. */
  Substitution * prev() const { return prev_; }

  // Overrides

  void trace() const;

private:
  const Type * left_;
  Type * right_;
  Type * lowerBound_;
  Type * upperBound_;

  Substitution * prev_;
};

/// -------------------------------------------------------------------
/// A PatternValue represents the value of a PatternVariable within a
/// specific environment.
class PatternValue : public Type {
public:
  PatternValue(BindingEnv * env, PatternVar * var)
    : Type(PatternVal)
    , env_(env)
    , var_(var)
  {}

  const BindingEnv * env() const { return env_; }
  PatternVar * var() const { return var_; }

  // The value that is currently bound to var in env. Can return null if there's no value
  // bound to var.
  Type * value() const;

  // Overrides

  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const;
  bool includes(const Type * other) const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  Expr * nullInitValue() const;
  void trace() const;
  void format(FormatStream & out) const;
  const llvm::Type * irType() const;

  static inline bool classof(const PatternValue *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == PatternVal;
  }

private:
  BindingEnv * env_;
  PatternVar * var_;
};

/// -------------------------------------------------------------------
/// Performs unification between types and produces a set of type
/// bindings.
class BindingEnv {
public:
  BindingEnv() : substitutions_(NULL) {}
  BindingEnv(const BindingEnv & env) : substitutions_(env.substitutions()) {}

  /** Return true if there are no variable bindings. */
  bool empty() const { return substitutions_ == NULL; }

  /** Reset all bindings. */
  void reset();

  /** If the given variable does not already have a definition in this environment,
      then bind it to a PatternValue instance
   */
  void defineVar(PatternVar * var);

  /** Perform unification from a pattern type to a value type. */
  bool unify(SourceContext * source, const TypeRef & pattern, const TypeRef & value,
      Variance variance);
  bool unify(SourceContext * source, Type * pattern, Type * value, Variance variance);

  /** Get the value for the specified pattern variable. */
  Type * get(const PatternVar * type) const;

  /** Get the value for the specified pattern variable. If the binding is to another
      variable, dereference that as well. */
  Type * dereference(Type * type) const;

  /** Given a type expression, return the equivalent expression where all
      pattern variables have been replaced with the corresponding type
      bindings for this environment.

      This function attempts to avoid creating new type objects when
      the input expression contains no type variables.
   */
  Type * subst(Type * in) const;
  TypeRef subst(const TypeRef & in) const;

  /** Given a type expression, replace all pattern variables with pattern values that
      are local to this specific environment. */
  //Type * relabel(Type * in);
  //TypeRef relabel(const TypeRef & in);
  TypeVector * relabel(TypeVector * in);

    /** Return a list of substitutions for this environment. */
  Substitution * substitutions() const {
    return substitutions_;
  }

  /** Set the list of substitutions for this environment. */
  void setSubstitutions(Substitution * s) {
    substitutions_ = s;
  }

  /** Add a new substitution into this environment. */
  Substitution * addSubstitution(const Type * left, Type * right);

  /** Add a new substitution into this environment (upper and lower bounds). */
  Substitution * addSubstitution(const Type * left, Type * upper, Type * lower);

  /** Given the left-hand side of a substutition, return the substitution. */
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
private:
  friend FormatStream & operator<<(FormatStream & out, const BindingEnv & env);

  Substitution * substitutions_;

  bool unifyPattern(SourceContext * source, PatternVar * pattern, Type * value, Variance variance);
  bool unifyAddressType(SourceContext * source, AddressType * pattern, Type * value);
  bool unifyPointerType(SourceContext * source, PointerType * pattern, Type * value);
  bool unifyNativeArrayType(SourceContext * source, NativeArrayType * pattern, Type * value);
  bool unifyCompositeType(SourceContext * source, CompositeType * pattern, CompositeType * value,
      Variance variance);
  bool unifyImpl(SourceContext * source, Type * pattern, Type * value, Variance variance);

  bool hasVar(const PatternVar * var) const;
};

FormatStream & operator<<(FormatStream & out, const BindingEnv & env);

} // namespace tart

#endif

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

#ifndef TART_DEFN_TEMPLATE_H
#include "tart/Defn/Template.h"
#endif

#ifndef TART_SEMA_INFER_CONSTRAINTSET_H
#include "tart/Sema/Infer/ConstraintSet.h"
#endif

#ifndef LLVM_ADT_DENSEMAP_H
#include "llvm/ADT/DenseMap.h"
#endif

#ifndef LLVM_ADT_SMALLPTRSET_H
#include "llvm/ADT/SmallPtrSet.h"
#endif

namespace tart {

class TypeVariable;
class TypeFunction;
class TypeFunctionCall;
class TypeAssignment;
class AmbiguousParameterType;
class AmbiguousResultType;
class AmbiguousTypeParamType;
class AddressType;
class TypeLiteralType;
class NativeArrayType;
class FlexibleArrayType;
class UnionType;

/// -------------------------------------------------------------------
/// Performs unification between types and produces a set of type
/// bindings.
class BindingEnv {
public:
  BindingEnv() : assignments_(NULL), stateCount_(0) {}
  BindingEnv(const BindingEnv & env)
    : assignments_(env.assignments())
    , stateCount_(env.stateCount_)
  {}

  /** Return true if there are no variable bindings. */
  bool empty() const { return assignments_ == NULL; }

  /** Reset all type assignments. */
  void reset();

  /** Perform unification from a pattern type to a value type. */
  bool unify(SourceContext * source, QualifiedType left, QualifiedType right,
      Constraint::Kind kind, const ProvisionSet & provisions = ProvisionSet());

  /** Get the type assignment for the specified type variable. */
  const TypeAssignment * getAssignment(const TypeVariable * var, const GC * context) const;

  /** Return a list of type assignments in this environment. */
  TypeAssignment * assignments() const {
    return assignments_;
  }

  /** Convert the current type assignments to a type variable map, only using type
      assignments from the specified context. */
  void toTypeVarMap(QualifiedTypeVarMap & map, GC * context);

  /** Assign 'value' to the type variable 'var'. This will create a new type assignment
      in the environment. */
  TypeAssignment * assign(const TypeVariable * var, const Type * value, GC * context = NULL);

  /** Return a token that can be used to backtrack to the current state. */
  unsigned stateCount() const {
    return stateCount_;
  }

  /** Increment the current state counter by 1 and return it. */
  unsigned nextState() {
    return ++stateCount_;
  }

  /** Backtrack to a previous state. */
  void backtrack(unsigned state);

  /** Sort assignments by dependency - put assignments before other assignments that
      refer to them. */
  void sortAssignments();

  /** Update all type assignments to their current subsitutions, resolving conflicting
      constraints if possible. */
  bool updateAssignments(SourceLocation loc, GC * context = NULL);

  /** Attempt to find a solution that satisfies all current constraints on the type assignments. */
  bool reconcileConstraints(GC * context);

  // Used for displaying in debugger only, return value is ephemeral.
  //const char * str() const;
  void dumpAssignment(TypeAssignment * ta) const;
  void dumpProvisions(const ProvisionSet & provisions) const;
  void dump() const;

    // Overrides

  void trace() const;

private:
  friend FormatStream & operator<<(FormatStream & out, const BindingEnv & env);
  friend class TypeAssignment;

  TypeAssignment * assignments_;
  unsigned stateCount_;

  bool unifyImpl(SourceContext * source, QualifiedType left, QualifiedType right,
      Constraint::Kind kind, const ProvisionSet & provisions);
  bool unifyWithTypeVar(SourceContext * source, Qualified<TypeAssignment> ta, QualifiedType value,
      Constraint::Kind kind, const ProvisionSet & provisions);
  bool unifyWithTypeFunctionCall(SourceContext * source, Qualified<TypeFunctionCall> ta,
      QualifiedType value, Constraint::Kind kind, const ProvisionSet & provisions);
  bool unifyWithAmbiguousType(SourceContext * source, QualifiedType amb,
      QualifiedType value, Constraint::Kind kind, const ProvisionSet & provisions);
  bool unifyAddressType(SourceContext * source, const AddressType * left, const Type * right);
  bool unifyNativeArrayType(SourceContext * source, const NativeArrayType * left,
      const Type * right);
  bool unifyFlexibleArrayType(SourceContext * source, const FlexibleArrayType * left,
      const Type * right);
  bool unifyTypeLiteralType(SourceContext * source, const TypeLiteralType * left,
      const Type * right);
  bool unifyCompositeType(SourceContext * source, const CompositeType * left,
      const CompositeType * right);
  bool unifyUnionType(SourceContext * source, const UnionType * left, const UnionType * right);
  bool unifyUnionMemberType(SourceContext * source, const UnionType * left, QualifiedType right);
  bool unifyTupleType(SourceContext * source, const TupleType * left, const TupleType * right);

  /** Return true if 'ty' is a type variable, and this environment contains a type assignment
      for it. */
  bool isAssigned(QualifiedType ty) const;

  void cycleCheck(const Type * var, const Type * value);
};

FormatStream & operator<<(FormatStream & out, const BindingEnv & env);

} // namespace tart

#endif

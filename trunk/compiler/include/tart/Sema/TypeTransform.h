/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_TYPETRANSFORM_H
#define TART_SEMA_TYPETRANSFORM_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#ifndef TART_SEMA_BINDINGENV_H
#include "tart/Sema/BindingEnv.h"
#endif

namespace tart {

class EnumType;
class UnionType;
class UnitType;
class PatternVar;
class PatternValue;
class TypeConstraint;
class TypeAlias;

/// -------------------------------------------------------------------
/// A framework for general transformations on type expressions.
class TypeTransform {
public:
  TypeRef transform(const TypeRef & in) { return visit(in); }
  const Type * transform(const Type * in) { return visit(in); }

  TypeRef visit(const TypeRef & in);
  const Type * visit(const Type * in);
  virtual const Type * visitPrimitiveType(const PrimitiveType * in);
  virtual const Type * visitCompositeType(const CompositeType * in);
  virtual const Type * visitEnumType(const EnumType * in);
  virtual const Type * visitFunctionType(const FunctionType * in);
  virtual const Type * visitUnionType(const UnionType * in);
  virtual const Type * visitTupleType(const TupleType * in);
  virtual const Type * visitAddressType(const AddressType * in);
  virtual const Type * visitPointerType(const PointerType * in);
  virtual const Type * visitNativeArrayType(const NativeArrayType * in);
  virtual const Type * visitUnitType(const UnitType * in);
  virtual const Type * visitPatternVar(const PatternVar * in);
  virtual const Type * visitPatternValue(const PatternValue * in);
  virtual const Type * visitTypeConstraint(const TypeConstraint * in);

  virtual const Type * visitTypeAlias(const TypeAlias * in);
};

/// -------------------------------------------------------------------
/// A transform that does a substitution via an environment.
class SubstitutionTransform : public TypeTransform {
public:
  SubstitutionTransform(const BindingEnv & env) : env_(env) {}

  const Type * visitPatternVar(const PatternVar * in);
  const Type * visitPatternValue(const PatternValue * in);
  const Type * visitCompositeType(const CompositeType * in);
  const Type * visitTypeConstraint(const TypeConstraint * in);

protected:
  const BindingEnv & env_;
};

/// -------------------------------------------------------------------
/// A transform that relabels all pattern variables.
class RelabelTransform : public SubstitutionTransform {
public:
  RelabelTransform(const BindingEnv & env) : SubstitutionTransform(env) {}

  const Type * visitPatternVar(const PatternVar * in);

private:
  BindingEnv vars_;
};

} // namespace tart

#endif // TART_SEMA_TYPETRANSFORM

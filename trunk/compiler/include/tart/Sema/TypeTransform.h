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
  TypeVector * transform(TypeVector * in) { return visit(in); }
  TypeRef transform(const TypeRef & in) { return visit(in); }
  Type * transform(Type * in) { return visit(in); }

  TypeVector * visit(TypeVector * in);
  TypeRef visit(const TypeRef & in);
  Type * visit(Type * in);
  virtual Type * visitPrimitiveType(PrimitiveType * in);
  virtual Type * visitCompositeType(CompositeType * in);
  virtual Type * visitEnumType(EnumType * in);
  virtual Type * visitFunctionType(FunctionType * in);
  virtual Type * visitUnionType(UnionType * in);
  virtual Type * visitAddressType(AddressType * in);
  virtual Type * visitPointerType(PointerType * in);
  virtual Type * visitNativeArrayType(NativeArrayType * in);
  virtual Type * visitUnitType(UnitType * in);
  virtual Type * visitPatternVar(PatternVar * in);
  virtual Type * visitPatternValue(PatternValue * in);
  virtual Type * visitTypeConstraint(TypeConstraint * in);

  virtual Type * visitTypeAlias(TypeAlias * in);
};

/// -------------------------------------------------------------------
/// A transform that does a substitution via an environment.
class SubstitutionTransform : public TypeTransform {
public:
  SubstitutionTransform(const BindingEnv & env) : env_(env) {}

  Type * visitPatternVar(PatternVar * in);
  Type * visitPatternValue(PatternValue * in);
  Type * visitCompositeType(CompositeType * in);
  Type * visitTypeConstraint(TypeConstraint * in);

protected:
  const BindingEnv & env_;
};

/// -------------------------------------------------------------------
/// A transform that relabels all pattern variables.
class RelabelTransform : public SubstitutionTransform {
public:
  RelabelTransform(const BindingEnv & env) : SubstitutionTransform(env) {}

  Type * visitPatternVar(PatternVar * in);

private:
  BindingEnv vars_;
};

} // namespace tart

#endif // TART_SEMA_TYPETRANSFORM

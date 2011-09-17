/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_TYPETRANSFORM_H
#define TART_SEMA_TYPETRANSFORM_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_TYPE_TEMPLATE_H
#include "tart/Defn/Template.h"
#endif

namespace tart {

class EnumType;
class UnionType;
class UnitType;
class TypeVariable;
class TypeAssignment;
class TypeConstraint;
class TypeAlias;
class TypeLiteralType;
class AddressType;
class FlexibleArrayType;

typedef llvm::DenseMap<const TypeVariable *, const Type *> TypeVarMap;

/// -------------------------------------------------------------------
/// A framework for general transformations on type expressions.
class TypeTransform {
public:
  virtual ~TypeTransform() {}

  //const Type * transform(const Type * in) { return visit(in); }
  QualifiedType transform(QualifiedType in) { return visit(in); }

  const Type * operator()(const Type * in) { return visit(in).unqualified(); }
  QualifiedType operator()(QualifiedType in) { return visit(in); }

  //QualifiedType visit(const Type * in);
  QualifiedType visit(QualifiedType in);
//  {
//    return QualifiedType(visit(in.type()), in.qualifiers());
//  }

  virtual const Type * visitPrimitiveType(const PrimitiveType * in);
  virtual const Type * visitCompositeType(const CompositeType * in);
  virtual const Type * visitEnumType(const EnumType * in);
  virtual const Type * visitFunctionType(const FunctionType * in);
  virtual const Type * visitUnionType(const UnionType * in);
  virtual const TupleType * visitTupleType(const TupleType * in);
  virtual const Type * visitAddressType(const AddressType * in);
  virtual const Type * visitTypeLiteralType(const TypeLiteralType * in);
  virtual const Type * visitNativeArrayType(const NativeArrayType * in);
  virtual const Type * visitFlexibleArrayType(const FlexibleArrayType * in);
  virtual const Type * visitUnitType(const UnitType * in);
  virtual QualifiedType visitTypeVariable(const TypeVariable * in);
  virtual QualifiedType visitTypeAssignment(const TypeAssignment * in);
  virtual QualifiedType visitTypeConstraint(const TypeConstraint * in);

  virtual QualifiedType visitTypeAlias(const TypeAlias * in);
};

/// -------------------------------------------------------------------
/// A transform that does a substitution via an environment.
class SubstitutionTransform : public TypeTransform {
public:
  SubstitutionTransform(const QualifiedTypeVarMap & vars) : vars_(vars) {}

  const Type * visitCompositeType(const CompositeType * in);
  QualifiedType visitTypeVariable(const TypeVariable * in);
  QualifiedType visitTypeAssignment(const TypeAssignment * in);
  QualifiedType visitTypeConstraint(const TypeConstraint * in);

protected:
  const QualifiedTypeVarMap & vars_;
};

/// -------------------------------------------------------------------
/// A transform that relabels all pattern variables.
class RelabelTransform : public SubstitutionTransform {
public:
  RelabelTransform(const QualifiedTypeVarMap & vars) : SubstitutionTransform(vars) {}

  QualifiedType visitTypeVariable(const TypeVariable * in);
};

/// -------------------------------------------------------------------
/// A transform that replaces all type bindings with their bound values.
class NormalizeTransform : public SubstitutionTransform {
public:
  NormalizeTransform() : SubstitutionTransform(vars_) {}

  QualifiedType visitTypeAssignment(const TypeAssignment * in);

  const QualifiedTypeVarMap vars_;
};

/// -------------------------------------------------------------------
/// A transform that replaces all unsized integer types with
/// appropriate sized integer types.
class IntegerSizingTransform : public TypeTransform {
public:
  IntegerSizingTransform() {}

  const Type * visitPrimitiveType(const PrimitiveType * in);
};

} // namespace tart

#endif // TART_SEMA_TYPETRANSFORM

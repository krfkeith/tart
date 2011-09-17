/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"

#include "tart/Type/AmbiguousTypeParamType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/UnitType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/TypeAlias.h"
#include "tart/Type/TypeConstraint.h"
#include "tart/Type/TypeLiteral.h"

#include "tart/Sema/TypeTransform.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// TypeTransform

QualifiedType TypeTransform::visit(QualifiedType in) {
  if (isErrorResult(in)) {
    return in;
  }

  const Type * unqual = in.unqualified();
  unsigned qualifiers = in.qualifiers();
  if (in->typeClass() == Type::Alias) {
    return visit(static_cast<const TypeAlias *>(unqual)->value()) | qualifiers;
  }

  switch (in->typeClass()) {
    case Type::Primitive:
      return QualifiedType(
          visitPrimitiveType(static_cast<const PrimitiveType *>(unqual)),
          qualifiers);

    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol:
      return QualifiedType(
          visitCompositeType(static_cast<const CompositeType *>(unqual)),
          qualifiers);

    case Type::Enum:
      return QualifiedType(
          visitEnumType(static_cast<const EnumType *>(unqual)),
          qualifiers);

    case Type::Function:
      return QualifiedType(
          visitFunctionType(static_cast<const FunctionType *>(unqual)),
          qualifiers);

    case Type::Union:
      return QualifiedType(
          visitUnionType(static_cast<const UnionType *>(unqual)),
          qualifiers);

    case Type::Tuple:
      return QualifiedType(
          visitTupleType(static_cast<const TupleType *>(unqual)),
          qualifiers);

    case Type::NAddress:
      return QualifiedType(
          visitAddressType(static_cast<const AddressType *>(unqual)),
          qualifiers);

    case Type::NArray:
      return QualifiedType(
          visitNativeArrayType(static_cast<const NativeArrayType *>(unqual)),
          qualifiers);

    case Type::FlexibleArray:
      return QualifiedType(
          visitFlexibleArrayType(static_cast<const FlexibleArrayType *>(unqual)),
          qualifiers);

    case Type::TypeLiteral:
      return QualifiedType(
          visitTypeLiteralType(static_cast<const TypeLiteralType *>(unqual)),
          qualifiers);

    case Type::Unit:
      return QualifiedType(visitUnitType(static_cast<const UnitType *>(unqual)), qualifiers);

    case Type::Alias:
      return visitTypeAlias(static_cast<const TypeAlias *>(unqual)) | qualifiers;

    case Type::TypeVar:
      return visitTypeVariable(static_cast<const TypeVariable *>(unqual)) | qualifiers;

    case Type::Assignment:
      return visitTypeAssignment(static_cast<const TypeAssignment *>(unqual)) | qualifiers;

    case Type::AmbiguousResult:
    case Type::AmbiguousParameter:
    case Type::AmbiguousTypeParam:
      return visitTypeConstraint(static_cast<const TypeConstraint *>(unqual)) | qualifiers;

    default:
      diag.fatal() << "Type class not handled: " << in->typeClass();
      return NULL;
  }
}

const Type * TypeTransform::visitPrimitiveType(const PrimitiveType * in) {
  return in;
}

const Type * TypeTransform::visitCompositeType(const CompositeType * in) {
  return in;
}

const Type * TypeTransform::visitEnumType(const EnumType * in) {
  return in;
}

const Type * TypeTransform::visitFunctionType(const FunctionType * in) {
  return in;
}

const Type * TypeTransform::visitUnionType(const UnionType * in) {
  const TupleType * typeArgs = visitTupleType(in->typeArgs());
  if (typeArgs != in->typeArgs()) {
    return UnionType::get(typeArgs->members());
  }

  return in;
}

const TupleType * TypeTransform::visitTupleType(const TupleType * in) {
  QualifiedTypeList members;
  bool isSame = true;
  for (TupleType::const_iterator it = in->members().begin(); it != in->members().end(); ++it) {
    QualifiedType ref = visit(*it);
    members.push_back(ref);
    if (ref != *it) {
      isSame = false;
    }
  }

  if (isSame) {
    return in;
  }

  return TupleType::get(members);
}

const Type * TypeTransform::visitAddressType(const AddressType * in) {
  const AddressType * np = static_cast<const AddressType *>(in);
  if (np->typeParam(0).isNull()) {
    return in;
  }

  QualifiedType elemType = visit(np->typeParam(0));
  if (elemType == np->typeParam(0)) {
    return in;
  }

  return AddressType::get(elemType);
}

const Type * TypeTransform::visitNativeArrayType(const NativeArrayType * in) {
  const TupleType * typeArgs = visitTupleType(in->typeArgs());
  if (typeArgs != in->typeArgs()) {
    return NativeArrayType::get(typeArgs);
  }

  return in;
}

const Type * TypeTransform::visitFlexibleArrayType(const FlexibleArrayType * in) {
  const TupleType * typeArgs = visitTupleType(in->typeArgs());
  if (typeArgs != in->typeArgs()) {
    return FlexibleArrayType::get(typeArgs);
  }

  return in;
}

const Type * TypeTransform::visitTypeLiteralType(const TypeLiteralType * in) {
  if (in->typeParam(0).isNull()) {
    return in;
  }

  QualifiedType elemType = visit(in->typeParam(0));
  if (elemType == in->typeParam(0)) {
    return in;
  }

  return TypeLiteralType::get(elemType.type());
}

const Type * TypeTransform::visitUnitType(const UnitType * in) {
  return in;
}

QualifiedType TypeTransform::visitTypeVariable(const TypeVariable * in) {
  return in;
}

QualifiedType TypeTransform::visitTypeAssignment(const TypeAssignment * in) {
  return in;
}

QualifiedType TypeTransform::visitTypeConstraint(const TypeConstraint * in) {
  return in;
}

QualifiedType TypeTransform::visitTypeAlias(const TypeAlias * in) {
  // TODO: This strips type modifiers.
  return visit(static_cast<const TypeAlias *>(in)->value());
}

// -------------------------------------------------------------------
// SubstitutionTransform

QualifiedType SubstitutionTransform::visitTypeVariable(const TypeVariable * in) {
  QualifiedTypeVarMap::const_iterator it = vars_.find(in);
  if (it != vars_.end()) {
    return it->second.type();
  } else {
    return in;
  }
}

QualifiedType SubstitutionTransform::visitTypeAssignment(const TypeAssignment * in) {
  QualifiedType result = in->value();
  return result ? result : in;
}

const Type * SubstitutionTransform::visitCompositeType(const CompositeType * in) {
  if (in->typeDefn() == NULL) {
    return in;
  } else if (in->typeDefn()->isTemplate()) {
    Defn * def = in->typeDefn()->templateSignature()->instantiate(
        in->typeDefn()->location(), vars_);
    if (def != NULL) {
      return cast<TypeDefn>(def)->typePtr();
    } else {
      return NULL;
    }
  } else if (in->typeDefn()->isTemplateMember()) {
    if (const TypeDefn * td = dyn_cast_or_null<TypeDefn>(in->typeDefn()->parentDefn())) {
      // Transform the parent (recursively)
      if (const CompositeType * parent = dyn_cast<CompositeType>(td->typePtr())) {
        const CompositeType * transformedParent = cast<CompositeType>(visitCompositeType(parent));
        // Now look for a matching entry
        DefnList defns;
        AnalyzerBase::analyzeType(transformedParent, Task_PrepMemberLookup);
        transformedParent->memberScope()->lookupMember(in->typeDefn()->name(), defns, false);

        // Attempt to find the instantiated definition that corresponds with the original template
        // definition 'def'. This can be identified because it has the same value for 'ast'.
        for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
          if ((*it)->ast() == in->typeDefn()->ast()) {
            return cast<TypeDefn>(*it)->typePtr();
          }
        }
      }
    }

    DFAIL("Implement");
  } else if (in->typeDefn()->isPartialInstantiation()) {
    TemplateInstance * tinst = in->typeDefn()->templateInstance();
    if (tinst == NULL) {
      return in;
    }
    Template * tm = tinst->templateDefn()->templateSignature();
    QualifiedTypeVarMap varValues(vars_);
    // Add type param mappings.
    size_t numVars = tm->patternVarCount();
    for (size_t i = 0; i < numVars; ++i) {
      const TypeVariable * param = tm->patternVar(i);
      QualifiedType value = tinst->typeArg(i);
      QualifiedType svalue = visit(value);
      if (svalue) {
        varValues[param] = svalue;
      }
    }

    Defn * def = tm->instantiate(SourceLocation(), varValues);
    if (def != NULL) {
      //assureNoTypeVariables(cast<TypeDefn>(def)->typeValue());
      return cast<TypeDefn>(def)->typePtr();
    } else {
      return NULL;
    }
  }

  //assureNoTypeVariables(in);
  return in;
}

QualifiedType SubstitutionTransform::visitTypeConstraint(const TypeConstraint * in) {
  const TypeConstraint * constraint = static_cast<const TypeConstraint *>(in);
  if (constraint->isSingular()) {
    return visit(constraint->singularValue());
  }

  if (const AmbiguousTypeParamType * tpt = dyn_cast<AmbiguousTypeParamType>(in)) {
    QualifiedType base = visit(tpt->base());
    if (base == tpt->base()) {
      return tpt;
    }
    return AmbiguousTypeParamType::forType(base, tpt->match(), tpt->paramIndex());
  }

  diag.debug() << "Type constraint: " << in;
  DFAIL("Type constraint not handled");
  return in;
}

// -------------------------------------------------------------------
// RelabelTransform

QualifiedType RelabelTransform::visitTypeVariable(const TypeVariable * in) {
  QualifiedTypeVarMap::const_iterator it = vars_.find(in);
  DASSERT(it != vars_.end()) << "Type variable " << in << " not found!";
  return it->second.type();
}

// -------------------------------------------------------------------
// NormalizeTransform

QualifiedType NormalizeTransform::visitTypeAssignment(const TypeAssignment * in) {
  QualifiedType value = in->value();
  return value ? value : in;
}

// -------------------------------------------------------------------
// IntegerSizingTransform

const Type * IntegerSizingTransform::visitPrimitiveType(const PrimitiveType * in) {
  if (in->isUnsizedIntType()) {
    const UnsizedIntType * unsized = static_cast<const UnsizedIntType *>(in);
    unsigned bitsRequired = std::max(
        static_cast<const PrimitiveType *>(PrimitiveType::intType())->numBits(),
        unsized->signedBitsRequired());
    if (bitsRequired <= 64 || unsized->isNegative()) {
      return PrimitiveType::fitIntegerType(bitsRequired, false);
    }
    return PrimitiveType::fitIntegerType(unsized->unsignedBitsRequired(), true);
  }
  return in;
}

} // namespace tart

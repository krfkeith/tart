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

const Type * TypeTransform::visit(const Type * in) {
  if (isErrorResult(in)) {
    return in;
  }

  if (in->typeClass() == Type::Alias) {
    return visit(static_cast<const TypeAlias *>(in)->value());
  }

  switch (in->typeClass()) {
    case Type::Primitive:
      return visitPrimitiveType(static_cast<const PrimitiveType *>(in));

    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol:
      return visitCompositeType(static_cast<const CompositeType *>(in));

    case Type::Enum:
      return visitEnumType(static_cast<const EnumType *>(in));

    case Type::Function:
      return visitFunctionType(static_cast<const FunctionType *>(in));

    case Type::Union:
      return visitUnionType(static_cast<const UnionType *>(in));

    case Type::Tuple:
      return visitTupleType(static_cast<const TupleType *>(in));

    case Type::NAddress:
      return visitAddressType(static_cast<const AddressType *>(in));

    case Type::NArray:
      return visitNativeArrayType(static_cast<const NativeArrayType *>(in));

    case Type::FlexibleArray:
      return visitFlexibleArrayType(static_cast<const FlexibleArrayType *>(in));

    case Type::TypeLiteral:
      return visitTypeLiteralType(static_cast<const TypeLiteralType *>(in));

    case Type::Unit:
      return visitUnitType(static_cast<const UnitType *>(in));

    case Type::Alias:
      return visitTypeAlias(static_cast<const TypeAlias *>(in));

    case Type::TypeVar:
      return visitTypeVariable(static_cast<const TypeVariable *>(in));

    case Type::Assignment:
      return visitTypeAssignment(static_cast<const TypeAssignment *>(in));

    case Type::AmbiguousResult:
    case Type::AmbiguousParameter:
    case Type::AmbiguousTypeParam:
      return visitTypeConstraint(static_cast<const TypeConstraint *>(in));

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
  const TupleType * members = cast<TupleType>(visit(&in->members()));
  if (members != &in->members()) {
    // TODO: Need to sort the members.
    return UnionType::get(members->members());
  }

  return in;
}

const Type * TypeTransform::visitTupleType(const TupleType * in) {
  TypeList members;
  bool isSame = true;
  for (TupleType::const_iterator it = in->members().begin(); it != in->members().end(); ++it) {
    const Type * ref = visit(*it);
    members.push_back(const_cast<Type *>(ref));
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
  if (np->typeParam(0) == NULL) {
    return in;
  }

  const Type * elemType = visit(np->typeParam(0));
  if (elemType == np->typeParam(0)) {
    return in;
  }

  return AddressType::get(elemType);
}

const Type * TypeTransform::visitNativeArrayType(const NativeArrayType * in) {
  const TupleType * typeArgs = cast<TupleType>(visit(in->typeArgs()));
  if (typeArgs != in->typeArgs()) {
    return NativeArrayType::get(typeArgs);
  }

  return in;
}

const Type * TypeTransform::visitFlexibleArrayType(const FlexibleArrayType * in) {
  const TupleType * typeArgs = cast<TupleType>(visit(in->typeArgs()));
  if (typeArgs != in->typeArgs()) {
    return FlexibleArrayType::get(typeArgs);
  }

  return in;
}

const Type * TypeTransform::visitTypeLiteralType(const TypeLiteralType * in) {
  if (in->typeParam(0) == NULL) {
    return in;
  }

  const Type * elemType = visit(in->typeParam(0));
  if (elemType == in->typeParam(0)) {
    return in;
  }

  return TypeLiteralType::get(elemType);
}

const Type * TypeTransform::visitUnitType(const UnitType * in) {
  return in;
}

const Type * TypeTransform::visitTypeVariable(const TypeVariable * in) {
  return in;
}

const Type * TypeTransform::visitTypeAssignment(const TypeAssignment * in) {
  return in;
}

const Type * TypeTransform::visitTypeConstraint(const TypeConstraint * in) {
  return in;
}

const Type * TypeTransform::visitTypeAlias(const TypeAlias * in) {
  // TODO: This strips type modifiers.
  return visit(static_cast<const TypeAlias *>(in)->value());
}

// -------------------------------------------------------------------
// SubstitutionTransform

const Type * SubstitutionTransform::visitTypeVariable(const TypeVariable * in) {
  TypeVarMap::const_iterator it = vars_.find(in);
  if (it != vars_.end()) {
    return it->second;
  } else {
    return in;
  }
}

const Type * SubstitutionTransform::visitTypeAssignment(const TypeAssignment * in) {
  const Type * result = in->value();
  return result != NULL ? result : in;
}

const Type * SubstitutionTransform::visitCompositeType(const CompositeType * in) {
  if (in->typeDefn() == NULL) {
    return in;
  } else if (in->typeDefn()->isTemplate()) {
    Defn * def = in->typeDefn()->templateSignature()->instantiate(
        in->typeDefn()->location(), vars_);
    if (def != NULL) {
      return cast<TypeDefn>(def)->typeValue();
    } else {
      return NULL;
    }
  } else if (in->typeDefn()->isTemplateMember()) {
    if (const TypeDefn * td = dyn_cast_or_null<TypeDefn>(in->typeDefn()->parentDefn())) {
      // Transform the parent (recursively)
      if (const CompositeType * parent = dyn_cast<CompositeType>(td->typeValue())) {
        const CompositeType * transformedParent = cast<CompositeType>(visitCompositeType(parent));
        // Now look for a matching entry
        DefnList defns;
        AnalyzerBase::analyzeType(transformedParent, Task_PrepMemberLookup);
        transformedParent->memberScope()->lookupMember(in->typeDefn()->name(), defns, false);

        // Attempt to find the instantiated definition that corresponds with the original template
        // definition 'def'. This can be identified because it has the same value for 'ast'.
        for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
          if ((*it)->ast() == in->typeDefn()->ast()) {
            return cast<TypeDefn>(*it)->typeValue();
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
    TypeVarMap varValues(vars_);
    // Add type param mappings.
    size_t numVars = tm->patternVarCount();
    for (size_t i = 0; i < numVars; ++i) {
      TypeVariable * param = tm->patternVar(i);
      const Type * value = tinst->typeArg(i);
      const Type * svalue = visit(value);
      if (svalue != NULL) {
        varValues[param] = svalue;
      }
    }

    Defn * def = tm->instantiate(SourceLocation(), varValues);
    if (def != NULL) {
      //assureNoTypeVariables(cast<TypeDefn>(def)->typeValue());
      return cast<TypeDefn>(def)->typeValue();
    } else {
      return NULL;
    }
  }

  //assureNoTypeVariables(in);
  return in;
}

const Type * SubstitutionTransform::visitTypeConstraint(const TypeConstraint * in) {
  const TypeConstraint * constraint = static_cast<const TypeConstraint *>(in);
  if (constraint->isSingular()) {
    return visit(constraint->singularValue());
  }

  if (const AmbiguousTypeParamType * tpt = dyn_cast<AmbiguousTypeParamType>(in)) {
    const Type * base = visit(tpt->base());
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

const Type * RelabelTransform::visitTypeVariable(const TypeVariable * in) {
  TypeVarMap::const_iterator it = vars_.find(in);
  DASSERT(it != vars_.end()) << "Type variable " << in << " not found!";
  return it->second;
}

// -------------------------------------------------------------------
// NormalizeTransform

const Type * NormalizeTransform::visitTypeAssignment(const TypeAssignment * in) {
  const Type * value = in->value();
  return value != NULL ? value : in;
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

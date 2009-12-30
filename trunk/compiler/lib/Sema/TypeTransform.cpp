/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/UnitType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/TypeAlias.h"
#include "tart/CFG/TypeConstraint.h"
#include "tart/CFG/TypeLiteral.h"
#include "tart/CFG/Template.h"

#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/TypeTransform.h"

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

    case Type::TypeLiteral:
      return visitTypeLiteralType(static_cast<const TypeLiteralType *>(in));

    case Type::Unit:
      return visitUnitType(static_cast<const UnitType *>(in));

    case Type::Alias:
      return visitTypeAlias(static_cast<const TypeAlias *>(in));

    case Type::Pattern:
      return visitTypeVariable(static_cast<const TypeVariable *>(in));

    case Type::PatternVal:
      return visitPatternValue(static_cast<const PatternValue *>(in));

    case Type::ResultOf:
    case Type::ParameterOf:
    case Type::TupleOf:
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
    return UnionType::get(in->location(), members->members());
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

const Type * TypeTransform::visitPatternValue(const PatternValue * in) {
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
  Type * value = env_.get(in);
  return value != NULL ? visit(value) : in;
}

const Type * SubstitutionTransform::visitPatternValue(const PatternValue * in) {
  Type * result = in->value();
  return result != NULL ? result : in;
}

const Type * SubstitutionTransform::visitCompositeType(const CompositeType * in) {
  if (in->typeDefn() == NULL) {
    return in;
  } else if (in->typeDefn()->isTemplate()) {
    Defn * def = in->typeDefn()->templateSignature()->instantiate(SourceLocation(), env_);
    if (def != NULL) {
      return cast<TypeDefn>(def)->typeValue();
    } else {
      return NULL;
    }
  } else if (in->typeDefn()->isTemplateMember()) {
    DFAIL("Implement");
  } else if (in->typeDefn()->isPartialInstantiation()) {
    TemplateInstance * tinst = in->typeDefn()->templateInstance();
    TemplateSignature * tsig = tinst->templateDefn()->templateSignature();
    BindingEnv partialEnv(env_);
    // Add type param mappings.
    size_t numVars = tsig->patternVarCount();
    for (size_t i = 0; i < numVars; ++i) {
      TypeVariable * param = tsig->patternVar(i);
      const Type * value = tinst->typeArg(i);
      const Type * svalue = visit(value);
      if (svalue != NULL) {
        partialEnv.addSubstitution(param, svalue);
      }
    }

    Defn * def = tsig->instantiate(SourceLocation(), partialEnv);
    if (def != NULL) {
      //assureNoTypeVariables(cast<TypeDefn>(def)->typeValue());
      return cast<TypeDefn>(def)->typeValue();
    } else {
      return NULL;
    }
  }

#if 0
  if (in->typeDefn() == NULL) {
    return in;
  } else if (in->typeDefn()->isTemplate()) {
    Defn * def = in->typeDefn()->templateSignature()->instantiate(SourceLocation(), *this);
    if (def != NULL) {
      assureNoTypeVariables(cast<TypeDefn>(def)->typeValue());
      return cast<TypeDefn>(def)->typeValue();
    } else {
      return NULL;
    }
  } else if (in->typeDefn()->isTemplateMember()) {
    DFAIL("Implement");
  } else if (in->typeDefn()->isPartialInstantiation()) {
  }

  return in;
#endif

  //assureNoTypeVariables(in);
  return in;
}

const Type * SubstitutionTransform::visitTypeConstraint(const TypeConstraint * in) {
  const TypeConstraint * constraint = static_cast<const TypeConstraint *>(in);
  if (constraint->isSingular()) {
    return constraint->singularValue();
  }

  DFAIL("Type constraint not handled");
  return in;
}

const Type * RelabelTransform::visitTypeVariable(const TypeVariable * in) {
  Type * value = env_.get(in);
  if (value != NULL) {
    return value;
  }

  value = vars_.get(in);
  if (value != NULL) {
    return value;
  }

  PatternValue * val = new PatternValue(&vars_, in);
  vars_.addSubstitution(in, val);
  return val;
}

} // namespace tart

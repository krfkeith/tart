/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/TypeAlias.h"
#include "tart/CFG/TypeConstraint.h"
#include "tart/CFG/Template.h"

#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/TypeTransform.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// TypeTransform

TypeVector * TypeTransform::visit(TypeVector * in) {
  bool isSame = true;
  TypeRefList typeRefs;
  for (TypeVector::iterator it = in->begin(); it != in->end(); ++it) {
    TypeRef ref = visit(*it);
    typeRefs.push_back(ref);
    if (ref != *it) {
      isSame = false;
    }
  }

  if (isSame) {
    return in;
  } else {
    return TypeVector::get(typeRefs);
  }
}

TypeRef TypeTransform::visit(const TypeRef & in) {
  TypeRef result(in);
  // TODO: Handle preservation of modifiers.
  if (in.type()->typeClass() == Type::Alias) {
    return visit(static_cast<TypeAlias *>(in.type())->value());
  }

  result.setType(transform(in.type()));
  return result;
}

Type * TypeTransform::visit(Type * in) {
  if (isErrorResult(in)) {
    return in;
  }

  switch (in->typeClass()) {
    case Type::Primitive:
      return visitPrimitiveType(static_cast<PrimitiveType *>(in));

    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol:
      return visitCompositeType(static_cast<CompositeType *>(in));

    case Type::Enum:
      return visitEnumType(static_cast<EnumType *>(in));

    case Type::Function:
      return visitFunctionType(static_cast<FunctionType *>(in));

    case Type::Union:
      return visitUnionType(static_cast<UnionType *>(in));

    case Type::NAddress:
      return visitAddressType(static_cast<AddressType *>(in));

    case Type::NPointer:
      return visitPointerType(static_cast<PointerType *>(in));

    case Type::NArray:
      return visitNativeArrayType(static_cast<NativeArrayType *>(in));

    case Type::SingleValue:
      return visitSingleValueType(static_cast<SingleValueType *>(in));

    case Type::Alias:
      return visitTypeAlias(static_cast<TypeAlias *>(in));

    case Type::Pattern:
      return visitPatternVar(static_cast<PatternVar *>(in));

    case Type::PatternVal:
      return visitPatternValue(static_cast<PatternValue *>(in));

    case Type::Constraint:
      return visitTypeConstraint(static_cast<TypeConstraint *>(in));

    default:
      diag.fatal() << "Type class not handled: " << in->typeClass();
      return NULL;
  }
}

Type * TypeTransform::visitPrimitiveType(PrimitiveType * in) {
  return in;
}

Type * TypeTransform::visitCompositeType(CompositeType * in) {
  return in;
}

Type * TypeTransform::visitEnumType(EnumType * in) {
  return in;
}

Type * TypeTransform::visitFunctionType(FunctionType * in) {
  return in;
}

Type * TypeTransform::visitUnionType(UnionType * in) {
  TypeRefList members;
  for (TypeVector::iterator it = in->members().begin(); it != in->members().end(); ++it) {
    members.push_back(visit(*it));
  }

  // TODO: Need to sort the members.
  TypeVector * tv = TypeVector::get(members);
  if (tv != &in->members()) {
    return UnionType::create(in->location(), members);
  }

  return in;
}

Type * TypeTransform::visitAddressType(AddressType * in) {
  const AddressType * np = static_cast<const AddressType *>(in);
  if (!np->typeParam(0).isDefined()) {
    return in;
  }

  TypeRef elemType = visit(np->typeParam(0));
  if (elemType == np->typeParam(0)) {
    return in;
  }

  return AddressType::get(elemType);
}

Type * TypeTransform::visitPointerType(PointerType * in) {
  if (!in->typeParam(0).isDefined()) {
    return in;
  }

  TypeRef elemType = visit(in->typeParam(0));
  if (elemType == in->typeParam(0)) {
    return in;
  }

  return PointerType::get(elemType);
}

Type * TypeTransform::visitNativeArrayType(NativeArrayType * in) {
  if (!in->typeParam(0).isDefined()) {
    return in;
  }

  TypeRef elemType = visit(in->typeParam(0));
  if (elemType == in->typeParam(0)) {
    return in;
  }

  return NativeArrayType::get(elemType, in->size());
}

Type * TypeTransform::visitSingleValueType(SingleValueType * in) {
  return in;
}

Type * TypeTransform::visitPatternVar(PatternVar * in) {
  return in;
}

Type * TypeTransform::visitPatternValue(PatternValue * in) {
  return in;
}

Type * TypeTransform::visitTypeConstraint(TypeConstraint * in) {
  return in;
}

Type * TypeTransform::visitTypeAlias(TypeAlias * in) {
  // TODO: This strips type modifiers.
  return visit(static_cast<TypeAlias *>(in)->value().type());
}

// -------------------------------------------------------------------
// SubstitutionTransform

Type * SubstitutionTransform::visitPatternVar(PatternVar * in) {
  Type * value = env_.get(in);
  return value != NULL ? visit(value) : in;
}

Type * SubstitutionTransform::visitPatternValue(PatternValue * in) {
  Type * result = in->value();
  return result != NULL ? result : in;
}

Type * SubstitutionTransform::visitCompositeType(CompositeType * in) {
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
      PatternVar * param = tsig->patternVar(i);
      Type * value = tinst->paramValues()[i];
      Type * svalue = visit(value);
      if (svalue != NULL) {
        partialEnv.addSubstitution(param, svalue);
      }
    }

    Defn * def = tsig->instantiate(SourceLocation(), partialEnv);
    if (def != NULL) {
      //assureNoPatternVars(cast<TypeDefn>(def)->typeValue());
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
      assureNoPatternVars(cast<TypeDefn>(def)->typeValue());
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

  //assureNoPatternVars(in);
  return in;
}

Type * SubstitutionTransform::visitTypeConstraint(TypeConstraint * in) {
  TypeConstraint * constraint = static_cast<TypeConstraint *>(in);
  if (constraint->isSingular()) {
    return constraint->singularValue().type();
  }

  DFAIL("Type constraint not handled");
  return in;
}

Type * RelabelTransform::visitPatternVar(PatternVar * in) {
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

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Template.h"

#include "tart/Type/Type.h"
#include "tart/Type/TypeRelation.h"
#include "tart/Type/TypeAlias.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/UnitType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Type/AmbiguousParameterType.h"
#include "tart/Type/AmbiguousResultType.h"
#include "tart/Type/AmbiguousTypeParamType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Sema/Infer/TypeAssignment.h"
#include "tart/Sema/CallCandidate.h"

namespace tart {

static bool isEqualTuple(const TupleType * ltt, const TupleType * rtt) {
  if (ltt == rtt) {
    return true;
  }
  if (ltt->size() == rtt->size()) {
    size_t size = ltt->size();
    for (size_t i = 0; i < size; ++i) {
      if (!TypeRelation::isEqual(ltt->member(i), rtt->member(i))) {
        return false;
      }
      return true;
    }
  }
  return false;
}

static bool isEqualComposite(const CompositeType * lct, const CompositeType * rct) {
  // Check if both classes derive from the same AST.
  TypeDefn * ldef = lct->typeDefn();
  TypeDefn * rdef = rct->typeDefn();
  if (ldef != NULL &&
      rdef != NULL &&
      ldef->ast() != NULL &&
      ldef->ast() == rdef->ast()) {
    // Now just need to test if the type params are the same.
    while (ldef != NULL && rdef != NULL) {
      // They should both be instances, or neither.
      DASSERT(ldef->isTemplateInstance() == rdef->isTemplateInstance());
      if (ldef->isTemplateInstance()) {
        // Compare type parameters for equivalence.
        return isEqualTuple(
            ldef->templateInstance()->typeArgs(),
            rdef->templateInstance()->typeArgs());
      } else if (ldef->isTemplate() && rdef->isTemplate()) {
        DFAIL("Implement");
      }

      // It's possible that both sides are template instance members. Try again at the
      // enclosing level.
      ldef = dyn_cast<TypeDefn>(ldef->parentDefn());
      rdef = dyn_cast<TypeDefn>(rdef->parentDefn());
    }
  }
  return false;
}

static bool isEqualFunction(const FunctionType * lfn, const FunctionType * rfn) {
  if (lfn->params().size() != rfn->params().size() ||
      lfn->isStatic() != rfn->isStatic()) {
    return false;
  }

  // Note that selfParam types are not compared. I *think* that's right, but
  // I'm not sure.

  // Also note that we aren't comparing type parameters, just function parameters.
  // Again, I *think* that's right. Having two functions with identical names & signatures
  // but having different type params would be ...weird.

  DASSERT(lfn->returnType() != NULL);
  DASSERT(rfn->returnType() != NULL);
  if (!TypeRelation::isEqual(lfn->returnType(), rfn->returnType())) {
    return false;
  }

  size_t numParams = lfn->params().size();
  for (size_t i = 0; i < numParams; ++i) {
    if (!TypeRelation::isEqual(lfn->param(i)->type(), rfn->param(i)->type())) {
      return false;
    }
    if (lfn->param(i)->isVariadic() != rfn->param(i)->isVariadic()) {
      return false;
    }
  }

  return true;
}

static bool isEqualUnion(const UnionType * lut, const UnionType * rut) {
  return isEqualTuple(lut->typeArgs(), rut->typeArgs());
}

bool TypeRelation::isEqual(const Type * lt, const Type * rt) {
  // Early out
  if (lt == rt) {
    return true;
  }

  switch (rt->typeClass()) {
    case Type::Alias:
      return isEqual(lt, static_cast<const TypeAlias *>(rt)->value());

    case Type::AmbiguousParameter:
    case Type::AmbiguousPhi:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam: {
      TypeExpansion expansion;
      rt->expand(expansion);
      if (expansion.empty()) {
        return false;
      }
      for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
        if (!isEqual(lt, *it)) {
          return false;
        }
      }
      return true;
    }

    case Type::Assignment: {
      const TypeAssignment * ta = static_cast<const TypeAssignment *>(rt);
      if (ta->value() != NULL) {
        return isEqual(lt, ta->value());
      } else {
        // For now, we presume that if all constraints on the type find lt acceptable,
        // then it's *possible* that the TA could equal lt.
        return ta->constraints().accepts(lt);
      }
    }

    default:
      break;
  }

  switch (lt->typeClass()) {
    case Type::Alias:
      return isEqual(static_cast<const TypeAlias *>(lt)->value(), rt);

    case Type::Primitive:
    case Type::Enum:
    case Type::TypeVar:
      // These types are unique by reference
      return false;

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      const CompositeType * lct = static_cast<const CompositeType *>(lt);
      if (const CompositeType * rct = dyn_cast<CompositeType>(rt)) {
        return isEqualComposite(lct, rct);
      }

      return false;
    }

    case Type::NAddress: {
      const AddressType * lat = static_cast<const AddressType *>(lt);
      if (const AddressType * rat = dyn_cast<AddressType>(rt)) {
        return isEqual(lat->typeParam(0), rat->typeParam(0));
      }
      return false;
    }

    case Type::NArray: {
      const NativeArrayType * lnat = static_cast<const NativeArrayType *>(lt);
      if (const NativeArrayType * rnat = dyn_cast<NativeArrayType>(rt)) {
        return isEqual(lnat->typeParam(0), rnat->typeParam(0)) && lnat->size() == rnat->size();
      }
      return false;
    }

    case Type::FlexibleArray: {
      const FlexibleArrayType * lfat = static_cast<const FlexibleArrayType *>(lt);
      if (const FlexibleArrayType * rfat = dyn_cast<FlexibleArrayType>(rt)) {
        return isEqual(lfat->typeParam(0), rfat->typeParam(0));
      }
      return false;
    }

    case Type::Function: {
      const FunctionType * lfn = static_cast<const FunctionType *>(lt);
      if (const FunctionType * rfn = dyn_cast<FunctionType>(rt)) {
        return isEqualFunction(lfn, rfn);
      }
      return false;
    }

    case Type::Unit: {
      const UnitType * lu = static_cast<const UnitType *>(lt);
      if (const UnitType * ru = dyn_cast<UnitType>(rt)) {
        return lu->value()->isEqual(ru->value());
      }
      return false;
    }

    case Type::Tuple: {
      const TupleType * ltt = static_cast<const TupleType *>(lt);
      if (const TupleType * rtt = dyn_cast<TupleType>(rt)) {
        return isEqualTuple(ltt, rtt);
      }
      return false;
    }

    case Type::Union: {
      const UnionType * lut = static_cast<const UnionType *>(lt);
      if (const UnionType * rut = dyn_cast<UnionType>(rt)) {
        return isEqualUnion(lut, rut);
      }
      return false;
    }

    case Type::TypeLiteral: {
      const TypeLiteralType * ltl = static_cast<const TypeLiteralType *>(lt);
      if (const TypeLiteralType * rtl = dyn_cast<TypeLiteralType>(rt)) {
        return isEqual(ltl->literalType(), rtl->literalType());
      }
      return false;
    }

    case Type::AmbiguousParameter:
    case Type::AmbiguousPhi:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam: {
      TypeExpansion expansion;
      lt->expand(expansion);
      if (expansion.empty()) {
        return false;
      }
      for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
        if (!isEqual(*it, rt)) {
          return false;
        }
      }
      return true;
    }

    case Type::SizingOf: {
      const SizingOfConstraint * lsoc = static_cast<const SizingOfConstraint *>(lt);
      if (const SizingOfConstraint * rsoc = dyn_cast_or_null<SizingOfConstraint>(rt)) {
        return lsoc->isNegative() == rsoc->isNegative() && lsoc->intVal()->isEqual(rsoc->intVal());
      }
      return false;
    }

    case Type::Assignment: {
      const TypeAssignment * ta = static_cast<const TypeAssignment *>(lt);
      if (ta->value() != NULL) {
        return isEqual(ta->value(), rt);
      } else {
        return ta->constraints().accepts(rt);
      }
    }

    case Type::ModVariadic:
    case Type::CVQual:
    case Type::KindCount:
      DFAIL("Type class not supported by isEqual()");
      break;
  }
}

bool TypeRelation::isSubtype(const Type * ty, const Type * base) {
  if (ty == base) {
    return true;
  }

  // Special cases for ambiguous base types.
  switch (base->typeClass()) {
    case Type::Alias:
      return isSubtype(ty, static_cast<const TypeAlias *>(base)->value());

    case Type::Protocol:
      // Special case for protocols - implicit inheritance
      if (static_cast<const CompositeType *>(base)->isSupportedBy(ty)) {
        return true;
      }
      // Fall through and treat as a regular type
      break;

    case Type::AmbiguousParameter:
    case Type::AmbiguousPhi:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam: {
      TypeExpansion expansion;
      base->expand(expansion);
      if (expansion.empty()) {
        return false;
      }
      for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
        if (!isSubtype(ty, *it)) {
          return false;
        }
      }
      return true;
    }

    case Type::Assignment: {
      const TypeAssignment * ta = static_cast<const TypeAssignment *>(base);
      if (ta->value() != NULL) {
        return isSubtype(ty, ta->value());
      } else {
        bool any = false;
        for (ConstraintSet::const_iterator si = ta->begin(), sEnd = ta->end(); si != sEnd; ++si) {
          Constraint * cst = *si;
          if (cst->visited()) {
            any = true;
          } else if (cst->checkProvisions()) {
            if (cst->kind() == Constraint::UPPER_BOUND) {
              // There's no way to determine if this is true, so return false.
              return false;
            }

            cst->setVisited(true);
            if (!isSubtype(ty, cst->value())) {
              cst->setVisited(false);
              return false;
            }
            cst->setVisited(false);
            any = true;
          }
        }
        return any;
      }
    }

    default:
      break;
  }

  switch (ty->typeClass()) {
    case Type::Alias:
      return isSubtype(static_cast<const TypeAlias *>(ty)->value(), base);

    case Type::Primitive: {
      const PrimitiveType * pType = static_cast<const PrimitiveType *>(ty);
      if (const PrimitiveType * pBase = dyn_cast<PrimitiveType>(base)) {
        return pType->isSubtypeOf(pBase);
      }
      return false;
    }

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      const CompositeType * ctType = static_cast<const CompositeType *>(ty);
      if (const CompositeType * ctBase = dyn_cast<CompositeType>(base)) {
        if (isEqualComposite(ctType, ctBase)) {
          return true;
        }

        // Interfaces are always considered to be subclasses of Object.
        if (ctType->typeClass() == Type::Interface && ctBase == Builtins::typeObject.get()) {
          return true;
        }

        // They aren't the same, check all base classes
        const ClassList & bases = ctType->bases();
        for (ClassList::const_iterator it = bases.begin(); it != bases.end(); ++it) {
          if (isSubtype(*it, base)) {
            return true;
          }
        }
      }

      return false;
    }

    case Type::Enum: {
      // So we already know that ty != base.
      return false;
    }

    case Type::NAddress:
    case Type::NArray:
    case Type::FlexibleArray:
    case Type::Function:
    case Type::Unit:
    case Type::Tuple:
    case Type::Union:
    case Type::TypeLiteral:
    case Type::TypeVar:
      // None of these types support a subclass relationship, so equality is the only option.
      return isEqual(ty, base);

    case Type::AmbiguousParameter:
    case Type::AmbiguousPhi:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam: {
      TypeExpansion expansion;
      ty->expand(expansion);
      if (expansion.empty()) {
        return false;
      }
      for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
        if (!isSubtype(*it, base)) {
          return false;
        }
      }
      return true;
    }

    case Type::SizingOf: {
      const SizingOfConstraint * soc = static_cast<const SizingOfConstraint *>(ty);
      return soc->isSubtypeOf(base);
    }

    case Type::Assignment: {
      const TypeAssignment * ta = static_cast<const TypeAssignment *>(ty);
      if (ta->value() != NULL) {
        return isSubtype(ta->value(), base);
      } else {
        bool any = false;
        for (ConstraintSet::const_iterator si = ta->begin(), sEnd = ta->end(); si != sEnd; ++si) {
          Constraint * cst = *si;
          if (cst->visited()) {
            any = true;
          } else if (cst->checkProvisions()) {
            if (cst->kind() == Constraint::UPPER_BOUND) { // TODO: Wrong!
              // There's no way to determine if this is true, so return false.
              return false;
            }

            cst->setVisited(true);
            if (!isSubtype(cst->value(), base)) {
              cst->setVisited(false);
              return false;
            }
            cst->setVisited(false);
            any = true;
          }
        }
        return any;
      }
      return false;
    }

    case Type::ModVariadic:
    case Type::CVQual:
    case Type::KindCount:
      DFAIL("Type class not supported by isSubtype()");
      break;
  }

  return false;
}

} // namespace tart

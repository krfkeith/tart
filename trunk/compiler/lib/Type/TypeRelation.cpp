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

bool TypeRelation::isEqual(const Type * lhs, const Type * rhs) {
  // Early out
  if (lhs == rhs) {
    return true;
  }

  while (const TypeAssignment * ta = dyn_cast<TypeAssignment>(lhs)) {
    lhs = ta->value();
    if (lhs == NULL) {
      bool atLeastOne = false;
      for (ConstraintSet::const_iterator ci = ta->begin(); ci != ta->end(); ++ci) {
        Constraint * cst = * ci;
        if (!cst->visited() && cst->checkProvisions() && cst->kind() == Constraint::EXACT) {
          cst->setVisited(true);
          bool result = isEqual(cst->value(), rhs);
          cst->setVisited(false);
          if (!result) {
            return false;
          }
          atLeastOne = true;
        }
      }
      return atLeastOne;
    }
  }

  while (const TypeAssignment * ta = dyn_cast<TypeAssignment>(rhs)) {
    rhs = ta->value();
    if (rhs == NULL) {
      bool atLeastOne = false;
      for (ConstraintSet::const_iterator ci = ta->begin(); ci != ta->end(); ++ci) {
        Constraint * cst = * ci;
        if (!cst->visited() && cst->checkProvisions() && cst->kind() == Constraint::EXACT) {
          cst->setVisited(true);
          bool result = isEqual(lhs, cst->value());
          cst->setVisited(false);
          if (!result) {
            return false;
          }
          atLeastOne = true;
        }
      }
      return atLeastOne;
    }
  }

  if (lhs == rhs) {
    return true;
  }

  // Compare the ASTs to see if they derive from the same original symbol.
  if (lhs->typeDefn() != NULL &&
      rhs->typeDefn() != NULL &&
      lhs->typeDefn()->ast() != NULL &&
      lhs->typeDefn()->ast() == rhs->typeDefn()->ast()) {

    // Now test the type parameters to see if they are also equivalent.
    const TypeDefn * d1 = lhs->typeDefn();
    const TypeDefn * d2 = rhs->typeDefn();
    const ConstTypeList * lhsParams = NULL;
    const ConstTypeList * rhsParams = NULL;

    if (d1->isTemplate()) {
      lhsParams = &d1->templateSignature()->typeParams()->members();
    } else if (d1->isTemplateInstance()) {
      lhsParams = &d1->templateInstance()->typeArgs()->members();
    }

    if (d2->isTemplate()) {
      rhsParams = &d2->templateSignature()->typeParams()->members();
    } else if (d2->isTemplateInstance()) {
      rhsParams = &d2->templateInstance()->typeArgs()->members();
    }

    if (lhsParams == rhsParams) {
      return true;
    }

    if (lhsParams == NULL ||
        rhsParams == NULL ||
        lhsParams->size() != rhsParams->size()) {
      return false;
    }

    size_t numParams = lhsParams->size();
    for (size_t i = 0; i < numParams; ++i) {
      if (!isEqual((*lhsParams)[i], (*rhsParams)[i])) {
        return false;
      }
    }

    return true;
  } else if (lhs->typeClass() == rhs->typeClass()) {
    if (const TupleType * tt1 = dyn_cast<TupleType>(lhs)) {
      const TupleType * tt2 = static_cast<const TupleType *>(rhs);
      if (tt1->size() == tt2->size()) {
        size_t size = tt1->size();
        for (size_t i = 0; i < size; ++i) {
          if (!isEqual(tt1->member(i), tt2->member(i))) {
            return false;
          }
          return true;
        }
      }
    }
  }

  // TODO: Add functions, unions, etc.

  return false;
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

        // Check if both classes derive from the same AST.
        TypeDefn * tdType = ctType->typeDefn();
        TypeDefn * tdBase = ctBase->typeDefn();
        if (tdType != NULL &&
            tdBase != NULL &&
            tdType->ast() != NULL &&
            tdType->ast() == tdBase->ast()) {
          // Now just need to test if the type params are the same.
          while (tdType != NULL && tdBase != NULL) {
            // They should both be instances, or neither.
            DASSERT(tdType->isTemplateInstance() == tdBase->isTemplateInstance());
            if (tdType->isTemplateInstance()) {
              // Compare type parameters for equivalence.
              return isEqual(
                  tdType->templateInstance()->typeArgs(),
                  tdBase->templateInstance()->typeArgs());
            }
            // It's possible that both type and base are template instance members. Try
            // again at the enclosing level.
            tdType = dyn_cast<TypeDefn>(tdType->parentDefn());
            tdBase = dyn_cast<TypeDefn>(tdBase->parentDefn());
          }
          return false;
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

    case Type::NAddress: {
      const AddressType * at = static_cast<const AddressType *>(ty);
      if (const AddressType * atBase = dyn_cast<AddressType>(base)) {
        return isEqual(at->typeParam(0), atBase->typeParam(0));
      }
      return false;
    }

    case Type::NArray: {
      const NativeArrayType * na = static_cast<const NativeArrayType *>(ty);
      if (const NativeArrayType * naBase = dyn_cast<NativeArrayType>(base)) {
        return isEqual(na->typeParam(0), naBase->typeParam(0)) && na->size() == naBase->size();
      }
      return false;
    }

    case Type::FlexibleArray: {
      const FlexibleArrayType * fa = static_cast<const FlexibleArrayType *>(ty);
      if (const FlexibleArrayType * faBase = dyn_cast<FlexibleArrayType>(base)) {
        return isEqual(fa->typeParam(0), faBase->typeParam(0));
      }
      return false;
    }

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
    case Type::TupleOf:
    case Type::KindCount:
      DFAIL("Type class not supported by isSubtype()");
      break;
  }

  return false;
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Defn/FunctionDefn.h"

#include "tart/Type/AmbiguousTypeParamType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// AmbiguousTypeParamType

const Type * AmbiguousTypeParamType::forType(const Type * base, const Type * match,
    unsigned paramIndex) {
  base = dealias(base);
  match = dealias(match);
  switch (base->typeClass()) {
    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      const CompositeType * ctBase = static_cast<const CompositeType *>(base);
      if (const CompositeType * ctMatch = dyn_cast_or_null<CompositeType>(match)) {
        base = ctBase->findBaseSpecializing(ctMatch);
        if (base == NULL) {
          return NULL;
        } else if (base->numTypeParams() <= paramIndex) {
          return NULL;
        } else {
          return base->typeParam(paramIndex);
        }
      }
      return NULL;
    }

    case Type::NAddress:
    case Type::NArray:
    case Type::FlexibleArray: {
      if (paramIndex == 0 && base->typeClass() == match->typeClass()) {
        return base->typeParam(0);
      }
      return NULL;
    }

    case Type::AmbiguousParameter:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam:
    case Type::AmbiguousPhi:
    case Type::Assignment: {
      TypeExpansion expansion;
      base->expand(expansion);
      if (expansion.size() == 1) {
        return forType(*expansion.begin(), match, paramIndex);
      }
      return new AmbiguousTypeParamType(base, match, paramIndex);
    }

    default:
      return NULL;
  }
}

void AmbiguousTypeParamType::expand(TypeExpansion & out) const {
  TypeExpansion baseExpansion;
  base_->expand(baseExpansion);
  for (TypeExpansion::const_iterator it = baseExpansion.begin(); it != baseExpansion.end(); ++it) {
    const Type * ty = forType(*it, match_, paramIndex_);
    if (ty != NULL) {
      out.insert(ty);
    } else {
      out.insert(&BadType::instance);
    }
  }
}

void AmbiguousTypeParamType::trace() const {
  Type::trace();
  base_->mark();
}

void AmbiguousTypeParamType::format(FormatStream & out) const {
  out << "{(";
  TypeSetConstraint::format(out);
  out << ")[%" << paramIndex_ << "]}";
}

} // namespace tart

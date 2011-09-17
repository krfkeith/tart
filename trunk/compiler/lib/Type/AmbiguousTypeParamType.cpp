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

QualifiedType AmbiguousTypeParamType::forType(QualifiedType base, const Type * match,
    unsigned paramIndex) {
  base = dealias(base);
  match = dealias(match);
  switch (base->typeClass()) {
    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      const CompositeType * ctBase = static_cast<const CompositeType *>(base.type());
      if (const CompositeType * ctMatch = dyn_cast_or_null<CompositeType>(match)) {
        if (ctMatch != NULL) {
          base = ctBase->findBaseSpecializing(ctMatch);
          if (!base) {
            return QualifiedType();
          }
        }
        if (paramIndex >= base->numTypeParams()) {
          return QualifiedType();
        } else {
          return base->typeParam(paramIndex).type();
        }
      }
      return QualifiedType();
    }

    case Type::NAddress:
    case Type::NArray:
    case Type::FlexibleArray: {
      if (paramIndex == 0 && (match == NULL || base->typeClass() == match->typeClass())) {
        return base->typeParam(0).type();
      }
      return QualifiedType();
    }

    case Type::Tuple: {
      if (match != NULL || paramIndex >= base->numTypeParams()) {
        return QualifiedType();
      } else {
        return base->typeParam(paramIndex).type();
      }
    }

    case Type::AmbiguousParameter:
    case Type::AmbiguousResult:
    case Type::AmbiguousTypeParam:
    case Type::AmbiguousPhi:
    case Type::Assignment: {
      QualifiedTypeSet expansion;
      base->expand(expansion);
      if (expansion.size() == 1) {
        return forType(*expansion.begin(), match, paramIndex);
      }
      return new AmbiguousTypeParamType(base, match, paramIndex);
    }

    default:
      return QualifiedType();
  }
}

void AmbiguousTypeParamType::listProspects(ProspectList & out, const ProvisionSet & add) const {
  ProspectList plist;
  AmbiguousType::listProspects(plist, base_, add);
  for (ProspectList::const_iterator it = plist.begin(), itEnd = plist.end(); it != itEnd; ++it) {
    QualifiedType ty = forType(it->type(), match_, paramIndex_);
    if (ty) {
      out.push_back(Prospect(ty, it->provisions()));
    } else {
      out.push_back(Prospect(&BadType::instance, it->provisions()));
    }
  }
}

void AmbiguousTypeParamType::expand(QualifiedTypeSet & out) const {
  QualifiedTypeSet baseExpansion;
  base_->expand(baseExpansion);
  for (QualifiedTypeSet::iterator it = baseExpansion.begin(); it != baseExpansion.end(); ++it) {
    QualifiedType ty = forType(*it, match_, paramIndex_);
    if (ty) {
      out.insert(ty);
    } else {
      out.insert(&BadType::instance);
    }
  }
}

void AmbiguousTypeParamType::trace() const {
  Type::trace();
  base_->mark();
  safeMark(match_);
}

void AmbiguousTypeParamType::format(FormatStream & out) const {
  QualifiedTypeSet expansion;
  expand(expansion);
  if (expansion.empty()) {
    out << "{" << base_ << "[%" << paramIndex_ << "]}";
  } else {
    TypeSetConstraint::format(out);
  }
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Type/AmbiguousPhiType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/CallCandidate.h"

namespace tart {

// -------------------------------------------------------------------
// AmbiguousPhiConstraint

void AmbiguousPhiType::add(const Type * type) {
  DASSERT(type != NULL);
  types_.push_back(type);
}

void AmbiguousPhiType::expand(TypeExpansion & out) const {
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    const Type * ty = *it;
    ty->expand(out);
  }
}

const Type * AmbiguousPhiType::singularValue() const {
  DFAIL("Implement");
}

ConversionRank AmbiguousPhiType::convertTo(const Type * toType, const Conversion & cn) const {
  // Makes no sense to return a value when converting to a constraint.
  if (cn.resultValue != NULL) {
    return Incompatible;
  }

  if (toType == this) {
    return IdenticalTypes;
  }

  ConversionRank worstRank = IdenticalTypes;
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    const Type * ty = *it;
    ConversionRank rank = toType->canConvert(ty);
    if (rank < worstRank) {
      worstRank = rank;
      if (rank == Incompatible) {
        break;
      }
    }
  }

  return worstRank;
}

ConversionRank AmbiguousPhiType::convertImpl(const Conversion & conversion) const {
  ConversionRank worstRank = IdenticalTypes;

  if (conversion.fromType == this) {
    return IdenticalTypes;
  }

  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    const Type * ty = *it;
    ConversionRank rank = ty->convert(conversion);
    if (rank < worstRank) {
      worstRank = rank;
      if (rank == Incompatible) {
        break;
      }
    }
  }

  return worstRank;
}

bool AmbiguousPhiType::isSingular() const {
  return common_ != NULL && common_->isSingular();
}

bool AmbiguousPhiType::isReferenceType() const {
  DINVALID;
  return false;
}

void AmbiguousPhiType::trace() const {
  TypeConstraint::trace();
  markList(types_.begin(), types_.end());
}

void AmbiguousPhiType::format(FormatStream & out) const {
  out << "{PHI: ";
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    if (it != types_.begin()) {
      out << "|";
    }
    out << *it;
  }
  out << "}";
}

} // namespace tart

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

void AmbiguousPhiType::listProspects(ProspectList & out, const ProvisionSet & add) const {
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    const Type * ty = *it;
    AmbiguousType::listProspects(out, ty, add);
  }
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

bool AmbiguousPhiType::isSingular() const {
  return common_ != NULL && common_->isSingular();
}

bool AmbiguousPhiType::isReferenceType() const {
  DINVALID;
  return false;
}

void AmbiguousPhiType::trace() const {
  Type::trace();
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

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

void AmbiguousPhiType::add(QualifiedType type) {
  DASSERT(type);
  types_.push_back(type);
}

void AmbiguousPhiType::listProspects(ProspectList & out, const ProvisionSet & add) const {
  for (QualifiedTypeList::const_iterator it = types_.begin(), itEnd = types_.end();
      it != itEnd; ++it) {
    AmbiguousType::listProspects(out, *it, add);
  }
}

void AmbiguousPhiType::expandImpl(QualifiedTypeSet & out, unsigned qualifiers) const {
  for (QualifiedTypeList::const_iterator it = types_.begin(), itEnd = types_.end();
      it != itEnd; ++it) {
    it->expand(out, qualifiers);
  }
}

QualifiedType AmbiguousPhiType::singularValue() const {
  DFAIL("Implement");
}

bool AmbiguousPhiType::isSingular() const {
  return common_ && common_->isSingular();
}

bool AmbiguousPhiType::isReferenceType() const {
  DINVALID;
  return false;
}

void AmbiguousPhiType::trace() const {
  Type::trace();
  markArray(llvm::ArrayRef<QualifiedType>(types_));
}

void AmbiguousPhiType::format(FormatStream & out) const {
  out << "{PHI: ";
  for (QualifiedTypeList::const_iterator it = types_.begin(), itEnd = types_.end();
      it != itEnd; ++it) {
    if (it != types_.begin()) {
      out << "|";
    }
    out << *it;
  }
  out << "}";
}

} // namespace tart

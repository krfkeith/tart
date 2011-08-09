/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/AmbiguousType.h"
#include "tart/Type/AmbiguousParameterType.h"
#include "tart/Type/AmbiguousTypeParamType.h"
#include "tart/Type/AmbiguousResultType.h"
#include "tart/Type/AmbiguousPhiType.h"
#include "tart/Type/TypeAlias.h"

#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// AmbiguousType

void AmbiguousType::listProspects(ProspectList & out, const Type * ty, const ProvisionSet & add) {
  static int recursionCheck = 0;
  ++recursionCheck;
  DASSERT(recursionCheck < 50);

  switch (ty->typeClass()) {
    case Type::AmbiguousParameter: {
      static_cast<const AmbiguousParameterType *>(ty)->listProspects(out, add);
      break;
    }

    case Type::AmbiguousResult: {
      static_cast<const AmbiguousResultType *>(ty)->listProspects(out, add);
      break;
    }

    case Type::AmbiguousPhi: {
      static_cast<const AmbiguousPhiType *>(ty)->listProspects(out, add);
      break;
    }

    case Type::AmbiguousTypeParam: {
      static_cast<const AmbiguousTypeParamType *>(ty)->listProspects(out, add);
      break;
    }

    default:
      out.push_back(Prospect(ty, add));
      break;
  }

  --recursionCheck;
}

llvm::Type * AmbiguousType::irType() const {
  DINVALID;
  return NULL;
}

void AmbiguousType::formatImpl(FormatStream & out) const {
  static int recursionCheck = 0;
  ++recursionCheck;
  DASSERT(recursionCheck < 50);

  TypeExpansion expansion;
  expand(expansion);
  out << "{";
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    if (it != expansion.begin()) {
      out << "|";
    }

    out << *it;
  }
  out << "}";
  --recursionCheck;
}

} // namespace tart

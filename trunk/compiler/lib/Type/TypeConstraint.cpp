/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Defn/FunctionDefn.h"

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/TypeConstraint.h"
#include "tart/Type/TupleType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/CallCandidate.h"

namespace tart {

// -------------------------------------------------------------------
// TypeConstraint

llvm::Type * TypeConstraint::irType() const {
  DFAIL("TypeConstraint does not have an IRType");
}

/// -------------------------------------------------------------------
/// TypeSetConstraint

QualifiedType TypeSetConstraint::singularValue() const {
  QualifiedTypeSet expansion;
  expandImpl(expansion, 0);
  if (expansion.size() == 1) {
    return *expansion.begin();
  }

  return NULL;
}

bool TypeSetConstraint::isSingular() const {
  QualifiedType ty = singularValue();
  return ty; // && ty->isSingular();
}

bool TypeSetConstraint::isReferenceType() const {
  QualifiedTypeSet expansion;
  expandImpl(expansion, 0);
  for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
    QualifiedType ty = *it;
    if (!ty->isReferenceType()) {
      return false;
    }
  }

  return true;
}

void TypeSetConstraint::format(FormatStream & out) const {
  static int recursionCheck = 0;
  ++recursionCheck;
  DASSERT(recursionCheck < 50);

  QualifiedTypeSet expansion;
  expandImpl(expansion, 0);
  out << "{";
  for (QualifiedTypeSet::iterator it = expansion.begin(); it != expansion.end(); ++it) {
    if (it != expansion.begin()) {
      out << "|";
    }

    out << *it;
  }
  out << "}";
  --recursionCheck;
}

} // namespace tart

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

const Type * TypeSetConstraint::singularValue() const {
  TypeExpansion expansion;
  expand(expansion);
  if (expansion.size() == 1) {
    return *expansion.begin();
  }

  return NULL;
}

bool TypeSetConstraint::isEqual(const Type * other) const {
  // It's equal only if it's a equal to every member
  TypeExpansion expansion;
  expand(expansion);
  if (expansion.empty()) {
    return false;
  }
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    if (!ty->isEqual(other)) {
      return false;
    }
  }

  return true;
}

bool TypeSetConstraint::isSingular() const {
  const Type * ty = singularValue();
  return ty != NULL; // && ty->isSingular();
}

bool TypeSetConstraint::isReferenceType() const {
  TypeExpansion expansion;
  expand(expansion);
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
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

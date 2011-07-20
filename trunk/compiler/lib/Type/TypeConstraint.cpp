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

const llvm::Type * TypeConstraint::irType() const {
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

ConversionRank TypeSetConstraint::convertTo(const Type * toType, const Conversion & cn) const {
  // Makes no sense to return a value when converting to a constraint.
  if (cn.resultValue != NULL) {
    return Incompatible;
  }

  TypeExpansion expansion;
  expand(expansion);
  ConversionRank best = Incompatible;
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    ConversionRank rank = toType->canConvert(ty);
    if (rank > best) {
      best = rank;
      if (rank == IdenticalTypes) {
        break;
      }
    }
  }

  return best;
}

ConversionRank TypeSetConstraint::convertImpl(const Conversion & conversion) const {
  static int recursionCheck = 0;
  ++recursionCheck;
  DASSERT(recursionCheck < 50);
  TypeExpansion expansion;
  expand(expansion);
  ConversionRank best = Incompatible;
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    ConversionRank rank = ty->convert(conversion);
    if (rank > best) {
      best = rank;
      if (rank == IdenticalTypes) {
        break;
      }
    }
  }

  --recursionCheck;
  return best;
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

// -------------------------------------------------------------------
// SizingOfConstraint

unsigned SizingOfConstraint::signedBitsRequired() const {
  const llvm::APInt & intVal = intVal_->value()->getValue();
  return intVal.getMinSignedBits();
}

unsigned SizingOfConstraint::unsignedBitsRequired() const {
  const llvm::APInt & intVal = intVal_->value()->getValue();
  return isNegative_ ? INT32_MAX : intVal.getActiveBits();
}

bool SizingOfConstraint::isEqual(const Type * other) const {
  if (const SizingOfConstraint * soc = dyn_cast_or_null<SizingOfConstraint>(other)) {
    return isNegative_ == soc->isNegative() && intVal_->isEqual(soc->intVal());
  } else {
    return false;
  }
}

const Type * SizingOfConstraint::singularValue() const {
  DFAIL("Implement");
}

ConversionRank SizingOfConstraint::convertTo(const Type * toType, const Conversion & cn) const {
  return Incompatible;
}

ConversionRank SizingOfConstraint::convertImpl(const Conversion & conversion) const {
  // Can't convert *to* an unsized type.
  return Incompatible;
}

bool SizingOfConstraint::isSubtypeOf(const Type * other) const {
  if (const SizingOfConstraint * soc = dyn_cast<SizingOfConstraint>(other)) {
    if (soc == other) {
      return true;
    }
    diag.debug() << this;
    diag.debug() << soc;
    DFAIL("Implement");
    (void)soc;
  } else if (const PrimitiveType * pty = dyn_cast<PrimitiveType>(other)) {
    if (isNegative() && pty->isUnsignedType()) {
      return false;
    }

    if (pty->isUnsignedType()) {
      return unsignedBitsRequired() <= pty->numBits();
    } else {
      return signedBitsRequired() <= pty->numBits();
    }
  } else {
    return false;
  }
}

void SizingOfConstraint::trace() const {
  Type::trace();
  intVal_->mark();
}

void SizingOfConstraint::format(FormatStream & out) const {
  out << intVal_->value()->getValue().toString(10, true);
}

} // namespace tart

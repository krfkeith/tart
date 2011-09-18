/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/Type.h"
#include "tart/Type/QualifiedType.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

void assertQualifiersValid(unsigned qual) {
  qual &= QualifiedType::MUTABILITY_MASK;
  // Easy way to check if qual is a power of two
  if (qual != 0) {
    DASSERT((qual & (qual-1)) == 0) << "Invalid qualifiers: " << qual;
  }
}

unsigned combineQualifiers(unsigned left, unsigned right) {
  if (right & QualifiedType::MUTABLE) {
    left &= ~(QualifiedType::READONLY|QualifiedType::IMMUTABLE|QualifiedType::ADOPTED);
  }

  if (right & QualifiedType::IMMUTABLE) {
    left &= ~(QualifiedType::READONLY|QualifiedType::MUTABLE|QualifiedType::ADOPTED);
  }

  if (right & QualifiedType::READONLY) {
    left &= ~(QualifiedType::MUTABLE|QualifiedType::IMMUTABLE|QualifiedType::ADOPTED);
  }

  return left | right;
}

bool canAssignQualifiers(unsigned from, unsigned to) {
  assertQualifiersValid(from);
  assertQualifiersValid(to);
  from &= QualifiedType::MUTABILITY_MASK;
  to &= QualifiedType::MUTABILITY_MASK;
  switch (to) {
    case QualifiedType::READONLY:
      return true;

    case QualifiedType::MUTABLE:
    default:
      return (from & (QualifiedType::READONLY|QualifiedType::IMMUTABLE)) == 0;

    case QualifiedType::IMMUTABLE:
      return (from & (QualifiedType::READONLY|QualifiedType::MUTABLE)) == 0;
  }
}

void formatQualifiedType(FormatStream & out, const Type * ty, unsigned qualifiers) {
  if (qualifiers & QualifiedType::MUTABLE) {
    out << "mutable(";
  }

  if (qualifiers & QualifiedType::IMMUTABLE) {
    out << "immutable(";
  }

  if (qualifiers & QualifiedType::READONLY) {
    out << "readonly(";
  }

  if (qualifiers & QualifiedType::ADOPTED) {
    out << "adopted(";
  }

  if (qualifiers & QualifiedType::VOLATILE) {
    out << "volatile(";
  }

  out << ty;

  if (qualifiers & QualifiedType::VARIADIC) {
    out << " ... ";
  }

  if (qualifiers & QualifiedType::MUTABLE) {
    out << ")";
  }

  if (qualifiers & QualifiedType::IMMUTABLE) {
    out << ")";
  }

  if (qualifiers & QualifiedType::READONLY) {
    out << ")";
  }

  if (qualifiers & QualifiedType::ADOPTED) {
    out << ")";
  }

  if (qualifiers & QualifiedType::VOLATILE) {
    out << ")";
  }
}

} // namespace tart

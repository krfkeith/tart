/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/Type.h"
#include "tart/Type/QualifiedType.h"

namespace tart {

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

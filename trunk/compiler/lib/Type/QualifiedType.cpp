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

bool areQualifiersEquivalent(unsigned lq, unsigned rq) {
  assertQualifiersValid(lq);
  assertQualifiersValid(rq);
  lq &= (QualifiedType::READONLY | QualifiedType::IMMUTABLE);
  rq &= (QualifiedType::READONLY | QualifiedType::IMMUTABLE);
  return lq == rq;
}

bool canAssignQualifiers(unsigned src, unsigned dst) {
  assertQualifiersValid(src);
  assertQualifiersValid(dst);
  src &= QualifiedType::MUTABILITY_MASK;
  dst &= QualifiedType::MUTABILITY_MASK;
  switch (dst) {
    case QualifiedType::READONLY:
      return true;

    case QualifiedType::MUTABLE:
    default:
      return (src & (QualifiedType::READONLY|QualifiedType::IMMUTABLE)) == 0;

    case QualifiedType::IMMUTABLE:
      return (src & QualifiedType::IMMUTABLE) != 0;
  }
}

void formatQualifiedType(FormatStream & out, const Type * ty, unsigned qualifiers) {
  formatQualifiedTypePrefix(out, qualifiers);
  out << ty;
  formatQualifiedTypeSuffix(out, qualifiers);
}

void formatQualifiedTypePrefix(FormatStream & out, unsigned qualifiers) {
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
}

void formatQualifiedTypeSuffix(FormatStream & out, unsigned qualifiers) {
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

const char * typeTostr(const Type * ty, unsigned qual) {
  static llvm::SmallString<256> temp;
  StrFormatStream stream;
  stream.setFormatOptions(Format_Verbose);
  formatQualifiedTypePrefix(stream, qual);
  if (ty) {
    ty->format(stream);
  } else {
    stream << "<NULL>";
  }
  formatQualifiedTypeSuffix(stream, qual);
  stream.flush();

  temp.clear();
  temp += stream.str();
  return temp.c_str();
}

void dumpType(const Type * ty, unsigned qual) {
  FormatStream fs(llvm::outs());
  formatQualifiedTypePrefix(fs, qual);
  if (ty) {
    ty->format(fs);
  } else {
    fs << "<NULL>";
  }
  formatQualifiedTypeSuffix(fs, qual);
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/TypeOrdering.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/UnitType.h"
#include "tart/Type/TypeConstraint.h"
#include "tart/Defn/Template.h"
//#include "tart/Defn/FunctionDefn.h"
//#include "tart/Defn/Defn.h"
//#include "tart/Type/TypeAlias.h"
#include "tart/Sema/BindingEnv.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// ComparisonResult

ComparisonResult operator+(ComparisonResult r0, ComparisonResult r1) {
  switch (r0) {
    case EQUAL:
      return r1;

    case LEFT_FIRST:
      return r1 == EQUAL || r1 == LEFT_FIRST ? LEFT_FIRST : UNORDERED;

    case RIGHT_FIRST:
      return r1 == EQUAL || r1 == RIGHT_FIRST ? RIGHT_FIRST : UNORDERED;

    case UNORDERED:
    default:
      return UNORDERED;
  }
}

ComparisonResult operator-(ComparisonResult cr) {
  switch (cr) {
    case EQUAL:
    case UNORDERED:
    default:
      return cr;

    case LEFT_FIRST:
      return RIGHT_FIRST;

    case RIGHT_FIRST:
      return LEFT_FIRST;
  }
}

// -------------------------------------------------------------------
// TypeOrdering

ComparisonResult TypeOrdering::compare(const Type * t1, const Type * t2) {
  ComparisonResult result = EQUAL;
    //case Type::Alias:

#if 0
  switch (t1->typeClass()) {
    case Type::Alias:
      break;

    case Type::Pattern:
      return compareWithPattern(static_cast<const TypeVariable *>(t1), t2);

    case Type::PatternVal:
      return compareWithTypeBinding(static_cast<const TypeBinding *>(t1), t2);

    case Type::Constraint:
      return compareWithConstraint(static_cast<const TypeConstraint *>(t1), t2);
  }

  switch (t2->typeClass()) {
    case Type::Alias:
      break;

    case Type::Pattern:
      return -compareWithPattern(static_cast<const TypeVariable *>(t2), t1);

    case Type::PatternVal:
      return -compareWithTypeBinding(static_cast<const TypeBinding *>(t2), t1);

    case Type::Constraint:
      return -compareWithConstraint(static_cast<const TypeConstraint *>(t2), t1);
  }
#endif

  if (t1->typeClass() != t2->typeClass()) {
    return compareDissimilar(t1, t2);
  }

  switch (t1->typeClass()) {
    case Type::Primitive:
      return compare(
          static_cast<const PrimitiveType *>(t1),
          static_cast<const PrimitiveType *>(t2));

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
      return compare(
          static_cast<const CompositeType *>(t1),
          static_cast<const CompositeType *>(t2));

    case Type::Enum:
      return compare(
          static_cast<const EnumType *>(t1),
          static_cast<const EnumType *>(t2));

    case Type::Function:
      return compare(
          static_cast<const FunctionType *>(t1),
          static_cast<const FunctionType *>(t2));

    case Type::Tuple:
      return compare(
          static_cast<const TupleType *>(t1),
          static_cast<const TupleType *>(t2));

    case Type::Union:
      return compare(
          static_cast<const UnionType *>(t1),
          static_cast<const UnionType *>(t2));

    case Type::NAddress:
      return compare(
          static_cast<const AddressType *>(t1),
          static_cast<const AddressType *>(t2));

    case Type::NArray:
      return compare(
          static_cast<const NativeArrayType *>(t1),
          static_cast<const NativeArrayType *>(t2));

    case Type::FlexibleArray:
      return compare(
          static_cast<const FlexibleArrayType *>(t1),
          static_cast<const FlexibleArrayType *>(t2));

    case Type::Unit:
      return compare(
          static_cast<const UnitType *>(t1),
          static_cast<const UnitType *>(t2));

    default:
      DFAIL("invalid type");
  }
}

ComparisonResult TypeOrdering::compare(const PrimitiveType * t1, const PrimitiveType * t2) {
  return t1 == t2 ? EQUAL : UNORDERED;
}

ComparisonResult TypeOrdering::compare(const CompositeType * t1, const CompositeType * t2) {
  return t1 == t2 ? EQUAL : UNORDERED;
}

ComparisonResult TypeOrdering::compare(const EnumType * t1, const EnumType * t2) {
  return t1 == t2 ? EQUAL : UNORDERED;
}

ComparisonResult TypeOrdering::compare(const FunctionType * t1, const FunctionType * t2) {
  return t1 == t2 ? EQUAL : UNORDERED;
}

ComparisonResult TypeOrdering::compare(const UnionType * t1, const UnionType * t2) {
  return compare(t1->typeArgs(), t2->typeArgs());
}

ComparisonResult TypeOrdering::compare(const TupleType * t1, const TupleType * t2) {
  if (t1->size() != t2->size()) {
    return UNORDERED;
  }

  ComparisonResult result = EQUAL;
  size_t numElements = t1->size();
  for (size_t i = 0; i < numElements && result != UNORDERED; ++i) {
    result = result + compare(t1->member(i), t2->member(i));
  }

  return result;
}

ComparisonResult TypeOrdering::compare(const AddressType * t1, const AddressType * t2) {
  return t1 == t2 ? EQUAL : compare(t1->typeParam(0), t2->typeParam(0));
}

ComparisonResult TypeOrdering::compare(const NativeArrayType * t1, const NativeArrayType * t2) {
  return t1 == t2 ? EQUAL :
      compare(t1->typeParam(0), t2->typeParam(0)) + compare(t1->typeParam(1), t2->typeParam(1));
}

ComparisonResult TypeOrdering::compare(const FlexibleArrayType * t1, const FlexibleArrayType * t2) {
  return t1 == t2 ? EQUAL : compare(t1->typeParam(0), t2->typeParam(0));
}

ComparisonResult TypeOrdering::compare(const UnitType * t1, const UnitType * t2) {
  return t1 == t2 ? EQUAL : UNORDERED;
}

ComparisonResult TypeOrdering::compareDissimilar(const Type * t1, const Type * t2) {
  return UNORDERED;
}

ComparisonResult TypeOrdering::compareWithPattern(const TypeVariable * t1, const Type * t2) {
  return t1 == t2 ? EQUAL : UNORDERED;
}

ComparisonResult TypeOrdering::compareWithTypeBinding(const TypeBinding * t1, const Type * t2) {
  return t1 == t2 ? EQUAL : UNORDERED;
}

ComparisonResult TypeOrdering::compareWithConstraint(const TypeConstraint * t1, const Type * t2) {
  return t1 == t2 ? EQUAL : UNORDERED;
}

// -------------------------------------------------------------------
// LexicalTypeLess

bool LexicalTypeOrdering::operator()(const Type * t0, const Type * t1) const {
  return compare(t0, t1) < 0;
}

int LexicalTypeOrdering::compare(const Type * t0, const Type * t1) {
  //t0 = dealias(t0);
  //t0 = dealias(t0);

  if (t0->typeClass() > t1->typeClass()) {
    return -1;
  } else if (t1->typeClass() > t0->typeClass()) {
    return 1;
  }

  switch (t0->typeClass()) {
    case Type::Primitive:
      return compare(
          static_cast<const PrimitiveType *>(t0)->typeId(),
          static_cast<const PrimitiveType *>(t1)->typeId());

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
    case Type::Enum: {
      if (t0 == t1) {
        return 0;
      }

      int result = t0->typeDefn()->qualifiedName().compare(t1->typeDefn()->qualifiedName());
      if (result != 0) {
        return result;
      }

      if (t0->typeDefn()->isTemplateInstance()) {
        if (t1->typeDefn()->isTemplateInstance()) {
          return compare(
              t0->typeDefn()->templateInstance()->typeArgs(),
              t1->typeDefn()->templateInstance()->typeArgs());
        } else {
          return -1;
        }
      } else if (t1->typeDefn()->isTemplateInstance()) {
        return 1;
      }

      if (t0->typeDefn()->isTemplate()) {
        if (t1->typeDefn()->isTemplate()) {
          return compare(
              t0->typeDefn()->templateSignature()->typeParams(),
              t1->typeDefn()->templateSignature()->typeParams());
        } else {
          return -1;
        }
      } else if (t1->typeDefn()->isTemplate()) {
        return 1;
      }

      diag.debug() << Format_Type << "Unimplemented comparison between " << t0 << " and " << t1;
      DFAIL("Implement");
    }

    case Type::Function: {
      const FunctionType * ft0 = static_cast<const FunctionType *>(t0);
      const FunctionType * ft1 = static_cast<const FunctionType *>(t1);
      int result = compare(ft0->paramTypes(), ft1->paramTypes());
      if (result != 0) {
        return result;
      }
      result = compare(ft0->returnType(), ft1->returnType());
      if (result != 0) {
        return result;
      }
      return int(ft0->isStatic()) - int(ft1->isStatic());
    }

    case Type::Tuple: {
      const TupleType * tt0 = static_cast<const TupleType *>(t0);
      const TupleType * tt1 = static_cast<const TupleType *>(t1);
      for (size_t i = 0; ; ++i) {
        if (i < tt0->size() && i < tt1->size()) {
          int result = compare(tt0->member(i), tt1->member(i));
          if (result != 0) {
            return result;
          }
        } else if (tt0->size() < tt1->size()) {
          return -1;
        } else if (tt0->size() > tt1->size()) {
          return 1;
        } else {
          return 0;
        }
      }
    }

    case Type::Union:
      return compare(
          static_cast<const UnionType *>(t0)->typeArgs(),
          static_cast<const UnionType *>(t1)->typeArgs());

    case Type::NAddress:
      return compare(t0->typeParam(0), t1->typeParam(0));

    case Type::NArray:
      DFAIL("Implement");
      return compare(
          static_cast<const NativeArrayType *>(t0),
          static_cast<const NativeArrayType *>(t1));

    case Type::FlexibleArray:
      DFAIL("Implement");
      return compare(
          static_cast<const FlexibleArrayType *>(t0),
          static_cast<const FlexibleArrayType *>(t1));

    case Type::Unit: {
      const ConstantExpr * v0 = static_cast<const UnitType *>(t0)->value();
      const ConstantExpr * v1 = static_cast<const UnitType *>(t1)->value();
      DFAIL("Implement");
      return false;
    }

    default:
      DFAIL("invalid type");
  }

}

FormatStream & operator<<(FormatStream & out, ComparisonResult cr) {
  switch (cr) {
    case EQUAL: out << "EQUAL"; break;
    case LEFT_FIRST: out << "LEFT_FIRST"; break;
    case RIGHT_FIRST: out << "RIGHT_FIRST"; break;
    case UNORDERED: out << "UNORDERED"; break;
  }

  return out;
}

} // namespace tart
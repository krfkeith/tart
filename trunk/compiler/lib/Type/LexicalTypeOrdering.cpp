/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/UnitType.h"
#include "tart/Type/LexicalTypeOrdering.h"

#include "tart/Defn/Template.h"

#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// LexicalTypeOrdering

bool LexicalTypeOrdering::operator()(const Type * t0, const Type * t1) const {
  return compare(t0, t1) < 0;
}

bool LexicalTypeOrdering::operator()(const QualifiedType & t0, const QualifiedType & t1) const {
  return compare(t0, t1) < 0;
}

int LexicalTypeOrdering::compare(const QualifiedType & t0, const QualifiedType & t1) {
  int result = compare(t0.type(), t1.type());
  if (result != 0) {
    return result;
  }

  if (t0.qualifiers() < t1.qualifiers()) {
    return -1;
  } else if (t0.qualifiers() > t1.qualifiers()) {
    return 1;
  } else {
    return 0;
  }
}

int LexicalTypeOrdering::compare(const Type * t0, const Type * t1) {
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
      //const ConstantExpr * v0 = static_cast<const UnitType *>(t0)->value();
      //const ConstantExpr * v1 = static_cast<const UnitType *>(t1)->value();
      DFAIL("Implement");
      return false;
    }

    default:
      DFAIL("invalid type");
  }
}

} // namespace tart

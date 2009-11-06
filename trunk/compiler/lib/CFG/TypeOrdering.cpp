/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Type.h"
//#include "tart/CFG/TypeAlias.h"
//#include "tart/CFG/TypeConstraint.h"
//#include "tart/CFG/PrimitiveType.h"
//#include "tart/CFG/CompositeType.h"
//#include "tart/CFG/FunctionType.h"
//#include "tart/CFG/FunctionDefn.h"
//#include "tart/CFG/NativeType.h"
//#include "tart/CFG/UnionType.h"
//#include "tart/CFG/Defn.h"
//#include "tart/CFG/Template.h"
//#include "tart/Sema/BindingEnv.h"
//#include "tart/Common/Diagnostics.h"

namespace tart {

ComparisonResult combineResults(ComparisonResult r1, ComparisonResult r2) {
  switch (r1) {
    case EQUAL:
      return r2;

    case LEFT_FIRST:
      return r2 == EQUAL || r2 == LEFT_FIRST ? LEFT_FIRST : UNORDERED;

    case RIGHT_FIRST:
      return r2 == EQUAL || r2 == RIGHT_FIRST ? RIGHT_FIRST : UNORDERED;

    case UNORDERED:
      return UNORDERED;
  }
}

ComparisonResult compareSpecificity(const TypeVector * t1, const TypeVector * t2) {
  if (t1->size() != t2->size()) {
    return UNORDERED;
  }

  ComparisonResult result = EQUAL;
  size_t numElements = t1->size();
  for (size_t i = 0; i < numElements && result != UNORDERED; ++it) {
    result = combineResults(compareSpecificity((*t1)[i], (*t2)[i]));
  }

  return result;
}

ComparisonResult compareSpecificity(const TypeRef & t1, const TypeRef & t2) {
  ComparisonResult result = EQUAL;
  if (t1.modifiers() != t2.modifiers()) {
    if (t1.modifiers() & ~t2.modifiers() == 0) {
      result = RIGHT_FIRST;
    } else if (t2.modifiers() & ~t1.modifiers() == 0) {
      result = LEFT_FIRST;
    } else {
      return UNORDERED;
    }
  }

  return combineResults(result, compareSpecificity(t1.type(), t2.type()));
}

ComparisonResult compareSpecificity(const Type * t1, const Type * t2) {
  ComparisonResult result = EQUAL;

  if (t1->typeClass() != t2->typeClass()) {
    return UNORDERED;
  }

  switch (t1->typeClass()) {
    case Type::Primitive:
    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
    case Type::Enum:
    case Type::Function:
    case Type::BoundMethod:
    case Type::Tuple:
    case Type::Union:
    case Type::NAddress:
    case Type::NPointer:
    case Type::NArray:
    case Type::SingleValue:
    case Type::Alias:
    case Type::Pattern:
    case Type::PatternVal:
    case Type::Constraint:
      break;

    default:
      DFAIL("invalid type");
  }
}

} // namespace tart

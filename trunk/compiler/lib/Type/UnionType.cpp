/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Defn/Module.h"

#include "tart/Type/UnionType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeOrdering.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"
#include "tart/Objects/TargetSelection.h"

namespace tart {

namespace {
  typedef llvm::DenseMap<const Type *, UnionType *, Type::KeyInfo> UnionTypeMap;

  UnionTypeMap uniqueValues_;
  bool initFlag = false;

  class CleanupHook : public GC::Callback {
    void call() {
      uniqueValues_.clear();
      initFlag = false;
    }
  };

  CleanupHook hook;
}

// -------------------------------------------------------------------
// UnionType

UnionType * UnionType::get(const ConstTypeList & members) {
  if (!initFlag) {
    initFlag = true;
    GC::registerUninitCallback(&hook);
  }

  // Make sure that the set of types is disjoint, meaning that there are no types
  // in the set which are subtypes of one another.
  TypeList combined;
  for (ConstTypeList::const_iterator it = members.begin(); it != members.end(); ++it) {
    const Type * type = dealias(*it);

    bool addNew = true;
    for (TypeList::iterator m = combined.begin(); m != combined.end();) {
      if ((*m)->isEqual(type) || type->isSubtype(*m)) {
        addNew = false;
      } else if ((*m)->isSubtype(type)) { // TODO: Is isSubtype the right test for this?
        m = combined.erase(m);
        continue;
      }

      ++m;
    }

    if (addNew) {
      combined.push_back(const_cast<Type *>(type));
    }
  }

  std::sort(combined.begin(), combined.end(), LexicalTypeOrdering());

  TupleType * membersTuple = TupleType::get(combined);

  UnionTypeMap::iterator it = uniqueValues_.find(membersTuple);
  if (it != uniqueValues_.end()) {
    return it->second;
  }

  UnionType * utype = new UnionType(membersTuple);
  uniqueValues_[membersTuple] = utype;
  return utype;
}

UnionType::UnionType(TupleType * members)
  : TypeImpl(Union, Shape_Unset)
  , members_(members)
  , numValueTypes_(0)
  , numReferenceTypes_(0)
  , hasVoidType_(false)
  , hasNullType_(false)
{
  for (ConstTypeList::const_iterator it = members->begin(); it != members->end(); ++it) {
    const Type * memberType = dealias(*it);
    if (memberType->isVoidType()) {
      hasVoidType_ = true;
    } else if (memberType->isNullType()) {
      hasNullType_ = true;
    } else if (memberType->isReferenceType()) {
      numReferenceTypes_ += 1;
    } else {
      numValueTypes_ += 1;
    }
  }
}

size_t UnionType::numTypeParams() const {
  return members_->size();
}

const Type * UnionType::typeParam(int index) const {
  return (*members_)[index];
}

bool UnionType::hasRefTypesOnly() const {
  return numValueTypes_ == 0 && !hasVoidType_;
}

bool UnionType::isSingleOptionalType() const {
  if (numValueTypes_ == 0) {
    return (hasNullType_ && !hasVoidType_ && numReferenceTypes_ == 1);
  } else if (numReferenceTypes_ == 0) {
    return hasVoidType_ && !hasNullType_ && numValueTypes_ == 1;
  }

  return false;
}

const Type * UnionType::getFirstNonVoidType() const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    const Type * memberType = *it;
    if (!memberType->isVoidType() && !memberType->isNullType()) {
      return memberType;
    }
  }

  return NULL;
}

const llvm::Type * UnionType::createIRType() const {
#if LLVM_UNION_SUPPORT
  //shape_ = Shape_Small_RValue;
  shape_ = Shape_Large_Value;

  if (!hasRefTypesOnly()) {
    for (ConstTypeList::const_iterator it = members().begin(); it != members().end(); ++it) {
      const Type * type = dealias(*it);

      if (!type->isVoidType()) {
        irTypes_.push_back(type->irEmbeddedType());
      }

      if (type->typeShape() == Shape_Large_Value) {
        shape_ = Shape_Large_Value;
      }
    }

    const llvm::Type * discriminatorType = getDiscriminatorType();
    std::vector<const llvm::Type *> unionMembers;
    unionMembers.push_back(discriminatorType);
    if (irTypes_.size() == 1) {
      unionMembers.push_back(irTypes_[0]);
    } else {
      unionMembers.push_back(llvm::UnionType::get(&irTypes_[0], irTypes_.size()));
    }
    return llvm::StructType::get(llvm::getGlobalContext(), unionMembers);

  } else if (hasNullType_ && numReferenceTypes_ == 1) {
    // If it's Null or some reference type, then use the reference type.
    shape_ = Shape_Primitive;
    return getFirstNonVoidType()->irEmbeddedType();
  } else {
    shape_ = Shape_Primitive;
    return Builtins::typeObject->irParameterType();
  }
#else
  // Since LLVM does not support unions as first-class types, what we'll do is to find the
  // "largest" type and use that as the base representation. (Plus the discriminator field,
  // of course). However, we don't know what the size of a pointer is yet, so we'll calculate
  // the type size for both 32 and 64 bit machines. The only problem we might have is if the
  // largest type is different on 32 bit vs. 64 bits, in which case we'll just report failure
  // for now.

  size_t largestSize = 0;
  const Type * largestType = 0;   // Largest type on.

  //shape_ = Shape_Small_RValue;
  shape_ = Shape_Large_Value;

  // Create an array representing all of the IR types that correspond to the Tart types.
  for (ConstTypeList::const_iterator it = members().begin(); it != members().end(); ++it) {
    const Type * type = dealias(*it);

    const llvm::Type * irType = type->irEmbeddedType();
    irTypes_.push_back(irType);

    if (type->typeShape() == Shape_Large_Value) {
      shape_ = Shape_Large_Value;
    }

    size_t size = estimateTypeSize(irType);
    if (size > largestSize) {
      largestSize = size;
      largestType = type;
    }
  }

  DASSERT_OBJ(largestType != NULL, this);

  if (numValueTypes_ > 0 || hasVoidType_) {
    const llvm::Type * discriminatorType = getDiscriminatorType();
    const llvm::Type * largestIRType = largestType->irEmbeddedType();
    std::vector<const llvm::Type *> unionMembers;
    unionMembers.push_back(discriminatorType);
    unionMembers.push_back(largestIRType);
    return llvm::StructType::get(llvm::getGlobalContext(), unionMembers);
  } else if (hasNullType_ && numReferenceTypes_ == 1) {
    // If it's Null or some reference type, then use the reference type.
    shape_ = Shape_Primitive;
    const llvm::Type * ty = getFirstNonVoidType()->irEmbeddedType();
    DASSERT(!ty->isVoidTy());
    return ty;
  } else {
    shape_ = Shape_Primitive;
    return Builtins::typeObject->irParameterType();
  }
#endif
}

const llvm::Type * UnionType::irParameterType() const {
  const llvm::Type * type = irType();
  if (shape_ == Shape_Large_Value) {
    type = type->getPointerTo();
  }

  return type;
}

const llvm::Type * UnionType::getDiscriminatorType() const {
  size_t numStates = numValueTypes_;
  if (numReferenceTypes_ > 0 || hasVoidType_ || hasNullType_) {
    numStates += 1;
  }

  if (numStates == 2) {
    return llvm::Type::getInt1Ty(llvm::getGlobalContext());
  } else if (numStates < 256) {
    return llvm::Type::getInt8Ty(llvm::getGlobalContext());
  } else if (numStates < 0x10000) {
    return llvm::Type::getInt16Ty(llvm::getGlobalContext());
  } else {
    return llvm::Type::getInt32Ty(llvm::getGlobalContext());
  }
}

size_t UnionType::estimateTypeSize(const llvm::Type * type) {
  switch (type->getTypeID()) {
    case llvm::Type::VoidTyID:
    case llvm::Type::FloatTyID:
    case llvm::Type::DoubleTyID:
    case llvm::Type::X86_FP80TyID:
    case llvm::Type::FP128TyID:
    case llvm::Type::PPC_FP128TyID:
    case llvm::Type::LabelTyID:
    case llvm::Type::IntegerTyID:
    case llvm::Type::FunctionTyID:
      return type->getPrimitiveSizeInBits();

    case llvm::Type::PointerTyID: {
      const llvm::TargetData * td = TargetSelection::instance.targetData();
      return td->getPointerSizeInBits();
    }

    case llvm::Type::StructTyID: {
      size_t total = 0;
      unsigned numFields = type->getNumContainedTypes();
      for (unsigned i = 0; i < numFields; ++i) {
        total += estimateTypeSize(type->getContainedType(i));
        // TODO: Add padding?
      }

      return total;
    }

    case llvm::Type::ArrayTyID:
    case llvm::Type::VectorTyID:
      DFAIL("Implement");

    default:
      DFAIL("IllegalState");
  }
}

ConversionRank UnionType::convertImpl(const Conversion & cn) const {
  if (isEqual(cn.fromType)) {
    if (cn.resultValue != NULL) {
      *cn.resultValue = cn.fromValue;
    }

    return IdenticalTypes;
  }

  ConversionRank bestRank = Incompatible;
  const Type * bestType = NULL;

  // Create a temporary cn with no result value.
  Conversion ccTemp(cn);
  ccTemp.resultValue = NULL;
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    ConversionRank rank = (*it)->convert(ccTemp);
    if (rank > bestRank) {
      bestRank = rank;
      bestType = (*it);
    }
  }

  // Since we're converting to a union type, it's not identical.
  // TODO: Don't know if we really need this.
  //if (bestRank == IdenticalTypes) {
  //  bestRank = ExactConversion;
  //}

  if (bestType != NULL && cn.resultValue != NULL) {
    // Do the conversion to the best type first.
    bestRank = bestType->convertImpl(cn);

    // And now add a cast to the union type.
    if (*cn.resultValue != NULL) {
      int typeIndex = getTypeIndex(bestType);
      if (typeIndex < 0) {
        //diag.error(cn.fromValue) << "Cannot convert type '" << cn.fromType << "' to '" <<
        //    this << "'";
        return Incompatible;
      }

      CastExpr * result = new CastExpr(
          Expr::UnionCtorCast, cn.fromValue->location(), this, *cn.resultValue);
      result->setTypeIndex(typeIndex);
      *cn.resultValue = result;
    }
  }

  return bestRank;
}

ConversionRank UnionType::convertTo(const Type * toType, const Conversion & cn) const {
  if (isSingleOptionalType() && toType->isReferenceType()) {
    // Find the single optional type.
    const Type * memberType = getFirstNonVoidType();
    DASSERT(memberType != NULL);
    ConversionRank rank = toType->canConvert(memberType, cn.options);
    if (rank != Incompatible && cn.resultValue != NULL) {
      *cn.resultValue = new CastExpr(Expr::CheckedUnionMemberCast, SourceLocation(),
          toType, cn.fromValue);
    }

    return rank == IdenticalTypes ? ExactConversion : rank;
  }

  return Incompatible;
}

bool UnionType::isEqual(const Type * other) const {
  other = dealias(other);
  if (other == this) {
    return true;
  }

  // A union type is the same if it contains the same types (dealiased).
  if (const UnionType * u = dyn_cast<UnionType>(other)) {
    // Make sure that all types in u are in this.
    for (TupleType::const_iterator it = u->typeArgs()->begin(); it != u->typeArgs()->end(); ++it) {
      if (getTypeIndex(*it) < 0) {
        return false;
      }
    }

    // Make sure that all types in this are in u.
    for (TupleType::const_iterator it = typeArgs()->begin(); it != typeArgs()->end(); ++it) {
      if (u->getTypeIndex(*it) < 0) {
        return false;
      }
    }

    return true;
  }

  return false;
}

bool UnionType::isSingular() const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    if (!(*it)->isSingular()) {
      return false;
    }
  }

  return true;
}

bool UnionType::isSubtype(const Type * other) const {
  // TODO: Is this meaningful with unions?
  return isEqual(other);
  //DFAIL("Implement");
}

bool UnionType::includes(const Type * other) const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    if ((*it)->includes(other)) {
      return true;
    }
  }

  return false;
}

Expr * UnionType::nullInitValue() const {
  return NULL;
}

bool UnionType::containsReferenceType() const {
  if (numReferenceTypes_ > 0) {
    return true;
  }

  return members_->containsReferenceType();
}

TypeShape UnionType::typeShape() const {
  if (shape_ == Shape_Unset) {
    irType();
  }

  return shape_;
}

int UnionType::getTypeIndex(const Type * type) const {
  type = dealias(type);

  // If it only has reference types, then use subclass tests instead of
  // a discriminator field.
  if (hasRefTypesOnly()) {
    return 0;
  }

  // Otherwise, calculate the type index.
  int index = 0;
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    if (type->isEqual(*it)) {
      return index;
    }

    ++index;
  }

  return -1;
}

int UnionType::getNonVoidTypeIndex(const Type * type) const {
  type = dealias(type);

  // If it only has reference types, then use subclass tests instead of
  // a discriminator field.
  if (hasRefTypesOnly()) {
    return 0;
  }

  // Otherwise, calculate the type index.
  int index = 0;
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    if (!type->isVoidType()) {
      if (type->isEqual(*it)) {
        return index;
      }

      ++index;
    }
  }

  return -1;
}

Expr * UnionType::createDynamicCast(Expr * from, const Type * toType) const {
  const Type * fromType = dealias(from->type());
  if (toType->isEqual(fromType)) {
    return from;
  }

  // Logic for reference-only unions.
  if (hasRefTypesOnly() && toType->isReferenceType()) {
    if (hasNullType() && toType->isNullType()) {
      // If toType is the null type, then do a test for null.
      return new CastExpr(Expr::CheckedUnionMemberCast, from->location(), toType, from);
    } else if (const CompositeType * cto = dyn_cast<CompositeType>(toType)) {
      // If it's a common supertype of all members, then no need for a checked cast.
      if (!hasNullType() && isSupertypeOfAllMembers(cto)) {
        return new CastExpr(Expr::UnionMemberCast, from->location(), toType, from);
      }

      // If it's a subtype of any member, then do a checked cast.
      if (isSubtypeOfAnyMembers(cto)) {
        return new CastExpr(Expr::CheckedUnionMemberCast, from->location(), toType, from);
      }
    }
  }

  // Determine all of the possible member types that could represent an object of
  // type 'toType'.
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    const Type * memberType = *it;
    if (toType->canConvert(memberType)) {
      // TODO: Add additional cast if toType is not exactly the same type as the member.
      return new CastExpr(Expr::CheckedUnionMemberCast, from->location(), toType, from);
    }
  }

  diag.warn(from->location()) << "Union member cast from type '" << fromType << "' to '" <<
      toType << "' can never succeed.";
  return &Expr::ErrorVal;
}

bool UnionType::isSubtypeOfAnyMembers(const CompositeType * toType) const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    if (const CompositeType * ctype = dyn_cast<CompositeType>(*it)) {
      if (toType->isSubclassOf(ctype)) {
        return true;
      }
    }
  }

  return false;
}

bool UnionType::isSupertypeOfAllMembers(const CompositeType * toType) const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    const Type * memberType = *it;
    if (const CompositeType * ctype = dyn_cast<CompositeType>(memberType)) {
      if (!ctype->isSubclassOf(toType)) {
        return false;
      }
    } else if (!memberType->isNullType()) {
      return false;
    }
  }

  return true;
}

void UnionType::format(FormatStream & out) const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    if (it != members_->begin()) {
      out << " or ";
    }

    out << *it;
  }
}

void UnionType::trace() const {
  members_->mark();
}

} // namespace tart

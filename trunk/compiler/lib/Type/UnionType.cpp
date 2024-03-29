/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Defn/Module.h"

#include "tart/Type/UnionType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/LexicalTypeOrdering.h"
#include "tart/Type/TypeConversion.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"
#include "tart/Objects/TargetSelection.h"

#include "tart/Sema/Infer/TypeAssignment.h"

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

UnionType * UnionType::get(const QualifiedTypeList & members) {
  if (!initFlag) {
    initFlag = true;
    GC::registerUninitCallback(&hook);
  }

  // Make sure that the set of types is disjoint, meaning that there are no types
  // in the set which are subtypes of one another.
  QualifiedTypeList combined;
  for (QualifiedTypeList::const_iterator it = members.begin(); it != members.end(); ++it) {
    QualifiedType type = dealias(*it);

    bool addNew = true;

    // TODO: Flatten union types.
    for (QualifiedTypeList::iterator m = combined.begin(); m != combined.end();) {
      if (type->typeClass() == Type::Primitive || type->typeClass() == Type::Enum) {
        // Primitive types and enum types are not combined.
        addNew = *m != type;
      } else if (TypeRelation::isSubtype(type, *m)) {
        addNew = false;
      } else if (TypeRelation::isSubtype(*m, type)) { // TODO: Is isSubtype the right test for this?
        m = combined.erase(m);
        continue;
      }

      ++m;
    }

    if (addNew) {
      combined.push_back(type);
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
  for (QualifiedTypeList::const_iterator it = members->begin(); it != members->end(); ++it) {
    QualifiedType memberType = dealias(*it);
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

QualifiedType UnionType::typeParam(int index) const {
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

bool UnionType::isSingleNullableType() const {
  if (numValueTypes_ == 0) {
    return (hasNullType_ && !hasVoidType_ && numReferenceTypes_ == 1);
  }

  return false;
}

const Type * UnionType::getFirstNonVoidType() const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    QualifiedType memberType = *it;
    if (!memberType->isVoidType() && !memberType->isNullType()) {
      return memberType.type();
    }
  }

  return NULL;
}

llvm::Type * UnionType::createIRType() const {
#if LLVM_UNION_SUPPORT
  //shape_ = Shape_Small_RValue;
  shape_ = Shape_Large_Value;

  if (!hasRefTypesOnly()) {
    for (QualifiedTypeList::const_iterator it = members().begin(); it != members().end(); ++it) {
      const Type * type = dealias(*it);

      if (!type->isVoidType()) {
        irTypes_.push_back(type->irEmbeddedType());
      }

      if (type->typeShape() == Shape_Large_Value) {
        shape_ = Shape_Large_Value;
      }
    }

    llvm::Type * discriminatorType = getDiscriminatorType();
    std::vector<llvm::Type *> unionMembers;
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
  for (QualifiedTypeList::const_iterator it = members().begin(); it != members().end(); ++it) {
    QualifiedType type = dealias(*it);

    llvm::Type * irType = type->irEmbeddedType();
    irTypes_.push_back(irType);

    if (type->typeShape() == Shape_Large_Value) {
      shape_ = Shape_Large_Value;
    }

    size_t size = estimateTypeSize(irType);
    if (size > largestSize) {
      largestSize = size;
      largestType = type.unqualified();
    }
  }

  DASSERT_OBJ(largestType != NULL, this);

  if (numValueTypes_ > 0 || hasVoidType_) {
    llvm::Type * discriminatorType = getDiscriminatorType();
    llvm::Type * largestIRType = largestType->irEmbeddedType();
    std::vector<llvm::Type *> unionMembers;
    unionMembers.push_back(discriminatorType);
    unionMembers.push_back(largestIRType);
    return llvm::StructType::get(llvm::getGlobalContext(), unionMembers);
  } else if (hasNullType_ && numReferenceTypes_ == 1) {
    // If it's Null or some reference type, then use the reference type.
    shape_ = Shape_Primitive;
    llvm::Type * ty = getFirstNonVoidType()->irEmbeddedType();
    DASSERT(!ty->isVoidTy());
    return ty;
  } else {
    shape_ = Shape_Primitive;
    return Builtins::typeObject->irParameterType();
  }
#endif
}

llvm::Type * UnionType::irParameterType() const {
  llvm::Type * type = irType();
  if (shape_ == Shape_Large_Value) {
    type = type->getPointerTo();
  }

  return type;
}

llvm::Type * UnionType::getDiscriminatorType() const {
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

size_t UnionType::estimateTypeSize(llvm::Type * type) {
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

bool UnionType::isSingular() const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    if (!(*it)->isSingular()) {
      return false;
    }
  }

  return true;
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
    QualifiedType memberType = *it;
//    if (Qualified<TypeAssignment> ta = memberType.dyn_cast<TypeAssignment>()) {
//      if (TypeRelation::isEqual(memberType, type)) {
//        return index;
//      }
//    }
    if (TypeRelation::isEqual(type, memberType)) {
      return index;
    }

    ++index;
  }

  return -1;
}

Expr * UnionType::createDynamicCast(Expr * from, const Type * toType) const {
  QualifiedType fromType = dealias(from->type());
  if (TypeRelation::isEqual(toType, fromType)) {
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
      if (isSubtypeOfOfAnyMembers(cto)) {
        return new CastExpr(Expr::CheckedUnionMemberCast, from->location(), toType, from);
      }
    }
  }

  // Determine all of the possible member types that could represent an object of
  // type 'toType'.
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    QualifiedType memberType = *it;
    if (TypeConversion::check(memberType, toType)) {
      // TODO: Add additional cast if toType is not exactly the same type as the member.
      return new CastExpr(Expr::CheckedUnionMemberCast, from->location(), toType, from);
    }
  }

  diag.warn(from->location()) << "Union member cast from type '" << fromType << "' to '" <<
      toType << "' can never succeed.";
  return &Expr::ErrorVal;
}

bool UnionType::isSubtypeOfOfAnyMembers(const CompositeType * toType) const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    if (TypeRelation::isSubclass(toType, *it)) {
      return true;
    }
  }

  return false;
}

bool UnionType::isSupertypeOfAllMembers(const CompositeType * toType) const {
  for (TupleType::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    QualifiedType memberType = *it;
    if (!TypeRelation::isSubclass(memberType, toType)) {
      return false;
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

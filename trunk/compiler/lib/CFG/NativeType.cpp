/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/NativeType.h"
#include "tart/CFG/StaticType.h"
#include "tart/CFG/UnitType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Template.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"
#include "tart/Sema/BindingEnv.h"
#include <llvm/DerivedTypes.h>

namespace tart {

// -------------------------------------------------------------------
// AddressType

AddressType AddressType::prototype;
TypeDefn AddressType::typedefn(&Builtins::module, "__Address", NULL);
AddressType::TypeMap AddressType::uniqueTypes_;

void AddressType::initBuiltin() {
  TypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "Target"));

  // Create type parameters
  TemplateSignature * tsig = TemplateSignature::get(&typedefn, &Builtins::module);
  tsig->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.elementType_ = tsig->typeParam(0);
}

AddressType * AddressType::get(const Type * elemType) {
  elemType = dealias(elemType);
  TypeMap::iterator it = uniqueTypes_.find(elemType);
  if (it != uniqueTypes_.end()) {
    return it->second;
  }

  AddressType * addrType = new AddressType(elemType);
  uniqueTypes_[elemType] = addrType;
  return addrType;
}

AddressType::AddressType(const Type * elemType)
  : TypeImpl(Type::NAddress, Shape_Primitive)
  , elementType_(elemType)
{
  DASSERT_OBJ(!isa<UnitType>(elemType), elemType);
}

AddressType::AddressType() : TypeImpl(Type::NAddress, Shape_Primitive) {}

AddressType::~AddressType() {
  /*TypeMap::iterator it = uniqueTypes_.find(elementType_);
  if (it != uniqueTypes_.end()) {
    uniqueTypes_.erase(it);
  }*/
}

const llvm::Type * AddressType::createIRType() const {
  DASSERT_OBJ(elementType_ != NULL, this);
  const llvm::Type * type = elementType_->irEmbeddedType();
  return llvm::PointerType::getUnqual(type);
}

ConversionRank AddressType::convertImpl(const Conversion & cn) const {
  const Type * fromType = dealias(cn.getFromType());
  if (isa<AddressType>(fromType)) {
    const Type * fromElementType = fromType->typeParam(0);
    if (fromElementType == NULL) {
      DFAIL("No element type");
    }

    // For native pointers, the thing pointed to must be identical for
    // both types.

    Expr * fromValue = cn.fromValue;
    ConversionRank rank = Incompatible;
    if (elementType_->isEqual(fromElementType)) {
      rank = IdenticalTypes;
    } else {
      // Check conversion on element types
      if (elementType_->canConvert(fromElementType) == IdenticalTypes) {
        rank = IdenticalTypes;
      }
    }

    if (rank != Incompatible && cn.resultValue) {
      *cn.resultValue = fromValue;
    }

    return rank;
  } else if (const PrimitiveType * ptype = dyn_cast<PrimitiveType>(fromType)) {
    if (ptype->typeId() == TypeId_Null) {
      if (cn.resultValue) {
        *cn.resultValue = cn.fromValue;
      }

      return ExactConversion;
    }

    return Incompatible;
  } else {
    return Incompatible;
  }
}

bool AddressType::isSingular() const {
  return elementType_->isSingular();
}

bool AddressType::isEqual(const Type * other) const {
  if (const AddressType * np = dyn_cast<AddressType>(other)) {
    return elementType_->isEqual(np->elementType_);
  }

  return false;
}

bool AddressType::isSubtype(const Type * other) const {
  if (isEqual(other)) {
    return true;
  }

  return false;
}

void AddressType::format(FormatStream & out) const {
  out << "Address[" << elementType_ << "]";
}

// -------------------------------------------------------------------
// PointerType

PointerType PointerType::prototype;
TypeDefn PointerType::typedefn(&Builtins::module, "__Pointer", NULL);
PointerType::TypeMap PointerType::uniqueTypes_;

void PointerType::initBuiltin() {
  TypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "Target"));

  // Create type parameters
  TemplateSignature * tsig = TemplateSignature::get(&typedefn, &Builtins::module);
  tsig->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.elementType_ = tsig->typeParam(0);
}

PointerType * PointerType::get(const Type * elemType) {
  elemType = dealias(elemType);
  TypeMap::iterator it = uniqueTypes_.find(elemType);
  if (it != uniqueTypes_.end()) {
    return it->second;
  }

  PointerType * addrType = new PointerType(elemType);
  uniqueTypes_[elemType] = addrType;
  return addrType;
}

PointerType::PointerType(const Type * elemType)
  : TypeImpl(Type::NPointer, Shape_Primitive)
  , elementType_(elemType)
{
  DASSERT_OBJ(!isa<UnitType>(elemType), elemType);
}

PointerType::PointerType() : TypeImpl(Type::NPointer, Shape_Primitive) {}

const llvm::Type * PointerType::createIRType() const {
  const llvm::Type * type = elementType_->irEmbeddedType();
  return llvm::PointerType::getUnqual(type);
}

ConversionRank PointerType::convertImpl(const Conversion & cn) const {
  const Type * fromType = dealias(cn.getFromType());
  // Memory addresses can be silently converted to native pointers, but not vice-versa.
  if (isa<AddressType>(fromType) || isa<PointerType>(fromType)) {
    const Type * fromElementType = fromType->typeParam(0);
    if (fromElementType == NULL) {
      DFAIL("No element type");
    }

    // For native pointers, the thing pointed to must be identical for
    // both types.

    Expr * fromValue = cn.fromValue;
    ConversionRank bestRank = isa<AddressType>(fromType) ? ExactConversion : IdenticalTypes;
    ConversionRank rank = Incompatible;
    if (elementType_->isEqual(fromElementType)) {
      rank = bestRank;
    } else {
      // Check conversion on element types
      if (elementType_->canConvert(fromElementType) == IdenticalTypes) {
        rank = bestRank;
      }
    }

    if (rank != Incompatible && cn.resultValue) {
      if (rank == ExactConversion) {
        fromValue = CastExpr::bitCast(fromValue, this);
      }

      *cn.resultValue = fromValue;
    }

    return rank;
  } else if (const PrimitiveType * ptype = dyn_cast<PrimitiveType>(fromType)) {
    if (ptype->typeId() == TypeId_Null) {
      if (cn.resultValue) {
        *cn.resultValue = cn.fromValue;
      }

      return ExactConversion;
    }

    return Incompatible;
  } else {
    return Incompatible;
  }
}

bool PointerType::isSingular() const {
  return elementType_->isSingular();
}

bool PointerType::isEqual(const Type * other) const {
  if (const PointerType * np = dyn_cast<PointerType>(other)) {
    return elementType_->isEqual(np->elementType_);
  }

  return false;
}

bool PointerType::isSubtype(const Type * other) const {
  if (isEqual(other)) {
    return true;
  }

  return false;
}

void PointerType::format(FormatStream & out) const {
  out << "__Pointer[" << elementType_ << "]";
}

// -------------------------------------------------------------------
// NativeArrayType

NativeArrayType NativeArrayType::prototype;
TypeDefn NativeArrayType::typedefn(&Builtins::module, "NativeArray", NULL);
NativeArrayType::TypeMap NativeArrayType::uniqueTypes_;

void NativeArrayType::initBuiltin() {
  TypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "ElementType"));
  typeParams.push_back(new TypeVariable(SourceLocation(), "Length", &IntType::instance));

  // Create type parameters
  TemplateSignature * tsig = TemplateSignature::get(&typedefn, &Builtins::module);
  tsig->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.typeArgs_ = tsig->typeParams();
}

NativeArrayType * NativeArrayType::get(const TupleType * typeArgs) {
  TypeMap::iterator it = uniqueTypes_.find(typeArgs);
  if (it != uniqueTypes_.end()) {
    return it->second;
  }

  NativeArrayType * arrayType = new NativeArrayType(typeArgs);
  uniqueTypes_[typeArgs] = arrayType;
  return arrayType;
}

NativeArrayType::NativeArrayType(const TupleType * typeArgs)
  : TypeImpl(Type::NArray, Shape_Large_Value)
  , typeArgs_(typeArgs)
//  , size_(sz)
{
  DASSERT(!isa<UnitType>((*typeArgs)[0]));
  size_ = cast<ConstantInteger>(cast<UnitType>((*typeArgs)[1])->value())->value()->getZExtValue();
  //DFAIL("Implement sz");
}

NativeArrayType::NativeArrayType() : TypeImpl(Type::NArray, Shape_Large_Value) {}

const Type * NativeArrayType::typeParam(int index) const {
  return (*typeArgs_)[index];
}

const Type * NativeArrayType::elementType() const {
  return (*typeArgs_)[0];
}

const llvm::Type * NativeArrayType::createIRType() const {
  //DASSERT_OBJ(elementType().isNonVoidType(), this);
  return llvm::ArrayType::get(elementType()->irEmbeddedType(), size());
}

ConversionRank NativeArrayType::convertImpl(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (const NativeArrayType * naFrom =
      dyn_cast<NativeArrayType>(fromType)) {
    const Type * fromElementType = naFrom->elementType();
    if (fromElementType == NULL) {
      DFAIL("No element type");
    }

    if (size() != naFrom->size() && size() != 0) {
      return Incompatible;
    }

    // Check conversion on element types
    Conversion elementConversion(dealias(fromElementType));
    if (elementType()->convert(elementConversion) == IdenticalTypes) {
      if (cn.resultValue) {
        *cn.resultValue = cn.fromValue;
      }

      return IdenticalTypes;
    }

    diag.fatal() << Format_Verbose << "Wants to convert from " << fromType << " to " << this;
    //DFAIL("Implement");
    return Incompatible;
  } else {
    return Incompatible;
  }
}

bool NativeArrayType::isSingular() const {
  return typeArgs_->isSingular();
}

bool NativeArrayType::isSubtype(const Type * other) const {
  return isEqual(other);
  //DFAIL("Implement");
}

bool NativeArrayType::isEqual(const Type * other) const {
  if (const NativeArrayType * na = dyn_cast<NativeArrayType>(other)) {
    return typeArgs_ == na->typeArgs_;
  }

  return false;
}

void NativeArrayType::format(FormatStream & out) const {
  out << "NativeArray[" << elementType() << ", " << size() << "]";
}

} // namespace tart

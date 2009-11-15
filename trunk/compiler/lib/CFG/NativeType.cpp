/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/NativeType.h"
#include "tart/CFG/StaticType.h"
#include "tart/CFG/UnitType.h"
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
  // Create type parameters
  TemplateSignature * tsig = TemplateSignature::get(&typedefn, &Builtins::module);
  tsig->addParameter(SourceLocation(), "Target");

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.elementType_ = tsig->params()[0];
}

AddressType * AddressType::get(TypeRef elemType) {
  elemType = dealias(elemType);
  TypeMap::iterator it = uniqueTypes_.find(elemType);
  if (it != uniqueTypes_.end()) {
    return it->second;
  }

  AddressType * addrType = new AddressType(elemType);
  uniqueTypes_[elemType] = addrType;
  return addrType;
}

AddressType::AddressType(const TypeRef & elemType)
  : TypeImpl(Type::NAddress)
  , elementType_(elemType)
{
  DASSERT_OBJ(!isa<UnitType>(elemType.type()), elemType);
}

AddressType::AddressType() : TypeImpl(Type::NAddress) {}

AddressType::~AddressType() {
  /*TypeMap::iterator it = uniqueTypes_.find(elementType_);
  if (it != uniqueTypes_.end()) {
    uniqueTypes_.erase(it);
  }*/
}

const llvm::Type * AddressType::createIRType() const {
  DASSERT_OBJ(elementType_.isDefined(), this);
  const llvm::Type * type = elementType_.irEmbeddedType();
  return llvm::PointerType::getUnqual(type);
}

ConversionRank AddressType::convertImpl(const Conversion & cn) const {
  const Type * fromType = dealias(cn.getFromType());
  if (isa<AddressType>(fromType)) {
    TypeRef fromElementType = fromType->typeParam(0);
    if (fromElementType.isUndefined()) {
      DFAIL("No element type");
    }

    // For native pointers, the thing pointed to must be identical for
    // both types.

    Expr * fromValue = cn.fromValue;
    ConversionRank rank = Incompatible;
    if (elementType_.isEqual(fromElementType)) {
      rank = IdenticalTypes;
    } else {
      // Check conversion on element types
      if (elementType_.canConvert(fromElementType.type()) == IdenticalTypes) {
        rank = IdenticalTypes;
      }
    }

    if (rank != Incompatible && cn.resultValue) {
      *cn.resultValue = fromValue;
    }

    return rank;
  } else {
    return Incompatible;
  }
}

bool AddressType::isSingular() const {
  return elementType_.isSingular();
}

bool AddressType::isEqual(const Type * other) const {
  if (const AddressType * np = dyn_cast<AddressType>(other)) {
    return elementType_.isEqual(np->elementType_);
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
  // Create type parameters
  TemplateSignature * tsig = TemplateSignature::get(&typedefn, &Builtins::module);
  tsig->addParameter(SourceLocation(), "Target");

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.elementType_ = tsig->params()[0];
}

PointerType * PointerType::get(TypeRef elemType) {
  elemType = dealias(elemType);
  TypeMap::iterator it = uniqueTypes_.find(elemType);
  if (it != uniqueTypes_.end()) {
    return it->second;
  }

  PointerType * addrType = new PointerType(elemType);
  uniqueTypes_[elemType] = addrType;
  return addrType;
}

PointerType::PointerType(const TypeRef & elemType)
  : TypeImpl(Type::NPointer)
  , elementType_(elemType)
{
  DASSERT_OBJ(!isa<UnitType>(elemType.type()), elemType);
}

PointerType::PointerType() : TypeImpl(Type::NPointer) {}

const llvm::Type * PointerType::createIRType() const {
  DASSERT_OBJ(elementType_.isDefined(), this);
  const llvm::Type * type = elementType_.irEmbeddedType();
  return llvm::PointerType::getUnqual(type);
}

ConversionRank PointerType::convertImpl(const Conversion & cn) const {
  const Type * fromType = dealias(cn.getFromType());
  // Memory addresses can be silently converted to native pointers, but not vice-versa.
  if (isa<AddressType>(fromType) || isa<PointerType>(fromType)) {
    TypeRef fromElementType = fromType->typeParam(0);
    if (fromElementType.isUndefined()) {
      DFAIL("No element type");
    }

    // For native pointers, the thing pointed to must be identical for
    // both types.

    Expr * fromValue = cn.fromValue;
    ConversionRank bestRank = isa<AddressType>(fromType) ? ExactConversion : IdenticalTypes;
    ConversionRank rank = Incompatible;
    if (elementType_.isEqual(fromElementType)) {
      rank = bestRank;
    } else {
      // Check conversion on element types
      if (elementType_.canConvert(fromElementType.type()) == IdenticalTypes) {
        rank = bestRank;
      }
    }

    if (rank != Incompatible && cn.resultValue) {
      if (rank == ExactConversion) {
        fromValue = new CastExpr(
            Expr::BitCast, SourceLocation(), const_cast<PointerType *>(this), fromValue);
      }

      *cn.resultValue = fromValue;
    }

    return rank;
    //DFAIL("Implement");
  } else {
    return Incompatible;
  }
}

bool PointerType::isSingular() const {
  return elementType_.isSingular();
}

bool PointerType::isEqual(const Type * other) const {
  if (const PointerType * np = dyn_cast<PointerType>(other)) {
    return elementType_.isEqual(np->elementType_);
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

NativeArrayType NativeArrayType::instance(NULL, 0, &NativeArrayType::typedefn, &Builtins::module);
TypeDefn NativeArrayType::typedefn(&Builtins::module, "NativeArray", &instance);

NativeArrayType * NativeArrayType::get(const TypeRef & elemType, uint64_t sz) {
  return create(elemType.type(), sz);
}

NativeArrayType * NativeArrayType::create(Type * elemType, uint64_t sz) {
  // Create the template instance
  TypeRef elemTypeRef(elemType);
  TemplateInstance * tinst = new TemplateInstance(&typedefn, TypeVector::get(elemTypeRef));
  tinst->paramValues().push_back(elemType);
  //tinst->templateArgs().push_back(elemType);

  TypeDefn * tdef = new TypeDefn(&Builtins::module, typedefn.name());
  NativeArrayType * np = new NativeArrayType(elemType, sz, tdef, tinst);
  tdef->setTypeValue(np);
  tdef->addTrait(Defn::Unsafe);
  tdef->setSingular(elemType->isSingular());
  tdef->setTemplateInstance(tinst);
  tdef->createQualifiedName(NULL);
  return np;
}

NativeArrayType::NativeArrayType(Type * elemType, uint64_t sz, TypeDefn * defn,
    Scope * parentScope)
  : DeclaredType(Type::NArray, defn, parentScope)
  , elementType_(elemType)
  , size_(sz)
{
  if (elemType) {
    DASSERT_OBJ(!isa<UnitType>(elemType), elemType);
  }
}

void NativeArrayType::initBuiltin() {
  // Create type parameters
  TemplateSignature * tsig = TemplateSignature::get(&typedefn, &Builtins::module);
  tsig->addParameter(SourceLocation(), "T");

  // Length
  tsig->addParameter(SourceLocation(), "N", &IntType::instance);

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
}

const llvm::Type * NativeArrayType::createIRType() const {
  DASSERT_OBJ(elementType_ != NULL, this);
  return llvm::ArrayType::get(elementType_->irEmbeddedType(), size_);
}

ConversionRank NativeArrayType::convertImpl(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (const NativeArrayType * naFrom =
      dyn_cast<NativeArrayType>(fromType)) {
    Type * fromElementType = naFrom->typeParam(0).type();
    if (fromElementType == NULL) {
      DFAIL("No element type");
    }

    if (size_ != naFrom->size() && size_ != 0) {
      return Incompatible;
    }

    // Check conversion on element types
    Conversion elementConversion(dealias(fromElementType));
    if (elementType_->convert(elementConversion) == IdenticalTypes) {
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
  return elementType_->isSingular();
}

bool NativeArrayType::isSubtype(const Type * other) const {
  return isEqual(other);
  //DFAIL("Implement");
}

bool NativeArrayType::isEqual(const Type * other) const {
  if (const NativeArrayType * na = dyn_cast<NativeArrayType>(other)) {
    if (elementType_->isEqual(na->elementType_)) {
      return size() == na->size();
    }
  }

  return false;
}

void NativeArrayType::format(FormatStream & out) const {
  out << "NativeArray[" << elementType_ << ", " << size_ << "]";
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/NativeType.h"
#include "tart/CFG/StaticType.h"
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

AddressType * AddressType::get(const TypeRef & elemType) {
  return get(elemType.type());
}

AddressType * AddressType::get(Type * elemType) {
  elemType = dealias(elemType);
  TypeMap::iterator it = uniqueTypes_.find(elemType);
  if (it != uniqueTypes_.end()) {
    return it->second;
  }

  AddressType * addrType = new AddressType(elemType);
  uniqueTypes_[elemType] = addrType;
  return addrType;
}

AddressType::AddressType(Type * elemType)
  : TypeImpl(Type::Address)
  , elementType_(elemType)
{
  DASSERT_OBJ(!isa<NonTypeConstant>(elemType), elemType);
}

AddressType::AddressType()
  : TypeImpl(Type::Address)
  , elementType_(NULL)
{
}

AddressType::~AddressType() {
  //TypeMap::iterator it = uniqueTypes_.find(elementType_);
  //if (it != uniqueTypes_.end()) {
  //  uniqueTypes_.erase(it);
  //}
}

const llvm::Type * AddressType::createIRType() const {
  DASSERT_OBJ(elementType_.isDefined(), this);
  const llvm::Type * type = elementType_.irEmbeddedType();
  return llvm::PointerType::getUnqual(type);
}

ConversionRank AddressType::convertImpl(const Conversion & cn) const {
  const Type * fromType = dealias(cn.getFromType());
  if (isa<AddressType>(fromType)) {
    Type * fromElementType = fromType->typeParam(0).type();
    if (fromElementType == NULL) {
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
      Conversion elementConversion(dealias(fromElementType));
      elementConversion.bindingEnv = cn.bindingEnv;
      if (elementType_.convert(elementConversion) == IdenticalTypes) {
        rank = IdenticalTypes;
      }
    }

    if (rank != Incompatible && cn.resultValue) {
      *cn.resultValue = fromValue;
    }

    //diag.debug() << Format_Verbose <<
    //    "Wants to convert from " << elementConversion.fromType << " to " <<
    //    elementType_ << " [" << elementType_->convert(elementConversion) << "]";
    return rank;
    //DFAIL("Implement");
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
// NativePointerType

NativePointerType NativePointerType::instance(NULL, &NativePointerType::typedefn,
    &Builtins::module);
TypeDefn NativePointerType::typedefn(&Builtins::module, "NativePointer", &instance);

// TODO: Constant folding.
//NativePointerType * NativePointerType::get(Type * elemType) {
//  DASSERT(elemType != NULL);
//  NativePointerType * result = new NativePointerType(&typedefn);
//  result->setTypeArg(0, elemType);
//  return result;
//}

NativePointerType * NativePointerType::get(const TypeRef & elemType) {
  return create(elemType.type());
}

NativePointerType * NativePointerType::create(Type * elemType) {
  // Create the template instance
  TemplateInstance * tinst = new TemplateInstance(&typedefn);
  tinst->paramValues().push_back(elemType);
  tinst->templateArgs().push_back(elemType);

  TypeDefn * tdef = new TypeDefn(&Builtins::module, typedefn.name());
  NativePointerType * np = new NativePointerType(elemType, tdef, tinst);
  tdef->setTypeValue(np);
  tdef->addTrait(Defn::Unsafe);
  tdef->setSingular(elemType->isSingular());
  tdef->setTemplateInstance(tinst);
  tdef->createQualifiedName(NULL);
  return np;
}

NativePointerType::NativePointerType(Type * elemType, TypeDefn * defn,
    Scope * parentScope)
  : DeclaredType(Type::NativePointer, defn, parentScope)
  , elementType_(elemType)
{
  if (elemType) {
    DASSERT_OBJ(!isa<NonTypeConstant>(elemType), elemType);
  }
}

void NativePointerType::initBuiltin() {
  // Create type parameters
  TemplateSignature * tsig = TemplateSignature::get(&typedefn, &Builtins::module);
  tsig->addParameter(SourceLocation(), "TargetType");

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
}

const llvm::Type * NativePointerType::createIRType() const {
  DASSERT_OBJ(elementType_ != NULL, this);
  const llvm::Type * type = elementType_->irEmbeddedType();
  return llvm::PointerType::getUnqual(type);
}

ConversionRank NativePointerType::convertImpl(const Conversion & cn) const {
  const Type * fromType = dealias(cn.getFromType());
  // Memory addresses can be silently converted to native pointers, but not vice-versa.
  if (isa<AddressType>(fromType) || isa<NativePointerType>(fromType)) {
    Type * fromElementType = fromType->typeParam(0).type();
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
      Conversion elementConversion(dealias(fromElementType));
      elementConversion.bindingEnv = cn.bindingEnv;
      if (elementType_->convert(elementConversion) == IdenticalTypes) {
        rank = bestRank;
      }
    }

    if (rank != Incompatible && cn.resultValue) {
      if (rank == ExactConversion) {
        fromValue = new CastExpr(
            Expr::BitCast, SourceLocation(), const_cast<NativePointerType *>(this), fromValue);
      }

      *cn.resultValue = fromValue;
    }

    //diag.debug() << Format_Verbose <<
    //    "Wants to convert from " << elementConversion.fromType << " to " <<
    //    elementType_ << " [" << elementType_->convert(elementConversion) << "]";
    return rank;
    //DFAIL("Implement");
  } else {
    return Incompatible;
  }
}

bool NativePointerType::isSingular() const {
  return elementType_->isSingular();
}

bool NativePointerType::isEqual(const Type * other) const {
  if (const NativePointerType * np = dyn_cast<NativePointerType>(other)) {
    return elementType_->isEqual(np->elementType_);
  }

  return false;
}

bool NativePointerType::isSubtype(const Type * other) const {
  if (isEqual(other)) {
    return true;
  }

  return false;
}

void NativePointerType::format(FormatStream & out) const {
  out << "NativePointer[" << elementType_ << "]";
}

// -------------------------------------------------------------------
// NativeArrayType

NativeArrayType NativeArrayType::instance(NULL, 0, &NativeArrayType::typedefn, &Builtins::module);
TypeDefn NativeArrayType::typedefn(&Builtins::module, "NativeArray", &instance);

/*NativeArrayType * NativeArrayType::get(Type * elemType, uint64_t size) {
  DASSERT(elemType != NULL);
  NativeArrayType * result = new NativeArrayType(&typedefn);
  result->setTypeArg(0, elemType);
  result->setSize(size);
  return result;
}*/

NativeArrayType * NativeArrayType::get(const TypeRef & elemType, uint64_t sz) {
  return create(elemType.type(), sz);
}

NativeArrayType * NativeArrayType::create(Type * elemType, uint64_t sz) {
  // Create the template instance
  TemplateInstance * tinst = new TemplateInstance(&typedefn);
  tinst->paramValues().push_back(elemType);
  tinst->templateArgs().push_back(elemType);

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
  : DeclaredType(Type::NativeArray, defn, parentScope)
  , elementType_(elemType)
  , size_(sz)
{
  if (elemType) {
    DASSERT_OBJ(!isa<NonTypeConstant>(elemType), elemType);
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
    elementConversion.bindingEnv = cn.bindingEnv;
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

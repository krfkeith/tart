/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Module.h"
#include "tart/Defn/Template.h"

#include "tart/Type/NativeType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/UnitType.h"
#include "tart/Type/TupleType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/DerivedTypes.h"
#include "llvm/Support/CommandLine.h"

namespace tart {

namespace {
  /** Code to clear the cached type maps during a collection. */
  template<class TypeMap>
  class TypeMapRoot : public GCRootBase {
  public:
    TypeMapRoot(TypeMap & typeMap) : typeMap_(typeMap) {}

  private:
    void trace() const {
      for (typename TypeMap::const_iterator it = typeMap_.begin(); it != typeMap_.end(); ++it) {
        it->first->mark();
        it->second->mark();
      }
    }

    TypeMap & typeMap_;
  };
}

// -------------------------------------------------------------------
// AddressType

AddressType AddressType::prototype;
TypeDefn AddressType::typedefn(&Builtins::module, "__Address", NULL);
AddressType::TypeMap AddressType::uniqueTypes_;
ASTBuiltIn AddressType::biDef(&AddressType::typedefn);

void AddressType::initBuiltin() {
  // Make the type map a garbage collection root
  static TypeMapRoot<TypeMap> root(uniqueTypes_);

  TypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "Target"));

  // Create type parameters
  Template * tm = Template::get(&typedefn, &Builtins::module);
  tm->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.elementType_ = tm->typeParam(0);
}

AddressType * AddressType::get(const Type * elemType) {
  if (elemType == NULL) {
    return NULL;
  }
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

AddressType::~AddressType() {}

const llvm::Type * AddressType::createIRType() const {
  DASSERT_OBJ(elementType_ != NULL, this);
  if (elementType_->isVoidType()) {
    return llvm::Type::getInt8PtrTy(llvm::getGlobalContext());
  }
  const llvm::Type * type = elementType_->irEmbeddedType();
  return type->getPointerTo();
}

ConversionRank AddressType::convertImpl(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (isa<AddressType>(fromType)) {
    const Type * fromElementType = dealias(fromType->typeParam(0));
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
        *cn.resultValue = ConstantNull::get(cn.fromValue->location(), this);
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

Expr * AddressType::nullInitValue() const {
  return ConstantNull::get(SourceLocation(), this);
}

unsigned AddressType::getHashValue() const {
  unsigned result = elementType_->getHashValue();
  result ^= Type::NAddress;
  return result;
}

void AddressType::format(FormatStream & out) const {
  out << elementType_ << "^";
}

void AddressType::trace() const {
  Type::trace();
  safeMark(elementType_);
}

// -------------------------------------------------------------------
// NativeArrayType

NativeArrayType NativeArrayType::prototype;
TypeDefn NativeArrayType::typedefn(&Builtins::module, "NativeArray", NULL);
NativeArrayType::TypeMap NativeArrayType::uniqueTypes_;

void NativeArrayType::initBuiltin() {
  // Make the type map a garbage collection root
  static TypeMapRoot<TypeMap> root(uniqueTypes_);

  TypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "ElementType"));
  typeParams.push_back(new TypeVariable(SourceLocation(), "Length", &Int32Type::instance));

  // Create type parameters
  Template * tm = Template::get(&typedefn, &Builtins::module);
  tm->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.typeArgs_ = tm->typeParams();
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

const llvm::Type * NativeArrayType::irParameterType() const {
  return irType()->getPointerTo();
}

ConversionRank NativeArrayType::convertImpl(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (const NativeArrayType * naFrom = dyn_cast<NativeArrayType>(fromType)) {
    const Type * fromElementType = dealias(naFrom->elementType());
    if (fromElementType == NULL) {
      DFAIL("No element type");
    }

    if (size() != naFrom->size() && size() != 0) {
      return Incompatible;
    }

    // Check conversion on element types
    Conversion elementConversion(fromElementType);
    if (elementType()->convert(elementConversion) == IdenticalTypes) {
      if (cn.resultValue) {
        *cn.resultValue = cn.fromValue;
      }

      return IdenticalTypes;
    }

    diag.fatal() << Format_Verbose << "Wants to convert from " << fromType << " to " << this;
    //DFAIL("Implement");
    return Incompatible;
  } else if (const CompositeType * cfrom = dyn_cast<CompositeType>(fromType)) {
    // Special case for initializing a native type from an array literal.
    if (cfrom->typeDefn()->ast() == Builtins::typeArray->typeDefn()->ast()) {
      Conversion elementConversion(cfrom->typeParam(0));
      if (elementType()->convert(elementConversion) == IdenticalTypes) {
        if (cn.resultValue) {
          *cn.resultValue = cn.fromValue;
        }

        return IdenticalTypes;
      }
    }
    return Incompatible;
  } else {
    return Incompatible;
  }
}

bool NativeArrayType::isSingular() const {
  return typeArgs_->isSingular();
}

bool NativeArrayType::isEqual(const Type * other) const {
  if (const NativeArrayType * na = dyn_cast<NativeArrayType>(other)) {
    return typeArgs_ == na->typeArgs_;
  }

  return false;
}

unsigned NativeArrayType::getHashValue() const {
  unsigned result = elementType()->getHashValue();
  result ^= Type::NArray;
  return result;
}

void NativeArrayType::format(FormatStream & out) const {
  out << "NativeArray[" << elementType() << ", " << size() << "]";
}

void NativeArrayType::trace() const {
  Type::trace();
  safeMark(typeArgs_);
}

// -------------------------------------------------------------------
// FlexibleArrayType

FlexibleArrayType FlexibleArrayType::prototype;
TypeDefn FlexibleArrayType::typedefn(&Builtins::module, "FlexibleArray", NULL);
FlexibleArrayType::TypeMap FlexibleArrayType::uniqueTypes_;

void FlexibleArrayType::initBuiltin() {
  // Make the type map a garbage collection root
  static TypeMapRoot<TypeMap> root(uniqueTypes_);

  TypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "ElementType"));

  // Create type parameters
  Template * tm = Template::get(&typedefn, &Builtins::module);
  tm->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.typeArgs_ = tm->typeParams();
}

FlexibleArrayType * FlexibleArrayType::get(const TupleType * typeArgs) {
  TypeMap::iterator it = uniqueTypes_.find(typeArgs);
  if (it != uniqueTypes_.end()) {
    return it->second;
  }

  FlexibleArrayType * arrayType = new FlexibleArrayType(typeArgs);
  uniqueTypes_[typeArgs] = arrayType;
  return arrayType;
}

FlexibleArrayType::FlexibleArrayType(const TupleType * typeArgs)
  : TypeImpl(Type::FlexibleArray, Shape_Large_Value)
  , typeArgs_(typeArgs)
{
  DASSERT(!isa<UnitType>((*typeArgs)[0]));
}

FlexibleArrayType::FlexibleArrayType() : TypeImpl(Type::FlexibleArray, Shape_Large_Value) {}

const Type * FlexibleArrayType::typeParam(int index) const {
  return (*typeArgs_)[index];
}

const Type * FlexibleArrayType::elementType() const {
  return (*typeArgs_)[0];
}

const llvm::Type * FlexibleArrayType::createIRType() const {
  return llvm::ArrayType::get(elementType()->irEmbeddedType(), 0);
}

ConversionRank FlexibleArrayType::convertImpl(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (const FlexibleArrayType * naFrom =
      dyn_cast<FlexibleArrayType>(fromType)) {
    const Type * fromElementType = dealias(naFrom->elementType());
    if (fromElementType == NULL) {
      DFAIL("No element type");
    }

    // Check conversion on element types
    Conversion elementConversion(fromElementType);
    if (elementType()->convert(elementConversion) == IdenticalTypes) {
      if (cn.resultValue) {
        *cn.resultValue = cn.fromValue;
      }

      return IdenticalTypes;
    }

    diag.fatal() << Format_Verbose << "Wants to convert from " << fromType << " to " << this;
    return Incompatible;
  } else {
    return Incompatible;
  }
}

bool FlexibleArrayType::isSingular() const {
  return typeArgs_->isSingular();
}

bool FlexibleArrayType::isEqual(const Type * other) const {
  if (const FlexibleArrayType * na = dyn_cast<FlexibleArrayType>(other)) {
    return typeArgs_ == na->typeArgs_;
  }

  return false;
}

unsigned FlexibleArrayType::getHashValue() const {
  unsigned result = elementType()->getHashValue();
  result ^= Type::FlexibleArray;
  return result;
}

void FlexibleArrayType::format(FormatStream & out) const {
  out << "FlexibleArray[" << elementType() << "]";
}

void FlexibleArrayType::trace() const {
  Type::trace();
  safeMark(typeArgs_);
}


} // namespace tart

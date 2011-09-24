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
#include "tart/Type/TypeRelation.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/DerivedTypes.h"
#include "llvm/Support/CommandLine.h"

namespace tart {

namespace {
  /** Code to trace cached type references. */
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

  QualifiedTypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "Target"));

  // Create type parameters
  Template * tm = Template::get(&typedefn, &Builtins::module);
  tm->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setValue(&prototype);
  typedefn.addTrait(Defn::Unsafe);

  prototype.elementType_ = tm->typeParam(0);
}

AddressType * AddressType::get(QualifiedType elemType) {
  if (elemType.isNull()) {
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

AddressType::AddressType(QualifiedType elemType)
  : TypeImpl(Type::NAddress, Shape_Primitive)
  , elementType_(elemType)
{
  DASSERT(!elemType.isa<UnitType>());
}

AddressType::AddressType() : TypeImpl(Type::NAddress, Shape_Primitive) {}

AddressType::~AddressType() {}

llvm::Type * AddressType::createIRType() const {
  DASSERT_OBJ(!elementType_.isNull(), this);
  if (elementType_->isVoidType()) {
    return llvm::Type::getInt8PtrTy(llvm::getGlobalContext());
  }
  llvm::Type * type = elementType_->irEmbeddedType();
  return type->getPointerTo();
}

bool AddressType::isSingular() const {
  return elementType_->isSingular();
}

Expr * AddressType::nullInitValue() const {
  return ConstantNull::get(SourceLocation(), this);
}

void AddressType::format(FormatStream & out) const {
  out << elementType_ << "^";
}

void AddressType::trace() const {
  Type::trace();
  safeMark(elementType_.unqualified());
}

// -------------------------------------------------------------------
// NativeArrayType

NativeArrayType NativeArrayType::prototype;
TypeDefn NativeArrayType::typedefn(&Builtins::module, "NativeArray", NULL);
NativeArrayType::TypeMap NativeArrayType::uniqueTypes_;

void NativeArrayType::initBuiltin() {
  // Make the type map a garbage collection root
  static TypeMapRoot<TypeMap> root(uniqueTypes_);

  QualifiedTypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "ElementType"));
  typeParams.push_back(new TypeVariable(SourceLocation(), "Length", &Int32Type::instance));

  // Create type parameters
  Template * tm = Template::get(&typedefn, &Builtins::module);
  tm->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setValue(&prototype);
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
{
  DASSERT(!(*typeArgs)[0].isa<UnitType>());
  size_ = cast<ConstantInteger>((*typeArgs)[1].cast<UnitType>()->value())->value()->getZExtValue();
}

NativeArrayType::NativeArrayType() : TypeImpl(Type::NArray, Shape_Large_Value) {}

QualifiedType NativeArrayType::typeParam(int index) const {
  return (*typeArgs_)[index];
}

QualifiedType NativeArrayType::elementType() const {
  return (*typeArgs_)[0];
}

llvm::Type * NativeArrayType::createIRType() const {
  return llvm::ArrayType::get(elementType()->irEmbeddedType(), size());
}

llvm::Type * NativeArrayType::irParameterType() const {
  return irType()->getPointerTo();
}

bool NativeArrayType::isSingular() const {
  return typeArgs_->isSingular();
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

  QualifiedTypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "ElementType"));

  // Create type parameters
  Template * tm = Template::get(&typedefn, &Builtins::module);
  tm->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setValue(&prototype);
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
  DASSERT(!(*typeArgs)[0].isa<UnitType>());
}

FlexibleArrayType::FlexibleArrayType() : TypeImpl(Type::FlexibleArray, Shape_Large_Value) {}

QualifiedType FlexibleArrayType::typeParam(int index) const {
  return (*typeArgs_)[index];
}

QualifiedType FlexibleArrayType::elementType() const {
  return (*typeArgs_)[0];
}

llvm::Type * FlexibleArrayType::createIRType() const {
  return llvm::ArrayType::get(elementType()->irEmbeddedType(), 0);
}

bool FlexibleArrayType::isSingular() const {
  return typeArgs_->isSingular();
}

void FlexibleArrayType::format(FormatStream & out) const {
  out << "FlexibleArray[" << elementType() << "]";
}

void FlexibleArrayType::trace() const {
  Type::trace();
  safeMark(typeArgs_);
}


} // namespace tart

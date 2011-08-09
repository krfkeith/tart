/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/StructBuilder.h"
#include "tart/Gen/CodeGenerator.h"

#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/VariableDefn.h"
#include "tart/Type/CompositeType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

using namespace llvm;

StructBuilder::StructBuilder(CodeGenerator & gen) : gen_(gen) {}

StructBuilder & StructBuilder::createObjectHeader(const Type * type) {
  ConstantList objMembers;
  objMembers.push_back(gen_.getTypeInfoBlockPtr(cast<CompositeType>(type)));
  objMembers.push_back(gen_.getIntVal(0));
  StructType * objectStructType = cast<StructType>(Builtins::typeObject->irTypeComplete());
  members_.push_back(ConstantStruct::get(objectStructType, objMembers));
  return *this;
}

StructBuilder & StructBuilder::addField(llvm::Constant * value) {
  DASSERT(value->getType() != NULL);
  members_.push_back(value);
  return *this;
}

StructBuilder & StructBuilder::addNullField(const Type * type) {
  llvm::PointerType * irType = cast<llvm::PointerType>(type->irEmbeddedType());
  return addField(ConstantPointerNull::get(irType));
}

StructBuilder & StructBuilder::addIntegerField(const Type * type, int32_t value) {
  members_.push_back(ConstantInt::get(cast<IntegerType>(type->irType()), value, true));
  return *this;
}

StructBuilder & StructBuilder::addIntegerField(VariableDefn * var, int32_t value) {
  return addIntegerField(var->type(), value);
}

StructBuilder & StructBuilder::addStringField(StringRef strval) {
  return addField(gen_.genStringLiteral(strval));
}

StructBuilder & StructBuilder::addPointerField(VariableDefn * var, llvm::Constant * value) {
  return addField(llvm::ConstantExpr::getPointerCast(value, var->type()->irEmbeddedType()));
}

StructBuilder & StructBuilder::addArrayField(
    const Type * elementType, llvm::ArrayRef<llvm::Constant *> values) {
  ArrayType * arrayType = ArrayType::get(elementType->irEmbeddedType(), values.size());
  return addField(ConstantArray::get(arrayType, values));
}

StructBuilder & StructBuilder::addArrayField(
    const VariableDefn * arrayVar, llvm::ArrayRef<llvm::Constant *> values) {
  if (const CompositeType * arrayType = dyn_cast<CompositeType>(arrayVar->type())) {
    addArrayField(arrayType->typeParam(0), values);
  } else {
    DFAIL("Not an array type");
  }

  return *this;
}

StructBuilder & StructBuilder::combine(const Type * type) {
  Constant * c = ConstantStruct::get(cast<StructType>(type->irType()), members_);
  members_.clear();
  members_.push_back(c);
  return *this;
}


llvm::Constant * StructBuilder::buildAnon() const {
  return ConstantStruct::getAnon(gen_.context(), members_, false);
}

llvm::Constant * StructBuilder::build(llvm::Type * expectedType) const {
  StructType * objectStructType = cast<StructType>(expectedType);
  return ConstantStruct::get(objectStructType, members_);
}

llvm::Constant * StructBuilder::build(const CompositeType * expectedType) const {
  return build(expectedType->irTypeComplete());
}

llvm::Constant * StructBuilder::buildBody(llvm::StructType * stype) const {
  SmallVector<llvm::Type *, 16> memberTypes;
  for (SmallVectorImpl<llvm::Constant *>::const_iterator it = members_.begin(),
      itEnd = members_.end(); it != itEnd; ++it) {
    memberTypes.push_back((*it)->getType());
  }

  DASSERT(stype->isOpaque());
  stype->setBody(memberTypes, false);
  return ConstantStruct::get(stype, members_);
}


} // namespace tart

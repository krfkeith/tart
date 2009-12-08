/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/TypeLiteral.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/UnitType.h"
#include "tart/Objects/Builtins.h"

namespace tart {

// -------------------------------------------------------------------
// TypeLiteralType

TypeLiteralType TypeLiteralType::prototype;
TypeDefn TypeLiteralType::typedefn(&Builtins::module, "TypeLiteral", NULL);
TypeLiteralType::TypeMap TypeLiteralType::uniqueTypes_;

void TypeLiteralType::initBuiltin() {
  // Create type parameters
  TypeList typeParams;
  typeParams.push_back(new PatternVar(SourceLocation(), "T"));
  TemplateSignature * tsig = TemplateSignature::get(&typedefn, &Builtins::module);
  tsig->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);
  typedefn.addTrait(Defn::Nonreflective);

  prototype.literalType_ = tsig->typeParam(0);
}

TypeLiteralType * TypeLiteralType::get(const Type * literalType) {
  DASSERT(literalType != NULL);
  literalType = dealias(literalType);
  TypeMap::iterator it = uniqueTypes_.find(literalType);
  if (it != uniqueTypes_.end()) {
    return it->second;
  }

  TypeLiteralType * addrType = new TypeLiteralType(literalType);
  uniqueTypes_[literalType] = addrType;
  return addrType;
}

TypeLiteralType::TypeLiteralType(const Type * literalType)
  : TypeImpl(Type::TypeLiteral)
  , literalType_(literalType)
{
  DASSERT_OBJ(!isa<UnitType>(literalType), literalType);
}

TypeLiteralType::TypeLiteralType() : TypeImpl(Type::TypeLiteral) {}

TypeLiteralType::~TypeLiteralType() {
  /*TypeMap::iterator it = uniqueTypes_.find(literalType_);
  if (it != uniqueTypes_.end()) {
    uniqueTypes_.erase(it);
  }*/
}

const llvm::Type * TypeLiteralType::createIRType() const {
//  DASSERT_OBJ(literalType_ != NULL, this);
//  const llvm::Type * type = literalType_->irEmbeddedType();
//  return llvm::PointerType::getUnqual(type);
}

ConversionRank TypeLiteralType::convertImpl(const Conversion & cn) const {
  const Type * fromType = dealias(cn.getFromType());
  if (isa<TypeLiteralType>(fromType)) {
    const Type * fromliteralType = fromType->typeParam(0);
    DASSERT(fromliteralType != NULL);

    if (literalType_->isEqual(fromliteralType)) {
      if (cn.resultValue) {
        *cn.resultValue = cn.fromValue;
      }

      return IdenticalTypes;
    }
  }
  return Incompatible;
}

bool TypeLiteralType::isSingular() const {
  return literalType_->isSingular();
}

bool TypeLiteralType::isEqual(const Type * other) const {
  if (const TypeLiteralType * np = dyn_cast<TypeLiteralType>(other)) {
    return literalType_->isEqual(np->literalType_);
  }

  return false;
}

bool TypeLiteralType::isSubtype(const Type * other) const {
  return isEqual(other);
}

void TypeLiteralType::format(FormatStream & out) const {
  out << "TypeLiteral[" << literalType_ << "]";
}


} // namespace tart

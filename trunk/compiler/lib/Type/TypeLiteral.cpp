/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"
#include "tart/Defn/Module.h"

#include "tart/Type/TypeLiteral.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/UnitType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

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
// TypeLiteralType

TypeLiteralType TypeLiteralType::prototype;
TypeDefn TypeLiteralType::typedefn(&Builtins::module, "TypeLiteral", NULL);
TypeLiteralType::TypeMap TypeLiteralType::uniqueTypes_;

void TypeLiteralType::initBuiltin() {
  // Make the type map a garbage collection root
  static TypeMapRoot<TypeMap> root(uniqueTypes_);

  // Create type parameters
  QualifiedTypeList typeParams;
  typeParams.push_back(new TypeVariable(SourceLocation(), "T"));
  Template * tm = Template::get(&typedefn, &Builtins::module);
  tm->setTypeParams(TupleType::get(typeParams));

  // Add to builtin name space
  Builtins::module.addMember(&typedefn);
  typedefn.setQualifiedName(typedefn.name());
  typedefn.setTypeValue(&prototype);

  prototype.literalType_ = tm->typeParam(0).type();
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
  : TypeImpl(Type::TypeLiteral, Shape_None)
  , literalType_(literalType)
{
  DASSERT_OBJ(!isa<UnitType>(literalType), literalType);
}

TypeLiteralType::TypeLiteralType() : TypeImpl(Type::TypeLiteral, Shape_None) {}

TypeLiteralType::~TypeLiteralType() {
  /*TypeMap::iterator it = uniqueTypes_.find(literalType_);
  if (it != uniqueTypes_.end()) {
    uniqueTypes_.erase(it);
  }*/
}

llvm::Type * TypeLiteralType::createIRType() const {
  return Builtins::typeType.irEmbeddedType();
}

bool TypeLiteralType::isSingular() const {
  return literalType_->isSingular();
}

void TypeLiteralType::format(FormatStream & out) const {
  out << "TypeLiteral[" << literalType_ << "]";
}

} // namespace tart

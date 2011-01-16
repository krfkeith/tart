/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"

#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/NamespaceDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/PropertyDefn.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/TypeLiteral.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/UnitType.h"
#include "tart/CFG/TypeOrdering.h"

#include "tart/Meta/Tags.h"
#include "tart/Meta/VarInt.h"

#include "tart/Gen/ReflectionMetadata.h"
#include "tart/Gen/Reflector.h"

namespace tart {

namespace {

struct TypeOrder {
  bool operator()(
      const ReflectionMetadata::TypeArrayElement & t0,
      const ReflectionMetadata::TypeArrayElement & t1) {
    if (t0.second.useCount > t1.second.useCount) return true;
    if (t0.second.useCount < t1.second.useCount) return false;
    return LexicalTypeOrdering::compare(t0.first, t1.first) > 0;
  }
};

}

/// -------------------------------------------------------------------
/// ReflectionMetadata

void ReflectionMetadata::addTypeRef(const Type * type) {
  DASSERT(type != NULL);
  TypeMap::iterator it = types_.find(type);
  if (it != types_.end()) {
    it->second.useCount++;
    return;
  }

  switch (type->typeClass()) {
    case Type::Primitive:
    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol:
    case Type::Enum:
    case Type::Function:
    case Type::Union:
    case Type::Tuple:
    case Type::NAddress:
    case Type::NArray:
    case Type::FlexibleArray:
    case Type::Unit:
      types_[type] = TagInfo(1);
      break;

    case Type::TypeLiteral: {
      const TypeLiteralType * ttype = static_cast<const TypeLiteralType *>(type);
//      return visitTypeLiteralType(static_cast<const TypeLiteralType *>(in));
      break;
    }

    case Type::Alias:
      DFAIL("Not handled");
      break;

    case Type::TypeVar: {
      const TypeVariable * typeVar = static_cast<const TypeVariable *>(type);
      //types_[typeVar] = TagInfo(1);
      //mmd_.names().addName(typeVar->name())->use();
      break;
    }

    default:
      diag.fatal() << "Type class not handled: " << type->typeClass();
  }
}

size_t ReflectionMetadata::addGlobalRef(llvm::Constant * globalPtr) {
  size_t index = globalRefs_.size();
  globalRefs_.push_back(globalPtr);
  return index;
}

void ReflectionMetadata::assignIndices() {
  for (TypeMap::iterator it = types_.begin(); it != types_.end(); ++it) {
    const Type * type = it->first;
    typeRefs_.push_back(*it);
  }

  std::sort(typeRefs_.begin(), typeRefs_.end(), TypeOrder());
  for (unsigned i = 0; i < typeRefs_.size(); ++i) {
    typeRefs_[i].second.index = i;
    types_[typeRefs_[i].first].index = i;
  }
}

void ReflectionMetadata::dump() const {
  diag.debug() << "Reflection metadata for " << reflectedDefn_->linkageName();
  diag.indent();
  if (!typeRefs_.empty()) {
    diag.debug() << typeRefs_.size() << " unique types added";
    diag.indent();
    for (TypeArray::const_iterator it = typeRefs_.begin(); it != typeRefs_.end(); ++it) {
      diag.debug() << Format_Verbose << it->second.index << " " << it->first <<
          " (" << it->second.useCount << ")";
    }
    diag.unindent();
  }

  diag.unindent();
}

void ReflectionMetadata::encodeTypeRef(const Type * type, llvm::raw_ostream & out) {
  DASSERT(type != NULL);
  TypeMap::iterator it = types_.find(type);
  if (it == types_.end()) {
    diag.fatal() << "Attempt to encode unregistered type: " << type;
  } else {
    out << VarInt(it->second.index);
  }
}


} // namespace tart


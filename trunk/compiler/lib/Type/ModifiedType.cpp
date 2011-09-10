/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/ModifiedType.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

namespace {
  // Use for creating a map of modified types.
  typedef std::pair<const Type *, Type::TypeClass> KeyType;

  // Structure used when using type ref as a key.
  struct KeyInfo {
    static inline KeyType getEmptyKey() {
      return KeyType(Type::KeyInfo::getEmptyKey(), Type::ModVolatile);
    }

    static inline KeyType getTombstoneKey() {
      return KeyType(Type::KeyInfo::getTombstoneKey(), Type::ModVolatile);
    }

    static unsigned getHashValue(const KeyType & val) {
      return Type::KeyInfo::getHashValue(val.first) ^ unsigned(val.second);
    }

    static bool isEqual(const KeyType & lhs, const KeyType & rhs) {
      return lhs.first == rhs.first && lhs.second == rhs.second;
    }

    static bool isPod() { return true; }
  };

  // Cache of modified types.
  typedef llvm::DenseMap<KeyType, ModifiedType *, KeyInfo> TypeMap;
  static TypeMap uniqueTypes;

  // Class to trace cached type references.
  class TypeMapRoot : public GCRootBase {
  private:
    void trace() const {
      for (TypeMap::const_iterator it = uniqueTypes.begin(); it != uniqueTypes.end(); ++it) {
        it->first.first->mark();
        it->second->mark();
      }
    }
  };
}

// -------------------------------------------------------------------
// ModifiedType

ModifiedType * ModifiedType::get(TypeClass cls, const Type * baseType) {
  static TypeMapRoot root;
  if (baseType == NULL) {
    return NULL;
  }

  baseType = dealias(baseType);
  KeyType key = KeyType(baseType, cls);
  TypeMap::iterator it = uniqueTypes.find(key);
  if (it != uniqueTypes.end()) {
    return it->second;
  }

  ModifiedType * mType = new ModifiedType(cls, baseType);
  uniqueTypes[key] = mType;
  return mType;
}

ModifiedType::ModifiedType(TypeClass cls, const Type * baseType)
  : Type(cls)
  , baseType_(baseType)
{
  DASSERT(cls >= ModifiersBegin && cls <= ModifiersEnd);
}

void ModifiedType::format(FormatStream & out) const {
  switch (int(typeClass())) {
  case ModVariadic:
    out << baseType_ << " ... ";
    break;

  case ModMutable:
    out << "mutable(" << baseType_ << ")";
    break;

  case ModImmutable:
    out << "immutable(" << baseType_ << ")";
    break;

  case ModReadOnly:
    out << "readonly(" << baseType_ << ")";
    break;

  case ModAdopted:
    out << "adopted(" << baseType_ << ")";
    break;

  case ModVolatile:
    out << "volatile(" << baseType_ << ")";
    break;
  }
}

} // namespace tart

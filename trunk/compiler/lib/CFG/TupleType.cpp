/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/TupleType.h"
//#include "tart/CFG/Module.h"
//#include "tart/CFG/PrimitiveType.h"
//#include "tart/CFG/CompositeType.h"
//#include "tart/CFG/TupleType.h"
#include "tart/Common/Diagnostics.h"
//#include "tart/Objects/Builtins.h"

namespace tart {

namespace {
  /// -------------------------------------------------------------------
  /// Represents a sub-range of a list of type references.

  typedef std::pair<TypeList::const_iterator, TypeList::const_iterator> TypeTupleKey;

  struct TypeTupleKeyInfo {
    static inline TypeTupleKey getEmptyKey() {
      return TypeTupleKey(&emptyKey, &emptyKey + 1);
    }

    static inline TypeTupleKey getTombstoneKey() {
      return TypeTupleKey(&tombstoneKey, &tombstoneKey + 1);
    }

    static unsigned getHashValue(const TypeTupleKey & key) {
      unsigned result = 0;
      for (TypeList::const_iterator it = key.first; it != key.second; ++it) {
        result *= 0x5bd1e995;
        result ^= result >> 24;
        result ^= Type::KeyInfo::getHashValue(*it);
      }

      return result;
    }

    static bool isEqual(const TypeTupleKey & lhs, const TypeTupleKey & rhs) {
      size_t lhsBytes = (uint8_t *)lhs.second - (uint8_t *)lhs.first;
      size_t rhsBytes = (uint8_t *)rhs.second - (uint8_t *)rhs.first;
      if (lhsBytes == rhsBytes) {
        TypeList::const_iterator li = lhs.first;
        TypeList::const_iterator ri = rhs.first;
        for (; li != lhs.second; ++li, ++ri) {
          if (!Type::KeyInfo::isEqual(*li, *ri)) {
            return false;
          }
        }

        return true;
      }

      return false;
    }

    static bool isPod() { return false; }
    static Type * const emptyKey;
    static Type * const tombstoneKey;
  };

  TypeTupleKey iterPair(TupleType * tv) {
    return TypeTupleKey(tv->begin(), tv->end());
  }

  Type * const TypeTupleKeyInfo ::emptyKey = NULL;
  Type * const TypeTupleKeyInfo ::tombstoneKey = NULL;

  typedef llvm::DenseMap<TypeTupleKey, TupleType *, TypeTupleKeyInfo> TupleTypeMap;

  TupleTypeMap uniqueValues_;
}

// -------------------------------------------------------------------
// TupleType

TupleType * TupleType::get(const Type * typeArg) {
  return get(const_cast<const_iterator>(&typeArg), const_cast<const_iterator>(&typeArg + 1));
}

TupleType * TupleType::get(TypeList::const_iterator first, TypeList::const_iterator last) {
  TupleTypeMap::iterator it = uniqueValues_.find(TypeTupleKey(first, last));
  if (it != uniqueValues_.end()) {
    return it->second;
  }

  TupleType * newEntry = new TupleType(first, last);
  uniqueValues_[iterPair(newEntry)] = newEntry;
  return newEntry;
}

TupleType::TupleType(TypeList::const_iterator first, TypeList::const_iterator last)
  : TypeImpl(Tuple)
  , members_(first, last)
{
}

const llvm::Type * TupleType::createIRType() const {
}

ConversionRank TupleType::convertImpl(const Conversion & cn) const {
#if 0
  if (isEqual(cn.fromType)) {
    if (cn.resultValue != NULL) {
      *cn.resultValue = cn.fromValue;
      return IdenticalTypes;
    }
  }

  ConversionRank bestRank = Incompatible;
  Type * bestType = NULL;

  // Create a temporary cn with no result value.
  Conversion ccTemp(cn);
  ccTemp.resultValue = NULL;
  for (TypeRefList::const_iterator it = members_->begin(); it != members_->end(); ++it) {
    ConversionRank rank = it->type()->convert(ccTemp);
    if (rank > bestRank) {
      bestRank = rank;
      bestType = it->type();
    }
  }

  // Since we're converting to a union type, it's not identical.
  // TODO: Don't know if we really need this.
  //if (bestRank == IdenticalTypes) {
  //  bestRank = ExactConversion;
  //}

  if (bestType != NULL && cn.resultValue != NULL) {
    // Do the conversion to the best type first.
    bestRank = bestType->convertImpl(cn);

    // And now add a cast to the union type.
    if (*cn.resultValue != NULL) {
      int typeIndex = getTypeIndex(bestType);
      CastExpr * result = new CastExpr(
          Expr::UnionCtorCast,
          cn.fromValue->location(),
          const_cast<UnionType *>(this),
          *cn.resultValue);
      result->setTypeIndex(typeIndex);
      *cn.resultValue = result;
    }
  }

  return bestRank;
#endif
}

bool TupleType::isEqual(const Type * other) const {
  if (other == this) {
    return true;
  }

  return false;
}

bool TupleType::isSingular() const {
  for (TypeList::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if (!(*it)->isSingular()) {
      return false;
    }
  }

  return true;
}

bool TupleType::isSubtype(const Type * other) const {
  DFAIL("Implement");
}

bool TupleType::includes(const Type * other) const {
  for (TypeList::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if (!(*it)->includes(other)) {
      return true;
    }
  }

  return false;
}

void TupleType::format(FormatStream & out) const {
  for (TypeList::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if (it != members_.begin()) {
      out << ", ";
    }

    out << *it;
  }
}

void TupleType::trace() const {
  markList(members_.begin(), members_.end());
}

} // namespace tart

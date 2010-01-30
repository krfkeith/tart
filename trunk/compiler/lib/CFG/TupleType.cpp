/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/TupleType.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

namespace {
  /// -------------------------------------------------------------------
  /// Represents a sub-range of a list of type references.

  typedef std::pair<TupleType::const_iterator, TupleType::const_iterator> TypeTupleKey;

  struct TypeTupleKeyInfo {
    static inline TypeTupleKey getEmptyKey() {
      return TypeTupleKey(&emptyKey, &emptyKey + 1);
    }

    static inline TypeTupleKey getTombstoneKey() {
      return TypeTupleKey(&tombstoneKey, &tombstoneKey + 1);
    }

    static unsigned getHashValue(const TypeTupleKey & key) {
      unsigned result = 0;
      for (TupleType::const_iterator it = key.first; it != key.second; ++it) {
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
        TupleType::const_iterator li = lhs.first;
        TupleType::const_iterator ri = rhs.first;
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
  bool initFlag = false;

  class CleanupHook : public GC::Callback {
    void call() {
      uniqueValues_.clear();
      initFlag = false;
    }
  };


  CleanupHook hook;
}

// -------------------------------------------------------------------
// TupleType

TupleType * TupleType::get(const Type * typeArg) {
  return get(const_cast<const_iterator>(&typeArg), const_cast<const_iterator>(&typeArg + 1));
}

TupleType * TupleType::get(TupleType::const_iterator first, TupleType::const_iterator last) {
  if (!initFlag) {
    initFlag = true;
    GC::registerUninitCallback(&hook);
  }

  TupleTypeMap::iterator it = uniqueValues_.find(TypeTupleKey(first, last));
  if (it != uniqueValues_.end()) {
    return it->second;
  }

  TupleType * newEntry = new TupleType(first, last);
  uniqueValues_[iterPair(newEntry)] = newEntry;
  return newEntry;
}

TupleType::TupleType(TupleType::const_iterator first, TupleType::const_iterator last)
  : TypeImpl(Tuple, Shape_Unset)
  , members_(first, last)
{
}

const llvm::Type * TupleType::createIRType() const {
  // Members of the class
  std::vector<const llvm::Type *> fieldTypes;
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    const llvm::Type * fieldType = (*it)->irEmbeddedType();
    if (fieldType == NULL) {
      return NULL;
    }

    fieldTypes.push_back(fieldType);
  }

  llvm::Type * result = llvm::StructType::get(llvm::getGlobalContext(), fieldTypes);
  shape_ = isLargeIRType(result) ? Shape_Large_Value : Shape_Small_RValue;
  return result;
}

const llvm::Type * TupleType::irParameterType() const {
  const llvm::Type * type = irType();
  if (shape_ == Shape_Large_Value) {
    return llvm::PointerType::get(type, 0);
  } else {
    return type;
  }
}

ConversionRank TupleType::convertImpl(const Conversion & cn) const {

  const TupleType * fromType = dyn_cast<TupleType>(cn.getFromType());
  if (fromType == NULL) {
    return Incompatible;
  }

  if (isEqual(fromType)) {
    if (cn.resultValue != NULL) {
      *cn.resultValue = cn.fromValue;
    }

    return IdenticalTypes;
  }

  size_t fieldCount = members_.size();
  if (fromType->numTypeParams() != fieldCount) {
    return Incompatible;
  }

  ExprList args;
  ConversionRank rank = IdenticalTypes;
  bool identical = true;
  for (size_t i = 0; i < fieldCount; ++i) {
    const Type * fromFieldType = fromType->member(i);
    const Type * toFieldType = members_[i];
    Expr * fieldResult = NULL;

    // Convert each field individually
    Conversion fieldConversion(fromFieldType);
    fieldConversion.options = Conversion::Coerce;

    if (cn.fromValue != NULL) {
      if (TupleCtorExpr * tce = dyn_cast<TupleCtorExpr>(cn.fromValue)) {
        fieldConversion.fromValue = tce->arg(i);
      } else {
        DFAIL("Implement tuple GetElement");
      }

      fieldConversion.resultValue = &fieldResult;
    }

    ConversionRank fieldRank = toFieldType->convert(fieldConversion);
    if (fieldRank == Incompatible) {
      return Incompatible;
    }

    rank = std::min(rank, fieldRank);

    if (cn.fromValue != NULL) {
      DASSERT(fieldResult != NULL);
      args.push_back(fieldResult);

      if (fieldResult != fieldConversion.fromValue) {
        identical = false;
      }
    }
  }

  if (cn.resultValue != NULL) {
    DASSERT(cn.fromValue != NULL);
    if (identical) {
      *cn.resultValue = cn.fromValue;
    } else {
      *cn.resultValue = new TupleCtorExpr(cn.fromValue->location(), this, args);
    }
  }

  return rank;
}

bool TupleType::isEqual(const Type * other) const {
  if (other == this) {
    return true;
  }

  return false;
}

bool TupleType::isSingular() const {
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if (!(*it)->isSingular()) {
      return false;
    }
  }

  return true;
}

bool TupleType::isSubtype(const Type * other) const {
  return isEqual(other);
}

bool TupleType::includes(const Type * other) const {
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if (!(*it)->includes(other)) {
      return true;
    }
  }

  return false;
}

Expr * TupleType::nullInitValue() const {
  ExprList initializers;
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    Expr * memberInit = (*it)->nullInitValue();
    if (memberInit == NULL) {
      return NULL;
    } else {
      initializers.push_back(memberInit);
    }
  }

  return new TupleCtorExpr(SourceLocation(), this, initializers);
}

TypeShape TupleType::typeShape() const {
  if (shape_ == Shape_Unset) {
    if (isLargeIRType(irType())) {
      shape_ = Shape_Large_Value;
    } else {
      shape_ = Shape_Small_RValue;
    }
  }

  return shape_;
}

void TupleType::formatMembers(FormatStream & out) const {
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if (it != members_.begin()) {
      out << ", ";
    }

    out << *it;
  }
}

void TupleType::format(FormatStream & out) const {
  out << "(";
  formatMembers(out);
  out << ")";
}

void TupleType::trace() const {
  markList(members_.begin(), members_.end());
}

bool TupleType::containsBadType() const {
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if (isErrorResult(*it)) {
      return true;
    }
  }

  return false;
}

} // namespace tart

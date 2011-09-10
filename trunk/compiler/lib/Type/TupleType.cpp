/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Type/TupleType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/TargetSelection.h"

#include "llvm/DerivedTypes.h"

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

  class TupleTypeWeakPtr : public GCWeakPtr<TupleType> {
  public:
    TupleTypeWeakPtr() {}
    TupleTypeWeakPtr(TupleType * ptr) : GCWeakPtr<TupleType>(ptr) {}
    TupleTypeWeakPtr(TupleTypeWeakPtr & wp) : GCWeakPtr<TupleType>(wp) {}
  private:
    void finalize() const;
  };

  typedef llvm::DenseMap<TypeTupleKey, TupleType *, TypeTupleKeyInfo> TupleTypeMap;

  TupleTypeMap uniqueValues_;
  bool initFlag = false;

  void TupleTypeWeakPtr::finalize() const {
    uniqueValues_.erase(iterPair(get()));
  }

  class CleanupHook : public GC::Callback {
    void call() {
      uniqueValues_.clear();
      initFlag = false;
    }
  };

  CleanupHook hook;

  class TupleTypeRoot : public GCRootBase {
    void trace() const {
      for (TupleTypeMap::const_iterator it = uniqueValues_.begin(); it != uniqueValues_.end();
          ++it) {
        // The key contains pointers to the value, so only trace the value.
        it->second->mark();
      }
    }
  };
}

// -------------------------------------------------------------------
// TupleType

TupleType * TupleType::get(const Type * typeArg) {
  return get(const_cast<const_iterator>(&typeArg), const_cast<const_iterator>(&typeArg + 1));
}

TupleType * TupleType::get(TupleType::const_iterator first, TupleType::const_iterator last) {
  // Make the type map a garbage collection root.
  static TupleTypeRoot root;

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
  containsReferenceType_ = false;
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if ((*it)->containsReferenceType()) {
      containsReferenceType_ = true;
      break;
    }
  }
}

llvm::Type * TupleType::createIRType() const {
  // Members of the class
  std::vector<llvm::Type *> fieldTypes;
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    llvm::Type * fieldType = (*it)->irEmbeddedType();
    if (fieldType == NULL) {
      return NULL;
    }

    fieldTypes.push_back(fieldType);
  }

  llvm::Type * result = llvm::StructType::get(llvm::getGlobalContext(), fieldTypes);
  uint64_t size = TargetSelection::instance.targetData()->getTypeSizeInBits(result);

  if (size > 2 * TargetSelection::instance.targetData()->getPointerSizeInBits()) {
    shape_ = Shape_Large_Value;
  } else if (containsReferenceType_) {
    // TODO: Should be small l-value but that's not working for some reason.
    shape_ = Shape_Small_LValue;
    //shape_ = Shape_Large_Value;
  } else {
    shape_ = Shape_Small_RValue;
  }
  return result;
}

llvm::Type * TupleType::irParameterType() const {
  llvm::Type * type = irType();
  if (shape_ == Shape_Large_Value) {
    return type->getPointerTo();
  } else {
    return type;
  }
}

bool TupleType::isSingular() const {
  for (TupleType::const_iterator it = members_.begin(); it != members_.end(); ++it) {
    if (!(*it)->isSingular()) {
      return false;
    }
  }

  return true;
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
    irType();
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

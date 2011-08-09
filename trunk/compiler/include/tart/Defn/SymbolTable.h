/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DEFN_SYMBOLTABLE_H
#define TART_DEFN_SYMBOLTABLE_H

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#ifndef LLVM_ADT_STRINGMAP_H
#include <llvm/ADT/StringMap.h>
#endif

#ifndef LLVM_ADT_SMALLVECTOR_H
#include <llvm/ADT/SmallVector.h>
#endif

namespace tart {

class FormatStream;

/// -------------------------------------------------------------------
/// Mapping of names to definitions
class SymbolTable {
public:
  typedef llvm::SmallVector<Defn *, 4> Entry;
  typedef llvm::StringMap<Entry, llvm::BumpPtrAllocator> NameDefnMap;
  typedef NameDefnMap::iterator iterator;
  typedef NameDefnMap::const_iterator const_iterator;

  SymbolTable() {}
  virtual ~SymbolTable() {}

  /** Add a new declaration to this scope. */
  SymbolTable::Entry * add(Defn * d);

  /** Get the count of items in the scope */
  size_t count() const { return map_.size(); }

  /** Find a declaration by name */
  const Entry * findSymbol(StringRef key) const {
    NameDefnMap::const_iterator it = map_.find(key);
    if (it != map_.end()) {
      return &it->second;
    } else {
      return NULL;
    }
  }

  iterator begin() { return map_.begin(); }
  const_iterator begin() const { return map_.begin(); }
  iterator end() { return map_.end(); }
  const_iterator end() const { return map_.end(); }

  /** Clear the symbol table. */
  void clear() { map_.clear(); }

  /** GC trace function */
  void trace() const;

  /** Debugging helper function. */
  void getDebugSummary(FormatStream & out) const;

private:

  // Map of declarations by name
  NameDefnMap map_;
};

/// -------------------------------------------------------------------
/// Mapping of names to declarations
class OrderedSymbolTable : public SymbolTable {
public:
  OrderedSymbolTable() : first_(NULL), last_(NULL) {}

  void clear() {
    SymbolTable::clear();
    first_ = last_ = NULL;
  }

  /** Add a new declaration to this scope. */
  SymbolTable::Entry * add(Defn * d);

  /** Get the first decl in the list by order. */
  Defn * first() const { return first_; }

private:
  Defn * first_;
  Defn * last_;
};

}

#endif

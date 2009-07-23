/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_CFG_SYMBOLTABLE_H
#define TART_CFG_SYMBOLTABLE_H

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#include <string>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/SmallVector.h>

namespace tart {
    
/// -------------------------------------------------------------------
/// Mapping of names to definitions
class SymbolTable {
public:
  typedef llvm::SmallVector<Defn *, 4> Entry;

private:
  typedef llvm::StringMap<Entry> decl_map_t;

  // Map of declarations by name
  decl_map_t map;
  
public:
  typedef decl_map_t::iterator iterator;
  
  SymbolTable() {}
  virtual ~SymbolTable() {}
  
  /** Add a new declaration to this scope. */
  SymbolTable::Entry * add(Defn * d);
  
  /** Get the count of items in the scope */
  size_t getCount() const { return map.size(); }

  /** Find a declaration by name */
  const Entry * findSymbol(const char * key) const {
    decl_map_t::const_iterator it = map.find(key, key + strlen(key));
    if (it != map.end()) {
      return &it->second;
    } else {
      return NULL;
    }
  }
  
  iterator begin() { return map.begin(); }
  iterator end() { return map.end(); }
  
  /** GC trace function */
  void trace() const;

  /** Debugging helper function. */
  void getDebugSummary(std::string & out) const;
};

/// -------------------------------------------------------------------
/// Mapping of names to declarations
class OrderedSymbolTable : public SymbolTable {
private:
  Defn * firstSymbol;
  Defn * lastSymbol;
  
public:
  OrderedSymbolTable() : firstSymbol(NULL), lastSymbol(NULL) {}
  
  /** Add a new declaration to this scope. */
  SymbolTable::Entry * add(Defn * d);
  
  /** Get the first decl in the list by order. */
  Defn * getFirst() const { return firstSymbol; }
};

}

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Defn.h"
#include "tart/CFG/SymbolTable.h"

namespace tart {

SymbolTable::Entry * SymbolTable::add(Defn * member) {
  const char * key = member->getName();
  assert(key != NULL);
  Entry & entry = map.GetOrCreateValue(key, key + strlen(key)).getValue();
  entry.push_back(member);
  return &entry;
}

void SymbolTable::trace() const {
  for (decl_map_t::const_iterator it = map.begin(); it != map.end(); ++it) {
    const SymbolTable::Entry & entry = it->second;
    for (SymbolTable::Entry::const_iterator si = entry.begin(); si != entry.end(); ++si) {
      (*si)->mark();
    }
  }
}

void SymbolTable::getDebugSummary(std::string & out) const {
  //size_t count = getCount();
  size_t count = 0;
  char buffer[32];

  out.push_back('{');
  for (decl_map_t::const_iterator it = map.begin(); it != map.end(); ++it) {
    if (count > 8) {
      sprintf(buffer, " + %zd more...", map.size() - count);
      out.append(buffer);
      break;
    }

    if (it != map.begin()) {
      out.append(", ");
    }

    out.push_back('\'');
    out.append(it->second.front()->getName());
    out.push_back('\'');

    if (it->second.size() > 1) {
      sprintf(buffer, "(%zd)", it->second.size());
      out.append(buffer);
    }

    ++count;
  }
  out.push_back('}');
}

SymbolTable::Entry * OrderedSymbolTable::add(Defn * member) {
  SymbolTable::Entry * result = SymbolTable::add(member);

  if (lastSymbol != NULL) {
    lastSymbol->nextInScope_ = member;
  } else {
    firstSymbol = member;
  }

  lastSymbol = member;
  return result;
}

}

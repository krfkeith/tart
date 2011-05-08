/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Defn.h"
#include "tart/Defn/SymbolTable.h"

namespace tart {

SymbolTable::Entry * SymbolTable::add(Defn * member) {
  const char * key = member->name();
  assert(key != NULL);
  Entry & entry = map_.GetOrCreateValue(key, key + strlen(key)).getValue();
  entry.push_back(member);
  return &entry;
}

void SymbolTable::trace() const {
  for (decl_map_t::const_iterator it = map_.begin(); it != map_.end(); ++it) {
    const SymbolTable::Entry & entry = it->second;
    for (SymbolTable::Entry::const_iterator si = entry.begin(); si != entry.end(); ++si) {
      (*si)->markDeferred();
    }
  }
}

void SymbolTable::getDebugSummary(std::string & out) const {
  //size_t count = count();
  size_t count = 0;
  char buffer[32];

  out.push_back('{');
  for (decl_map_t::const_iterator it = map_.begin(); it != map_.end(); ++it) {
    if (count > 8) {
      sprintf(buffer, " + %zd more...", map_.size() - count);
      out.append(buffer);
      break;
    }

    if (it != map_.begin()) {
      out.append(", ");
    }

    out.push_back('\'');
    out.append(it->second.front()->name());
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

  if (last_ != NULL) {
    last_->nextInScope_ = member;
  } else {
    first_ = member;
  }

  last_ = member;
  return result;
}

}

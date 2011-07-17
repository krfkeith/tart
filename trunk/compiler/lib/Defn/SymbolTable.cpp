/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Defn.h"
#include "tart/Defn/SymbolTable.h"

namespace tart {

SymbolTable::Entry * SymbolTable::add(Defn * member) {
  const char * key = member->name();
  assert(key != NULL);
  Entry & entry = map_.GetOrCreateValue(llvm::StringRef(key)).getValue();
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

void SymbolTable::getDebugSummary(FormatStream & out) const {
  size_t count = 0;
  out << "{";
  for (decl_map_t::const_iterator it = map_.begin(); it != map_.end(); ++it) {
    if (count > 8) {
      out << " + " << map_.size() - count << " more...";
      break;
    }

    if (it != map_.begin()) {
      out << ", ";
    }

    out << "\'" << it->second.front()->name() << "\'";

    if (it->second.size() > 1) {
      out << "(" << it->second.size() << ")";
    }

    ++count;
  }
  out << "}";
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

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"
#include "tart/Meta/Tags.h"
#include "tart/Meta/VarInt.h"
#include "tart/Meta/NameTable.h"
#include <vector>

namespace tart {

using llvm::StringRef;

namespace {

/// -------------------------------------------------------------------
/// Comparator for names by use count.

struct NameOrder {
  bool operator()(NameTable::Name * n0, NameTable::Name * n1) {
    if (n0->useCount() > n1->useCount()) return true;
    if (n1->useCount() > n0->useCount()) return false;
    if (n0->isCompound() != n1->isCompound()) {
      return n0->isCompound();
    }

    if (n0->isCompound()) {
      NameTable::CompoundName * c0 = static_cast<NameTable::CompoundName *>(n0);
      NameTable::CompoundName * c1 = static_cast<NameTable::CompoundName *>(n1);
      if ((*this)(c0->value().first, c1->value().first)) return true;
      if ((*this)(c1->value().first, c0->value().first)) return false;
      return (*this)(c0->value().second, c1->value().second);
    } else {
      NameTable::SimpleName * s0 = static_cast<NameTable::SimpleName *>(n0);
      NameTable::SimpleName * s1 = static_cast<NameTable::SimpleName *>(n1);
      return s0->value().compare(s1->value()) < 0;
    }

    return false;
  }
};

}

// -------------------------------------------------------------------
// NameTable::Name

long NameTable::Name::encodedIndex() const {
  return (index() << 1) + (isCompound() ? COMPOUND_NAME_FLAG : 0);
}

// -------------------------------------------------------------------
// NameTable::SimpleName

void NameTable::SimpleName::format(FormatStream & out) const {
  out << value_;
}

// -------------------------------------------------------------------
// NameTable::CompoundName

void NameTable::CompoundName::format(FormatStream & out) const {
  out << value_.first << "." << value_.second;
}

// -------------------------------------------------------------------
// NameTable

NameTable::~NameTable() {
  // TODO: write destructor.
}

NameTable::Name * NameTable::getName(const StringRef name) const {
  SimpleNameMap::const_iterator it = simpleNames_.find(name);
  if (it == simpleNames_.end()) {
    return NULL;
  }

  return it->second;
}

NameTable::Name * NameTable::getName(NameTable::Name * scope, NameTable::Name * member) const {
  NameTable::CompoundName::value_type key = std::make_pair(scope, member);
  CompoundNameMap::const_iterator it = compoundNames_.find(key);
  if (it != compoundNames_.end()) {
    return it->second;
  }

  return NULL;
}

NameTable::Name * NameTable::getQualifiedName(StringRef name) const {
  std::pair<StringRef, StringRef> parts = name.rsplit('.');
  if (parts.second.empty()) {
    return getName(name);
  } else {
    NameTable::Name * first = getQualifiedName(parts.first);
    NameTable::Name * second = getName(parts.second);
    return getName(first, second);
  }
}

NameTable::Name * NameTable::addName(const StringRef name) {
  SimpleNameMap::value_type & entry = simpleNames_.GetOrCreateValue(name);
  if (entry.second == NULL) {
    entry.second = new NameTable::SimpleName(entry.first());
  }
  return entry.second;
}

NameTable::Name * NameTable::addName(NameTable::Name * scope, NameTable::Name * member) {
  CompoundName::value_type key = std::make_pair(scope, member);
  CompoundNameMap::iterator it = compoundNames_.find(key);
  if (it != compoundNames_.end()) {
    return it->second;
  }

  CompoundName * value = new CompoundName(key);
  compoundNames_[key] = value;
  scope->use();
  member->use();
  return value;
}

NameTable::Name * NameTable::addQualifiedName(StringRef name) {
  std::pair<StringRef, StringRef> parts = name.rsplit('.');
  if (parts.second.empty()) {
    return addName(name);
  } else {
    Name * first = addQualifiedName(parts.first);
    Name * second = addName(parts.second);
    return addName(first, second);
  }
}

void NameTable::assignIndices() {
  sortedSimpleNames_.reserve(simpleNames_.size());
  unsigned simpleNameSize = 0;
  for (SimpleNameMap::iterator it = simpleNames_.begin(); it != simpleNames_.end(); ++it) {
    simpleNameSize += it->second->value().size() + 1;
    sortedSimpleNames_.push_back(it->second);
  }

  std::sort(sortedSimpleNames_.begin(), sortedSimpleNames_.end(), NameOrder());
  for (unsigned i = 0; i < sortedSimpleNames_.size(); ++i) {
    sortedSimpleNames_[i]->setIndex(i);
  }

  CompoundNameArray compoundNames;
  sortedCompoundNames_.reserve(compoundNames_.size());
  for (CompoundNameMap::iterator it = compoundNames_.begin(); it != compoundNames_.end(); ++it) {
    sortedCompoundNames_.push_back(it->second);
  }

  std::sort(sortedCompoundNames_.begin(), sortedCompoundNames_.end(), NameOrder());
  for (unsigned i = 0; i < sortedCompoundNames_.size(); ++i) {
    sortedCompoundNames_[i]->setIndex(i);
  }
}

void NameTable::writeStringTable(llvm::raw_ostream & out) {
  out << VarInt(simpleNames_.size());
  for (SimpleNameArray::iterator it = sortedSimpleNames_.begin(); it != sortedSimpleNames_.end();
      ++it) {
    StringRef name = (*it)->value();
    out << VarInt(name.size()) << name;
  }
  out.flush();
}

void NameTable::writeCompoundNameTable(llvm::raw_ostream & out) {
  out << VarInt(compoundNames_.size());
  for (CompoundNameArray::iterator it = sortedCompoundNames_.begin();
      it != sortedCompoundNames_.end(); ++it) {
    CompoundName * name = *it;
    out << VarInt(name->first()->encodedIndex()) << VarInt(name->second()->encodedIndex());
  }
  out.flush();
}


} // namespace tart

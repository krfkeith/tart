/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Scope.h"
#include "tart/Defn/Defn.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// Scope

/** Convenience function used by testing code. */
Defn * Scope::lookupSingleMember(const char * ident, bool inherit) const {
  DefnList defns;
  if (lookupMember(ident, defns, inherit) && defns.size() == 1) {
    return defns.front();
  }

  return NULL;
}

/** Debugging function to dump the scope hierarchy. */
void Scope::dump() const {
  for (const Scope * s = this; s != NULL; s = s->parentScope()) {
    s->dumpHierarchy(true);
  }
}

/// -------------------------------------------------------------------
/// IterableScope

Scope * IterableScope::parentScope() const {
  return parentScope_;
}

void IterableScope::setParentScope(Scope * parent)  {
  DASSERT(parent != NULL);
  parentScope_ = parent;
}

void IterableScope::addMember(Defn * d) {
  DASSERT_OBJ(d->definingScope() == NULL, d);
  SymbolTable::Entry * entry = members_.add(d);
  d->setDefiningScope(this);
}

bool IterableScope::lookupMember(const char * name, DefnList & defs, bool inherit) const {
  const SymbolTable::Entry * entry = members_.findSymbol(name);
  bool found = false;
  if (entry != NULL) {
    defs.append(entry->begin(), entry->end());
    found = true;
  }

  if (inherit) {
    for (ScopeSet::const_iterator it = auxScopes_.begin(); it != auxScopes_.end(); ++it) {
      if ((*it)->lookupMember(name, defs, true)) {
        found = true;
      }
    }
  }

  return found;
}

void IterableScope::trace() const {
  // We don't need to trace the parent pointer here (we can't since it
  // isn't derived from GC). The parent object will be reachable via another
  // path.
  members_.trace();
}

void IterableScope::dumpHierarchy(bool full) const {
  std::string out;
  if (scopeName_) {
    out.append("[");
    out.append(scopeName_);
    out.append("]");
  }

  members_.getDebugSummary(out);

  diag.indent();
  for (ScopeSet::const_iterator it = auxScopes_.begin(); it != auxScopes_.end(); ++it) {
    (*it)->dumpHierarchy(full);
  }
  diag.unindent();

  diag.writeLnIndent(out);
}

/// -------------------------------------------------------------------
/// LocalScope

void LocalScope::addMember(Defn * d) {
  DASSERT(d->storageClass() == Storage_Local);
  IterableScope::addMember(d);
}

void LocalScope::trace() const {
  IterableScope::trace();
}

}

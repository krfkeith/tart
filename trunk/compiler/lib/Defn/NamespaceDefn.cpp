/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/NamespaceDefn.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// NamespaceDefn
NamespaceDefn::NamespaceDefn(Module * m, StringRef name)
  : Defn(Namespace, m, name)
{
  setStorageClass(Storage_Global);
  members.setScopeName(name);
}

NamespaceDefn::NamespaceDefn(Module * m, const ASTDecl * de)
  : Defn(Namespace, m, de)
{
  setStorageClass(Storage_Global);
  members.setScopeName(name());
}

void NamespaceDefn::format(FormatStream & out) const {
  out << name_;
}

void NamespaceDefn::trace() const {
  Defn::trace();
  members.trace();
}


} // namespace tart

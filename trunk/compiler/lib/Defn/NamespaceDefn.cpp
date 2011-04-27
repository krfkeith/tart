/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/*#include "tart/Defn/Defn.h"
#include "tart/Defn/Scope.h"
#include "tart/Type/Type.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Defn/Module.h"
#include "tart/Defn/Template.h"
#include "tart/AST/ASTDecl.h"
#include "tart/Objects/Builtins.h"*/
#include "tart/Defn/NamespaceDefn.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// NamespaceDefn
NamespaceDefn::NamespaceDefn(Module * m, const char * name)
  : Defn(Namespace, m, name)
{
  members.setScopeName(name);
}

NamespaceDefn::NamespaceDefn(Module * m, const ASTDecl * de)
  : Defn(Namespace, m, de)
{
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

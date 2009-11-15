/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/*#include "tart/CFG/Defn.h"
#include "tart/CFG/Scope.h"
#include "tart/CFG/Type.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Template.h"
#include "tart/AST/ASTDecl.h"
#include "tart/Objects/Builtins.h"*/
#include "tart/CFG/NamespaceDefn.h"
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
  out << "namespace " << name_;
}

void NamespaceDefn::trace() const {
  Defn::trace();
  members.trace();
}


} // namespace tart

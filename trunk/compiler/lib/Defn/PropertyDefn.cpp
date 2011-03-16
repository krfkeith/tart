/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// -------------------------------------------------------------------
// PropertyDefn

PropertyDefn::PropertyDefn(DefnType dtype, Module * m, const ASTPropertyDecl * ast)
  : ValueDefn(dtype, m, ast)
  , type_(NULL)
  , getter_(NULL)
  , setter_(NULL)
{
  accessorScope_.setScopeName(ast_->name());
}

void PropertyDefn::trace() const {
  ValueDefn::trace();
  safeMark(type_);
  safeMark(getter_);
  safeMark(setter_);
}

void PropertyDefn::format(FormatStream & out) const {
  if (out.getShowQualifiedName()) {
    if (out.getShowType() && enclosingClassDefn()) {
      out << enclosingClassDefn() << "." << name_;
    } else {
      out << qname_;
    }
  } else {
    out << name_;
  }
}

// -------------------------------------------------------------------
// IndexerDefn
void IndexerDefn::trace() const {
  PropertyDefn::trace();
}

void IndexerDefn::format(FormatStream & out) const {
  if (out.isVerbose()) {
    out << "def ";
  }

  if (out.getShowQualifiedName() && !qname_.empty()) {
    if (out.getShowType() && parentDefn()) {
      out << parentDefn() << "[]";
    } else {
      out << qname_;
    }
  } else {
    out << "[]";
  }

  const FunctionType * ftype = dyn_cast_or_null<FunctionType>(type());
  if (out.getShowType() && ftype != NULL) {
    out << "(";
    formatParameterList(out, ftype->params());
    out << ")";
    if (!ftype->isVoidType()) {
      out << ":" << ftype->returnType();
    }
  }
}

} // namespace tart

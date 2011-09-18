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

PropertyDefn::PropertyDefn(DefnType dtype, Module * m, StringRef name)
  : ValueDefn(dtype, m, name)
  , type_(NULL)
  , getter_(NULL)
  , setter_(NULL)
{
  accessorScope_.setScopeName(name);
}

void PropertyDefn::trace() const {
  ValueDefn::trace();
  accessorScope_.trace();
  safeMark(type_.unqualified());
  safeMark(getter_);
  safeMark(setter_);
}

void PropertyDefn::addAccessor(FunctionDefn * accessor) {
  if (accessor->name() == "get") {
    if (getter_ != NULL) {
      diag.error(this) << "Multiple getters not supported";
      return;
    }
    getter_ = accessor;
  } else if (accessor->name() == "set") {
    if (setter_ != NULL) {
      diag.error(this) << "Multiple setters not supported";
      return;
    }
    setter_ = accessor;
  }

  accessorScope_.addMember(accessor);
  accessor->setParentDefn(this);
  accessor->createQualifiedName(this);
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

  Qualified<FunctionType> ftype = type().dyn_cast_or_null<FunctionType>();
  if (out.getShowType() && ftype) {
    out << "(";
    formatParameterList(out, ftype->params());
    out << ")";
    if (!ftype->isVoidType()) {
      out << ":" << ftype->returnType();
    }
  }
}

} // namespace tart

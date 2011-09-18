/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"

namespace tart {

// -------------------------------------------------------------------
// TypeDefn

Expr * TypeDefn::asExpr() {
  if (expr_ == NULL) {
    expr_ = new TypeLiteralExpr(ast_ ? ast_->location() : SourceLocation(), typePtr());
  }

  return expr_;
}

void TypeDefn::trace() const {
  Defn::trace();
  value_.unqualified()->mark();
  safeMark(expr_);
}

void TypeDefn::format(FormatStream & out) const {
  if (out.getShowQualifiedName()) {
    if (out.getShowType() && enclosingClassDefn()) {
      out << enclosingClassDefn() << "." << name_;
    } else {
      out << qname_;
    }
  } else {
    out << name_;
  }

  if (isTemplate() /*&& out.getShowType()*/) {
    templateSignature()->format(out);
  } else if (templateInstance() != NULL) {
    templateInstance()->format(out);
  }
}

} // namespace tart

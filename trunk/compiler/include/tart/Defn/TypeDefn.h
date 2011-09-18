/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DEFN_TYPEDEFN_H
#define TART_DEFN_TYPEDEFN_H

#ifndef TART_DEFN_DEFN_H
#include "tart/Defn/Defn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A definition of a named type, such as a class or struct.
class TypeDefn : public Defn {
public:
  /** Constructor that takes a name */
  TypeDefn(Module * m, StringRef name, QualifiedType value = QualifiedType())
    : Defn(Typedef, m, name)
    , value_(value)
    , expr_(NULL)
    , definingScope_(NULL)
  {}

  /** Constructor that takes an AST declaration. */
  TypeDefn(Module * m, const ASTDecl * de)
    : Defn(Typedef, m, de)
    , expr_(NULL)
    , definingScope_(NULL)
  {}

  /** Return an expression reference to this type. */
  Expr * asExpr();

  /** Return the type expression defined by this definition. */
  const Type * typePtr() const { return value_.type(); }
  Type * mutableTypePtr() const { return const_cast<Type *>(value_.unqualified()); }
  void setTypePtr(Type * type) { value_ = QualifiedType(type, value_.qualifiers()); }

  /** Return the type expression defined by this definition. */
  QualifiedType value() const { return value_; }
  void setValue(QualifiedType value) { value_ = value; }

  // Overrides

  Scope * definingScope() const { return definingScope_; }
  void setDefiningScope(Scope * scope) { definingScope_ = scope; }
  void mark() const { GC::mark(); }
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const TypeDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Typedef;
  }

protected:
  QualifiedType value_;
  Expr * expr_;
  Scope * definingScope_;

  Type::TypeClass defnType_ToTypeClass(DefnType dt);
};

inline FormatStream & operator<<(FormatStream & out, const TypeDefn * type) {
  type->format(out);
  return out;
}

} // namespace tart

#endif

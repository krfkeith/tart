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
protected:
  Type * value;
  Expr * expr_;
  Scope * definingScope_;

  Type::TypeClass defnType_ToTypeClass(DefnType dt);

public:
  /** Constructor that takes a name */
  TypeDefn(Module * m, const char * name, Type * val = NULL)
    : Defn(Typedef, m, name)
    , value(val)
    , expr_(NULL)
    , definingScope_(NULL)
  {}

  /** Constructor that takes an AST declaration. */
  TypeDefn(Module * m, const ASTDecl * de)
    : Defn(Typedef, m, de)
    , value(NULL)
    , expr_(NULL)
    , definingScope_(NULL)
  {}

  /** Return an expression reference to this type. */
  Expr * asExpr();

  /** Return the type expression defined by this definition. */
  const Type * typeValue() const { return value; }
  Type * typeValue() { return value; }
  void setTypeValue(Type * type) { value = type; }

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
};

inline FormatStream & operator<<(FormatStream & out, const TypeDefn * type) {
  type->format(out);
  return out;
}

} // namespace tart

#endif

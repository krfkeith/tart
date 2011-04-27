/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DEFN_PROPERTYDEFN_H
#define TART_DEFN_PROPERTYDEFN_H

#ifndef TART_DEFN_DEFN_H
#include "tart/Defn/Defn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A definition of a property
class PropertyDefn : public ValueDefn {
public:
  enum AnalysisPass {
    AttributePass,
    AccessorCreationPass,
    PropertyTypePass,
    AccessorTypePass,
    AccessorAnalysisPass,
    CompletionPass,
    PassCount
  };

  typedef tart::PassMgr<AnalysisPass, PassCount> PassMgr;
  typedef PassMgr::PassSet PassSet;

  /** Constructor that takes an AST */
  PropertyDefn(DefnType dtype, Module * m, const ASTPropertyDecl * ast);

  /** Constructor that takes a name */
  PropertyDefn(DefnType dtype, Module * m, const char * name);

  FunctionDefn * getter() const { return getter_; }
  void setGetter(FunctionDefn * f) { getter_ = f; }

  FunctionDefn * setter() const { return setter_; }
  void setSetter(FunctionDefn * f) { setter_ = f; }

  /** Add either a getter or setter depending on the name. */
  void addAccessor(FunctionDefn * accessor);

  const IterableScope & accessorScope() const { return accessorScope_; }
  IterableScope & accessorScope() { return accessorScope_; }

  void setType(const Type * t) { type_ = t; }

  /** Which analysis passes are running / have run. */
  const PassMgr & passes() const { return passes_; }
  PassMgr & passes() { return passes_; }

  // Overrides

  const Type * type() const { return type_; }
  void trace() const;
  void format(FormatStream & out) const;
  void setDefiningScope(Scope * scope) {
    accessorScope_.setParentScope(scope);
    ValueDefn::setDefiningScope(scope);
  }

  static inline bool classof(const PropertyDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Property || de->defnType() == Indexer;
  }

private:
  const Type * type_;
  IterableScope accessorScope_;  // Scope in which getter/setter are defined.
  FunctionDefn * getter_;    // The getter method
  FunctionDefn * setter_;    // The setter method
  PassMgr passes_;
};

/// -------------------------------------------------------------------
/// A definition of an indexer
class IndexerDefn : public PropertyDefn {
public:
  /** Constructor that takes an AST */
  IndexerDefn(DefnType dtype, Module * m, const ASTPropertyDecl * ast)
    : PropertyDefn(dtype, m, ast)
  {}

  /** Constructor that takes a name */
  IndexerDefn(DefnType dtype, Module * m, const char * name)
    : PropertyDefn(dtype, m, name)
  {}

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const IndexerDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Indexer;
  }
};

} // namespace tart

#endif // TART_DEFN_PROPERTYDEFN_H

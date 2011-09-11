/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DEFN_DEFN_H
#define TART_DEFN_DEFN_H

#ifndef TART_EXPR_CONSTANT_H
#include "tart/Expr/Constant.h"
#endif

#ifndef TART_DEFN_SCOPE_H
#include "tart/Defn/Scope.h"
#endif

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_AST_DECL_H
#include "tart/AST/ASTDecl.h"
#endif

#ifndef TART_COMMON_PASSMGR_H
#include "tart/Common/PassMgr.h"
#endif

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

namespace llvm {
class Value;
class Function;
class Type;
class MDNode;
}

namespace tart {

class FunctionType;
class Template;
class TemplateInstance;
class Intrinsic;
class PropertyDefn;
class Module;

/// -------------------------------------------------------------------
/// A binding of a name to an object.
class Defn : public GC, public Formattable, public Locatable {
public:
  enum DefnType {
    Typedef,
    Namespace,
    Var,
    Let,
    Property,
    Indexer,
    Function,
    Macro,
    Parameter,
    Mod,
    ExplicitImport,
    MacroArg,

    DefnTypeCount,
  };

  enum Trait {
    Unsafe,                 // Requires unsafe language extensions
    Singular,               // Has no unbound template params
    Synthetic,              // Generated via template
    Reflect,                // Generate reflection data for this definition.
    TemplateMember,         // Is a member of a template
    PartialInstantiation,   // A template instance whose variables are unbound template params.
    RequestStackTrace,      // Set on catch variables that want stack tracing.
    Mergeable,              // Request type weakening (coalesce with more general version.)
    Scaffold,               // Temporary definition used for type inference

    TraitCount,
  };

  typedef SmallEnumSet<Trait, TraitCount> Traits;

protected:
  friend class OrderedSymbolTable;
  friend class IterableScope;
  friend class Template;
  friend class TemplateInstance;

  DefnType defnType_;         // What type of defn this is
  SourceLocation loc;         // Location where this was defined.
  StringRef name_;            // Local name (copied from decl)
  const ASTDecl * ast_;       // The source declaration of this defintion
  const llvm::MDNode * md_;   // Metadata node.
  DeclModifiers modifiers_;   // Modifier flags
  StorageClass storage_;      // Storage class for the defn.
  Module * module_;           // Module in which this symbol is defined.
  Defn * parentDefn_;         // Definition which encloses this one.
  Defn * nextInScope_;        // Pointer to the next defn in parent scope
  llvm::SmallString<0> qname_;// Fully-qualified name.
  mutable llvm::SmallString<0> lnkName;// External linkage name
  Template * tsig_;  // Template signature
  TemplateInstance * tinst_;  // Template arguments
  ExprList attrs_;            // List of attributes
  Traits traits_;             // Traits of this defn

public:
  /** Constructor that takes a name */
  Defn(DefnType dtype, Module * module, StringRef name);

  /** Constructor that takes an AST declaration and a parent. */
  Defn(DefnType dtype, Module * module, const ASTDecl * de);

  /** The type of this definition. */
  DefnType defnType() const { return defnType_; }

  /** Get the (short) name of this declaration. */
  StringRef name() const { return name_; }

  /** Get the fully qualified name of the definition. */
  StringRef qualifiedName() const;

  /** Set the fully qualified name of the definition. */
  void setQualifiedName(StringRef name) { qname_ = name; }

  /** Get the linkage name of this definition. */
  virtual StringRef linkageName() const;

  /** Set the linkage name of this definition. */
  void setLinkageName(const std::string & name) { lnkName = name; }

  /** Create the qualified name of this definition from the name of the
      parent scope combined with the local name. Only call this after we
      have added this definition to a parent scope. */
  void createQualifiedName(Defn * parent);

  /** Get the AST declaration that declared this definition. */
  const ASTDecl * ast() const { return ast_; }
  virtual const ASTDeclList & astMembers() const {
    return ast_->members();
  }

  /** Get the Metadata node that declared this definition. */
  const llvm::MDNode * mdNode() const { return md_; }
  void setMDNode(const llvm::MDNode * md) { md_ = md; }

  /** Get the source location where this definition was defined. */
  const SourceLocation & location() const;
  void setLocation(const SourceLocation & l) { loc = l; }

  /** Get the list of attributes. */
  const ExprList & attrs() const { return attrs_; }
  ExprList & attrs() { return attrs_; }

  /** Get the list of traits. */
  const Traits & traits() const { return traits_; }
  Traits & traits() { return traits_; }
  bool hasTrait(Trait t) const { return traits_.contains(t); }
  void addTrait(Trait t) { traits_.add(t); }
  void removeTrait(Trait t) { traits_.remove(t); }
  void copyTrait(Defn * from, Trait t) {
    if (from->hasTrait(t)) { traits_.add(t); }
  }

  // Various trait convenience methods

  virtual bool isSingular() const { return traits_.contains(Singular); }
  bool isSynthetic() const { return traits_.contains(Synthetic); }
  bool isReflected() const { return traits_.contains(Reflect); }
  bool isUnsafe() const { return traits_.contains(Unsafe); }
  bool isScaffold() const { return traits_.contains(Scaffold); }
  bool isReadOnly() const { return (modifiers_.flags & ReadOnly) != 0; }
  bool isMutable() const { return (modifiers_.flags & Mutable) != 0; }
  bool isImmutable() const { return (modifiers_.flags & Immutable) != 0; }

  const DeclModifiers & modifiers() const { return modifiers_; }

  void setSingular(bool t) {
    if (t) {
      traits_.add(Singular);
    } else {
      traits_.remove(Singular);
    }
  }

  // Member/Container methods.

  /** Get the scope in which this is defined. */
  virtual Scope * definingScope() const = 0;
  virtual void setDefiningScope(Scope * scope) = 0;

  /** Get the module which contains this declaration. */
  Module * module() const { return module_; }

  /** Get the module which originally defines this defn. This will be different than 'module'
      for defns which are template instances - it returns the module in which the template
      was defined. */
  Module * sourceModule() const;

  /** Get the defn in which this is defined, if any. */
  Defn * parentDefn() const { return parentDefn_; }
  void setParentDefn(Defn * defn) { parentDefn_ = defn; }

  /** Return the ancestor of this definition that is defined at the top level within the
      module.  */
  Defn * moduleLevelParent() const;

  /** If the immediate enclosing decl is a class or struct, return it.
      If it's a property, then return the class enclosing the property. */
  TypeDefn * enclosingClassDefn() const;

  /** Pointer to the next declaration in the defining scope, in order of declaration. */
  Defn * nextInScope() const { return nextInScope_; }

  // Template methods

  /** Return true if this declaration is a template. */
  bool isTemplate() const { return tsig_ != NULL; }

  /** Return true if this declaration has an ancestor which is a template. */
  bool isTemplateMember() const { return traits_.contains(TemplateMember); }

  /** Return true if this declaration is part of a partially-instantiated template, meaning
      a template whose parameters are pattern variables of another template. */
  bool isPartialInstantiation() const { return traits_.contains(PartialInstantiation); }

  /** Returns true if this symbol is a template *and* has one or more pattern variables
      in its template signature. */
  bool hasUnboundTypeParams() const;

  /** Return the template signature object. */
  const Template * templateSignature() const { return tsig_; }
  Template * templateSignature() { return tsig_; }
  void setTemplateSignature(Template * tsig) { tsig_ = tsig; }

  /** Return true if this declaration is a template. */
  bool isTemplateInstance() const { return tinst_ != NULL; }

  /** If this is a template specialization, return the arguments. */
  const TemplateInstance * templateInstance() const { return tinst_; }
  TemplateInstance * templateInstance() { return tinst_; }
  void setTemplateInstance(TemplateInstance * ti) { tinst_ = ti; }

  // Modifier methods

  /** The visibility of this value. */
  Visibility visibility() const { return modifiers_.visibility; }
  void setVisibility(Visibility vis) { modifiers_.visibility = vis; }

  /** The storage class of this value. */
  StorageClass storageClass() const { return storage_; }
  void setStorageClass(StorageClass sc) { storage_ = sc; }

#if 0
  /** The condition that indicates whether this symbol is enabled. */
  const Expr * getCondition() const { return modifiers.condition; }
#endif

  // Internal methods made visible for testing

  /** Print out all of the scopes for this type. */
  void dumpHierarchy(bool full = true) const;

  void trace() const;
  static inline bool classof(const Defn *) { return true; }
};

/** EnumSet of defn types. */
typedef SmallEnumSet<Defn::DefnType, Defn::DefnTypeCount> DefnTypeSet;

/** DefnTypeSet of defn types that are assignable values. */
static const DefnTypeSet LVALUE_DEFS = DefnTypeSet::of(
    Defn::Var, Defn::Let, Defn::Property, Defn::Indexer, Defn::Parameter);

/** DefnTypeSet of defn types that have types. */
static const DefnTypeSet TYPED_DEFNS = DefnTypeSet::of(
    Defn::Var, Defn::Let, Defn::MacroArg, Defn::Property, Defn::Function, Defn::Macro,
    Defn::Indexer, Defn::Parameter);

/** DefnTypeSet of defn types that are methods. */
static const DefnTypeSet METHOD_DEFS = DefnTypeSet::of(
    Defn::Function, Defn::Macro, Defn::Indexer);

/// -------------------------------------------------------------------
/// A definition that has a type - variable, function, etc.
class ValueDefn : public Defn {
public:
  /** Constructor that takes a name */
  ValueDefn(DefnType dtype, Module * m, StringRef name)
    : Defn(dtype, m, name)
    , definingScope_(NULL)
  {}

  /** Constructor that takes an AST declaration. */
  ValueDefn(DefnType dtype, Module * m, const ASTDecl * de)
    : Defn(dtype, m, de)
    , definingScope_(NULL)
  {}

  /** Type of this variable. */
  virtual const Type * type() const = 0;
  const Type * canonicalType() const { return dealias(type()); }

  /** Class in which this is defined, or NULL if defining scope is not a class. */
  const CompositeType * definingClass() const;
  CompositeType * definingClass();

  // Overrides

  Scope * definingScope() const { return definingScope_; }
  void setDefiningScope(Scope * scope) { definingScope_ = scope; }

  void trace() const;
  static inline bool classof(const ValueDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return TYPED_DEFNS.contains(de->defnType());
  }

protected:
  Scope * definingScope_;
};

/// -------------------------------------------------------------------
/// A reference an imported symbol as declared by an 'import' statement.
class ExplicitImportDefn : public Defn {
private:
  ExprList importValues_;
  Scope * definingScope_;

public:
  ExplicitImportDefn(Module * m, StringRef name, const ExprList & defs)
    : Defn(ExplicitImport, m, name)
    , importValues_(defs)
    , definingScope_(NULL)
  {
  }

  /** Get the value(s) of this import. */
  const ExprList & importValues() const { return importValues_; }
  ExprList & importValues() { return importValues_; }

  // Overrides

  Scope * definingScope() const { return definingScope_; }
  void setDefiningScope(Scope * scope) { definingScope_ = scope; }
  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ExplicitImportDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == ExplicitImport;
  }
};

// -------------------------------------------------------------------
// Functions

/** Format a list of parameters as comma-separated values. */
void formatParameterList(FormatStream & out, const ParameterList & params);

} // namespace tart

#endif // TART_DEFN_DEFN_H

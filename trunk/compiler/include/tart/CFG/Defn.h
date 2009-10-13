/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_DEFN_H
#define TART_CFG_DEFN_H

#ifndef TART_CFG_CONSTANT_H
#include "tart/CFG/Constant.h"
#endif

#ifndef TART_CFG_SCOPE_H
#include "tart/CFG/Scope.h"
#endif

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#ifndef TART_AST_DECL_H
#include "tart/AST/ASTDecl.h"
#endif

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

#ifndef TART_COMMON_SMALLENUMSET_H
#include "tart/Common/SmallEnumSet.h"
#endif

namespace llvm {
class Value;
class Function;
class Type;
}

namespace tart {

class FunctionType;
class TemplateSignature;
class TemplateInstance;
class Intrinsic;

/// -------------------------------------------------------------------
/// The various passes of analysis for a definition.
enum DefnPass {

  /** Create scope members. */
  Pass_CreateMembers,

  /** For classes: Resolve base class references. */
  Pass_ResolveBaseTypes,

  /** For all defn types - resolve attribute references. */
  Pass_ResolveAttributes,

  /** Determine what constructors the class has. */
  Pass_AnalyzeConstructors,

  /** Analyze all fields. */
  Pass_AnalyzeFields,

  /** Analyze types that are members of types. */
  Pass_AnalyzeMemberTypes,

  /** Analyze all methods. */
  Pass_AnalyzeMethods,

  /** Compile method tables and resolve all overloads. */
  Pass_ResolveOverloads,

  /** Handle initialization of static members. */
  Pass_ResolveStaticInitializers,

  /** Determine function return type. */
  Pass_ResolveReturnType,

  /** Determine function parameter types. */
  Pass_ResolveParameterTypes,

  /** Determine function modifiers (abstract, etc.) */
  Pass_ResolveModifiers,

  /** Determine variable type. */
  Pass_ResolveVarType,

  /** For native pointers and arrays. */
  Pass_ResolveElementType,

  /** Build CFG pass */
  Pass_CreateCFG,

  /** Resolve an import statement. */
  Pass_ResolveImport,

  /** Resolve all module members. */
  Pass_ResolveModuleMembers,

  /** Number of analysis passes. */
  DefnPassCount
};

/** EnumSet of defn states. */
typedef SmallEnumSet<DefnPass, DefnPassCount> DefnPasses;

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
    TemplateParam,
    Mod,
    ExplicitImport,

    DefnTypeCount,
  };

  enum Trait {
    Final,                  // Can't be overridden
    Abstract,               // Can't be instantiated
    ReadOnly,               // Can't be written to from non-privileged code
    Const,                  // Declares a field to be immutable.
    Extern,                 // Externally defined function
    Unsafe,                 // Requires unsafe language extensions
    Ctor,                   // Constructor function
    Singular,               // Has no unbound template params
    Synthetic,              // Generated via template
    Undefined,              // Undefining a definition in a base class.
    Nonreflective,          // Don't generate reflection data for this class.
    TemplateMember,         // Is a member of a template
    PartialInstantiation,   // A template instance whose variables are unbound template params.
    CompileTimeEvaluable,   // If set, it means that this def can be evaluated in the compiler.

    //Commutative = (1<<10),  // A function whose order of arguments can be reversed
    //Associative = (1<<11),  // A varargs function that can be combined with itself.

    TraitCount,
  };

  typedef SmallEnumSet<Trait, TraitCount> Traits;

protected:
  friend class OrderedSymbolTable;
  friend class IterableScope;
  friend class TemplateSignature;
  friend class TemplateInstance;

  DefnType defnType_;         // What type of defn this is
  SourceLocation loc;         // Location where this was defined.
  const char * name_;         // Local name (copied from decl)
  const ASTDecl * ast_;        // The source declaration of this defintion
  DeclModifiers modifiers_;   // Modifier flags
  Module * module_;           // Module in which this symbol is defined.
  Defn * parentDefn_;         // Definition which encloses this one.
  Defn * nextInScope_;        // Pointer to the next defn in parent scope
  std::string qname_;         // Fully-qualified name.
  mutable std::string lnkName;// External linkage name
  TemplateSignature * tsig_;  // Template signature
  TemplateInstance * tinst_;  // Template arguments
  DefnPasses running_;         // Analysis passes currently in progress
  DefnPasses finished_;        // Analysis passes completed
  ExprList attrs_;             // List of attributes
  Traits traits_;              // Traits of this defn

public:
  /** Constructor that takes a name */
  Defn(DefnType dtype, Module * module, const char * name);

  /** Constructor that takes an AST declaration and a parent. */
  Defn(DefnType dtype, Module * module, const ASTDecl * de);

  /** The type of this definition. */
  DefnType defnType() const { return defnType_; }

  /** Get the (short) name of this declaration. */
  const char * name() const { return name_; }

  /** Get the fully qualified name of the definition. */
  const std::string & qualifiedName() const;
  std::string & qualifiedName();

  /** Set the fully qualified name of the definition. */
  void setQualifiedName(const std::string & name) { qname_ = name; }

  /** Get the linkage name of this definition. */
  virtual const std::string & linkageName() const;

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

  /** Get the source location where this definition was defined. */
  const SourceLocation & location() const;
  void setLocation(const SourceLocation & l) { loc = l; }

  /** Get the list of attributes. */
  const ExprList & attrs() const { return attrs_; }
  ExprList & attrs() { return attrs_; }

  /** Find the first attribute of the specified type. */
  const Expr * findAttribute(const Type * attrType) const;
  const Expr * findAttribute(const char * attrTypeName) const;

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

  bool isAbstract() const { return traits_.contains(Abstract); }
  bool isFinal() const { return traits_.contains(Final); }
  virtual bool isSingular() const { return traits_.contains(Singular); }
  bool isSynthetic() const { return traits_.contains(Synthetic); }
  bool isExtern() const { return traits_.contains(Extern); }
  bool isCtor() const { return traits_.contains(Ctor); }
  bool isUndefined() const { return traits_.contains(Undefined); }

  void setSingular(bool t) {
    if (t) {
      traits_.add(Singular);
    } else {
      traits_.remove(Singular);
    }
  }

  // Member/Container methods.

  /** Add a null-terminated list of members to this defn. */
  void addMembers(Defn ** member);

  /** Get the scope in which this is defined. */
  virtual Scope * definingScope() const = 0;
  virtual void setDefiningScope(Scope * scope) = 0;

  /** Get the module in which this declaration was defined. */
  virtual Module * module() const { return module_; }

  /** Get the defn in which this is defined, if any. */
  Defn * parentDefn() const { return parentDefn_; }
  void setParentDefn(Defn * defn) { parentDefn_ = defn; }

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

  /** Return the template signature object. */
  const TemplateSignature * templateSignature() const { return tsig_; }
  TemplateSignature * templateSignature() { return tsig_; }
  void setTemplateSignature(TemplateSignature * tsig) { tsig_ = tsig; }

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
  StorageClass storageClass() const { return modifiers_.storageClass; }
  void setStorageClass(StorageClass sc) { modifiers_.storageClass = sc; }

#if 0
  /** The condition that indicates whether this symbol is enabled. */
  const Expr * getCondition() const { return modifiers.condition; }
#endif

  // Analysis pass methods

  /** Return the set of passes in progress. */
  DefnPasses & running() { return running_; }

  /** Return true if the specified pass is running. */
  bool isPassRunning(DefnPass pass) const { return running_.contains(pass); }

  /** Return true if the specified pass is finished. */
  bool isPassFinished(DefnPass pass) const { return finished_.contains(pass); }

  /** Return the set of analysis passes completed so far. */
  DefnPasses & finished() { return finished_; }

  /** Mark a pass has started. */
  bool beginPass(DefnPass pass);

  /** Mark a pass as ended. */
  void finishPass(DefnPass pass) {
    running_.remove(pass);
    finished_.add(pass);
  }

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
    Defn::Var, Defn::Let, Defn::Property, Defn::Function, Defn::Macro,
    Defn::Indexer, Defn::Parameter, Defn::TemplateParam);

/** DefnTypeSet of defn types that are methods. */
static const DefnTypeSet METHOD_DEFS = DefnTypeSet::of(
    Defn::Function, Defn::Macro, Defn::Indexer);

/// -------------------------------------------------------------------
/// A definition of a namespace
class NamespaceDefn : public Defn {
private:
  IterableScope members;

public:
  /** Constructor that takes a name */
  NamespaceDefn(Module * m, const char * name);

  /** Constructor that takes an AST declaration. */
  NamespaceDefn(Module * m, const ASTDecl * de);

  /** Get the scope containing the members of this namespace. */
  const IterableScope & memberScope() const { return members; }
  IterableScope & memberScope() { return members; }

  // Overrides

  Scope * definingScope() const { return members.parentScope(); }
  void setDefiningScope(Scope * scope) { members.setParentScope(scope); }
  void format(FormatStream & out) const;
  void trace() const;

  static inline bool classof(const NamespaceDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Namespace;
  }
};

/// -------------------------------------------------------------------
/// A definition that has a type - variable, function, etc.
class ValueDefn : public Defn {
protected:
  Scope * definingScope_;

public:
  /** Constructor that takes a name */
  ValueDefn(DefnType dtype, Module * m, const char * name)
    : Defn(dtype, m, name)
    , definingScope_(NULL)
  {}

  /** Constructor that takes an AST declaration. */
  ValueDefn(DefnType dtype, Module * m, const ASTDecl * de)
    : Defn(dtype, m, de)
    , definingScope_(NULL)
  {}

  /** Type of this variable. */
  virtual TypeRef type() const = 0;

  // Overrides

  Scope * definingScope() const { return definingScope_; }
  void setDefiningScope(Scope * scope) { definingScope_ = scope; }

  void trace() const;
  static inline bool classof(const ValueDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return TYPED_DEFNS.contains(de->defnType());
  }
};

/// -------------------------------------------------------------------
/// A definition of a variable
class VariableDefn : public ValueDefn {
public:
  /** Constructor that takes a name */
  VariableDefn(DefnType dtype, Module * m, const char * name, Expr * value = NULL)
    : ValueDefn(dtype, m, name)
    , type_(value ? value->type() : NULL)
    , initValue_(value)
    , irValue_(NULL)
    , memberIndex_(0)
    , memberIndexRecursive_(0)
    , isConstant_(dtype == Defn::Let)
  {}

  /** Constructor that takes an AST declaration. */
  VariableDefn(DefnType dtype, Module * m, const ASTDecl * de)
    : ValueDefn(dtype, m, de)
    , type_(NULL)
    , initValue_(NULL)
    , irValue_(NULL)
    , memberIndex_(0)
    , memberIndexRecursive_(0)
, isConstant_(dtype == Defn::Let)
  {}

  /** Initial value for this variable. */
  const Expr * initValue() const { return initValue_; }
  Expr * initValue() { return initValue_; }
  void setInitValue(Expr * e) { initValue_ = e; }

  /** IR representation of this function. */
  llvm::Value * irValue() const { return irValue_; }
  void setIRValue(llvm::Value * ir) const { irValue_ = ir; }

  /** For member variables, the index of this field within the class. */
  int memberIndex() const { return memberIndex_; }
  void setMemberIndex(int index) { memberIndex_ = index; }

  /** For member variables, the index of this field within the class. */
  int memberIndexRecursive() const { return memberIndexRecursive_; }
  void setMemberIndexRecursive(int index) { memberIndexRecursive_ = index; }

  /** Set the type of this variable. */
  void setType(const TypeRef & ty) { type_= ty; }

  /** True if the value of this variable is always the initializer. This will always be
      true for 'let' variables except in the special case of 'let' variables that
      are of instance scope and which are initialized in the constructor. */
  bool isConstant() const { return isConstant_; }
  void setIsConstant(bool isConstant) { isConstant_ = isConstant; }

  // Overrides

  TypeRef type() const { return type_; }
  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const VariableDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Let || de->defnType() == Var;
  }

private:
  TypeRef type_;
  Expr * initValue_;
  mutable llvm::Value * irValue_;
  int memberIndex_;
  int memberIndexRecursive_;
  bool isConstant_;
};

/// -------------------------------------------------------------------
/// A definition of a property
class PropertyDefn : public ValueDefn {
private:
  TypeRef type_;
  IterableScope accessorScope_;  // Scope in which getter/setter are defined.
  FunctionDefn * getter_;    // The getter method
  FunctionDefn * setter_;    // The setter method

public:
  /** Constructor that takes an AST */
  PropertyDefn(DefnType dtype, Module * m, const ASTPropertyDecl * ast)
    : ValueDefn(dtype, m, ast)
    , getter_(NULL)
    , setter_(NULL)
  {
    accessorScope_.setScopeName(ast_->name());
  }

  FunctionDefn * getter() const { return getter_; }
  void setGetter(FunctionDefn * f) { getter_ = f; }

  FunctionDefn * setter() const { return setter_; }
  void setSetter(FunctionDefn * f) { setter_ = f; }

  const Scope & accessorScope() const { return accessorScope_; }
  Scope & accessorScope() { return accessorScope_; }

  void setType(const TypeRef & t) { type_ = t; }

  // Overrides

  TypeRef type() const { return type_; }
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
};

/// -------------------------------------------------------------------
/// A definition of an indexer
class IndexerDefn : public PropertyDefn {
public:
  /** Constructor that takes an AST */
  IndexerDefn(DefnType dtype, Module * m, const ASTPropertyDecl * ast)
    : PropertyDefn(dtype, m, ast)
  {}

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const IndexerDefn *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Indexer;
  }
};

/// -------------------------------------------------------------------
/// A reference an imported symbol as declared by an 'import' statement.
class ExplicitImportDefn : public Defn {
private:
  ExprList importValues_;
  Scope * definingScope_;

public:
  ExplicitImportDefn(Module * m, const char * name, const ExprList & defs)
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

/** Return the string name of a pass. */
const char * getPassName(DefnPass pass);

/** Stream operator for pass names. */
FormatStream & operator<<(FormatStream & out, DefnPass pass);

} // namespace tart

#endif

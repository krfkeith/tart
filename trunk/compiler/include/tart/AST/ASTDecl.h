/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_AST_ASTDECL_H
#define TART_AST_ASTDECL_H

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

#ifndef TART_AST_DOCCOMMENT_H
#include "tart/AST/DocComment.h"
#endif

namespace tart {

class Stmt;

/// -------------------------------------------------------------------
/// Visibility of a declaration
enum Visibility {
  Internal,           // Visible to this package only
  Public,
  Protected,
  Private,

  Default = Internal
};

/// -------------------------------------------------------------------
/// Storage class of a declaration
enum StorageClass {
  Storage_Global,     // A global variable.
  Storage_Instance,   // Instance variable - one copy per instance
  Storage_Class,      // Class variable - one copy per class
  Storage_Static,     // Static variable - one copy only
  Storage_Local,      // A local variable
};

/// -------------------------------------------------------------------
// Defines the possible relationships between a type and its overloads
enum Variance {
  Invariant = 0,      // Overloads must be the same time
  Covariant,          // Overloads can be the same or broader type
  Contravariant,      // Overloads can be the same or narrower type
};

/// -------------------------------------------------------------------
/// Declaration flags
enum DeclFlags {
  Final = (1<<0),         // Can't be overridden
  Abstract = (1<<1),      // Can't be instantiated
  ReadOnly = (1<<2),      // You can't modify it, although someone else can
  Mutable = (1<<3),       // Can be modified even if object is otherwise readonly
  Immutable = (1<<4),     // Guaranteed never to change
  Adopted = (1<<5),       // Inherits mutability from parent object
  Undef = (1<<6),         // Undefined method
  Override = (1<<7),      // Overridden method
  Static = (1<<8),        // Was declared static
};

/// -------------------------------------------------------------------
/// Flags that apply to function parameters
enum ParameterFlags {
  Param_Variadic    = (1<<0),   // Multiple args
  Param_KeywordOnly = (1<<1),   // Only settable via keyword
  Param_Explicit    = (1<<2),   // No type conversion - type must be exact
  Param_Star        = (1<<3),   // Parameter type is expanded tuple
};

/// -------------------------------------------------------------------
/// Declaration modifiers
struct DeclModifiers {
  uint32_t        flags;
  Visibility      visibility;
  ASTNode       * condition;

  DeclModifiers()
    : flags(0), visibility(Public), condition(NULL) {}

  DeclModifiers(const DeclModifiers & src)
    : flags(src.flags)
    , visibility(src.visibility)
    , condition(src.condition) {}

  DeclModifiers(uint32_t flg)
    : flags(flg), condition(NULL) {}
};

/// ---------------------------------------------------------------
/// A definition is anything that binds an expression to a name.
class ASTDecl : public ASTNode {
protected:

  StringRef name_;
  DeclModifiers modifiers_;
  ASTDeclList members_;
  ASTNodeList imports_;
  ASTNodeList attributes_;
  DocComment docComment_;

  // Protected constructor, since this type is abstract. */
  ASTDecl(NodeType ntype, const SourceLocation & loc, StringRef nm,
      const DeclModifiers & mods)
      : ASTNode(ntype, loc)
      , name_(nm)
      , modifiers_(mods)
  {}

public:

  /** The name of this declaration. */
  StringRef name() const { return name_; }

  /** The list of member definitions. */
  const ASTDeclList & members() const { return members_; }
  ASTDeclList & members() { return members_; }

  /** The list of imports. */
  const ASTNodeList & imports() const { return imports_; }
  ASTNodeList & imports() { return imports_; }

  /** Add a new member to this decl. */
  void addMember(ASTDecl * member) { members_.push_back(member); }

  /** The modifier for this definition. */
  const DeclModifiers & modifiers() const { return modifiers_; }
  DeclModifiers & modifiers() { return modifiers_; }
  //StorageClass storageClass() const { return modifiers_.storageClass; }
  Visibility visibility() const { return modifiers_.visibility; }

  /** The list of attributes. */
  const ASTNodeList & attributes() const { return attributes_; }
  ASTNodeList & attributes() { return attributes_; }

  /** The doc comment for this declaration. */
  const DocComment & docComment() const { return docComment_; }
  DocComment & docComment() { return docComment_; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTDecl *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() >= ASTNode::DefFirst && e->nodeType() <= ASTNode::DefLast;
  }
};

/// ---------------------------------------------------------------
/// A namespace definition
class ASTNamespace : public ASTDecl {
public:
  // Protected constructor, since this type is abstract. */
  ASTNamespace(const SourceLocation & loc, StringRef nm)
      : ASTDecl(Namespace, loc, nm, DeclModifiers())
  {}

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTNamespace *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() == ASTNode::Namespace;
  }
};

/// ---------------------------------------------------------------
/// A type definition
class ASTTypeDecl : public ASTDecl {
private:
  ASTNodeList bases_;

public:
  // Protected constructor, since this type is abstract. */
  ASTTypeDecl(NodeType ntype, const SourceLocation & loc, StringRef nm,
      const ASTNodeList & baseList, const DeclModifiers & mods)
      : ASTDecl(ntype, loc, nm, mods)
      , bases_(baseList)
  {}

  /** The list of base types. */
  const ASTNodeList & bases() const { return bases_; }
  ASTNodeList & bases() { return bases_; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTTypeDecl *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() >= ASTNode::Class &&
          e->nodeType() <= ASTNode::TypeAlias;
  }
};

/// ---------------------------------------------------------------
/// A variable (let or var) definition
class ASTVarDecl : public ASTDecl {
protected:
  ASTNode * type_;
  ASTNode * value_;

public:
  ASTVarDecl(NodeType ntype, const SourceLocation & loc, StringRef nm,
      ASTNode * ptype, ASTNode * val, const DeclModifiers & mods)
    : ASTDecl(ntype, loc, nm, mods)
    , type_(ptype)
    , value_(val)
  {}


  /** The type of a variable is it's declared type. */
  const ASTNode * type() const { return type_; }
  ASTNode * type() { return type_; }

  /** The value of a variable is its initializer. */
  const ASTNode * value() const { return value_; }
  ASTNode * value() { return value_; }
  void setValue(ASTNode * val) { value_ = val; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTVarDecl *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() == ASTNode::Let ||
          e->nodeType() == ASTNode::Var ||
          e->nodeType() == ASTNode::Param;
  }
};

/// ---------------------------------------------------------------
/// A property definition
class ASTPropertyDecl : public ASTDecl {
private:
  ASTNode * type_;
  ASTFunctionDecl * getter_;
  ASTFunctionDecl * setter_;
  ASTParamList params_;

public:
  ASTPropertyDecl(NodeType ntype, const SourceLocation & loc, StringRef nm,
      ASTNode * ptype, const DeclModifiers & mods)
    : ASTDecl(ntype, loc, nm, mods)
    , type_(ptype)
    , getter_(NULL)
    , setter_(NULL)
  {}

  ASTPropertyDecl(const SourceLocation & loc, StringRef nm, ASTNode * ptype,
      const DeclModifiers & mods)
    : ASTDecl(ASTNode::Prop, loc, nm, mods)
    , type_(ptype)
    , getter_(NULL)
    , setter_(NULL)
  {}

  /** The property type. */
  const ASTNode * type() const { return type_; }
  ASTNode * type() { return type_; }

  /** The getter method. */
  const ASTFunctionDecl * getter() const { return getter_; }
  ASTFunctionDecl * getter() { return getter_; }
  void setGetter(ASTFunctionDecl * fd) { getter_ = fd; }

  /** The setter method. */
  const ASTFunctionDecl * setter() const { return setter_; }
  ASTFunctionDecl * setter() { return setter_; }
  void setSetter(ASTFunctionDecl * fd) { setter_ = fd; }

  /** The list of indexer parameters. */
  const ASTParamList & params() const { return params_; }
  ASTParamList & params() { return params_; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTPropertyDecl *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() == ASTNode::Prop ||
          e->nodeType() == ASTNode::Idx;
  }
};

/// ---------------------------------------------------------------
/// A function definition
class ASTFunctionDecl : public ASTDecl {
private:
  ASTParamList params_;
  ASTNode * returnType_;
  Stmt * body_;

public:
  ASTFunctionDecl(NodeType ntype, const SourceLocation & loc, StringRef nm,
      const ASTParamList & paramList, ASTNode * rtype, const DeclModifiers & mods)
    : ASTDecl(ntype, loc, nm, mods)
    , params_(paramList)
    , returnType_(rtype)
    , body_(NULL)
  {}

  /** The list of function parameters. */
  const ASTParamList & params() const { return params_; }
  ASTParamList & params() { return params_; }

  /** The function return type. */
  const ASTNode * returnType() const { return returnType_; }
  ASTNode * returnType() { return returnType_; }

  /** The function body. */
  const Stmt * body() const { return body_; }
  Stmt * body() { return body_; }
  void setBody(Stmt * b) { body_ = b; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTFunctionDecl *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() == ASTNode::Function || e->nodeType() == ASTNode::Macro;
  }
};

/// ---------------------------------------------------------------
/// A parameter definition
class ASTParameter : public ASTVarDecl {
private:
  int flags_;

public:
  ASTParameter(const SourceLocation & loc, StringRef name,
      ASTNode * typ, ASTNode * val, int flgs)
    : ASTVarDecl(Param, loc, name, typ, val, DeclModifiers())
    , flags_(flgs)
  {}

  int flags() const { return flags_; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTParameter *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() == ASTNode::Param;
  }
};

/// ---------------------------------------------------------------
/// A template type variable
class ASTTypeVariable : public ASTVarDecl {
public:
  enum ConstraintType {
    IS_INSTANCE,
    IS_SUBTYPE,
    IS_SUPERTYPE,
    IS_QUALIFIER,
    IS_TYPE_CTOR,
  };

  ASTTypeVariable(const SourceLocation & loc, StringRef name, ASTNode * typ,
      ConstraintType constraint, bool isVariadic)
    : ASTVarDecl(TypeVar, loc, name, typ, NULL, DeclModifiers())
    , constraint_(constraint)
    , isVariadic_(isVariadic)
  {}

  ConstraintType constraint() const { return constraint_; }

  bool isVariadic() const { return isVariadic_; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTTypeVariable *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() == ASTNode::TypeVar;
  }

private:
  ConstraintType constraint_;
  bool isVariadic_;
};

/// ---------------------------------------------------------------
/// A template definition
class ASTTemplate : public ASTDecl {
private:
  ASTDecl * body_;
  ASTNodeList params_;
  ASTNodeList requirements_;

public:
  ASTTemplate(ASTDecl * bod, ASTNodeList & paramList, ASTNodeList & requirements)
      : ASTDecl(Template, bod->location(), bod->name(), bod->modifiers())
      , body_(bod)
      , params_(paramList)
      , requirements_(requirements)
  {}

  /** The body of the template. */
  const ASTDecl * body() const { return body_; }
  ASTDecl * body() { return body_; }

  /** The list of template parameters. */
  const ASTNodeList & params() const { return params_; }
  ASTNodeList & params() { return params_; }

  /** The list of template constraints. */
  const ASTNodeList & requirements() const { return requirements_; }
  ASTNodeList & requirements() { return requirements_; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTTemplate *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->nodeType() == ASTNode::Template;
  }
};

// ---------------------------------------------------------------
// Convenience functions

/** Format a list of parameters as comma-separated values. */
void formatParamList(FormatStream & out, const ASTParamList & params_);

/** Format a list of parameters as comma-separated values. */
void formatTemplateParamList(FormatStream & out, const ASTNodeList & params_);

}

#endif

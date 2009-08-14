/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_AST_ASTDECL_H
#define TART_AST_ASTDECL_H

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
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
  Storage_Param,      // A parameter
  Storage_Closure,    // A closure variable
};

/// -------------------------------------------------------------------
// Defines the possible relationships between a type and its overloads
enum Variance {
  Invariant = 0,      // Overloads must be the same time
  Covariant,          // Overloads can be the same or broader type
  Contravariant,      // Overloads can be the same or narrower type
};

/// -------------------------------------------------------------------
/// Attribute targets - constraints on where attributes can be placed.
enum AttributeTargets {
  AttributeTarget_Class       = (1<<0),
  AttributeTarget_Interface   = (1<<1),
  AttributeTarget_Struct      = (1<<2),
  AttributeTarget_Enum        = (1<<3),
  AttributeTarget_Attribute   = (1<<4),
  AttributeTarget_Function    = (1<<5),
  AttributeTarget_Parameter   = (1<<6),
  AttributeTarget_ReturnValue = (1<<7),
  AttributeTarget_Field       = (1<<8),
  AttributeTarget_Property    = (1<<9),
  AttributeTarget_Template    = (1<<10),
  AttributeTarget_Macro       = (1<<11),
  AttributeTarget_Constructor = (1<<12),
};

/// -------------------------------------------------------------------
/// Attribute propagation
enum AttributePropagation {
  Propagate_Descendants = (1<<0), // Attribute copied to descendents.
  Propagate_Members = (1<<1),     // Attribute copied to members (with appropriate target type).
  Propagate_Callers = (1<<2),     // Attribute copied to callers.
};

/// -------------------------------------------------------------------
/// Attribute retention
enum AttributeRetention {
  Retain_CompileTime = (1<<0),
  Retain_RunTime = (1<<1),
};

/// -------------------------------------------------------------------
/// Declaration flags
enum DeclFlags {
  Final = (1<<0),         // Can't be overridden
  Abstract = (1<<1),      // Can't be instantiated
  ReadOnly = (1<<2),      // Can't be written to from non-privileged code
  Modified = (1<<4),      // Whether definition was modified after creation.
  Extern = (1<<5),        // Externally defined function
  Native = (1<<6),        // Native function
  Export = (1<<7),        // Exported symbol
};

/// -------------------------------------------------------------------
/// Flags that apply to function parameters
enum ParameterFlags {
  Param_Variadic    = (1<<0),   // Multiple args
  Param_KeywordOnly = (1<<1),   // Only settable via keyword
  Param_Explicit    = (1<<2),   // No type conversion - type must be exact
};

/// -------------------------------------------------------------------
/// Declaration modifiers
struct DeclModifiers {
  uint32_t        flags;
  StorageClass    storageClass;
  Visibility      visibility;
  ASTNode       * condition;

  DeclModifiers()
    : flags(0), storageClass(Storage_Global), visibility(Internal),
      condition(NULL) {}

  DeclModifiers(const DeclModifiers & src)
    : flags(src.flags)
    , storageClass(src.storageClass)
    , visibility(src.visibility)
    , condition(src.condition) {}

  DeclModifiers(StorageClass sc, uint32_t flg = 0)
    : flags(flg), storageClass(sc), visibility(Public), condition(NULL) {}
};

/// ---------------------------------------------------------------
/// A definition is anything that binds an expression to a name.
class ASTDecl : public ASTNode {
protected:

  const char * name_;
  DeclModifiers modifiers_;
  ASTDeclList members_;
  ASTNodeList attributes_;

  // Protected constructor, since this type is abstract. */
  ASTDecl(NodeType type, const SourceLocation & loc, const char * nm,
      const DeclModifiers & mods)
      : ASTNode(type, loc)
      , name_(nm)
      , modifiers_(mods)
  {}

public:

  /** The name of this declaration. */
  const char * getName() const { return name_; }

  /** The list of member definitions. */
  const ASTDeclList & members() const { return members_; }
  ASTDeclList & members() { return members_; }

  /** Add a new member to this decl. */
  void addMember(ASTDecl * member) { members_.push_back(member); }

  /** The modifier for this definition. */
  const DeclModifiers & modifiers() const { return modifiers_; }
  StorageClass storageClass() const { return modifiers_.storageClass; }
  Visibility visibility() const { return modifiers_.visibility; }

  /** The list of attributes. */
  const ASTNodeList & attributes() const { return attributes_; }
  ASTNodeList & attributes() { return attributes_; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTDecl *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->getNodeType() >= ASTNode::DefFirst &&
        e->getNodeType() <= ASTNode::DefLast;
  }
};

/// ---------------------------------------------------------------
/// A namespace definition
class ASTNamespace : public ASTDecl {
public:
  // Protected constructor, since this type is abstract. */
  ASTNamespace(const SourceLocation & loc, const char * nm)
      : ASTDecl(Namespace, loc, nm, DeclModifiers())
  {}

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTNamespace *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->getNodeType() == ASTNode::Namespace;
  }
};

/// ---------------------------------------------------------------
/// A type definition
class ASTTypeDecl : public ASTDecl {
private:
  ASTNodeList bases_;

public:
  // Protected constructor, since this type is abstract. */
  ASTTypeDecl(NodeType type, const SourceLocation & loc, const char * nm,
      const ASTNodeList & baseList, const DeclModifiers & mods)
      : ASTDecl(type, loc, nm, mods)
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
      return e->getNodeType() >= ASTNode::Class &&
          e->getNodeType() <= ASTNode::Enum;
  }
};

/// ---------------------------------------------------------------
/// A variable (let or var) definition
class ASTVarDecl : public ASTDecl {
protected:
  ASTNode * type;
  ASTNode * value;

public:
  ASTVarDecl(NodeType type, const SourceLocation & loc, const char * nm,
      ASTNode * ptype, ASTNode * val, const DeclModifiers & mods)
    : ASTDecl(type, loc, nm, mods)
    , type(ptype)
    , value(val)
  {}


  /** The type of a variable is it's declared type. */
  const ASTNode * getType() const { return type; }
  ASTNode * getType() { return type; }

  /** The value of a variable is its initializer. */
  const ASTNode * getValue() const { return value; }
  ASTNode * getValue() { return value; }
  void setValue(ASTNode * val) { value = val; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTVarDecl *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->getNodeType() == ASTNode::Let ||
          e->getNodeType() == ASTNode::Var ||
          e->getNodeType() == ASTNode::Param;
  }
};

/// ---------------------------------------------------------------
/// A property definition
class ASTPropertyDecl : public ASTDecl {
private:
  ASTNode * type;
  ASTFunctionDecl * getter_;
  ASTFunctionDecl * setter_;
  ASTParamList params_;

public:
  ASTPropertyDecl(NodeType type, const SourceLocation & loc, const char * nm,
      ASTNode * ptype, const DeclModifiers & mods)
    : ASTDecl(type, loc, nm, mods)
    , type(ptype)
    , getter_(NULL)
    , setter_(NULL)
  {}

  ASTPropertyDecl(const SourceLocation & loc, const char * nm, ASTNode * ptype,
      const DeclModifiers & mods)
    : ASTDecl(ASTNode::Prop, loc, nm, mods)
    , type(ptype)
    , getter_(NULL)
    , setter_(NULL)
  {}

  /** The property type. */
  const ASTNode * getType() const { return type; }
  ASTNode * getType() { return type; }

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
      return e->getNodeType() == ASTNode::Prop ||
          e->getNodeType() == ASTNode::Idx;
  }
};

/// ---------------------------------------------------------------
/// A function definition
class ASTFunctionDecl : public ASTDecl {
private:
  ASTParamList params_;
  ASTNode * returnType_;
  Stmt * body;
  int generatorIndex;

public:
  ASTFunctionDecl(NodeType type, const SourceLocation & loc, const char * nm,
      ASTParamList & paramList, ASTNode * rtype, const DeclModifiers & mods)
    : ASTDecl(type, loc, nm, mods)
    , params_(paramList)
    , returnType_(rtype)
    , body(NULL)
  {}

  /** The list of function parameters. */
  const ASTParamList & params() const { return params_; }
  ASTParamList & params() { return params_; }

  /** The function return type. */
  const ASTNode * returnType() const { return returnType_; }
  ASTNode * returnType() { return returnType_; }

  /** The function body. */
  const Stmt * getBody() const { return body; }
  Stmt * getBody() { return body; }
  void setBody(Stmt * b) { body = b; }

  /** For functions that are generators, the number of yield statements. */
  int nextGeneratorIndex() { return generatorIndex++; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTFunctionDecl *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->getNodeType() == ASTNode::Function ||
          e->getNodeType() == ASTNode::Macro;
  }
};

/// ---------------------------------------------------------------
/// A parameter definition
class ASTParameter : public ASTVarDecl {
private:
  int flags_;

public:
  ASTParameter(const SourceLocation & loc, const char * name,
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
      return e->getNodeType() == ASTNode::Param;
  }
};

/// ---------------------------------------------------------------
/// A template pattern variable
class ASTPatternVar : public ASTVarDecl {
private:
  int flags;

public:
  ASTPatternVar(const SourceLocation & loc, const char * name,
      ASTNode * typ, int flgs = 0)
    : ASTVarDecl(PatternVar, loc, name, typ, NULL, DeclModifiers())
    , flags(flgs)
  {}

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTPatternVar *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->getNodeType() == ASTNode::PatternVar;
  }
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
      : ASTDecl(Template, bod->getLocation(), bod->getName(), bod->modifiers())
      , body_(bod)
      , params_(paramList)
      , requirements_(requirements)
  {}

  /** The body of the template. */
  const ASTDecl * getBody() const { return body_; }
  ASTDecl * getBody() { return body_; }

  /** The list of template parameters. */
  const ASTNodeList & params() const { return params_; }
  ASTNodeList & params() { return params_; }

  /** The list of template parameters. */
  const ASTNodeList & requirements() const { return requirements_; }
  ASTNodeList & requirements() { return requirements_; }

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const ASTTemplate *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->getNodeType() == ASTNode::Template;
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

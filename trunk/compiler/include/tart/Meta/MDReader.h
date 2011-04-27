/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_MDREADER_H
#define TART_META_MDREADER_H

namespace llvm {
class NamedMDNode;
class MDNode;
class MDString;
}

namespace tart {

class ASTTemplate;
class Module;
class Scope;
class Defn;
class TypeDefn;
class EnumType;
class AttributeInfo;

/// -------------------------------------------------------------------
/// Helper class for reading metadata nodes.

class NodeRef {
public:
  NodeRef() : node_(NULL) {}
  NodeRef(const llvm::MDNode * node) : node_(node) {}
  NodeRef(const NodeRef & nref) : node_(nref.node_) {}

  NodeRef & operator=(const NodeRef & nref) {
    node_ = nref.node_;
    return *this;
  }

  NodeRef & operator=(const llvm::MDNode * node) {
    node_ = node;
    return *this;
  }

  /** Direct reference to the metadata node. */
  const llvm::MDNode * node() const { return node_; }

  /** Return the Nth argument of the node. May be null. */
  llvm::Value * arg(unsigned n) const;

  /** Whether this node reference is null. */
  bool isNull() const { return node_ == NULL; }

  /** Return the number of operands of the node. Returns 0 if
      this reference is null. */
  unsigned size() const;

  /** Return the nth arg as an integer. Aborts if the nth argument
      was null or the wrong type. */
  uint32_t intArg(unsigned n) const;

  /** Return the nth arg as a StringRef. Aborts if the nth argument
      was null or the wrong type. */
  llvm::StringRef strArg(unsigned n) const;

  /** Return the nth arg as a StringRef, or an empty string if the
      argument was NULL. */
  llvm::StringRef optStrArg(unsigned n) const;

  /** Return the nth arg as a NodeRef. Aborts if the nth argument was
      null or the wrong type. */
  NodeRef nodeArg(unsigned n) const;

  /** Return the nth arg as a NodeRef. Can return a null NodeRef if
      the argument was null. */
  NodeRef optNodeArg(unsigned n) const;

private:
  const llvm::MDNode * node_;
};

/// -------------------------------------------------------------------
/// Metadata reader.

class MDReader {
public:
  enum FieldIndices {
    // Module fields
    FIELD_MODULE_VERSION = 0,
    FIELD_MODULE_SOURCE,
    FIELD_MODULE_DEPS,
    FIELD_MODULE_TIMESTAMP,
    FIELD_MODULE_IMPORTS,
    FIELD_MODULE_EXPORTS,

    // Defn fields
    FIELD_DEFN_TYPE = 0,
    FIELD_DEFN_NAME,
    FIELD_DEFN_LOCATION,
    FIELD_DEFN_MODS,
    FIELD_DEFN_ATTRS,

    // Next field after DEFN fields
    FIELD_DEFN_NEXT,

    // For templates, the AST is always in slot 4
    FIELD_DEFN_AST = FIELD_DEFN_NEXT,

    // Typedef fields
    FIELD_TYPEDEF_IMPORTS = FIELD_DEFN_NEXT,
    FIELD_TYPEDEF_MEMBERS,
    FIELD_TYPEDEF_DETAILS,

    // Function fields
    FIELD_FUNCTION_RTYPE = FIELD_DEFN_NEXT,
    FIELD_FUNCTION_PARAMS,
    FIELD_FUNCTION_AST,
    FIELD_FUNCTION_VALUE,

    // Param fields
    FIELD_PARAM_TYPE = FIELD_DEFN_NEXT,
    FIELD_PARAM_DEFAULT,

    // Variable fields
    FIELD_VAR_TYPE = FIELD_DEFN_NEXT,
    FIELD_VAR_INIT,

    // Property fields
    FIELD_PROP_TYPE = FIELD_DEFN_NEXT,
    FIELD_PROP_ACCESSORS,

    // Namespace fields
    FIELD_NS_IMPORTS = FIELD_DEFN_NEXT,
    FIELD_NS_MEMBERS,

    // Typealias fields
    FIELD_TYPEALIAS_VALUE = FIELD_DEFN_NEXT,

    // Enum constants
    FIELD_ECONST_NAME = 0,
    FIELD_ECONST_VALUE,
  };

  MDReader(Module * module, Defn * subject) : module_(module), subject_(subject) {}

  /** Read the module-level data. */
  bool read(llvm::NamedMDNode * md);

  /** Read imports for the given definition. */
  bool readImports(Defn * de, ASTNodeList & imports);

  /** Read member definitions. */
  bool readMembers(Defn * parentDefn);

  /** Read member definitions. */
  bool readMembers(NodeRef members, Scope * scope, Defn * parentDefn);

  /** Read base classes for composite type. */
  bool readCompositeDetails(CompositeType * ty, TypeList & bases);

  /** Read base class for enum types. */
  const Type * readEnumBase(TypeDefn * ety);

  /** Read enum constants. */
  bool readEnumConstants(TypeDefn * ety);

  /** Read enum members. */
  bool readTypeMembers(TypeDefn * tdef, Scope * memberScope);

  /** Read template and function parameters for functions. */
  bool readFunctionType(FunctionDefn * fn);

  /** Read function body. */
  const Stmt * readFunctionBody(FunctionDefn * fn);

  /** Read type for properties. */
  bool readPropertyType(PropertyDefn * prop);

  /** Read accessors for properties. */
  bool readPropertyAccessors(PropertyDefn * prop);

  /** Read in the attribute list for a definition. */
  bool readAttributeList(Defn * de);

  /** Read in the template signature for the given definition. */
  bool readTemplateSignature(Defn * de, Scope * parent, const llvm::MDString * args);

  /** Read in a template definition. */
  ASTTemplate * readTemplate(SourceLocation loc, llvm::StringRef source, llvm::StringRef name);

private:
  bool readModuleImports(NodeRef node);

  Defn * readMember(NodeRef node, Scope * parent, StorageClass storage);

  bool readAccessorList(PropertyDefn * prop, NodeRef node);
  bool readExpressionList(SourceLocation loc, NodeRef exprs, ExprList & out);
  Expr * readExpression(SourceLocation loc, NodeRef node);
  const Type * readTypeRef(llvm::StringRef str);
  bool lookupName(llvm::StringRef qname, DefnList & result);
  Defn * lookupSymbol(llvm::StringRef name);

  Defn * setSubject(Defn * newSubject) {
    Defn * prev = subject_;
    subject_ = newSubject;
    return prev;
  }

  Module * module_;
  Defn * subject_;
  uint32_t version_;
};

} // namespace tart

#endif // TART_META_METADATAWRITER_H

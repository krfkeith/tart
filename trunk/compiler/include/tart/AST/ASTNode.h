/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_AST_ASTNODE_H
#define TART_AST_ASTNODE_H

#ifndef TART_COMMON_GC_H
#include "tart/Common/GC.h"
#endif

#ifndef TART_COMMON_SOURCELOCATION_H
#include "tart/Common/SourceLocation.h"
#endif

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

#ifndef LLVM_ADT_SMALLVECTOR_H
#include "llvm/ADT/SmallVector.h"
#endif

#ifndef LLVM_CONSTANTS_H
#include "llvm/Constants.h"
#endif

namespace tart {

using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::cast;
using llvm::cast_or_null;
using llvm::isa;

class Defn;

/// -------------------------------------------------------------------
/// Forward declarations
class ASTNode;
class ASTDecl;
class ASTParameter;
class ASTFunctionDecl;

/// -------------------------------------------------------------------
/// Container types
typedef llvm::SmallVector<ASTNode *, 4> ASTNodeList;
typedef llvm::SmallVector<const ASTNode *, 4> ASTConstNodeList;
typedef llvm::SmallVector<ASTDecl *, 8> ASTDeclList;
typedef llvm::SmallVector<ASTParameter *, 8> ASTParamList;

/// ---------------------------------------------------------------
/// Base class of all AST nodes.
class ASTNode : public GC, public Formattable, public Locatable {
public:
  enum NodeType {
    #define NODE_TYPE(x) x,
    #include "ASTNodeType.def"
    NodeTypeCount,

    // First and last declaration node types
    DefFirst = Class,
    DefLast = AnonClass,

    StmtFirst = Block,
    StmtLast = MatchAs
  };

protected:
  const NodeType nodeType_;
  SourceLocation loc;

public:
  ASTNode(NodeType nt, const SourceLocation & sl)
    : nodeType_(nt)
    , loc(sl)
  {}

  virtual ~ASTNode() {}

  /** Return the type of this AST node. */
  NodeType nodeType() const { return nodeType_; }

  /** Where in the source file this expression comes from. */
  const SourceLocation & location() const { return loc; }
  SourceLocation & location() { return loc; }

  /** Produce a string representation of this node and its children. */
  const std::string toString(int formatOptions = Format_Default) const;

  /** Produce a textual representation of this node and its children. */
  virtual void format(FormatStream & out) const;

  /** True if this is an error node. */
  bool isInvalid() const { return nodeType_ == Invalid; }

  // Overrides

  void trace() const { loc.trace(); }
  static inline bool classof(const ASTNode *) { return true; }

  /** A placeholder node used to signal an error in parsing. */
  static ASTNode INVALID;
};

/// -------------------------------------------------------------------
/// A reference to a name
class ASTIdent : public ASTNode {
private:
  const char * value_;

public:
  // Constructor needs to be public because we create static versions of this.
  ASTIdent(const SourceLocation & loc, const char * v)
    : ASTNode(Id, loc)
    , value_(v)
  {}

  // Alternate version of the constructor used for qualified names.
  ASTIdent(NodeType nt, const SourceLocation & loc, const char * v)
    : ASTNode(nt, loc)
    , value_(v)
  {}

  ASTIdent * get(const SourceLocation & loc, const char * value) {
    return new ASTIdent(loc, value);
  }

  const char * value() const { return value_; }

  void format(FormatStream & out) const;
  static inline bool classof(const ASTIdent *) { return true; }
  static inline bool classof(const ASTNode * ast) {
      return ast->nodeType() == ASTNode::Id;
  }

  // Infix operator names
  static ASTIdent operatorAdd;
  static ASTIdent operatorSub;
  static ASTIdent operatorMul;
  static ASTIdent operatorDiv;
  static ASTIdent operatorMod;
  static ASTIdent operatorBitAnd;
  static ASTIdent operatorBitOr;
  static ASTIdent operatorBitXor;
  static ASTIdent operatorLogicalAnd;
  static ASTIdent operatorLogicalOr;
  static ASTIdent operatorRSh;
  static ASTIdent operatorLSh;
  static ASTIdent operatorEq;
  static ASTIdent operatorNe;
  static ASTIdent operatorLT;
  static ASTIdent operatorGT;
  static ASTIdent operatorLE;
  static ASTIdent operatorGE;
  static ASTIdent operatorPLT;
  static ASTIdent operatorPGT;
  static ASTIdent operatorPLE;
  static ASTIdent operatorPGE;
  static ASTIdent operatorContains;

  // Unary operator names
  static ASTIdent operatorNegate;
  static ASTIdent operatorSucc;
  static ASTIdent operatorPred;
};

/// -------------------------------------------------------------------
/// A reference to a member
class ASTMemberRef : public ASTNode {
private:
  ASTNode * qualifier_;
  const char * memberName_;

public:
  // Constructor needs to be public because we create static versions of this.
  ASTMemberRef(const SourceLocation & loc, ASTNode * qual, const char * name)
    : ASTNode(Member, loc)
    , qualifier_(qual)
    , memberName_(name)
  {}

  ASTMemberRef * get(const SourceLocation & loc, ASTNode * qual, const char * name) {
    return new ASTMemberRef(loc, qual, name);
  }

  /** The object that contains the member. */
  const ASTNode * qualifier() const { return qualifier_; }
  ASTNode * qualifier() { return qualifier_; }

  /** The name of the member. */
  const char * memberName() const { return memberName_; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ASTMemberRef *) { return true; }
  static inline bool classof(const ASTNode * ast) {
      return ast->nodeType() == ASTNode::Member;
  }
};

/// -------------------------------------------------------------------
/// Base class for literals - ints, floats, etc.
template<class ValueType, ASTNode::NodeType type>
class ASTLiteral : public ASTNode {
private:
  ValueType value_;

public:
  ASTLiteral(const SourceLocation & loc, const ValueType & val)
      : ASTNode(type, loc)
      , value_(val)
  {}

  /** The value of this literal. */
  const ValueType & value() const { return value_; }

  // Overrides

  void format(FormatStream & out) const;
  static inline bool classof(const ASTLiteral *) { return true; }
  static inline bool classof(const ASTNode * ast) {
    return ast->nodeType() == type;
  }
};

/// -------------------------------------------------------------------
/// Various literal types
typedef ASTLiteral<llvm::APInt, ASTNode::LitInt> ASTIntegerLiteral;
typedef ASTLiteral<llvm::APFloat, ASTNode::LitFloat> ASTFloatLiteral;
typedef ASTLiteral<llvm::APFloat, ASTNode::LitDouble> ASTDoubleLiteral;
typedef ASTLiteral<std::string, ASTNode::LitString> ASTStringLiteral;
typedef ASTLiteral<uint32_t, ASTNode::LitChar> ASTCharLiteral;
typedef ASTLiteral<bool, ASTNode::LitBool> ASTBoolLiteral;

/// -------------------------------------------------------------------
/// A node that contains one or more child nodes.
class ASTOper : public ASTNode {
protected:
  // List of operands to this operator
  ASTNodeList args_;

public:
  ASTOper(NodeType type, const SourceLocation & loc)
      : ASTNode(type, loc) {}

  ASTOper(NodeType type, ASTNode * a0)
      : ASTNode(type, a0->location()) {
    args_.push_back(a0);
  }

  ASTOper(NodeType type, const SourceLocation & loc, ASTNode * a0)
      : ASTNode(type, loc) {
    args_.push_back(a0);
  }

  ASTOper(NodeType type, ASTNode * a0, ASTNode * a1)
      : ASTNode(type, a0->location() | a1->location()) {
    args_.push_back(a0);
    args_.push_back(a1);
  }

  ASTOper(NodeType type, const ASTNodeList & alist)
      : ASTNode(type, SourceLocation()) {
    args_.append(alist.begin(), alist.end());
    for (ASTNodeList::const_iterator it = alist.begin(); it != alist.end(); ++it) {
      loc |= (*it)->location();
    }
  }

  ASTOper(NodeType type, const SourceLocation & loc, const ASTNodeList & alist)
      : ASTNode(type, loc)
      , args_(alist) {
  }

  /** Return the list of operands for this operation. */
  const ASTNodeList & args() const {
    return args_;
  }

  ASTNodeList & args() {
    return args_;
  }

  /** Return the list of operands for this operation. */
  const ASTNode * arg(int i) const {
    return args_[i];
  }

  /** Append an operand to the list of operands. */
  void append(ASTNode * node) {
    args_.push_back(node);
    loc |= node->location();
  }

  /** Return the number of arguments. */
  size_t count() const {
    return args_.size();
  }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ASTOper *) {
    return true;
  }
};

/// -------------------------------------------------------------------
/// A call expression
class ASTCall : public ASTOper {
private:
  ASTNode * func_;

public:
  ASTCall(const SourceLocation & loc, ASTNode * f, const ASTNodeList & argList)
    : ASTOper(Call, loc, argList)
    , func_(f)
  {
  }

  /** Function to be called. */
  const ASTNode * func() const { return func_; }
  ASTNode * func() { return func_; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ASTCall *) { return true; }
  static inline bool classof(const ASTNode * ast) {
    return ast->nodeType() == Call;
  }
};

/// -------------------------------------------------------------------
/// A template specialization
class ASTSpecialize : public ASTOper {
private:
  const ASTNode * templateExpr_;

public:
  ASTSpecialize(const SourceLocation & loc, const ASTNode * f,
      const ASTNodeList & argList)
    : ASTOper(Specialize, loc, argList)
    , templateExpr_(f)
  {
  }

  /** Function to be called. */
  const ASTNode * templateExpr() const {
    return templateExpr_;
  }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ASTSpecialize *) { return true; }
  static inline bool classof(const ASTNode * ast) {
    return ast->nodeType() == Specialize;
  }
};

/// -------------------------------------------------------------------
/// A keyword argument
class ASTKeywordArg : public ASTNode {
private:
  const ASTNode * arg_;
  const char * keyword_;

public:
  ASTKeywordArg(const SourceLocation & loc, const ASTNode * a, const char * kw)
      : ASTNode(Keyword, loc)
      , arg_(a)
      , keyword_(kw) {}

  const ASTNode * arg() const {
    return arg_;
  }

  const char * keyword() const {
    return keyword_;
  }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ASTKeywordArg *) { return true; }
  static inline bool classof(const ASTNode * ast) {
    return ast->nodeType() == ASTNode::Keyword;
  }
};

/// -------------------------------------------------------------------
/// An import expression
class ASTImport : public ASTNode {
  const ASTNode * path_;
  const char * asName_;
  bool unpack_;

public:
  ASTImport(const SourceLocation & loc, const ASTNode * p, const char * as,
      bool unpk = false)
      : ASTNode(ASTNode::Import, loc)
      , path_(p)
      , asName_(as)
      , unpack_(unpk)
  {}

  const ASTNode * path() const { return path_; }
  const char * asName() const { return asName_; }

  /** Whether to import the contents of the namespace instead of the namespace as a symbol. */
  bool unpack() const { return unpack_; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ASTImport *) { return true; }
  static inline bool classof(const ASTNode * ast) {
    return ast->nodeType() == ASTNode::Import;
  }
};

/// -------------------------------------------------------------------
/// A reference to a built-in definition
class ASTBuiltIn : public ASTNode {
private:
  Defn * value_;

public:
  // Constructor needs to be public because we create static versions of this.
  ASTBuiltIn(Defn * val)
    : ASTNode(BuiltIn, SourceLocation())
    , value_(val)
  {}

  Defn * value() const { return value_; }
  void setValue(Defn * de) { value_ = de; }

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ASTBuiltIn * ast) { return true; }
  static inline bool classof(const ASTNode * ast) {
      return ast->nodeType() == ASTNode::BuiltIn;
  }
};

/// -------------------------------------------------------------------
/// Utility functions

/** Return the string name of a node type. */
const char * nodeTypeName(ASTNode::NodeType ec);

/** Format a list of nodes as comma-separated values. */
void formatNodeList(FormatStream & out, const ASTNodeList & nodes);

inline FormatStream & operator<<(FormatStream & out, const ASTNodeList & nodes) {
  formatNodeList(out, nodes);
  return out;
}

} // namespace tart

#endif

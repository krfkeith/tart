/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/ASTNode.h"
#include "tart/CFG/Defn.h"
#include "llvm/ADT/StringExtras.h"

namespace tart {

/// ---------------------------------------------------------------
/// ASTNode name table
#ifdef NODE_TYPE
#undef NODE_TYPE
#endif

#define NODE_TYPE(x) #x,

namespace {
const char * NodeTypeNames[] = {
#include "tart/AST/ASTNodeType.def"
};
}

const char * nodeTypeName(ASTNode::NodeType ec) {
  uint32_t index = (uint32_t)ec;
  if (index < ASTNode::NodeTypeCount) {
    return NodeTypeNames[index];
  }
  return "<Invalid Node Type>";
}

/// ---------------------------------------------------------------
/// Utility functions

// Infix operators
ASTIdent ASTIdent::operatorAdd(SourceLocation(), "infixAdd");
ASTIdent ASTIdent::operatorSub(SourceLocation(), "infixSubtract");
ASTIdent ASTIdent::operatorMul(SourceLocation(), "infixMultiply");
ASTIdent ASTIdent::operatorDiv(SourceLocation(), "infixDivide");
ASTIdent ASTIdent::operatorMod(SourceLocation(), "infixModulus");
ASTIdent ASTIdent::operatorBitAnd(SourceLocation(), "infixBitAnd");
ASTIdent ASTIdent::operatorBitOr(SourceLocation(), "infixBitOr");
ASTIdent ASTIdent::operatorBitXor(SourceLocation(), "infixBitXor");
ASTIdent ASTIdent::operatorLogicalAnd(SourceLocation(), "infixLogicalAnd");
ASTIdent ASTIdent::operatorLogicalOr(SourceLocation(), "infixLogicalOr");
ASTIdent ASTIdent::operatorRSh(SourceLocation(), "infixRShift");
ASTIdent ASTIdent::operatorLSh(SourceLocation(), "infixLShift");
ASTIdent ASTIdent::operatorEq(SourceLocation(), "infixEQ");
ASTIdent ASTIdent::operatorNe(SourceLocation(), "infixNE");
ASTIdent ASTIdent::operatorLT(SourceLocation(), "infixLT");
ASTIdent ASTIdent::operatorGT(SourceLocation(), "infixGT");
ASTIdent ASTIdent::operatorLE(SourceLocation(), "infixLE");
ASTIdent ASTIdent::operatorGE(SourceLocation(), "infixGE");
ASTIdent ASTIdent::operatorPLT(SourceLocation(), "infixPossiblyLT");
ASTIdent ASTIdent::operatorPGT(SourceLocation(), "infixPossiblyGT");
ASTIdent ASTIdent::operatorPLE(SourceLocation(), "infixPossiblyLE");
ASTIdent ASTIdent::operatorPGE(SourceLocation(), "infixPossiblyGE");
ASTIdent ASTIdent::operatorContains(SourceLocation(), "infixContains");

ASTIdent ASTIdent::operatorNegate(SourceLocation(), "unaryNegate");
ASTIdent ASTIdent::operatorSucc(SourceLocation(), "successorOf");
ASTIdent ASTIdent::operatorPred(SourceLocation(), "predeccessorOf");

/// ---------------------------------------------------------------
/// Utility functions

void formatNodeList(FormatStream & out, const ASTNodeList & nodes) {
  for (ASTNodeList::const_iterator it = nodes.begin(); it != nodes.end();
      ++it) {
    if (it != nodes.begin()) {
      out << ", ";
    }

    out << *it;
  }
}

/// ---------------------------------------------------------------
/// ASTNode
void ASTNode::format(FormatStream & out) const {
  out << nodeTypeName(nodeType_);
}

/// ---------------------------------------------------------------
/// ASTIdent
void ASTIdent::format(FormatStream & out) const {
  out << value_;
}

/// -------------------------------------------------------------------
/// ASTMemberRef
void ASTMemberRef::format(FormatStream & out) const {
  out << qualifier_ << "." << memberName_;
}

void ASTMemberRef::trace() const {
  qualifier_->mark();
}

/// ---------------------------------------------------------------
/// ASTLiteral
template<>
void ASTIntegerLiteral::format(FormatStream & out) const {
  out << value_.toString(10, false);
}

template<>
void ASTFloatLiteral::format(FormatStream & out) const {
  out << llvm::ftostr(value_);
}

template<>
void ASTCharLiteral::format(FormatStream & out) const {
  // TODO: Should handle escapes here if specified via format options
  out << "'" << value_ << "'";
}

template<>
void ASTStringLiteral::format(FormatStream & out) const {
  // TODO: Should handle escapes here if specified via format options
  out << "\"" << value_ << "\"";
}

template<>
void ASTBoolLiteral::format(FormatStream & out) const {
  out << (value_ ? "true" : "false");
}

/// -------------------------------------------------------------------
/// ASTUnaryOp
void ASTUnaryOp::format(FormatStream & out) const {
  ASTNode::format(out);
  out << "(" << arg_ << ")";
}

void ASTUnaryOp::trace() const {
  ASTNode::trace();
  arg_->mark();
}

/// ---------------------------------------------------------------
/// ASTOper
void ASTOper::format(FormatStream & out) const {
  ASTNode::format(out);
  out << "(";
  formatNodeList(out, args_);
  out << ")";
}

void ASTOper::trace() const {
  for (ASTNodeList::const_iterator it = args_.begin(); it != args_.end(); ++it) {
    (*it)->mark();
  }
}

/// ---------------------------------------------------------------
/// ASTCall
void ASTCall::format(FormatStream & out) const {
  out << func_ << "(";
  formatNodeList(out, args_);
  out << ")";
}

void ASTCall::trace() const {
  ASTOper::trace();
  func_->mark();
}

/// ---------------------------------------------------------------
/// ASTCall
void ASTSpecialize::format(FormatStream & out) const {
  out << templateExpr_ << "[";
  formatNodeList(out, args_);
  out << "]";
}

void ASTSpecialize::trace() const {
  ASTOper::trace();
  templateExpr_->mark();
}

/// ---------------------------------------------------------------
/// ASTKeywordArg
void ASTKeywordArg::format(FormatStream & out) const {
  out << keyword_ << "=" << arg_;
}

void ASTKeywordArg::trace() const {
  arg_->mark();
}

/// -------------------------------------------------------------------
/// ASTImport
void ASTImport::format(FormatStream & out) const {
  out << "import " << path_;
}

void ASTImport::trace() const {
  path_->mark();
}

/// -------------------------------------------------------------------
/// ASTBuiltIn
void ASTBuiltIn::format(FormatStream & out) const {
  out << value_;
}

void ASTBuiltIn::trace() const {
  value_->mark();
}

} // namespace tart

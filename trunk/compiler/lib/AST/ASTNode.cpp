/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/AST/ASTNode.h"
#include "tart/CFG/Defn.h"
#include <llvm/ADT/StringExtras.h>

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

const char * getNodeTypeName(ASTNode::NodeType ec) {
  uint32_t index = (uint32_t)ec;
  if (index < ASTNode::NodeTypeCount) {
    return NodeTypeNames[index];
  }
  return "<Invalid Node Type>";
}

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
  out << getNodeTypeName(nodeType_);
}

/// ---------------------------------------------------------------
/// ASTIdent
void ASTIdent::format(FormatStream & out) const {
  out << value;
}

/// -------------------------------------------------------------------
/// ASTMemberRef
void ASTMemberRef::format(FormatStream & out) const {
  out << qualifier << "." << memberName;
}

void ASTMemberRef::trace() const {
  qualifier->mark();
}

/// ---------------------------------------------------------------
/// ASTLiteral
template<>
void ASTIntegerLiteral::format(FormatStream & out) const {
  out << value.toString(10, false);
}

template<>
void ASTFloatLiteral::format(FormatStream & out) const {
  out << llvm::ftostr(value);
}

template<>
void ASTCharLiteral::format(FormatStream & out) const {
  // TODO: Should handle escapes here if specified via format options
  out << "'" << value << "'";
}

template<>
void ASTStringLiteral::format(FormatStream & out) const {
  // TODO: Should handle escapes here if specified via format options
  out << "\"" << value << "\"";
}

template<>
void ASTBoolLiteral::format(FormatStream & out) const {
  out << (value ? "true" : "false");
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
  out << func << "(";
  formatNodeList(out, args_);
  out << ")";
}

void ASTCall::trace() const {
  ASTOper::trace();
  func->mark();
}

/// ---------------------------------------------------------------
/// ASTCall
void ASTSpecialize::format(FormatStream & out) const {
  out << templateExpr << "<";
  formatNodeList(out, args_);
  out << ">";
}

void ASTSpecialize::trace() const {
  ASTOper::trace();
  templateExpr->mark();
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
  out<< "import " << path;
}

void ASTImport::trace() const {
  path->mark();
}

/// -------------------------------------------------------------------
/// ASTBuiltIn
void ASTBuiltIn::format(FormatStream & out) const {
  out << value;
}

void ASTBuiltIn::trace() const {
  value->mark();
}

} // namespace tart

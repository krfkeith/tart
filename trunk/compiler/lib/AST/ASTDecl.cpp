/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/AST/ASTDecl.h"
#include "tart/AST/Stmt.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// ---------------------------------------------------------------
// ASTDecl
void ASTDecl::trace() const {
  markList(members_.begin(), members_.end());
  markList(attributes_.begin(), attributes_.end());
}

void ASTDecl::format(FormatStream & out) const {
  DFAIL("implement");
}

// ---------------------------------------------------------------
// ASTNamespace
void ASTNamespace::trace() const {
  ASTDecl::trace();
}

void ASTNamespace::format(FormatStream & out) const {
  out << "namespace " << getName();
}

// ---------------------------------------------------------------
// ASTTypeDecl
void ASTTypeDecl::trace() const {
  ASTDecl::trace();
  markList(bases_.begin(), bases_.end());
}

void ASTTypeDecl::format(FormatStream & out) const {
  switch (getNodeType()) {
    case Class:
      out << "class ";
      break;

    case Struct:
      out << "struct ";
      break;

    case Interface:
      out << "interface ";
      break;

    case Enum:
      out << "enum ";
      break;
      
    default:
      DFAIL("Illegal state");
  }
  
  out << getName();
}

// ---------------------------------------------------------------
// ASTVarDecl
void ASTVarDecl::trace() const {
  ASTDecl::trace();
  safeMark(type);
  safeMark(value);
}

void ASTVarDecl::format(FormatStream & out) const {
  switch (getNodeType()) {
    case Let:
      out << "let ";
      break;

    case Var:
      out << "var ";
      break;

    default:
      DFAIL("Illegal state");
  }
  
  out << getName();
  if (type) {
    out << ":" << type;
  }

  if (value) {
    out << " = " << value;
  }
}

// ---------------------------------------------------------------
// PropertyDecl
void ASTPropertyDecl::trace() const {
  ASTDecl::trace();
  safeMark(type);
  safeMark(getter_);
  safeMark(setter_);
  markList(params_.begin(), params_.end());
}

void ASTPropertyDecl::format(FormatStream & out) const {
  out << "def " << getName();
  if (!params_.empty()) {
    out << "(";
    formatParamList(out, params_);
    out << ")";
  }

  if (type) {
    out << ":" << type;
  }
}

// ---------------------------------------------------------------
// ASTFunctionDecl
void ASTFunctionDecl::trace() const {
  ASTDecl::trace();
  markList(params_.begin(), params_.end());
  safeMark(returnType_);
  safeMark(body);
}

void ASTFunctionDecl::format(FormatStream & out) const {
  switch (nodeType) {
    case AnonFn:
      out << "fn ";
      break;
    
    case Function:
      out << "def " << getName() << " ";
      break;
    
    case Macro:
      out << "macro " << getName() << " ";
      break;
    
    default:
      DFAIL("Illegal state");
  }
  
  out << "(";
  formatParamList(out, params_);
  out << ")";
  
  if (returnType_) {
    out << " -> " << returnType_;
  }
  
  if (nodeType != AnonFn) {
    if (body) {
      out << body;
    }
  }
}

// ---------------------------------------------------------------
// ASTParameter
void ASTParameter::trace() const {
  ASTVarDecl::trace();
}

void ASTParameter::format(FormatStream & out) const {
  if (getName()) {
    out << getName();
  }
  
  if (type) {
    out << ":" << type;
  }

  if (value) {
    out << " = " << value;
  }
}

// ---------------------------------------------------------------
// ASTPatternVar
void ASTPatternVar::trace() const {
  ASTVarDecl::trace();
}

void ASTPatternVar::format(FormatStream & out) const {
  out << "%" << getName();
}

// ---------------------------------------------------------------
// ASTTemplate
void ASTTemplate::trace() const {
  ASTDecl::trace();
  markList(params_.begin(), params_.end());
  markList(requirements_.begin(), requirements_.end());
  safeMark(body_);
}

void ASTTemplate::format(FormatStream & out) const {
  out << "[";
  formatTemplateParamList(out, params_);
  out << "] " << body_;
  if (!requirements_.empty()) {
    diag.warn(this) << "Implement formatting of requirements";
  }
}

// ---------------------------------------------------------------
// Utility functions
void formatParamList(FormatStream & out, const ASTParamList & params) {
  for (ASTParamList::const_iterator it = params.begin(); it != params.end(); ++it) {
    if (it != params.begin()) {
      out << ", ";
    }
    
    out << *it;
  }
}

void formatTemplateParamList(FormatStream & out, const ASTNodeList & params) {
  for (ASTNodeList::const_iterator it = params.begin(); it != params.end(); ++it) {
    if (it != params.begin()) {
      out << ", ";
    }
    
    out << *it;
  }
}

}

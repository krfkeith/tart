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
  out << "namespace " << name();
}

// ---------------------------------------------------------------
// ASTTypeDecl
void ASTTypeDecl::trace() const {
  ASTDecl::trace();
  markList(bases_.begin(), bases_.end());
}

void ASTTypeDecl::format(FormatStream & out) const {
  switch (nodeType()) {
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

  out << name();
}

// ---------------------------------------------------------------
// ASTVarDecl
void ASTVarDecl::trace() const {
  ASTDecl::trace();
  safeMark(type_);
  safeMark(value_);
}

void ASTVarDecl::format(FormatStream & out) const {
  switch (nodeType()) {
    case Let:
      out << "let ";
      break;

    case Var:
      out << "var ";
      break;

    case VarList:
      out << "varlist ";
      break;

    default:
      DFAIL("Illegal state");
  }

  out << name();
  if (type_) {
    out << ":" << type_;
  }

  if (nodeType() == VarList) {
    out << "(";
    for (ASTDeclList::const_iterator it = members_.begin(); it != members_.end(); ++it) {
      if (it != members_.begin()) {
        out << ", ";
      }

      out << *it;
    }
    out << ")";
  }

  if (value_) {
    out << " = " << value_;
  }
}

// ---------------------------------------------------------------
// PropertyDecl
void ASTPropertyDecl::trace() const {
  ASTDecl::trace();
  safeMark(type_);
  safeMark(getter_);
  safeMark(setter_);
  markList(params_.begin(), params_.end());
}

void ASTPropertyDecl::format(FormatStream & out) const {
  out << "def " << name();
  if (!params_.empty()) {
    out << "(";
    formatParamList(out, params_);
    out << ")";
  }

  if (type_) {
    out << ":" << type_;
  }
}

// ---------------------------------------------------------------
// ASTFunctionDecl
void ASTFunctionDecl::trace() const {
  ASTDecl::trace();
  markList(params_.begin(), params_.end());
  safeMark(returnType_);
  safeMark(body_);
}

void ASTFunctionDecl::format(FormatStream & out) const {
  switch (nodeType_) {
    case AnonFn:
      out << "fn ";
      break;

    case Function:
      out << "def " << name() << " ";
      break;

    case Macro:
      out << "macro " << name() << " ";
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

  if (nodeType_ != AnonFn) {
    if (body_) {
      out << body_;
    }
  }
}

// ---------------------------------------------------------------
// ASTParameter
void ASTParameter::trace() const {
  ASTVarDecl::trace();
}

void ASTParameter::format(FormatStream & out) const {
  if (name()) {
    out << name();
  }

  if (type_) {
    out << ":" << type_;
  }

  if (value_) {
    out << " = " << value_;
  }
}

// ---------------------------------------------------------------
// ASTTypeVariable
void ASTTypeVariable::trace() const {
  ASTVarDecl::trace();
}

void ASTTypeVariable::format(FormatStream & out) const {
  out << "%" << name();
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

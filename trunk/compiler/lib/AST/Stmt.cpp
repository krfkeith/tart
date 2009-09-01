/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/Stmt.h"
#include "tart/AST/ASTDecl.h"

namespace tart {

// -------------------------------------------------------------------
// Stmt
void Stmt::format(FormatStream & out) const {
  switch (nodeType()) {
    case Break:
      out << "break;";
      break;

    case Continue:
      out << "continue;";
      break;

    default:
      assert(false);
  }
}

// -------------------------------------------------------------------
// Block
void BlockStmt::trace() const {
  markList(statements.begin(), statements.end());
}

void BlockStmt::format(FormatStream & out) const {
  out << "{";
  for (StmtList::const_iterator it = statements.begin(); it != statements.end(); ++it) {
    out << *it;
  }
  out << "}";
}

// -------------------------------------------------------------------
// Expression
void ExprStmt::trace() const {
  Stmt::trace();
  safeMark(value_);
}

void ExprStmt::format(FormatStream & out) const {
  out << value_ << ";";
}

Stmt * ExprStmt::get(ASTNode * ex) {
  return new ExprStmt(Stmt::Expression, ex->location(), ex);
}

// -------------------------------------------------------------------
// Return
void ReturnStmt::trace() const {
  ExprStmt::trace();
}

void ReturnStmt::format(FormatStream & out) const {
  out << "return " << value() << ";";
}

// -------------------------------------------------------------------
// Yield
void YieldStmt::trace() const {
  ExprStmt::trace();
}

void YieldStmt::format(FormatStream & out) const {
  out << "yield " << value() << ";";
}

// -------------------------------------------------------------------
// Throw
void ThrowStmt::trace() const {
  ExprStmt::trace();
}

void ThrowStmt::format(FormatStream & out) const {
  out << "throw " << value() << ";";
}

// -------------------------------------------------------------------
// ASTDecl
void DeclStmt::trace() const {
  Stmt::trace();
  safeMark(decl_);
}

void DeclStmt::format(FormatStream & out) const {
  out << decl_;
}

Stmt * DeclStmt::get(ASTDecl * de) {
  return new DeclStmt(de->location(), de);
}

// -------------------------------------------------------------------
// If
void IfStmt::trace() const {
  Stmt::trace();
  safeMark(testExpr_);
  safeMark(thenSt_);
  safeMark(elseSt_);
}

void IfStmt::format(FormatStream & out) const {
  out << "If (" << testExpr_ << ", " << thenSt_;
  if (elseSt_) {
    out << ", " << elseSt_;
  }
  out << ")";
}

// -------------------------------------------------------------------
// While
void WhileStmt::trace() const {
  Stmt::trace();
  safeMark(testExpr_);
  safeMark(body_);
}

void WhileStmt::format(FormatStream & out) const {
  out << "While (" << testExpr_;
  if (body_) {
    out << ", " << body_;
  }
  out << ")";
}

// -------------------------------------------------------------------
// For
void ForStmt::trace() const {
  Stmt::trace();
  safeMark(initExpr_);
  safeMark(testExpr_);
  safeMark(incrExpr_);
  safeMark(body_);
}

void ForStmt::format(FormatStream & out) const {
  out << "For (";
  if (initExpr_) {
    out << initExpr_;
  }

  out << "; ";
  if (testExpr_) {
    out << testExpr_;
  }

  out << "; ";
  if (incrExpr_) {
    out << incrExpr_;
  }

  out << "; ";
  if (body_) {
    out << body_;
  }

  out << ")";
}

// -------------------------------------------------------------------
// For each
void ForEachStmt::trace() const {
  Stmt::trace();
  safeMark(loopVars_);
  safeMark(iterExpr_);
  safeMark(body_);
}

void ForEachStmt::format(FormatStream & out) const {
  out << "ForEach (";
  if (loopVars_) {
    out << loopVars_;
  }

  out << "; ";
  if (iterExpr_) {
    out << iterExpr_;
  }

  out << "; ";
  if (body_) {
    out << body_;
  }

  out << ")";
}

// -------------------------------------------------------------------
// Try
void TryStmt::trace() const {
  Stmt::trace();
  for (StmtList::const_iterator it = catchList_.begin(); it != catchList_.end(); ++it) {
    (*it)->mark();
  }
  safeMark(body_);
  safeMark(elseSt_);
  safeMark(finallySt_);
}

void TryStmt::format(FormatStream & out) const {
  out << "Try (";
  if (body_) {
    out << body_;
  }

  for (StmtList::const_iterator it = catchList_.begin(); it != catchList_.end(); ++it) {
    out << " " << *it << ";";
  }

  if (elseSt_) {
    out << " Else " << elseSt_;
  }

  if (finallySt_) {
    out << " Finally " << finallySt_;
  }

  out << ")";
}

// -------------------------------------------------------------------
// Catch
void CatchStmt::trace() const {
  Stmt::trace();
  safeMark(exceptDecl_);
  safeMark(body_);
}

void CatchStmt::format(FormatStream & out) const {
  out << "Catch (";
  if (exceptDecl_) {
    out << exceptDecl_;
  }

  out << "; ";
  if (body_) {
    out << body_;
  }

  out << ")";
}

// -------------------------------------------------------------------
// Switch

void SwitchStmt::trace() const {
  Stmt::trace();
  safeMark(testExpr_);
  markList(caseList_.begin(), caseList_.end());
}

void SwitchStmt::format(FormatStream & out) const {
  out << "Switch (" << testExpr_ << ", ";
  for (StmtList::const_iterator it = caseList_.begin(); it != caseList_.end(); ++it) {
    out << " " << *it << ";";
  }

  out << ")";
}

// -------------------------------------------------------------------
// Case

void CaseStmt::trace() const {
  Stmt::trace();
  safeMark(caseExpr_);
  safeMark(body_);
}

void CaseStmt::format(FormatStream & out) const {
  out << "Case (" << caseExpr_ << ", " << body_ << ")";
}

// -------------------------------------------------------------------
// Classify

void ClassifyStmt::trace() const {
  Stmt::trace();
  safeMark(testExpr_);
  markList(caseList_.begin(), caseList_.end());
}

void ClassifyStmt::format(FormatStream & out) const {
  out << "Classify (" << testExpr_ << ", ";
  for (StmtList::const_iterator it = caseList_.begin(); it != caseList_.end(); ++it) {
    out << " " << *it << ";";
  }

  out << ")";
}

}

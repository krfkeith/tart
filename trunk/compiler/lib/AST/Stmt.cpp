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
  safeMark(value);
}

void ExprStmt::format(FormatStream & out) const {
  out << value << ";";
}

Stmt * ExprStmt::get(ASTNode * ex) {
  return new ExprStmt(Stmt::Expression, ex->getLocation(), ex);
}

// -------------------------------------------------------------------
// Return
void ReturnStmt::trace() const {
  ExprStmt::trace();
}

void ReturnStmt::format(FormatStream & out) const {
  out << "return " << getValue() << ";";
}

// -------------------------------------------------------------------
// Yield
void YieldStmt::trace() const {
  ExprStmt::trace();
}

void YieldStmt::format(FormatStream & out) const {
  out << "yield " << getValue() << ";";
}

// -------------------------------------------------------------------
// Throw
void ThrowStmt::trace() const {
  ExprStmt::trace();
}

void ThrowStmt::format(FormatStream & out) const {
  out << "throw " << getValue() << ";";
}

// -------------------------------------------------------------------
// ASTDecl
void DeclStmt::trace() const {
  Stmt::trace();
  safeMark(decl);
}

void DeclStmt::format(FormatStream & out) const {
  out << decl;
}

Stmt * DeclStmt::get(ASTDecl * de) {
  return new DeclStmt(de->getLocation(), de);
}

// -------------------------------------------------------------------
// If
void IfStmt::trace() const {
  Stmt::trace();
  safeMark(testExpr);
  safeMark(thenSt);
  safeMark(elseSt);
}

void IfStmt::format(FormatStream & out) const {
  out << "If (" << testExpr << ", " << thenSt;
  if (elseSt) {
    out << ", " << elseSt;
  }
  out << ")";
}

// -------------------------------------------------------------------
// While
void WhileStmt::trace() const {
  Stmt::trace();
  safeMark(testExpr);
  safeMark(loopBody);
}

void WhileStmt::format(FormatStream & out) const {
  out << "While (" << testExpr;
  if (loopBody) {
    out << ", " << loopBody;
  }
  out << ")";
}

// -------------------------------------------------------------------
// For
void ForStmt::trace() const {
  Stmt::trace();
  safeMark(initExpr);
  safeMark(testExpr);
  safeMark(incrExpr);
  safeMark(loopBody);
}

void ForStmt::format(FormatStream & out) const {
  out << "For (";
  if (initExpr) {
    out << initExpr;
  }

  out << "; ";
  if (testExpr) {
    out << testExpr;
  }

  out << "; ";
  if (incrExpr) {
    out << incrExpr;
  }

  out << "; ";
  if (loopBody) {
    out << loopBody;
  }

  out << ")";
}

// -------------------------------------------------------------------
// For each
void ForEachStmt::trace() const {
  Stmt::trace();
  safeMark(loopVars);
  safeMark(iterExpr);
  safeMark(loopBody);
}

void ForEachStmt::format(FormatStream & out) const {
  out << "ForEach (";
  if (loopVars) {
    out << loopVars;
  }

  out << "; ";
  if (iterExpr) {
    out << iterExpr;
  }

  out << "; ";
  if (loopBody) {
    out << loopBody;
  }

  out << ")";
}

// -------------------------------------------------------------------
// Try
void TryStmt::trace() const {
  Stmt::trace();
  for (StmtList::const_iterator it = catchList.begin(); it != catchList.end(); ++it) {
    (*it)->mark();
  }
  safeMark(bodySt);
  safeMark(elseSt);
  safeMark(finallySt);
}

void TryStmt::format(FormatStream & out) const {
  out << "Try (";
  if (bodySt) {
    out << bodySt;
  }

  for (StmtList::const_iterator it = catchList.begin(); it != catchList.end(); ++it) {
    out << " " << *it << ";";
  }

  if (elseSt) {
    out << " Else " << elseSt;
  }

  if (finallySt) {
    out << " Finally " << finallySt;
  }

  out << ")";
}

// -------------------------------------------------------------------
// Catch
void CatchStmt::trace() const {
  Stmt::trace();
  safeMark(exceptDecl);
  safeMark(bodySt);
}

void CatchStmt::format(FormatStream & out) const {
  out << "Catch (";
  if (exceptDecl) {
    out << exceptDecl;
  }

  out << "; ";
  if (bodySt) {
    out << bodySt;
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

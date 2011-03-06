/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/StmtExprs.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/VariableDefn.h"
#include "tart/CFG/FunctionDefn.h"

namespace tart {

// -------------------------------------------------------------------
// SeqExpr

void SeqExpr::format(FormatStream & out) const {
  out << "{";
  formatExprList(out, args());
  out << "}";
}

bool SeqExpr::isSideEffectFree() const {
  for (const_iterator it = begin(); it != end(); ++it) {
    if (!(*it)->isSideEffectFree()) {
      return false;
    }
  }
  return true;
}

bool SeqExpr::isSingular() const {
  for (const_iterator it = begin(); it != end(); ++it) {
    if (!(*it)->isSingular()) {
      return false;
    }
  }
  return true;
}

// -------------------------------------------------------------------
// IfExpr

IfExpr::IfExpr(const SourceLocation & loc, LocalScope * scope, Expr * test, Expr * thenVal,
    Expr * elseVal)
  : Expr(If, loc, &VoidType::instance)
  , scope_(scope)
  , test_(test)
  , thenVal_(thenVal)
  , elseVal_(elseVal)
{}

void IfExpr::format(FormatStream & out) const {
  out << "if (" << test_ << ") " << thenVal_;
  if (elseVal_) {
    out << " else " << elseVal_;
  }
}

void IfExpr::trace() const {
  Expr::trace();
  safeMark(scope_);
  safeMark(test_);
  safeMark(thenVal_);
  safeMark(elseVal_);
}

bool IfExpr::isSideEffectFree() const {
  return test_->isSideEffectFree() &&
      thenVal_->isSideEffectFree() &&
      (elseVal_ == NULL || elseVal_->isSideEffectFree());
}

bool IfExpr::isSingular() const {
  return test_->isSingular() &&
      thenVal_->isSingular() &&
      (elseVal_ == NULL || elseVal_->isSingular());
}

// -------------------------------------------------------------------
// WhileExpr

WhileExpr::WhileExpr(ExprType k, const SourceLocation & loc, LocalScope * scope, Expr * test,
    Expr * body)
  : Expr(k, loc, &VoidType::instance)
  , scope_(scope)
  , test_(test)
  , body_(body)
{}

void WhileExpr::format(FormatStream & out) const {
  if (exprType() == While) {
    out << "while (" << test_ << ") " << body_;
  } else {
    out << "do " << body_ << " while (" << test_ << ")";
  }
}

void WhileExpr::trace() const {
  Expr::trace();
  safeMark(scope_);
  safeMark(test_);
  safeMark(body_);
}

bool WhileExpr::isSideEffectFree() const {
  return false;
}

bool WhileExpr::isSingular() const {
  return test_->isSingular() && body_->isSingular();
}

// -------------------------------------------------------------------
// ForExpr

ForExpr::ForExpr(const SourceLocation & loc, LocalScope * scope,
    Expr * init, Expr * test, Expr * incr, Expr * body)
  : Expr(For, loc, &VoidType::instance)
  , scope_(scope)
  , init_(init)
  , test_(test)
  , incr_(incr)
  , body_(body)
{}

void ForExpr::format(FormatStream & out) const {
  out << "for (" << init_ << "; " << test_ << "; " << incr_ << " " << body_;
}

void ForExpr::trace() const {
  Expr::trace();
  safeMark(init_);
  safeMark(test_);
  safeMark(incr_);
  safeMark(body_);
}

bool ForExpr::isSideEffectFree() const {
  return false;
}

bool ForExpr::isSingular() const {
  return (test_ == NULL || test_->isSingular())
      && (init_ == NULL || init_->isSingular())
      && (incr_ == NULL || incr_->isSingular())
      && (body_ == NULL || body_->isSingular());
}

// -------------------------------------------------------------------
// ForEachExpr

ForEachExpr::ForEachExpr(const SourceLocation & loc, LocalScope * scope)
  : Expr(ForEach, loc, &VoidType::instance)
  , scope_(scope)
  , iterator_(NULL)
  , next_(NULL)
  , test_(NULL)
  , body_(NULL)
{}

void ForEachExpr::format(FormatStream & out) const {
  //out << "for (" << iter_ << "; " << test_ << "; " << incr_ << " " << body_;
}

void ForEachExpr::trace() const {
  Expr::trace();
  safeMark(iterator_);
  safeMark(next_);
  safeMark(test_);
  markList(assigns_.begin(), assigns_.end());
  safeMark(body_);
}

bool ForEachExpr::isSideEffectFree() const {
  return false;
}

bool ForEachExpr::isSingular() const {
  return iterator_->isSingular()
      && next_->isSingular()
      && test_->isSingular()
      && body_->isSingular()
      && all(assigns_.begin(), assigns_.end(), &Expr::isSingular);
}

// -------------------------------------------------------------------
// SwitchExpr

SwitchExpr::SwitchExpr(const SourceLocation & loc, Expr * value)
  : ArglistExpr(Switch, loc, &VoidType::instance)
  , value_(value)
  , elseCase_(NULL)
  , equalityTestFn_(NULL)
{}

void SwitchExpr::format(FormatStream & out) const {
  out << "switch (" << value_ << ") ";
  for (const_iterator it = begin(); it != end(); ++it) {
    out << " " << *it;
  }
}

void SwitchExpr::trace() const {
  ArglistExpr::trace();
  safeMark(value_);
  safeMark(elseCase_);
  safeMark(equalityTestFn_);
}

bool SwitchExpr::isSideEffectFree() const {
  return value_->isSideEffectFree() &&
      all(args_.begin(), args_.end(), &Expr::isSideEffectFree);
}

bool SwitchExpr::isSingular() const {
  return value_->isSingular() &&
      all(args_.begin(), args_.end(), &Expr::isSingular);
}

/// -------------------------------------------------------------------
/// CaseExpr

CaseExpr::CaseExpr(const SourceLocation & loc, const ExprList & caseVals, Expr * body)
  : ArglistExpr(Case, loc, caseVals, body->type())
  , body_(body)
{}

void CaseExpr::format(FormatStream & out) const {
  out << "case ";
  formatExprList(out, args_);
  out << " " << body_;
}

void CaseExpr::trace() const {
  ArglistExpr::trace();
  safeMark(body_);
}

bool CaseExpr::isSideEffectFree() const {
  return body_->isSideEffectFree() && all(args_.begin(), args_.end(), &Expr::isSideEffectFree);
}

bool CaseExpr::isSingular() const {
  return body_->isSingular() && all(args_.begin(), args_.end(), &Expr::isSingular);
}

// -------------------------------------------------------------------
// MatchExpr

MatchExpr::MatchExpr(const SourceLocation & loc, LocalScope * scope, Expr * value)
  : ArglistExpr(Match, loc, &VoidType::instance)
  , scope_(scope)
  , value_(value)
  , elseCase_(NULL)
{
}

void MatchExpr::format(FormatStream & out) const {
  out << "match ";
  for (const_iterator it = begin(); it != end(); ++it) {
    const MatchAsExpr * tc = cast<MatchAsExpr>(*it);
    out << " as " << tc->init() << " " << tc->body();
  }
}

void MatchExpr::trace() const {
  ArglistExpr::trace();
  safeMark(scope_);
  safeMark(value_);
  safeMark(elseCase_);
}

bool MatchExpr::isSideEffectFree() const {
  return value_->isSideEffectFree() && all(args_.begin(), args_.end(), &Expr::isSideEffectFree);
}

bool MatchExpr::isSingular() const {
  return value_->isSingular() && all(args_.begin(), args_.end(), &Expr::isSingular);
}

/// -------------------------------------------------------------------
/// MatchAsExpr

MatchAsExpr::MatchAsExpr(const SourceLocation & loc, LocalScope * scope, Expr * test, Expr * init,
    Expr * body)
  : Expr(MatchAs, loc, body->type())
  , scope_(scope)
  , test_(test)
  , init_(init)
  , body_(body)
{}

void MatchAsExpr::format(FormatStream & out) const {
  out << "as " << init_ << " " << body_;
}

void MatchAsExpr::trace() const {
  Expr::trace();
  safeMark(scope_);
  safeMark(test_);
  safeMark(init_);
  safeMark(body_);
}

bool MatchAsExpr::isSideEffectFree() const {
  return body_->isSideEffectFree();
}

bool MatchAsExpr::isSingular() const {
  return test_->isSingular() && init_->isSingular() && body_->isSingular();
}

// -------------------------------------------------------------------
// TryExpr

TryExpr::TryExpr(const SourceLocation & loc, Expr * body)
  : ArglistExpr(Try, loc, &VoidType::instance)
  , body_(body)
  , elseBlock_(NULL)
  , finallyBlock_(NULL)
{}

void TryExpr::format(FormatStream & out) const {
  out << "try ";
  for (const_iterator it = begin(); it != end(); ++it) {
    const CatchExpr * tc = cast<CatchExpr>(*it);
    out << " catch " << tc->var() << " " << tc->body();
  }
  if (finallyBlock_ != NULL) {
    out << " finally " << finallyBlock_;
  }
}

void TryExpr::trace() const {
  ArglistExpr::trace();
  safeMark(body_);
  safeMark(finallyBlock_);
}

bool TryExpr::isSideEffectFree() const {
  return body_->isSideEffectFree() &&
      all(args_.begin(), args_.end(), &Expr::isSideEffectFree) &&
      (finallyBlock_ == NULL || finallyBlock_->isSideEffectFree());
}

bool TryExpr::isSingular() const {
  return body_->isSingular() &&
      all(args_.begin(), args_.end(), &Expr::isSingular) &&
      (finallyBlock_ == NULL || finallyBlock_->isSingular());
}

/// -------------------------------------------------------------------
/// CatchExpr

CatchExpr::CatchExpr(const SourceLocation & loc, LocalScope * scope,
    VariableDefn * var, Expr * body)
  : Expr(Catch, loc, body->type())
  , scope_(scope)
  , var_(var)
  , body_(body)
{}

void CatchExpr::format(FormatStream & out) const {
  out << "catch ";
  if (var_ != NULL) {
    out << var_ << " ";
  }
  out << body_;
}

void CatchExpr::trace() const {
  Expr::trace();
  safeMark(scope_);
  safeMark(var_);
  safeMark(body_);
}

bool CatchExpr::isSideEffectFree() const {
  return body_->isSideEffectFree();
}

bool CatchExpr::isSingular() const {
  return (var_ == NULL || var_->isSingular()) && body_->isSingular();
}

/// -------------------------------------------------------------------
/// ReturnExpr

ReturnExpr::ReturnExpr(ExprType k, const SourceLocation & loc, Expr * value)
  : UnaryExpr(k, loc, &VoidType::instance, value)
{}

bool ReturnExpr::isSingular() const {
  return arg_ == NULL || arg_->isSingular();
}

void ReturnExpr::format(FormatStream & out) const {
  out << "return";
  if (arg_ != NULL) {
    out << " " << arg_;
  }
}

/// -------------------------------------------------------------------
/// ThrowExpr

ThrowExpr::ThrowExpr(const SourceLocation & loc, Expr * value)
  : UnaryExpr(Throw, loc, &VoidType::instance, value)
{}

/// -------------------------------------------------------------------
/// BranchExpr

BranchExpr::BranchExpr(ExprType k, const SourceLocation & loc)
  : Expr(k, loc, &VoidType::instance)
{}

/// -------------------------------------------------------------------
/// ReturnExpr

LocalProcedureExpr::LocalProcedureExpr(const SourceLocation & loc, Expr * value)
  : UnaryExpr(Expr::LocalProcedure, loc, &VoidType::instance, value)
{}

} // namespace tart

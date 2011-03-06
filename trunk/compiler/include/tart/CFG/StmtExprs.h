/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_STMTEXPRS_H
#define TART_CFG_STMTEXPRS_H

#ifndef TART_CFG_EXPR_H
#include "tart/CFG/Expr.h"
#endif

#include <llvm/Instructions.h>
#include <llvm/ADT/SetVector.h>

namespace tart {

class LocalScope;
class VariableDefn;
class CaseExpr;
class AsExpr;

/// -------------------------------------------------------------------
/// A sequence of expressions, to be evaluated in order.
class SeqExpr : public ArglistExpr {
public:
  /** Constructor. */
  SeqExpr(const SourceLocation & loc, LocalScope * scope, const ExprList & exprs, const Type * type)
    : ArglistExpr(Seq, loc, exprs, type)
    , scope_(scope)
  {}

  /** Constructor. */
  SeqExpr(const SourceLocation & loc, const Type * type)
    : ArglistExpr(Seq, loc, type)
    , scope_(NULL)
  {}

  /** The local scope of this block. */
  LocalScope * scope() const { return scope_; }

  // Overrides

  void format(FormatStream & out) const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const SeqExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Seq;
  }

private:
  LocalScope * scope_;
};

/// -------------------------------------------------------------------
/// An if-statement
class IfExpr : public Expr {
public:
  /** Constructor. */
  IfExpr(const SourceLocation & loc, LocalScope * scope, Expr * test, Expr * thenVal,
      Expr * elseVal);

  /** The implicit scope surrounding this statement. */
  LocalScope * scope() const { return scope_; }

  /** The test expression */
  Expr * test() const { return test_; }
  void setTest(Expr * e) { test_ = e; }

  /** The 'then' body of the if. */
  Expr * thenVal() const { return thenVal_; }
  void setThenVal(Expr * e) { thenVal_ = e; }

  /** The 'else' body of the if. */
  Expr * elseVal() const { return elseVal_; }
  void setElseVal(Expr * e) { elseVal_ = e; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const IfExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == If;
  }

private:
  LocalScope * scope_;
  Expr * test_;
  Expr * thenVal_;
  Expr * elseVal_;
};

/// -------------------------------------------------------------------
/// while statement
class WhileExpr : public Expr {
public:
  /** Constructor. */
  WhileExpr(ExprType k, const SourceLocation & loc, LocalScope * scope, Expr * test, Expr * body);

  /** The implicit scope surrounding this statement. */
  LocalScope * scope() const { return scope_; }

  /** The test expression */
  Expr * test() const { return test_; }
  void setTest(Expr * e) { test_ = e; }

  /** The loop body. */
  Expr * body() const { return body_; }
  void setBody(Expr * e) { body_ = e; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const WhileExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == While || ex->exprType() == DoWhile;
  }

private:
  LocalScope * scope_;
  Expr * test_;
  Expr * body_;
};

/// -------------------------------------------------------------------
/// for statement
class ForExpr : public Expr {
public:
  /** Constructor. */
  ForExpr(const SourceLocation & loc, LocalScope * scope,
      Expr * init, Expr * test, Expr * incr, Expr * body);

  /** The implicit scope surrounding this statement. */
  LocalScope * scope() const { return scope_; }

  /** The initialization expression */
  Expr * init() const { return init_; }
  void setInit(Expr * e) { init_ = e; }

  /** The test expression */
  Expr * test() const { return test_; }
  void setTest(Expr * e) { test_ = e; }

  /** The increment expression */
  Expr * incr() const { return incr_; }
  void setIncr(Expr * e) { incr_ = e; }

  /** The loop body */
  Expr * body() const { return body_; }
  void setBody(Expr * e) { body_ = e; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const ForExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == For;
  }

private:
  LocalScope * scope_;
  Expr * init_;
  Expr * test_;
  Expr * incr_;
  Expr * body_;
};

/// -------------------------------------------------------------------
/// foreach statement

class ForEachExpr : public Expr {
public:
  /** Constructor. */
  ForEachExpr(const SourceLocation & loc, LocalScope * scope);

  /** The implicit scope surrounding this statement. */
  LocalScope * scope() const { return scope_; }

  /** The expression that initializes the iteration. */
  Expr * iterator() const { return iterator_; }
  void setIterator(Expr * e) { iterator_ = e; }

  /** The expression that produces the next iteration value. */
  Expr * next() const { return next_; }
  void setNext(Expr * e) { next_ = e; }

  /** The expression determines if we are finished. */
  Expr * test() const { return test_; }
  void setTest(Expr * e) { test_ = e; }

  /** Assignments to iteration variables. */
  const ExprList & assigns() const { return assigns_; }
  ExprList & assigns() { return assigns_; }

  /** The loop body */
  Expr * body() const { return body_; }
  void setBody(Expr * e) { body_ = e; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const ForExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == For;
  }

private:
  LocalScope * scope_;
  Expr * iterator_;
  Expr * next_;
  Expr * test_;
  ExprList assigns_;
  Expr * body_;
};

/// -------------------------------------------------------------------
/// switch statement

class SwitchExpr : public ArglistExpr {
public:
  /** Constructor. */
  SwitchExpr(const SourceLocation & loc, Expr * value);

  /** The switch value */
  Expr * value() const { return value_; }
  void setValue(Expr * e) { value_ = e; }

  /** The default case. */
  Expr * elseCase() const { return elseCase_; }
  void setElseCase(Expr * defaultCase) { elseCase_ = defaultCase; }

  /** An expression which yields a function that can compare case values. */
  FunctionDefn * equalityTestFn() const { return equalityTestFn_; }
  void setEqualityTestFn(FunctionDefn * fn) { equalityTestFn_ = fn; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const SwitchExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Switch;
  }

private:
  Expr * value_;
  Expr * elseCase_;
  FunctionDefn * equalityTestFn_;
};

/// -------------------------------------------------------------------
/// A case-statement
class CaseExpr : public ArglistExpr {
public:
  /** Constructor. */
  CaseExpr(const SourceLocation & loc, const ExprList & caseVals, Expr * body);

  /** The body of the case statement. */
  Expr * body() const { return body_; }
  void setBody(Expr * e) { body_ = e; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const CaseExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Case;
  }

private:
  Expr * body_;
};

/// -------------------------------------------------------------------
/// match statement
class MatchExpr : public ArglistExpr {
public:
  /** Constructor. */
  MatchExpr(const SourceLocation & loc, LocalScope * scope, Expr * value);

  /** The implicit scope surrounding this statement. */
  LocalScope * scope() const { return scope_; }

  /** The expression to be matched */
  Expr * value() const { return value_; }
  void setValue(Expr * e) { value_ = e; }

  /** The default case. */
  Expr * elseCase() const { return elseCase_; }
  void setElseCase(Expr * defaultCase) { elseCase_ = defaultCase; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const MatchExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Match;
  }

private:
  LocalScope * scope_;
  Expr * value_;
  Expr * elseCase_;
};

/// -------------------------------------------------------------------
/// An 'as' statement within a match
class MatchAsExpr : public Expr {
public:
  /** Constructor. */
  MatchAsExpr(const SourceLocation & loc, LocalScope * scope, Expr * test, Expr * init,
      Expr * body);

  /** The implicit scope surrounding this statement. */
  LocalScope * scope() const { return scope_; }

  /** The type test. */
  Expr * test() const { return test_; }
  void setTest(Expr * e) { test_ = e; }

  /** The initialization of the as-variable. */
  Expr * init() const { return init_; }
  void setInit(Expr * e) { init_ = e; }

  /** The body of the case statement. */
  Expr * body() const { return body_; }
  void setBody(Expr * body) { body_ = body; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const MatchAsExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == MatchAs;
  }

private:
  LocalScope * scope_;
  Expr * test_;
  Expr * init_;
  Expr * body_;
};

/// -------------------------------------------------------------------
/// try statement

class TryExpr : public ArglistExpr {
public:

  /** Constructor. */
  TryExpr(const SourceLocation & loc, Expr * body);

  /** The body of the try block */
  Expr * body() const { return body_; }
  void setBody(Expr * body) { body_ = body; }

  /** The body of the 'else' block */
  Expr * elseBlock() const { return elseBlock_; }
  void setElseBlock(Expr * blk) { elseBlock_ = blk; }

  /** The body of the finally block */
  Expr * finallyBlock() const { return finallyBlock_; }
  void setFinallyBlock(Expr * blk) { finallyBlock_ = blk; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const TryExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Try;
  }

private:
  Expr * body_;
  Expr * elseBlock_;
  Expr * finallyBlock_;
};

/// -------------------------------------------------------------------
/// A 'catch' statement with a try.
class CatchExpr : public Expr {
public:
  /** Constructor. */
  CatchExpr(const SourceLocation & loc, LocalScope * scope, VariableDefn * typeVar, Expr * body);

  /** The implicit scope surrounding this statement. */
  LocalScope * scope() const { return scope_; }

  /** The variable to which the value is assigned if it matches the variable's type. */
  VariableDefn * var() const { return var_; }

  /** The body of the case statement. */
  Expr * body() const { return body_; }
  void setBody(Expr * body) { body_ = body; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const CatchExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Catch;
  }

private:
  LocalScope * scope_;
  VariableDefn * var_;
  Expr * body_;
};

/// -------------------------------------------------------------------
/// with statement

class WithExpr : public Expr {
public:

  /** Constructor. */
  WithExpr(const SourceLocation & loc, LocalScope * scope, Expr * body);

  /** The body of the try block */
  Expr * body() const { return body_; }
  void setBody(Expr * body) { body_ = body; }

  /** Assignments to scoped variables. */
  const ExprList & assigns() const { return assigns_; }
  ExprList & assigns() { return assigns_; }

  // Cleanups?

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const;
  bool isSingular() const;
  bool isLValue() const { return false; }

  static inline bool classof(const WithExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == With;
  }

private:
  LocalScope * scope_;
  Expr * body_;
  ExprList assigns_;
};

/// -------------------------------------------------------------------
/// A return, yield or throw statement
class ReturnExpr : public UnaryExpr {
public:
  /** Constructor. */
  ReturnExpr(ExprType k, const SourceLocation & loc, Expr * value);

  // Overrides

  bool isLValue() const { return false; }
  bool isSingular() const;
  void format(FormatStream & out) const;

  static inline bool classof(const ReturnExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Return || ex->exprType() == Yield;
  }
};

/// -------------------------------------------------------------------
/// A throw statement

class ThrowExpr : public UnaryExpr {
public:
  /** Constructor. */
  ThrowExpr(const SourceLocation & loc, Expr * value);

  // Overrides

  bool isLValue() const { return false; }

  static inline bool classof(const ThrowExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Throw;
  }
};

/// -------------------------------------------------------------------
/// A break or continue statement

class BranchExpr : public Expr {
public:

  /** Constructor. */
  BranchExpr(ExprType k, const SourceLocation & loc);

  // Overrides

  bool isSideEffectFree() const { return true; }
  bool isSingular() const { return true; }
  bool isLValue() const { return false; }

  static inline bool classof(const BranchExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Break || ex->exprType() == Continue || ex->exprType() == LocalReturn;
  }
};

/// -------------------------------------------------------------------
/// A local procedure, such as a macro expansion.

class LocalProcedureExpr : public UnaryExpr {
public:
  /** Constructor. */
  LocalProcedureExpr(const SourceLocation & loc, Expr * value);

  // Overrides

  bool isLValue() const { return false; }

  static inline bool classof(const ReturnExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == LocalProcedure;
  }
};

} // namespace tart

#endif

/* ================================================================ *
 TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_AST_STMT_H
#define TART_AST_STMT_H

#ifndef TART_COMMON_GC_H
#include "tart/Common/GC.h"
#endif

#ifndef TART_COMMON_SOURCELOCATION_H
#include "tart/Common/SourceLocation.h"
#endif

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

#include <llvm/Value.h>

namespace tart {

/// -------------------------------------------------------------------
/// Forward declarations.
class ASTDecl;
class ASTVarDecl;
class CodeGenerator;

// -------------------------------------------------------------------
// Statement base class
class Stmt: public ASTNode {
public:
  Stmt(NodeType stype, SourceLocation loc) :
    ASTNode(stype, loc) {
  }

  /** Return the location of the end of the statement. */
  virtual const SourceLocation & finalLocation() const { return location(); }

  void format(FormatStream & out) const;

  static inline bool classof(const Stmt *) { return true; }
  static inline bool classof(const ASTNode * e) {
    return e->nodeType() >= ASTNode::StmtFirst && e->nodeType() <= ASTNode::StmtLast;
  }
};

typedef llvm::SmallVector<Stmt *, 8> StmtList;

// -------------------------------------------------------------------
// Block statement
class BlockStmt: public Stmt {
public:
  BlockStmt(SourceLocation loc) :
    Stmt(Block, loc), finalLocation_(loc) {
  }

  BlockStmt(SourceLocation loc, const StmtList & stlist) :
    Stmt(Block, loc), statements(stlist), finalLocation_(loc) {
  }

  void append(Stmt * stmt) {
    statements.push_back(stmt);
  }

  void setFinalLocation(const SourceLocation & loc) {
    finalLocation_ = loc;
  }

  virtual const SourceLocation & finalLocation() const {
    return finalLocation_;
  }

  const StmtList & stmts() const { return statements; }
  StmtList & stmts() { return statements; }

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const BlockStmt *) {
    return true;
  }
  static inline bool classof(const ASTNode * e) {
    return e->nodeType() == ASTNode::Block;
  }

private:
  StmtList statements;
  SourceLocation finalLocation_;
};

// -------------------------------------------------------------------
// Expression statement
class ExprStmt: public Stmt {
public:
  ExprStmt(NodeType stype, SourceLocation loc, ASTNode * val) :
    Stmt(stype, loc), value_(val) {
  }

  const ASTNode * value() const { return value_; }
  void setValue(ASTNode * val) { value_ = val; }

  void trace() const;
  void format(FormatStream & out) const;

  static Stmt * get(ASTNode * ex);

protected:
  ASTNode * value_;
};

// -------------------------------------------------------------------
// Return statement
class ReturnStmt: public ExprStmt {
public:
  ReturnStmt(SourceLocation loc, ASTNode * ex) :
    ExprStmt(Return, loc, ex) {
  }

  void trace() const;
  void format(FormatStream & out) const;
};

// -------------------------------------------------------------------
// Return statement
class YieldStmt: public ExprStmt {
public:
  YieldStmt(SourceLocation loc, ASTNode * ex, int index) :
    ExprStmt(Yield, loc, ex), stateIndex_(index) {
  }

  int stateIndex() const { return stateIndex_; }

  void trace() const;
  void format(FormatStream & out) const;

private:
  int stateIndex_;
};

// -------------------------------------------------------------------
// Throw statement
class ThrowStmt: public ExprStmt {
public:
  ThrowStmt(SourceLocation loc, ASTNode * ex) :
    ExprStmt(Throw, loc, ex) {
  }

  void trace() const;
  void format(FormatStream & out) const;
};

// -------------------------------------------------------------------
// ASTDecl statement
class DeclStmt: public Stmt {
public:
  DeclStmt(SourceLocation loc, ASTDecl * de) :
    Stmt(LocalDecl, loc), decl_(de) {
  }

  const ASTDecl * decl() const { return decl_; }
  ASTDecl * decl() { return decl_; }

  void trace() const;
  void format(FormatStream & out) const;

  static Stmt * get(ASTDecl * de);

private:
  ASTDecl * decl_;
};

// -------------------------------------------------------------------
// If statement
class IfStmt: public Stmt {
public:
  IfStmt(SourceLocation loc, ASTNode * test, Stmt * th, Stmt * el) :
    Stmt(If, loc), testExpr_(test), thenSt_(th), elseSt_(el) {
  }

  /** The test expression. This may be a simple expression, a let-declaration,
      or a tuple of let-declarations. */
  const ASTNode * testExpr() const { return testExpr_; }
  void setTestExpr(ASTNode * test) { testExpr_ = test; }

  /** The statement to execute if the test is true. */
  Stmt * thenSt() const { return thenSt_; }

  /** The statement to execute if the test is false. */
  Stmt * elseSt() const { return elseSt_; }

  // Overrides

  virtual const SourceLocation & finalLocation() const {
    if (elseSt_)
      return elseSt_->finalLocation();
    else if (thenSt_)
      return thenSt_->finalLocation();
    else
      return location();
  }

  void format(FormatStream & out) const;
  void trace() const;

private:
  ASTNode * testExpr_;
  Stmt * thenSt_;
  Stmt * elseSt_;
};

// -------------------------------------------------------------------
// While statement
class WhileStmt: public Stmt {
public:
  WhileStmt(SourceLocation loc, ASTNode * test, Stmt * body) :
    Stmt(While, loc), testExpr_(test), body_(body) {
  }

  /** The test expression. This may be a simple expression, a let-declaration,
   or a tuple of let-declarations. */
  const ASTNode * testExpr() const { return testExpr_; }
  ASTNode * testExpr() { return testExpr_; }

  const Stmt * body() const { return body_; }
  Stmt * body() { return body_; }

  virtual const SourceLocation & finalLocation() const {
    if (body_)
      return body_->finalLocation();
    else
      return location();
  }

  void trace() const;
  void format(FormatStream & out) const;

private:
  ASTNode * testExpr_;
  Stmt * body_;
};

// -------------------------------------------------------------------
// For statement
class ForStmt: public Stmt {
public:
  ForStmt(SourceLocation loc, ASTNode * init, ASTNode * test, ASTNode * incr, Stmt * body) :
    Stmt(For, loc), initExpr_(init), testExpr_(test), incrExpr_(incr), body_(body) {
  }

  const ASTNode * initExpr() const { return initExpr_; }
  const ASTNode * testExpr() const { return testExpr_; }
  const ASTNode * incrExpr() const { return incrExpr_; }

  const Stmt * body() const { return body_; }
  Stmt * body() { return body_; }

  virtual const SourceLocation & finalLocation() const {
    if (body_)
      return body_->finalLocation();
    else
      return location();
  }

  void trace() const;
  void format(FormatStream & out) const;

private:
  ASTNode * initExpr_;
  ASTNode * testExpr_;
  ASTNode * incrExpr_;
  Stmt * body_;
};

// -------------------------------------------------------------------
// For each statement
class ForEachStmt: public Stmt {
public:
  ForEachStmt(SourceLocation loc, ASTVarDecl * lvars, ASTNode * iter, Stmt * body) :
    Stmt(ForEach, loc), loopVars_(lvars), iterExpr_(iter), body_(body)
  {
  }

  const ASTVarDecl * loopVars() const { return loopVars_; }
  ASTVarDecl * loopVars() { return loopVars_; }

  const ASTNode * iterExpr() const { return iterExpr_; }
  ASTNode * iterExpr() { return iterExpr_; }

  const Stmt * body() const { return body_; }
  Stmt * body() { return body_; }

  virtual const SourceLocation & finalLocation() const {
    if (body_)
      return body_->finalLocation();
    else
      return location();
  }

  void trace() const;
  void format(FormatStream & out) const;

private:
  ASTVarDecl * loopVars_;
  ASTNode * iterExpr_;
  Stmt * body_;
};

// -------------------------------------------------------------------
// Try statement
class TryStmt: public Stmt {
public:
  TryStmt(SourceLocation loc, Stmt * body) :
    Stmt(Try, loc), body_(body), elseSt_(NULL), finallySt_(NULL) {
  }

  Stmt * body() const { return body_; }

  const StmtList & catchList() const { return catchList_; }
  StmtList & catchList() { return catchList_; }

  Stmt * elseSt() const { return elseSt_; }
  void setElseSt(Stmt * st) { elseSt_ = st; }

  Stmt * finallySt() const { return finallySt_; }
  void setFinallySt(Stmt * st) { finallySt_ = st; }

  virtual const SourceLocation & finalLocation() const {
    if (finallySt_)
      return finallySt_->finalLocation();
    else if (elseSt_)
      return elseSt_->finalLocation();
    else if (!catchList_.empty())
      return catchList_.back()->finalLocation();
    else if (body_)
      return body_->finalLocation();
    else
      return location();
  }

  void trace() const;
  void format(FormatStream & out) const;

private:
  Stmt * body_;
  StmtList catchList_;
  Stmt * elseSt_;
  Stmt * finallySt_;
};

// -------------------------------------------------------------------
// Catch statement
class CatchStmt: public Stmt {
public:
  CatchStmt(SourceLocation loc, ASTDecl * ex, Stmt * body) :
    Stmt(Catch, loc), exceptDecl_(ex), body_(body) {
  }

  ASTDecl * exceptDecl() { return exceptDecl_; }
  Stmt * body() const { return body_; }

  void trace() const;
  void format(FormatStream & out) const;

private:
  ASTDecl * exceptDecl_;
  Stmt * body_;
};

// -------------------------------------------------------------------
// Switch statement
class SwitchStmt: public Stmt {
public:
  SwitchStmt(SourceLocation loc, ASTNode * testExpr)
    : Stmt(Switch, loc), testExpr_(testExpr) {
  }

  const StmtList & caseList() const { return caseList_; }
  StmtList & caseList() { return caseList_; }

  ASTNode * testExpr() const { return testExpr_; }

  void trace() const;
  void format(FormatStream & out) const;

private:
  ASTNode * testExpr_;
  StmtList caseList_;
};

// -------------------------------------------------------------------
// Case statement
class CaseStmt: public Stmt {
public:
  CaseStmt(SourceLocation loc, const ASTNodeList & caseExprs, Stmt * body)
    : Stmt(Case, loc), caseExprs_(caseExprs), body_(body) {
  }

  const ASTNodeList & caseExprs() const { return caseExprs_; }

  Stmt * body() const { return body_; }

  void trace() const;
  void format(FormatStream & out) const;

private:
  ASTNodeList caseExprs_;
  Stmt * body_;
};

// -------------------------------------------------------------------
// Classify statement
class ClassifyStmt: public Stmt {
public:
  ClassifyStmt(SourceLocation loc, ASTNode * testExpr)
    : Stmt(Classify, loc), testExpr_(testExpr) {
  }

  ASTNode * testExpr() const { return testExpr_; }

  const StmtList & caseList() const { return caseList_; }
  StmtList & caseList() { return caseList_; }

  void trace() const;
  void format(FormatStream & out) const;

private:
  ASTNode * testExpr_;
  StmtList caseList_;
};

// -------------------------------------------------------------------
// Classify/As statement
class ClassifyAsStmt: public Stmt {
public:
  ClassifyAsStmt(SourceLocation loc, ASTDecl * asDecl, Stmt * body)
    : Stmt(Case, loc), asDecl_(asDecl), body_(body) {
  }

  const ASTDecl * asDecl() const { return asDecl_; }

  Stmt * body() const { return body_; }

  void trace() const;
  void format(FormatStream & out) const;

private:
  ASTDecl * asDecl_;
  Stmt * body_;
};

// -------------------------------------------------------------------
// Jump to a label statement
class GotoStmt: public Stmt {
public:
  GotoStmt(NodeType stype, SourceLocation loc, const char * target) :
    Stmt(stype, loc), target_(target) {
  }

  const char * target() const { return target_; }
  void setTarget(const char * target) { target_ = target; }

  void format(FormatStream & out) const;

protected:
  const char * target_;
};

}

#endif

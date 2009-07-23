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
class CodeGenerator;

// -------------------------------------------------------------------
// Statement base class
class Stmt : public ASTNode {
public:
  Stmt(NodeType stype, SourceLocation loc)
    : ASTNode(stype, loc)
  {}
  
  /** Return the location of the end of the statement. */
  virtual const SourceLocation & getFinalLocation() const {
    return getLocation();
  }
  
  void format(FormatStream & out) const;
  static inline bool classof(const Stmt *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->getNodeType() >= ASTNode::StmtFirst &&
          e->getNodeType() <= ASTNode::StmtLast;
  }
};

typedef llvm::SmallVector<Stmt *, 8> StmtList;

// -------------------------------------------------------------------
// Block statement
class BlockStmt : public Stmt {
private:
  StmtList statements;
  SourceLocation finalLocation;
public:
  BlockStmt(SourceLocation loc)
    : Stmt(Block, loc)
    , finalLocation(loc)
  {
  }
  
  BlockStmt(SourceLocation loc, const StmtList & stlist)
    : Stmt(Block, loc)
    , statements(stlist)
    , finalLocation(loc)
  {
  }
  
  void append(Stmt * stmt) {
    statements.push_back(stmt);
  }
  
  void setFinalLocation(const SourceLocation & loc) {
    finalLocation = loc;
  }

  virtual const SourceLocation & getFinalLocation() const {
    return finalLocation;
  }
  
  const StmtList & getStmts() const { return statements; }
  StmtList & getStmts() { return statements; }

  void trace() const;
  void format(FormatStream & out) const;
  static inline bool classof(const BlockStmt *) { return true; }
  static inline bool classof(const ASTNode * e) {
      return e->getNodeType() == ASTNode::Block;
  }
};

// -------------------------------------------------------------------
// Expression statement
class ExprStmt : public Stmt {
protected:
  ASTNode * value;
public:
  ExprStmt(NodeType stype, SourceLocation loc, ASTNode * val)
    : Stmt(stype, loc)
    , value(val)
  {}
  
  const ASTNode * getValue() const { return value; }
  void setValue(ASTNode * val) { value = val; }

  void trace() const;
  void format(FormatStream & out) const;

  static Stmt * get(ASTNode * ex);
};

// -------------------------------------------------------------------
// Return statement
class ReturnStmt : public ExprStmt {
public:
  ReturnStmt(SourceLocation loc, ASTNode * ex)
    : ExprStmt(Return, loc, ex)
  {}
  
  void trace() const;
  void format(FormatStream & out) const;
};

// -------------------------------------------------------------------
// Return statement
class YieldStmt : public ExprStmt {
private:
  int stateIndex;
public:
  YieldStmt(SourceLocation loc, ASTNode * ex, int index)
      : ExprStmt(Yield, loc, ex)
      , stateIndex(index)
  {}
  
  int getStateIndex() const { return stateIndex; }
  
  void trace() const;
  void format(FormatStream & out) const;
};

// -------------------------------------------------------------------
// Throw statement
class ThrowStmt : public ExprStmt {
public:
  ThrowStmt(SourceLocation loc, ASTNode * ex)
    : ExprStmt(Throw, loc, ex)
  {}
  
  void trace() const;
  void format(FormatStream & out) const;
};

// -------------------------------------------------------------------
// ASTDecl statement
class DeclStmt : public Stmt {
private:
  ASTDecl * decl;
public:
  DeclStmt(SourceLocation loc, ASTDecl * de)
    : Stmt(LocalDecl, loc)
    , decl(de)
  {}

  const ASTDecl * getDecl() const { return decl; }
  ASTDecl * getDecl() { return decl; }

  void trace() const;
  void format(FormatStream & out) const;

  static Stmt * get(ASTDecl * de);
};

// -------------------------------------------------------------------
// If statement
class IfStmt : public Stmt {
private:
  ASTNode * testExpr;
  Stmt * thenSt;
  Stmt * elseSt;
public:
  IfStmt(SourceLocation loc, ASTNode * test, Stmt * th, Stmt * el)
    : Stmt(If, loc)
    , testExpr(test)
    , thenSt(th)
    , elseSt(el)
  {}

  /** The test expression. This may be a simple expression, a let-declaration,
      or a tuple of let-declarations. */
  const ASTNode * getTestExpr() const { return testExpr; }
  void setTestExpr(ASTNode * test) { testExpr = test; }

  /** The statement to execute if the test is true. */
  Stmt * getThenSt() const { return thenSt; }

  /** The statement to execute if the test is false. */
  Stmt * getElseSt() const { return elseSt; }
  
  // Overrides

  virtual const SourceLocation & getFinalLocation() const {
    if (elseSt) return elseSt->getFinalLocation();
    else if (thenSt) return thenSt->getFinalLocation();
    else return getLocation();
  }
  
  void format(FormatStream & out) const;
  void trace() const;
};

// -------------------------------------------------------------------
// While statement
class WhileStmt : public Stmt {
private:
  ASTNode * testExpr;
  Stmt * loopBody;
public:
  WhileStmt(SourceLocation loc, ASTNode * test, Stmt * body)
    : Stmt(While, loc)
    , testExpr(test)
    , loopBody(body)
  {
  }

  /** The test expression. This may be a simple expression, a let-declaration,
      or a tuple of let-declarations. */
  const ASTNode * getTestExpr() const { return testExpr; }
  ASTNode * getTestExpr() { return testExpr; }

  const Stmt * getLoopBody() const { return loopBody; }
  Stmt * getLoopBody() { return loopBody; }

  virtual const SourceLocation & getFinalLocation() const {
    if (loopBody) return loopBody->getFinalLocation();
    else return getLocation();
  }
  
  void trace() const;
  void format(FormatStream & out) const;
};

// -------------------------------------------------------------------
// For statement
class ForStmt : public Stmt {
private:
  ASTNode * initExpr;
  ASTNode * testExpr;
  ASTNode * incrExpr;
  Stmt * loopBody;
public:
  ForStmt(SourceLocation loc, ASTNode * init, ASTNode * test, ASTNode * incr,
      Stmt * body)
    : Stmt(For, loc)
    , initExpr(init)
    , testExpr(test)
    , incrExpr(incr)
    , loopBody(body)
  {
  }

  const ASTNode * getInitExpr() const { return initExpr; }
  const ASTNode * getTestExpr() const { return testExpr; }
  const ASTNode * getIncrExpr() const { return incrExpr; }

  const Stmt * getLoopBody() const { return loopBody; }
  Stmt * getLoopBody() { return loopBody; }

  virtual const SourceLocation & getFinalLocation() const {
    if (loopBody) return loopBody->getFinalLocation();
    else return getLocation();
  }
  
  void trace() const;
  void format(FormatStream & out) const;
};

// -------------------------------------------------------------------
// For each statement
class ForEachStmt : public Stmt {
private:
  ASTNode * loopVars;
  ASTNode * iterExpr;
  Stmt * loopBody;

  // Generated from the iterExpr
  //ASTNode * initExpr;
  //ASTNode * testExpr;
  //ASTNode * incrExpr;
public:
  ForEachStmt(SourceLocation loc, ASTNode * lvars, ASTNode * iter, Stmt * body)
    : Stmt(ForEach, loc)
    , loopVars(lvars)
    , iterExpr(iter)
    , loopBody(body)
    //, initExpr(NULL)
    //, testExpr(NULL)
    //, incrExpr(NULL)
  {
      //getTestScope().setScopeName("--foreach--");
  }

  const ASTNode * getLoopVars() const { return loopVars; }
  ASTNode * getLoopVars() { return loopVars; }
  
  const ASTNode * getIterExpr() const { return iterExpr; }
  ASTNode * getIterExpr() { return iterExpr; }

  const Stmt * getLoopBody() const { return loopBody; }
  Stmt * getLoopBody() { return loopBody; }

  //void setIterExpr(const ASTNode * iter) { iterExpr = iter; }
  
  //const ASTNode * getInitExpr() const { return initExpr; }
  //void setInitExpr(const ASTNode * expr) { initExpr = expr; }

  //const ASTNode * getTestExpr() const { return testExpr; }
  //void setTestExpr(const ASTNode * expr) { testExpr = expr; }

  //const ASTNode * getIncrExpr() const { return incrExpr; }
  //void setIncrExpr(const ASTNode * expr) { incrExpr = expr; }

  virtual const SourceLocation & getFinalLocation() const {
    if (loopBody) return loopBody->getFinalLocation();
    else return getLocation();
  }
  
  void trace() const;
  void format(FormatStream & out) const;
};
          
// -------------------------------------------------------------------
// Try statement
class TryStmt : public Stmt {
private:
  Stmt * bodySt;
  StmtList catchList;
  Stmt * elseSt;
  Stmt * finallySt;
public:
  TryStmt(SourceLocation loc, Stmt * body)
    : Stmt(Try, loc)
    , bodySt(body)
    , elseSt(NULL)
    , finallySt(NULL)
  {
  }

  Stmt * getBodySt() const { return bodySt; }
  
  const StmtList & getCatchList() const { return catchList; }
  StmtList & getCatchList() { return catchList; }

  Stmt * getElseSt() const { return elseSt; }
  void setElseSt(Stmt * st) { elseSt = st; }

  Stmt * getFinallySt() const { return finallySt; }
  void setFinallySt(Stmt * st) { finallySt = st; }

  virtual const SourceLocation & getFinalLocation() const {
    if (finallySt) return finallySt->getFinalLocation();
    else if (elseSt) return elseSt->getFinalLocation();
    else if (!catchList.empty()) return catchList.back()->getFinalLocation();
    else if (bodySt) return bodySt->getFinalLocation();
    else return getLocation();
  }
  
  void trace() const;
  void format(FormatStream & out) const;
};

// -------------------------------------------------------------------
// Catch statement
class CatchStmt : public Stmt {
private:
  ASTDecl * exceptDecl;
  Stmt * bodySt;
public:
  CatchStmt(SourceLocation loc, ASTDecl * ex, Stmt * body)
    : Stmt(Catch, loc)
    , exceptDecl(ex)
    , bodySt(body)
  {
  }

  ASTDecl * getExceptDecl() { return exceptDecl; }
  Stmt * getBodySt() const { return bodySt; }

  void trace() const;
  void format(FormatStream & out) const;
};
          
// -------------------------------------------------------------------
// Intrinsic statement - represents a built-in compiler primitive.
class IntrinsicStmt : public Stmt {
public:
  IntrinsicStmt() : Stmt(Intrinsic, SourceLocation()) {}
  
  //virtual llvm::Value * gen(CodeGenerator & gen, const ASTNodeList & args,
  //    const Type * retType) const = 0;
  
  void format(FormatStream & out) const;
};

}

#endif

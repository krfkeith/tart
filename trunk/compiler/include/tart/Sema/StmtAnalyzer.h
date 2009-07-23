/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_SEMA_STMTANALYZER_H
#define TART_SEMA_STMTANALYZER_H

#ifndef TART_SEMA_ANALYZERBASE_H
#include "tart/Sema/AnalyzerBase.h"
#endif

#ifndef TART_COMMON_SOURCELOCATION_H
#include "tart/Common/SourceLocation.h"
#endif

#ifndef TART_CFG_BLOCK_H
#include "tart/CFG/Block.h"
#endif

#include <llvm/ADT/DenseMap.h>

namespace tart {
  
class Stmt;
class BlockStmt;
class ExprStmt;
class IfStmt;
class WhileStmt;
class ForStmt;
class ForEachStmt;
class ThrowStmt;
class TryStmt;
class CatchStmt;
class ReturnStmt;
class YieldStmt;
class DeclStmt;
class Type;
class FunctionDefn;

/// -------------------------------------------------------------------
/// Declaration analyzer
class StmtAnalyzer : public AnalyzerBase {
public:
  /** Constructor. */
  StmtAnalyzer(FunctionDefn * func);

  /** The target function being analyzed. */
  FunctionDefn * getTarget() { return function; }
  
  /** Build the control flow graph for this function. */
  bool buildCFG();
  bool buildStmtCFG(const Stmt * st);
  bool buildBlockStmtCFG(const BlockStmt * st);
  bool buildExprStmtCFG(const ExprStmt * st);
  bool buildIfStmtCFG(const IfStmt * st);
  bool buildWhileStmtCFG(const WhileStmt * st);
  bool buildForStmtCFG(const ForStmt * st);
  bool buildForEachStmtCFG(const ForEachStmt * st);
  bool buildThrowStmtCFG(const ThrowStmt * st);
  bool buildTryStmtCFG(const TryStmt * st);
  bool buildReturnStmtCFG(const ReturnStmt * st);
  bool buildYieldStmtCFG(const YieldStmt * st);
  bool buildBreakStmtCFG(const Stmt * st);
  bool buildContinueStmtCFG(const Stmt * st);
  bool buildLocalDeclStmtCFG(const DeclStmt * st);

  /** Infer the function return type. */
  void inferReturnType();
  
  /** Warn about return type conflict. */
  void warnConflict(
    const SourceLocation & prevLoc, const Type * prevType,
    const SourceLocation & nextLoc, const Type * nextType) const;

  /** Do type inference on the expression if it is non-singular. */
  Expr * inferTypes(Expr * expr, Type * expectedType);

  /** Convert an AST to an expression, which might include a
      assignment. */
  Expr * astToAssignExpr(const ASTNode * ast, Type * expectedType);

  /** Convert an AST into an lvalue expression. */
  Expr * astToLValueExpr(const ASTNode * ast, Type * expectedType);
    
  /** Convert an AST to a test expression, which might include a
      let-declaration. */
  Expr * astToTestExpr(const ASTNode * test);

  /** Build expression tree from AST. Don't do any type inferencing yet. */
  Expr * astToExpr(const ASTNode * ast, Type * expectedType);

  /** Build defn from AST. Don't do any type inferencing yet. */
  Defn * astToDefn(const ASTDecl * ast);

  /** Set the unwind target - the block that will be executed when an exception is thrown. */
  Block * setUnwindTarget(Block * target) {
    Block * prevTarget = unwindTarget_;
    unwindTarget_ = target;
    return prevTarget;
  }

  /** Cause a 'return' statement to instead assign to a local variable.
      Used during macro expansion. Returns previous value. */
  LValueExpr * setMacroReturnVal(LValueExpr * retVal);
  
  /** Cause a 'return' statement to jump to a block instead of returning. */
  Block * setMacroReturnTarget(Block * blk);
  
  /** Create a new local scope and make it's parent the current scope.
      Do not yet set it as the current active scope. */
  LocalScope * createLocalScope(const char * scopeName);

  /** Return the current block. */
  Block * insertionBlock() const { return currentBlock_; }

  /** Set the insertion point to be the end of block 'blk'. Any instructions will be
      appended to this block; Any new blocks created will be inserted after this block. */
  void setInsertPos(Block * blk);

  /** Create a basic block in the current function. This will insert the new block at the
      current insertion point. */
  Block * createBlock(const char * name);

  /** True if any catch target can catch the specified exception type */
  static bool canCatch(TypeList & catchTypes, CompositeType * exceptionType);
  
  /** Define a 'local procedure' - a sequence of blocks within a function that can be invoked
      like a subroutine. This doesn't actually get compiled into a subroutine, however - it
      gets converted into a state-machine-like set of jumps that sets a state variable and
      then jumps back based on the value of that variable.
    */
  void defineLocalProcedure(Block * first, Block * last);

  /** Convert local calls and local returns to branches. */
  void flattenLocalProcedureCalls();

private:

  // Struct to keep track of "finally" and other statements that define a cleanup.
  struct CleanupHandler {
    CleanupHandler * prev;      // Enclosing cleanup handler
    Block * target;             // Block that handles cleanup.

    CleanupHandler(CleanupHandler * prev, Block * blk)
      : prev(prev)
      , target(blk)
    {}
  };

  // Struct to keep track of 'local procedures' - blocks of code that can be invoked
  // from more than one place within the body of the function.
  struct LocalProcedure {
    Block * first;            // The starting block of the procedure
    Block * last;             // The ending block of the procedure.
    VariableDefn * stateVar;  // The variable used to determine where to return to.
    LValueExpr * stateExpr;   // Expression used to set / get this state.
  
    // The set of blocks which get executed after the procedure returns.
    BlockList followingBlocks;

    LocalProcedure() : first(NULL), last(NULL), stateVar(NULL), stateExpr(NULL) {}
    
    int addFollowingBlock(Block * b);
  };

  typedef llvm::DenseMap<Block *, LocalProcedure> LocalProcedureMap;

  /** Find a local procedure by the first block. */
  LocalProcedure & findLocalProcedure(Block * first);

  FunctionDefn * function;
  Type * returnType;
  Type * yieldType_;
  BlockList & blocks;
  BlockList::iterator insertPos_;
  Block * currentBlock_;
  Block * continueTarget_;
  Block * breakTarget;
  Block * unwindTarget_;

  // The list of cleanup handlers for the current block.
  CleanupHandler * cleanups_;

  // The EHC which was in effect at the start of the current loop
  CleanupHandler * loopCleanups_;

  // Information to track local procedures.
  LocalProcedureMap localProcedures_;

  // Variables which are set while we are inside a macro expansion.
  LValueExpr * macroReturnVal_;
  Block * macroReturnTarget_;
};

}

#endif

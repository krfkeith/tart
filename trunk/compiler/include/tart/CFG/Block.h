/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_BLOCK_H
#define TART_CFG_BLOCK_H

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

#include <llvm/ADT/SmallVector.h>

namespace llvm {
class BasicBlock;
}

namespace tart {

class VariableDefn;
class FormatStream;

/// -------------------------------------------------------------------
/// Known types of block terminators
enum BlockTerm {
  BlockTerm_None,             // Block termination not set
  BlockTerm_Branch,           // Unconditional branch
  BlockTerm_Conditional,      // Conditional branch
  BlockTerm_Return,           // Return from function
  BlockTerm_Switch,           // Switch statement
  BlockTerm_Throw,            // Throw exception
  BlockTerm_ResumeUnwind,     // Resume a thrown exception
  BlockTerm_LocalReturn,      // Return to previous state (used by 'finally' and macros).
  BlockTerm_Catch,            // Catch exception - dispatch to catch handlers.
  BlockTerm_TraceCatch,       // Catch exception with backtrace enabled
  //Yield
};

/// -------------------------------------------------------------------
/// A Basic Block
class Block : public GC {
public:
  Block(const char * lbl)
    : label_(lbl)
    , index_(0)
    , terminator_(BlockTerm_None)
    , unwindTarget_(NULL)
    , irBlock_(NULL)
  {}

  /** Return the label for this block. */
  const char * label() const { return label_; }

  /** Sequence number for this block. */
  int index() const { return index_; }
  void setIndex(int index) { index_ = index; }

  /** Get the terminator instruction for this block. */
  BlockTerm terminator() { return terminator_; }
  void setTerminator(const SourceLocation & loc, BlockTerm t) {
    terminator_ = t;
    termLoc_ = loc;
  }

  /** Source location for the terminator. */
  const SourceLocation & termLocation() const { return termLoc_; }

  /** Return true if this block's terminator has been set. */
  bool hasTerminator() const {
    return terminator_ != BlockTerm_None;
  }

  /** Set the block terminator back to the initial state. */
  void clearTerminator() {
    terminator_ = BlockTerm_None;
    termLoc_ = SourceLocation();
    succs_.clear();
  }

  /** Append an expression to this block. */
  void append(Expr * en) { stmtExprs_.push_back(en); }

  /** The catch handler block for any exceptions thrown in this block. */
  Block * unwindTarget() { return unwindTarget_; }
  void setUnwindTarget(Block * blk) { unwindTarget_ = blk; }

  /** Create an unconditional branch terminator. */
  void branchTo(const SourceLocation & loc, Block * target);

  /** Creates a conditional branch terminator. */
  void condBranchTo(const SourceLocation & loc, Expr * test, Block * trueTarget,
      Block * falseTarget);

  /** Creates a return terminator. */
  void exitReturn(const SourceLocation & loc, Expr * returnVal);

  /** Create a throw block terminator. */
  void exitThrow(const SourceLocation & loc, Expr * except);

  /** Create a resume unwind block terminator. */
  void exitResumeUnwind(const SourceLocation & loc, Expr * except);

  /** Adds a switch or catch case. */
  void addCase(Expr * caseVal, Block * target);

  /** Split this block - transfer the end state of this block to the new block. */
  Block * split(const char * lbl = NULL) {
    Block * newBlock = new Block(lbl ? lbl : label_);
    newBlock->terminator_ = terminator_;
    newBlock->termLoc_ = termLoc_;
    std::swap(succs_, newBlock->succs_);
    std::swap(termExprs_, newBlock->termExprs_);
    terminator_ = BlockTerm_None;
    return newBlock;
  }

  llvm::BasicBlock * irBlock() const { return irBlock_; }
  void setIRBlock(llvm::BasicBlock * b) { irBlock_ = b; }

  /** The list of expression nodes which are evaluated from this block. */
  ExprList & exprs() { return stmtExprs_; }

  /** Return all of the predecessor blocks to this block. */
  BlockList & preds() { return preds_; }

  /** Return all of the successor blocks to this block. */
  BlockList & succs() { return succs_; }

  llvm::BasicBlock * succIRBlock(int index) const {
    return succs_[index]->irBlock();
  }

  /** Given a predecessor block, return it's relative index. */
  // TODO: Is this used?
  int predIndex(Block * blk) const {
    for (size_t i = 0; i < succs_.size(); ++i) {
      if (blk == succs_[i]) {
        return int(i);
      }
    }

    return -1;
  }

  /** Termination expressions. This has a different interpretation depending
      on the type of block terminator:

        - BlockTerm_None - n/a
        - BlockTerm_Branch - n/a
        - BlockTerm_Conditional - contains the test expression
        - BlockTerm_Return - contains the return value
        - BlockTerm_Throw - contains the exception
        - BlockTerm_Switch - contains an expression for each case value
        - BlockTerm_Catch - contains the exception types
  */
  ExprList & termExprs() { return termExprs_; }

  /** For terminators that have a single expression, return that expression. */
  Expr * termValue() {
    assert(termExprs_.size() == 1);
    return termExprs_.front();
  }

  void trace() const;

private:
  const char * label_;        // Label for this block.
  int index_;                 // Block sequence number within function.
  BlockList preds_;           // Predecessor blocks.
  BlockList succs_;           // Successor blocks.
  ExprList stmtExprs_;        // List of expressions to be evaluated
  BlockTerm terminator_;      // Type of block termination
  SourceLocation termLoc_;    // Terminator location
  ExprList termExprs_;        // Termination expressions
  Block * unwindTarget_;      // The catch handler for any exceptions in this block.
  llvm::BasicBlock * irBlock_;// The LLVM basic block for this one.
};

/** Stream operator for blocks, prints the block label. */
FormatStream & operator<<(FormatStream & out, Block * blk);

}

#endif

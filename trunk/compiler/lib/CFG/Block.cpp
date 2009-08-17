/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Block.h"
#include "tart/CFG/Expr.h"
#include "tart/CFG/Defn.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

void Block::branchTo(const SourceLocation & loc, Block * target) {
  DASSERT(terminator_ == BlockTerm_None);
  terminator_ = BlockTerm_Branch;
  termLoc_ = loc;
  succs_.push_back(target);
}

void Block::condBranchTo(const SourceLocation & loc, Expr * test, Block * trueTarget,
    Block * falseTarget) {
  DASSERT(terminator_ == BlockTerm_None);
  terminator_ = BlockTerm_Conditional;
  termLoc_ = loc;
  termExprs_.push_back(test);
  succs_.push_back(trueTarget);
  succs_.push_back(falseTarget);
}

void Block::addCase(Expr * caseVal, Block * target) {
  termExprs_.push_back(caseVal);
  succs_.push_back(target);
}

void Block::exitReturn(const SourceLocation & loc, Expr * returnVal) {
  termExprs_.clear();
  termExprs_.push_back(returnVal);
  terminator_ = BlockTerm_Return;
  termLoc_ = loc;
}

void Block::exitThrow(const SourceLocation & loc, Expr * exceptVal) {
  termExprs_.clear();
  termExprs_.push_back(exceptVal);
  terminator_ = BlockTerm_Throw;
  termLoc_ = loc;
}

void Block::exitResumeUnwind(const SourceLocation & loc, Expr * exceptVal) {
  termExprs_.clear();
  termExprs_.push_back(exceptVal);
  terminator_ = BlockTerm_ResumeUnwind;
  termLoc_ = loc;
}

void Block::trace() const {
  markList(preds_.begin(), preds_.end());
  markList(succs_.begin(), succs_.end());
  markList(stmtExprs_.begin(), stmtExprs_.end());

  // Need to handle terminators differently because they can be NULL.
  for (ExprList::const_iterator it = termExprs_.begin(); it != termExprs_.end(); ++it) {
    if (*it) {
      (*it)->mark();
    }
  }

  safeMark(unwindTarget_);
}

FormatStream & operator<<(FormatStream & out, Block * blk) {
  if (blk->index()) {
    out << blk->label() << "-" << blk->index();
  } else {
    out << blk->label();
  }
}

} // namespace tart

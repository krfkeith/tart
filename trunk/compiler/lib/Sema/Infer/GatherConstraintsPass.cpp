/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"

#include "tart/Type/AmbiguousPhiType.h"

#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/Infer/GatherConstraintsPass.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/Support/CommandLine.h"

namespace tart {

// -------------------------------------------------------------------
// GatherConstraintsPass

Expr * GatherConstraintsPass::visitCall(CallExpr * in) {
  if (!in->candidates().empty() && visited_.insert(in)) {
    // Note: pre-order traversal.
    Expr * result = CFGPass::visitCall(in);
    calls_.push_back(new CallSite(in));

    // If the function is NULL, it means that the function reference is
    // in the individual candidates.
    if (in->function() == NULL) {
      Candidates & cd = in->candidates();
      for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
        visitExpr((*it)->base());
      }
    }
    return result;
  } else {
    return CFGPass::visitCall(in);
  }
}


Expr * GatherConstraintsPass::visitAssign(AssignmentExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    conversions_.push_back(new AssignmentSite(in));
  }

  return CFGPass::visitAssign(in);
}

Expr * GatherConstraintsPass::visitPostAssign(AssignmentExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    conversions_.push_back(new AssignmentSite(in));
  }

  return CFGPass::visitPostAssign(in);
}

Expr * GatherConstraintsPass::visitTupleCtor(TupleCtorExpr * in) {
  in = cast<TupleCtorExpr>(CFGPass::visitTupleCtor(in));
  if (!in->isSingular() && visited_.insert(in)) {
    conversions_.push_back(new TupleCtorSite(in));
  }

  return in;
}

Expr * GatherConstraintsPass::visitIf(IfExpr * in) {
  CFGPass::visitIf(in);
  visitPHI(in);
  return in;
}

Expr * GatherConstraintsPass::visitSwitch(SwitchExpr * in) {
  CFGPass::visitSwitch(in);
  visitPHI(in);
  return in;
}

Expr * GatherConstraintsPass::visitMatch(MatchExpr * in) {
  CFGPass::visitMatch(in);
  visitPHI(in);
  return in;
}

void GatherConstraintsPass::visitPHI(Expr * in) {
  if (in->type().isa<AmbiguousPhiType>()) {
    conversions_.push_back(new PHISite(in));
  }
}

} // namespace tart

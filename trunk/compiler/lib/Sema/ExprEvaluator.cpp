/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Expr.h"
#include "tart/Sema/ExprEvaluator.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// ExprEvaluator

Expr * ExprEvaluator::eval(Expr * in) {
  switch (in->exprType()) {
  default:
    diag.fatal(in) << "Evaluation failure for expr type: " << exprTypeName(in->exprType());
    return NULL;
  }
}

}

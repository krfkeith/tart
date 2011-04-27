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

#ifndef TART_SEMA_EXPRANALYZER_H
#include "tart/Sema/ExprAnalyzer.h"
#endif

#include "llvm/ADT/DenseMap.h"

namespace tart {

class Stmt;
class FunctionDefn;

/// -------------------------------------------------------------------
/// Declaration analyzer
class StmtAnalyzer : public ExprAnalyzer {
public:
  /** Constructor. */
  StmtAnalyzer(FunctionDefn * func, const Stmt * body);

  /** The target function being analyzed. */
  FunctionDefn * function() { return currentFunction_; }

  /** Build the control flow graph for this function. */
  bool buildCFG();

private:
  const Stmt * body_;
  Type * yieldType_;
};

}

#endif

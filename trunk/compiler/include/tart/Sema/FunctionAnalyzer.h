/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_FUNCTIONANALYZER_H
#define TART_SEMA_FUNCTIONANALYZER_H

#ifndef TART_SEMA_DEFNANALYZER_H
#include "tart/Sema/DefnAnalyzer.h"
#endif

#ifndef TART_CFG_FUNCTIONDEFN_H
#include "tart/CFG/FunctionDefn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Declaration analyzer
class FunctionAnalyzer : public DefnAnalyzer {
private:
  FunctionDefn * target;

public:
  /** Constructor. */
  FunctionAnalyzer(FunctionDefn * func);

  /** Fully analyze the input defn and all of its descendants. */
  bool analyze(AnalysisTask task);
  bool runPasses(FunctionDefn::PassSet passesToRun);

  bool resolveReturnType();
  bool resolveParameterTypes();
  bool resolveModifiers();
  bool createCFG();
  bool merge();
  bool createReflectionData();
  bool analyzeRecursive(AnalysisTask task, FunctionDefn::AnalysisPass pass);
  void warnConflict(
      const SourceLocation & prevLoc, const Type * prevType,
      const SourceLocation & nextLoc, const Type * nextType) const;
};

} // namespace tart

#endif

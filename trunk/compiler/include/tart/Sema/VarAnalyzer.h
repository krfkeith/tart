/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_VARANALYZER_H
#define TART_SEMA_VARANALYZER_H

#ifndef TART_SEMA_DEFNANALYZER_H
#include "tart/Sema/DefnAnalyzer.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Analyzer for Let, Var and function/template parameters
class VarAnalyzer : public DefnAnalyzer {
public:
  /** Constructor. */
  VarAnalyzer(VariableDefn * var);
  VarAnalyzer(VariableDefn * var, Scope * scope, Module * module,
      Defn * subject, FunctionDefn * currentFunction);

  /** Fully analyze the input defn and all of its descendants. */
  bool analyze(AnalysisTask task);
  bool runPasses(VariableDefn::PassSet passesToRun);
  bool resolveVarType();
  bool analyzeTypeModifiers();
  bool resolveInitializers();

  void setTargetType(QualifiedType type);

private:
  VariableDefn * target;
  bool trace_;

  Expr * initializeNativeArray(Expr * initValue);
};

} // namespace tart

#endif

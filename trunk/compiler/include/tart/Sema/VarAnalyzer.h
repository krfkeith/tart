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
private:
  ValueDefn * target;
  
  void setTargetType(Type * type);

public:
  /** Constructor. */
  VarAnalyzer(ValueDefn * var);
  VarAnalyzer(ValueDefn * var, Module * module);

  /** Fully analyze the input defn and all of its descendants. */
  bool analyze(AnalysisTask task);
  bool resolveVarType();
};

}

#endif

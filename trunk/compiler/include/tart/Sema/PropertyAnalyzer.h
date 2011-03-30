/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_PROPERTYANALYZER_H
#define TART_SEMA_PROPERTYANALYZER_H

#ifndef TART_SEMA_DEFNANALYZER_H
#include "tart/Sema/DefnAnalyzer.h"
#endif

#ifndef TART_DEFN_PROPERTYDEFN_H
#include "tart/Defn/PropertyDefn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Analyzer for Let, Var and function/template parameters
class PropertyAnalyzer : public DefnAnalyzer {
private:
  PropertyDefn * target;

public:
  /** Constructor. */
  PropertyAnalyzer(PropertyDefn * var);

  /** Fully analyze the input defn and all of its descendants. */
  bool analyze(AnalysisTask task);
  bool runPasses(PropertyDefn::PassSet passesToRun);
  bool resolvePropertyType();
};

}

#endif

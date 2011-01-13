/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_NAMESPACEANALYZER_H
#define TART_SEMA_NAMESPACEANALYZER_H

#ifndef TART_SEMA_DEFNANALYZER_H
#include "tart/Sema/DefnAnalyzer.h"
#endif

#ifndef TART_CFG_NAMESPACEDEFN_H
#include "tart/CFG/NamespaceDefn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Analyzer for classes, structs, and interfaces.
class NamespaceAnalyzer : public DefnAnalyzer {
public:
  /** Constructor. */
  NamespaceAnalyzer(NamespaceDefn * target);

  bool analyze(AnalysisTask task);
  bool runPasses(NamespaceDefn::PassSet passesToRun);
  bool analyzeImports();
  bool analyzeMethods();
  bool resolveStaticInitializers();

private:
  NamespaceDefn * target;
};

}

#endif

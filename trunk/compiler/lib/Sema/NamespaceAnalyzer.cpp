/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Module.h"
#include "tart/Sema/NamespaceAnalyzer.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

static const NamespaceDefn::PassSet PASS_SET_LOOKUP = NamespaceDefn::PassSet::of(
  NamespaceDefn::ImportPass,
  NamespaceDefn::ScopeCreationPass
);

static const NamespaceDefn::PassSet PASS_SET_CODEGEN = NamespaceDefn::PassSet::of(
  NamespaceDefn::ImportPass,
  NamespaceDefn::ScopeCreationPass,
  NamespaceDefn::StaticInitializerPass
);

NamespaceAnalyzer::NamespaceAnalyzer(NamespaceDefn * de)
  : DefnAnalyzer(de->module(), de->definingScope(), de, NULL)
  , target(de)
{
  DASSERT(de != NULL);
}

bool NamespaceAnalyzer::analyze(AnalysisTask task) {
  TaskInProgress tip(target, task);
  switch (task) {
    case Task_PrepMemberLookup:
    //case Task_PrepCallOrUse:
      return runPasses(PASS_SET_LOOKUP);

    case Task_PrepTypeGeneration:
    case Task_PrepCodeGeneration:
    case Task_PrepEvaluation:
    case Task_PrepReflection:
      return runPasses(PASS_SET_CODEGEN);

    default:
      return true;
  }
}

bool NamespaceAnalyzer::runPasses(NamespaceDefn::PassSet passesToRun) {
  // Work out what passes need to be run.
  passesToRun.removeAll(target->passes().finished());
  if (passesToRun.empty()) {
    return true;
  }

  if (passesToRun.contains(NamespaceDefn::ImportPass) && !analyzeImports()) {
    return false;
  }

  if (passesToRun.contains(NamespaceDefn::ScopeCreationPass) &&
      target->passes().begin(NamespaceDefn::ScopeCreationPass)) {
    if (!createMembersFromAST(target)) {
      return false;
    }

    target->passes().finish(NamespaceDefn::ScopeCreationPass);
  }

  if (passesToRun.contains(NamespaceDefn::StaticInitializerPass) && !resolveStaticInitializers()) {
    return false;
  }

  return true;
}

bool NamespaceAnalyzer::analyzeImports() {
  if (target->passes().begin(NamespaceDefn::ImportPass)) {
    if (target->ast() != NULL) {
      DefnAnalyzer da(target->module(), &target->memberScope(), target, NULL);
      const ASTNodeList & imports = target->ast()->imports();
      for (ASTNodeList::const_iterator it = imports.begin(); it != imports.end(); ++it) {
        da.importIntoScope(cast<ASTImport>(*it), &target->memberScope());
      }
    }

    target->passes().finish(NamespaceDefn::ImportPass);
  }

  return true;
}

bool NamespaceAnalyzer::resolveStaticInitializers() {
  if (target->passes().begin(NamespaceDefn::StaticInitializerPass)) {
    for (Defn * m = target->memberScope().firstMember(); m != NULL; m = m->nextInScope()) {
      if (analyzeCompletely(m) && m->isSingular()) {
        //diag.debug(m) << "Analyzing " << m;
        target->module()->addSymbol(m);
      }
    }

    target->passes().finish(NamespaceDefn::StaticInitializerPass);
  }

  return true;
}

#if 0
bool NamespaceAnalyzer::checkNameConflicts() {
  bool success = true;
  if (type->passes().begin(NamespaceDefn::NamingConflictPass)) {
    Defn::DefnType dtype = target->defnType();
    const SymbolTable & symbols = type->members();
    for (SymbolTable::const_iterator entry = symbols.begin(); entry != symbols.end(); ++entry) {
      const SymbolTable::Entry & defns = entry->second;
      Defn::DefnType dtype = defns.front()->defnType();

      // First insure that all entries are the same type
      for (SymbolTable::Entry::const_iterator it = defns.begin(); it != defns.end(); ++it) {
        Defn * de = *it;
        if (de->defnType() != dtype) {
          diag.error(de) << "Definition of '" << de->name() << "' as '" << de <<
              "' conflicts with earlier definition:";
          diag.info(defns.front()) << defns.front();
          success = false;
          break;
        }
      }
    }

    type->passes().finish(NamespaceDefn::NamingConflictPass);
  }

  return success;
}
#endif

} // namespace tart

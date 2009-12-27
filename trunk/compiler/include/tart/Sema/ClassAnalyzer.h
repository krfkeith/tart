/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_CLASSANALYZER_H
#define TART_SEMA_CLASSANALYZER_H

#ifndef TART_SEMA_DEFNANALYZER_H
#include "tart/Sema/DefnAnalyzer.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Analyzer for classes, structs, and interfaces.
class ClassAnalyzer : public DefnAnalyzer {
public:
  /** Constructor. */
  ClassAnalyzer(TypeDefn * target);

  CompositeType * targetType() const;

  /** Fully analyze the input defn and all of its descendants. */
  bool analyze(AnalysisTask task);
  bool runPasses(CompositeType::PassSet passesToRun);

  bool analyzeBaseClasses();
  bool analyzeBaseClassesImpl();
  bool checkNameConflicts();
  bool analyzeImports();
  bool analyzeCoercers();
  bool analyzeConstructors();
  bool analyzeMemberTypes();
  bool analyzeFields();
  void analyzeConstructBase(FunctionDefn * ctor);
  bool analyzeMethods();
  bool analyzeOverloading();
  bool analyzeFieldTypes();
  bool analyzeCompletely();

  void overrideMembers();
  void overrideMethods(MethodList & table, const MethodList & overrides, bool canHide);
  void overridePropertyAccessors(MethodList & table, PropertyDefn * prop,
      const MethodList & accessors, bool canHide);
  void copyBaseClassMethods();
  void createInterfaceTables();
  void ensureUniqueSignatures(MethodList & methods);
  void addNewMethods();
  void checkForRequiredMethods();
  FunctionDefn * findOverride(const FunctionDefn * f, const MethodList & overrides);

  bool createDefaultConstructor();

private:
  TypeDefn * target;
};

}

#endif

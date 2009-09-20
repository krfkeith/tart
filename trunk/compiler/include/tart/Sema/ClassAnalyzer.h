/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_CLASSANALYZER_H
#define TART_SEMA_CLASSANALYZER_H

#ifndef TART_SEMA_DEFNANALYZER_H
#include "tart/Sema/DefnAnalyzer.h"
#endif

#include <llvm/ADT/SetVector.h>

namespace tart {

/// -------------------------------------------------------------------
/// Analyzer for classes, structs, and interfaces.
class ClassAnalyzer : public DefnAnalyzer {
private:
  TypeDefn * target;

public:
  /** Constructor. */
  ClassAnalyzer(TypeDefn * target);

  /** Fully analyze the input defn and all of its descendants. */
  bool analyze(AnalysisTask task);

  bool analyzeBaseClasses();
  bool analyzeBaseClassesImpl();
  bool analyzeConstructors();
  bool analyzeFields();
  void analyzeConstructBase(FunctionDefn * ctor);
  bool analyzeMethods();
  bool analyzeOverloading();
  bool analyzeStaticInitializers();

  void overrideMembers();
  void overrideMethods(MethodList & table, const MethodList & overrides, bool canHide);
  //void overridePropertyAccessors(MethodList & table, const MethodList & accessors, bool canHide);
  void copyBaseClassMethods();
  void createInterfaceTables();
  void ensureUniqueSignatures(MethodList & methods);
  void addNewMethods();
  void checkForRequiredMethods();
  bool hasSameSignature(FunctionDefn * f0, FunctionDefn * f1);
  FunctionDefn * findOverride(const FunctionDefn * f, const MethodList & overrides);
  bool canOverride(const FunctionDefn * base, const FunctionDefn * func);

  bool createDefaultConstructor();
};

}

#endif

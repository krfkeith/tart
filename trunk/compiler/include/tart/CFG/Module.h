/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_MODULE_H
#define TART_CFG_MODULE_H

#ifndef TART_AST_DECL_H
#include "tart/AST/ASTDecl.h"
#endif

#ifndef TART_CFG_DECL_H
#include "tart/CFG/Defn.h"
#endif

#ifndef TART_COMMON_AGENDA_H
#include "tart/Common/Agenda.h"
#endif

#ifndef TART_COMMON_SOURCEFILE_H
#include "tart/Common/SourceFile.h"
#endif

#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"

namespace llvm {
class Module;
}

namespace tart {

class PackageMgr;
class Module;

typedef llvm::SetVector<Module *> ModuleSet;
typedef llvm::SetVector<Defn *> DefnSet;
typedef llvm::DenseMap<TypePair, FunctionDefn *, TypePair::KeyInfo> ConverterMap;

/// -------------------------------------------------------------------
/// A translation unit.
class Module : public Defn, public IterableScope {
public:
  typedef llvm::DenseSet<const Type *, Type::KeyInfo> TypeSet;

  enum ModuleFlags {
    Module_Debug = (1<<0),      // Print debug options
    Module_Reflect = (1<<1),    // Generate reflection metadata
  };

  /** Construct a new module at the top level. */
  Module(ProgramSource * src, const std::string & qual, Scope * builtinScope);

  /** Construct a builtin module. */
  Module(ProgramSource * src, const std::string & qual);

  /** List of import statements. */
  const ASTNodeList & imports() const { return imports_; }
  ASTNodeList & imports() { return imports_; }

  /** Return the set of modules that were imported into this module. Includes implicit imports. */
  const ModuleSet & importModules() const { return importModules_; }

  /** Add a dependency from this module to the module in which the given definition was defined. */
  void addModuleDependency(Defn * de);

  /** List of AST declarations defined in this module. */
  const ASTDeclList & astMembers() const { return decls_; }
  ASTDeclList & astMembers() { return decls_; }

  /** Get the qualified name of this module's package. */
  const std::string packageName() const;

  /** The 'main' function for this module. */
  FunctionDefn * entryPoint() const { return entryPoint_; }
  void setEntryPoint(FunctionDefn * value) { entryPoint_ = value; }

  /** The 'programStart' function which calls 'main'. */
  FunctionDefn * programStartup() const { return programStartup_; }
  void setProgramStartup(FunctionDefn * value) { programStartup_ = value; }

  /** Return the definition corresponding to the primary symbol in this module. */
  Defn * primaryDefn() const;
  bool findPrimaryDefn();

  /** Get the set of defns which will be generated. */
  DefnSet & exportDefs() { return exportDefs_; }

  /** Get the set of defns which are referenced from this module. */
  DefnSet & importDefs() { return importDefs_; }

  /** Get the set of defns for which reflection data is generated. */
  DefnSet & reflectedDefs() { return reflectedDefs_; }

  /** Get the set of defns for which reflection data is generated. */
  TypeSet & reflectedTypes() { return reflectedTypes_; }

  /** Import this symbol into this module. If the symbol is from another module, add it
      to the list of imported symbols. If it is from this module, or if it is synthetic, then
      add it to the list of exported symbols. Also, add the symbol to the queue of symbols to
      be analyzed.

      Returns true if the symbol was not already added.
  */
  bool addSymbol(Defn * de);

  /** Return the next xref that has not been analyzed. */
  Defn * nextDefToAnalyze();

  /** Return the map of functions that perform type conversions. */
  const ConverterMap & converters() const { return converters_; }
  ConverterMap & converters() { return converters_; }

  /** Attempt to import a module by name. Returns the set of primary definitions for that module. */
  bool import(const char * qname, DefnList & defs, bool absPath);

  /** Process all import statements by adding an explicit import reference
      for each import into this module's symbol table. */
  bool processImportStatements();

  /** The source file for this module. */
  ProgramSource * moduleSource() const { return moduleSource_; }

  /** Remove all definitions from the module. */
  void clearDefns() {
    IterableScope::clear();
    decls_.clear();
    finished_.remove(Pass_ResolveModuleMembers);
  }

  bool isDebug() const { return (flags_ & Module_Debug) != 0; }
  bool isReflectionEnabled() const { return (flags_ & Module_Reflect) != 0; }

  llvm::Module * irModule();

  // Overrides

  Scope * definingScope() const { return IterableScope::parentScope(); }
  void setDefiningScope(Scope * scope) { IterableScope::setParentScope(scope); }
  bool lookupMember(const char * name, DefnList & defs, bool inherit) const;
  void format(FormatStream & out) const;
  void trace() const;

  static inline bool classof(const Module *) { return true; }
  static inline bool classof(const Defn * de) {
    return de->defnType() == Mod;
  }

private:
  friend class tart::PackageMgr;

  ProgramSource * moduleSource_;
  ASTNodeList imports_;
  ModuleSet importModules_;
  ASTDeclList decls_;
  DefnList primaryDefs_;
  DefnSet exportDefs_;
  DefnSet importDefs_;
  DefnSet reflectedDefs_;
  TypeSet reflectedTypes_;
  DefnList initDefs_;
  Agenda<Defn> defsToAnalyze_;
  FunctionDefn * entryPoint_;
  FunctionDefn * programStartup_;
  ConverterMap converters_;
  short flags_;

  // The LLVM module
  llvm::Module * irModule_;
};

}

#endif

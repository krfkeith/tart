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

#ifndef TART_COMMON_SOURCEFILE_H
#include "tart/Common/SourceFile.h"
#endif

#include <llvm/ADT/SetVector.h>

namespace llvm {
class Module;
}

namespace tart {

class PackageMgr;
class Module;

typedef llvm::SetVector<Module *> ModuleSet;
typedef llvm::SetVector<Defn *> DefnSet;
  
/// -------------------------------------------------------------------
/// A translation unit.
class Module : public Defn, public IterableScope {
private:
  friend class tart::PackageMgr;

  ProgramSource * moduleSource_;
  ASTNodeList imports_;
  ModuleSet importModules;
  ASTDeclList decls;
  DefnList primaryDefs;
  DefnSet xdefs;
  DefnSet xrefs;
  DefnList xrefsToAnalyze;
  size_t xrefsAnalyzed;
  FunctionDefn * entryPoint_;

  // The LLVM module
  llvm::Module * irModule_;

public:
  /** Construct a new module at the top level. */
  Module(ProgramSource * src, const std::string & qual, Scope * builtinScope);

  /** List of import statements. */
  const ASTNodeList & imports() const { return imports_; }
  ASTNodeList & imports() { return imports_; }

  /** List of AST declarations defined in this module. */
  const ASTDeclList & getASTMembers() const { return decls; }
  ASTDeclList & getASTMembers() { return decls; }
  
  /** Get the qualified name of this module's package. */
  const std::string packageName() const;

  /** The 'main' function for this module. */
  FunctionDefn * entryPoint() const { return entryPoint_; }
  void setEntryPoint(FunctionDefn * value) { entryPoint_ = value; }

  /** Return the definition corresponding to the primary symbol in this module. */
  Defn * primaryDefn() const;
  
  /** Get the set of decls which will be generated. */
  DefnSet & getXDefs() { return xdefs; }

  /** Add to the list of symbols to be emitted for this module. */
  bool addXDef(Defn * de);

  /** Get the set of decls which are referenced from this module. */
  DefnSet & getXRefs() { return xrefs; }

  /** Add a symbol which is referenced by this module. */
  bool addXRef(Defn * de);

  /** Return the next xref that has not been analyzed. */
  Defn * getNextXRefToAnalyze();

  /** Attempt to import a module by name. Returns the set of primary
      definitions for that module. */
  bool import(const char * qname, DefnList & defs);

  /** Process all import statements by adding an explicit import reference
      for each import into this module's symbol table. */
  bool processImportStatements();

  /** The source file for this module. */
  ProgramSource * moduleSource() const { return moduleSource_; }

  llvm::Module * getIRModule();

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
};

}

#endif

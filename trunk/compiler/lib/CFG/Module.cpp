/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Module.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Common/InternedString.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"
#include <llvm/Module.h>

namespace tart {

Module::Module(ProgramSource * src, const std::string & qual,
  Scope * builtinScope)
  : Defn(Mod, this, "<module>")
  , IterableScope(builtinScope)
  , moduleSource_(src)
  , irModule_(NULL)
{
  loc.file = src;
  qname_.assign(qual);
  addTrait(Singular);
  xrefsAnalyzed = 0;
  setScopeName(istrings.intern(qual));
}

const std::string Module::getPackageName() const {
  std::string result(qname_);
  size_t dot = result.rfind('.');
  if (dot == result.npos) {
    result.clear();
  } else {
    result.erase(dot, result.npos);
  }

  return result;
}

bool Module::import(const char * name, DefnList & defs) {
  Module * mod = PackageMgr::get().getModuleForImportPath(name);
  if (mod == NULL) {
    // Try our own package
    std::string packageName(qname_);
    size_t dot = packageName.rfind('.');
    if (dot != packageName.npos) {
      packageName.erase(dot, packageName.npos);
    }

    std::string importName(packageName);
    importName.push_back('.');
    importName.append(name);

    mod = PackageMgr::get().getModuleForImportPath(importName);
    if (mod == NULL && packageName != "tart.core") {
      // Try tart.core
      importName.assign("tart.core.");
      importName.append(name);
      mod = PackageMgr::get().getModuleForImportPath(importName);
    }
  }

  if (mod != NULL) {
    importModules.insert(mod);
    if (!mod->primaryDefs.empty()) {
      defs.append(mod->primaryDefs.begin(), mod->primaryDefs.end());
      return true;
    }

    diag.warn(SourceLocation()) << "Module '" << name <<
        "' has no primary defs?";
  }

  return false;
}

Defn * Module::getPrimaryDefn() const {
  Defn * result = NULL;
  std::string moduleName(getPackageName());
  if (!primaryDefs.empty()) {
    for (DefnList::const_iterator it = primaryDefs.begin();
        it != primaryDefs.end(); ++it) {
      Defn * def = *it;
      if (def->visibility() != Private) {
        if (result != NULL) {
          // TODO: This is incorrect - multiple definitions of some
          // types *are* allowed.
          diag.fatal(def) << "Multiple definitions of '" <<
              def->getName() << "'";
        } else {
          result = def;
        }
      }
    }
  }

  return result;
}

bool Module::processImportStatements() {
  // If not already done so, add the list of imported symbols to the
  // module's namespace.
  if (beginPass(Pass_ResolveImport)) {
    DefnAnalyzer da(this, this);
    for (ASTNodeList::const_iterator it = imports.begin();
        it != imports.end(); ++it) {
      da.importIntoScope(cast<ASTImport>(*it), this);
    }
    
    finishPass(Pass_ResolveImport);
  }
  
  return true;
}

bool Module::lookupMember(const char * name, DefnList & defs,
    bool inherit) const {
  
  if (!imports.empty() && !isPassFinished(Pass_ResolveImport)) {
    const_cast<Module *>(this)->processImportStatements();
  }
  
  return IterableScope::lookupMember(name, defs, inherit);
}

llvm::Module * Module::getIRModule() {
  if (irModule_ == NULL) {
    irModule_ = new llvm::Module(qname_);
  }
  return irModule_;
}

bool Module::addXDef(Defn * de) {
  if (xdefs.insert(de)) {
    if (!xrefs.count(de)) {
      xrefsToAnalyze.push_back(de);
    }
    return true;
  }
  
  return false;
}

bool Module::addXRef(Defn * de) {
  DASSERT_OBJ(de->isSingular(), de);
  if (de->module() != this) {
    if (xrefs.insert(de)) {
      if (!xdefs.count(de)) {
        xrefsToAnalyze.push_back(de);
      }
      return true;
    }
  }
  
  return false;
}

Defn * Module::getNextXRefToAnalyze() {
  if (xrefsAnalyzed < xrefsToAnalyze.size()) {
    return xrefsToAnalyze[xrefsAnalyzed++];
  }
  
  return NULL;
}

void Module::format(FormatStream & out) const {
  out << moduleSource_->getFilePath();
}

void Module::trace() const {
  Defn::trace();
  IterableScope::trace();
  safeMark(moduleSource_);
  markList(decls.begin(), decls.end());
  markList(primaryDefs.begin(), primaryDefs.end());
  
  //ModuleSet importModules;
  //DefnSet xdefs;
  //DefnSet xrefs;
  //DefnList xrefsToAnalyze;
}

}

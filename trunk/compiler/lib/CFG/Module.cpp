/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Module.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Common/InternedString.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"
#include <llvm/Module.h>
#include <llvm/Support/CommandLine.h>

static llvm::cl::opt<std::string>
DebugXDefs("debug-xdefs",
    llvm::cl::desc("Debug xdefs for module"), llvm::cl::value_desc("filename"));

namespace tart {

Module::Module(ProgramSource * src, const std::string & qual, Scope * builtinScope)
  : Defn(Mod, this, "<module>")
  , IterableScope(builtinScope)
  , moduleSource_(src)
  , irModule_(NULL)
  , entryPoint_(NULL)
  , debug_(false)
{
  loc.file = src;
  qname_.assign(qual);
  addTrait(Singular);
  defsAnalyzed_ = 0;
  setScopeName(istrings.intern(qual));

  debug_ = (DebugXDefs == qual);
}

const std::string Module::packageName() const {
  std::string result(qname_);
  size_t dot = result.rfind('.');
  if (dot == result.npos) {
    result.clear();
  } else {
    result.erase(dot, result.npos);
  }

  return result;
}


bool Module::import(const char * name, DefnList & defs, bool absPath) {
  Module * mod = PackageMgr::get().getModuleForImportPath(name);
  if (mod == NULL && !absPath) {
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
    importModules_.insert(mod);
    if (!mod->primaryDefs_.empty()) {
      defs.append(mod->primaryDefs_.begin(), mod->primaryDefs_.end());
      return true;
    }

    diag.warn(SourceLocation()) << "Module '" << name << "' has no primary defs?";
  }

  return false;
}

Defn * Module::primaryDefn() const {
  Defn * result = NULL;
  std::string moduleName(packageName());
  if (!primaryDefs_.empty()) {
    for (DefnList::const_iterator it = primaryDefs_.begin(); it != primaryDefs_.end(); ++it) {
      Defn * def = *it;
      if (def->visibility() != Private) {
        if (result != NULL) {
          // TODO: This is incorrect - multiple definitions of some
          // types *are* allowed.
          diag.fatal(def) << "Multiple definitions of '" << def->name() << "'";
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
    for (ASTNodeList::const_iterator it = imports_.begin(); it != imports_.end(); ++it) {
      da.importIntoScope(cast<ASTImport>(*it), this);
    }

    finishPass(Pass_ResolveImport);
  }

  return true;
}

bool Module::lookupMember(const char * name, DefnList & defs, bool inherit) const {
  if (!imports_.empty() && !isPassFinished(Pass_ResolveImport)) {
    const_cast<Module *>(this)->processImportStatements();
  }

  return IterableScope::lookupMember(name, defs, inherit);
}

llvm::Module * Module::irModule() {
  if (irModule_ == NULL) {
    irModule_ = new llvm::Module(qname_, llvm::getGlobalContext());
  }
  return irModule_;
}

bool Module::addSymbol(Defn * de) {
  if (isPassFinished(Pass_ResolveModuleMembers)) {
    diag.fatal(de) << "Too late to add symbol '" << de << "', analysis for module '" << this
        << "' has already finished.";
  }

  if (de->defnType() == Defn::ExplicitImport) {
    return false;
  }

  DASSERT_OBJ(de->isSingular(), de);
  if (de->module() == this || de->isSynthetic()) {
    if (exportDefs_.insert(de)) {
      DASSERT_OBJ(!importDefs_.count(de), de);
      defsToAnalyze_.push_back(de);
      if (debug_) {
        diag.info() << Format_Type << Format_QualifiedName << "Export: " << de->linkageName();
      }
      return true;
    }
  } else {
    if (importDefs_.insert(de)) {
      DASSERT_OBJ(!exportDefs_.count(de), de);
      defsToAnalyze_.push_back(de);
      if (debug_) {
        diag.info() << Format_Type << Format_QualifiedName << "Import: " << de->linkageName();
      }
      return true;
    }
  }

  return false;
}

Defn * Module::nextDefToAnalyze() {
  if (defsAnalyzed_ < defsToAnalyze_.size()) {
    return defsToAnalyze_[defsAnalyzed_++];
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
  markList(decls_.begin(), decls_.end());
  markList(primaryDefs_.begin(), primaryDefs_.end());
}

}

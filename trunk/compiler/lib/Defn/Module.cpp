/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Module.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Template.h"

#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/ScopeBuilder.h"

#include "tart/Common/InternedString.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"

#include "tart/Meta/MDReader.h"

#include "llvm/Module.h"
#include "llvm/Support/CommandLine.h"

static llvm::cl::opt<std::string>
DebugXDefs("debug-xdefs",
    llvm::cl::desc("Debug xdefs for module"), llvm::cl::value_desc("filename"));

llvm::cl::opt<bool>
NoReflect("noreflect", llvm::cl::desc("Don't generate reflection data"));

namespace tart {

Module::Module(llvm::StringRef qual, Scope * builtinScope)
  : Defn(Mod, this, "<module>")
  , IterableScope(builtinScope)
  , moduleSource_(NULL)
  , entryPoint_(NULL)
  , programStartup_(NULL)
  , flags_(Module_Reflect)
  , timestamp_(0, 0)
  , irModule_(NULL)
{
  loc.file = NULL;
  setQualifiedName(qual);
  addTrait(Singular);
  setScopeName(istrings.intern(qual));

  if (DebugXDefs == qual) {
    flags_ |= Module_Debug;
  }

  if (NoReflect) {
    flags_ &= ~Module_Reflect;
  }
}

Module::Module(ProgramSource * src, llvm::StringRef qual)
  : Defn(Mod, this, "<module>")
  , IterableScope(NULL)
  , moduleSource_(src)
  , entryPoint_(NULL)
  , programStartup_(NULL)
  , flags_(Module_Reflect)
  , timestamp_(0, 0)
  , irModule_(NULL)
{
  loc.file = src;
  setQualifiedName(qual);
  addTrait(Singular);
  setScopeName(istrings.intern(qual));
}

void Module::setQualifiedName(llvm::StringRef qual) {
  qname_ = qual;
  size_t dot = qual.rfind('.');
  if (dot == qual.npos) {
    name_ = istrings.intern(qual);
    packageName_.clear();
  } else {
    name_ = istrings.intern(qual.substr(dot + 1, qual.npos));
    packageName_ = qual.substr(0, dot);
  }
}

void Module::createMembers() {
  if (passes_.begin(ScopeCreationPass)) {
    ScopeBuilder::createScopeMembers(this);

    // Look for the primary declaration. This is the one with the same name as the module.
    if (!findPrimaryDefn()) {
      IterableScope::dump();
      diag.fatal() << "No primary symbol found in module '" << qualifiedName() << "'";
    }

    passes_.finish(ScopeCreationPass);
  }
}

void Module::addModuleDependency(Defn * de) {
  if (de != NULL) {
    Module * m = de->module();
    if (de->isSynthetic()) {
      if (de->isTemplateInstance()) {
        m = de->templateInstance()->templateDefn()->module();
      }
    }

    if (m != NULL && m != this) {
      importModules_.insert(m);
    }
  }
}

bool Module::import(llvm::StringRef name, DefnList & defs, bool absPath) {
  Module * mod = PackageMgr::get().loadModule(name);
  if (mod == NULL && !absPath) {
    // Try our own package
    llvm::SmallString<128> importName(packageName_);
    importName.push_back('.');
    importName += name;

    mod = PackageMgr::get().loadModule(importName);
    if (mod == NULL && packageName_ != "tart.core") {
      // Try tart.core
      importName = "tart.core.";
      importName += name;
      mod = PackageMgr::get().loadModule(importName);
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

bool Module::findPrimaryDefn() {
  llvm::StringRef primaryName = qname_;
  size_t dot = primaryName.rfind('.');
  if (dot != primaryName.npos) {
    primaryName = primaryName.substr(dot + 1, primaryName.npos);
  }

  return IterableScope::lookupMember(primaryName, primaryDefs_, false);
}

Defn * Module::primaryDefn() const {
  Defn * result = NULL;
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
  if (passes_.begin(ResolveImportsPass)) {
    DefnAnalyzer da(this, this, this, NULL);
    for (ASTNodeList::const_iterator it = imports_.begin(); it != imports_.end(); ++it) {
      da.importIntoScope(cast<ASTImport>(*it), this);
    }

    passes_.finish(ResolveImportsPass);
  }

  return true;
}

bool Module::lookupMember(llvm::StringRef name, DefnList & defs, bool inherit) const {
  if (!imports_.empty() && !passes_.isFinished(ResolveImportsPass)) {
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
  if (passes_.isFinished(CompletionPass)) {
    // It's ok to add it if it was already added.
    if (de->module() == this || de->isSynthetic()) {
      if (exportDefs_.count(de)) {
        return false;
      }
    } else {
      if (importDefs_.count(de)) {
        return false;
      }
    }
    diag.fatal(de) << Format_Verbose << "Too late to add symbol '" << de <<
        "', analysis for module '" << this << "' has already finished.";
  }

  if (de->defnType() == Defn::ExplicitImport) {
    return false;
  }

  DASSERT_OBJ(de->isSingular(), de);
  if (de->module() == this || de->isSynthetic()) {
    if (exportDefs_.insert(de)) {
      DASSERT_OBJ(!importDefs_.count(de), de);
      queueSymbol(de);
      if (isDebug()) {
        diag.info() << Format_Type << Format_QualifiedName << "Export: " << de;
      }
      return true;
    }
  } else {
    if (importDefs_.insert(de)) {
      DASSERT_OBJ(!exportDefs_.count(de), de);
      queueSymbol(de);
      if (isDebug()) {
        diag.info() << Format_Type << Format_QualifiedName << "Import: " << de;
      }
      return true;
    }
  }

  return false;
}

/** Add a symbol to the set of definitions to be reflected from this module. */
bool Module::reflect(Defn * de) {
  DASSERT(de != NULL);

  if (passes_.isFinished(CompletionPass)) {
    diag.fatal(de) << Format_Verbose << "Too late to reflect symbol '" << de <<
        "', analysis for module '" << this << "' has already finished.";
  }

  if (de->module() == this || de->isSynthetic()) {
    addSymbol(de);
    return reflectedDefs_.insert(de);
  } else {
    return false;
  }
}

void Module::queueSymbol(Defn * de) {
  defsToAnalyze_.append(de);
}

Defn * Module::nextDefToAnalyze() {
  if (!defsToAnalyze_.empty()) {
    return defsToAnalyze_.next();
  }

  return NULL;
}

void Module::format(FormatStream & out) const {
  out << moduleSource_->filePath();
}

void Module::trace() const {
  Defn::trace();
  IterableScope::trace();
  safeMark(moduleSource_);
  markList(decls_.begin(), decls_.end());
  markList(primaryDefs_.begin(), primaryDefs_.end());
  markList(imports_.begin(), imports_.end());
}

}

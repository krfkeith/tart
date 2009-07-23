/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Common/PackageMgr.h"

#include "tart/CFG/Module.h"
#include "tart/Parse/Parser.h"
#include "tart/Common/SourceFile.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"
#include "tart/Sema/ScopeBuilder.h"
#include <llvm/System/Path.h>
#include <llvm/Support/CommandLine.h>

namespace tart {
  
using tart::Module;

PackageMgr PackageMgr::instance;

static llvm::cl::opt<bool>
ShowImports("show-imports", llvm::cl::desc("Display imports"));

/// -------------------------------------------------------------------
/// A program unit.
Module * PackageMgr::getCachedModule(const std::string & qname) {
  // First, attempt to search the modules already loaded.
  ModuleMap::iterator it = modules.find(qname);
  if (it != modules.end()) {
    return it->second;
  }

  return NULL;
}

/// -------------------------------------------------------------------
/// A program unit.
Module * PackageMgr::getModuleForImportPath(const std::string & qname) {
  // First, attempt to search the modules already loaded.
  ModuleMap::iterator it = modules.find(qname);
  if (it != modules.end()) {
    if (ShowImports && it->second != NULL) {
      diag.debug() << "Import: Found module '" << qname << "' in module cache";
    }

    return it->second;
  }

  // Transform dots into path separators.
  for (PathList::iterator it = importPaths.begin(); it != importPaths.end(); ++it) {
    std::string filePath;
    filePath.reserve(it->size() + qname.size() + 1);
    filePath.append(*it);
    filePath.push_back('/');
    for (std::string::const_iterator ch = qname.begin(); ch != qname.end(); ++ch) {
      if (*ch == '.') {
        filePath.push_back('/');
      } else {
        filePath.push_back(*ch);
      }
    }

    filePath.append(".tart");
    llvm::sys::PathWithStatus path(filePath);
    if (path.canRead()) {
      // TODO: We should get the timestamp and store them in the
      // module, along with the file size and other info.

      // If it's a regular file.
      Module * mod = new Module(new SourceFile(path.toString()), qname,
          &Builtins::module);
      modules[qname] = mod;

      if (ShowImports) {
        diag.debug("Import: Loaded module '%s' from %s",
            qname.c_str(), path.c_str());
      }

      Parser parser(mod->moduleSource(), mod);
      if (parser.parse()) {
        // Look for the primary declaration. This is the one
        // with the same name as the module.
        std::string primaryName(qname);
        size_t dot = primaryName.rfind('.');
        if (dot != primaryName.npos) {
          primaryName.erase(0, dot + 1);
        }

        ScopeBuilder::createScopeMembers(mod);
        if (!mod->lookupMember(primaryName.c_str(), mod->primaryDefs, false)) {
          diag.fatal(SourceLocation()) << "No symbol '" << primaryName <<
            "' found in module '" << mod->qname_ << "'";
        }
      } else {
        return NULL;
      }

      return mod;
    }
  }

  // Add the fact that lookup failed to a 'miss set' so we don't have to
  // query the file system again.
  modules[qname] = NULL;
  return NULL;
}

void PackageMgr::addModule(Module * mod) {
  assert(modules.find(mod->getQualifiedName()) == modules.end());
  modules[mod->getQualifiedName()] = mod;
}

void PackageMgr::trace() {
  for (ModuleMap::const_iterator it = modules.begin(); it != modules.end(); ++it) {
    if (it->second != NULL) {
      it->second->mark();
    }
  }
}

}

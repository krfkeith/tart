/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "config.h"

#include "tart/Common/PackageMgr.h"
#include "tart/Common/SourceFile.h"
#include "tart/Common/Diagnostics.h"

#include "tart/Defn/Module.h"

#include "tart/Parse/Parser.h"

#include "tart/Objects/Builtins.h"

#include "tart/Sema/ScopeBuilder.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TimeValue.h"

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace tart {

using tart::Module;
using namespace llvm;
using namespace llvm::sys;

PackageMgr PackageMgr::instance_;

static cl::opt<bool>
ShowImports("show-imports", cl::desc("Display imports"));

namespace {
  llvm::sys::TimeValue filetime(llvm::StringRef path) {
    TimeValue result(0, 0);
#if HAVE_STAT
    struct stat st;
    llvm::SmallString<128> pathbuffer(path);
    if (stat(pathbuffer.c_str(), &st) == 0) {
      result.fromEpochTime(st.st_mtime);
    } else {
      diag.error() << "Error accessing file: " << path;
    }
    return result;
#else
  #error("Missing implementation for file last modified date.");
#endif
  }
}

// -------------------------------------------------------------------
// DirectoryImporter

bool DirectoryImporter::load(llvm::StringRef qualName, Module *& module) {
  // Transform dots into path separators to get the relative path.
  SmallString<128> relpath;
  for (StringRef::const_iterator ch = qualName.begin(); ch != qualName.end(); ++ch) {
    if (*ch == '.') {
      relpath.push_back('/');
    } else {
      relpath.push_back(*ch);
    }
  }

  // Transform dots into path separators.
  SmallString<128> filepath(path_);
  path::append(filepath, Twine(relpath));
  path::replace_extension(filepath, ".tart");
  bool exists = false;

  // Check for source file
  if (fs::exists(Twine(filepath), exists) == errc::success && exists) {
    // Get the file timestamp.
    llvm::sys::TimeValue timestamp = filetime(filepath);
    // TODO: We should get the timestamp and store them in the
    // module, along with the file size and other info.

    // If we haven't already found a bitcode file for this module.
    if (module == NULL) {
      module = new Module(qualName, &Builtins::module);
    } else if (module->timestamp() > timestamp) {
      diag.debug() << "Import: Module '" << qualName << "' exists, and is newer than timestamp";
    } else {
      diag.debug() << "Import: Module '" << qualName << "' exists, and is older than timestamp";
    }

    module->setModuleSource(new SourceFile(filepath));
    if (ShowImports) {
      diag.debug() << "Import: Found source module '" << qualName << "' at " << filepath;
    }

    Parser parser(module->moduleSource(), module);
    if (parser.parse()) {
      // Look for the primary declaration. This is the one with the same name as the module.
      ScopeBuilder::createScopeMembers(module);
      if (!module->findPrimaryDefn()) {
        diag.fatal() << "No primary symbol found in module '" << qualName << "'";
      }

      return true;
    } else {
      delete module;
      module = NULL;
      return true;
    }
  }

  // If we haven't found a module yet, check for bitcode file
  if (module == NULL) {
    path::replace_extension(filepath, ".bc");
    if (fs::exists(Twine(filepath), exists) == errc::success && exists) {
      module = new Module(qualName, &Builtins::module);
      module->timestamp() == filetime(filepath);
      // Load the bitcode file.
      if (ShowImports) {
        diag.debug() << "Import: Found bitcode module '" << qualName << "' at " << filepath;
      }
      DFAIL("Implewent");
      return true;
    }

    if (ShowImports) {
      diag.debug() << "Import: Didn't find module '" << qualName << "' at " << filepath;
    }
  }

  return false;
}

// -------------------------------------------------------------------
// PackageMgr

void PackageMgr::addImportPath(llvm::StringRef path) {
  bool success = false;
  if (fs::is_directory(path, success) == errc::success && success) {
    importPaths_.push_back(new DirectoryImporter(path));
  } else {
    diag.error() << "Unsupported path type: " << path;
  }
}

Module * PackageMgr::getCachedModule(llvm::StringRef qname) {
  // First, attempt to search the modules already loaded.
  ModuleMap::iterator it = modules_.find(qname);
  if (it != modules_.end()) {
    return it->second;
  }

  return NULL;
}

Module * PackageMgr::getModuleForImportPath(llvm::StringRef qname) {
  // First, attempt to search the modules already loaded.
  ModuleMap::iterator it = modules_.find(qname);
  if (it != modules_.end()) {
    if (ShowImports && it->second != NULL) {
      diag.debug() << "Import: Found module '" << qname << "' in module cache";
    }

    return it->second;
  }

  // If it's a regular file.
  Module * mod = NULL;

  // Search each element on the import path list.
  for (PathList::iterator it = importPaths_.begin(); it != importPaths_.end(); ++it) {
    Importer * imp = *it;
    if (imp->load(qname, mod)) {
      // If the module failed to load (NULL), or we found a module with source, then we're done
      if (mod == NULL || mod->moduleSource() != NULL) {
        break;
      }
    }
  }

  if (!mod && ShowImports) {
    diag.debug() << "Import: module '" << qname << "' NOT FOUND";
  }

  // Regardless of whether the module was found, record this result in the map
  // so we don't have to look it up again.
  modules_[qname] = mod;
  return mod;
}

void PackageMgr::addModule(Module * mod) {
  assert(modules_.find(mod->qualifiedName()) == modules_.end());
  modules_[mod->qualifiedName()] = mod;
}

void PackageMgr::trace() {
  for (ModuleMap::const_iterator it = modules_.begin(); it != modules_.end(); ++it) {
    if (it->second != NULL) {
      it->second->mark();
    }
  }
}

}

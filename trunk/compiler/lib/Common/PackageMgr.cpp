/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "config.h"

#include "tart/Common/PackageMgr.h"
#include "tart/Common/SourceFile.h"
#include "tart/Common/Diagnostics.h"

#include "tart/Defn/Module.h"

#include "tart/Parse/Parser.h"

#include "tart/Meta/MDReader.h"

#include "tart/Objects/Builtins.h"

#include "llvm/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TimeValue.h"
#include "llvm/Support/IRReader.h"

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
  llvm::sys::TimeValue filetime(StringRef path) {
    TimeValue result(0, 0);
#if HAVE_STAT
    struct stat st;
    SmallString<128> pathbuffer(path);
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

bool DirectoryImporter::load(StringRef qualName, Module *& module) {
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

  // Check for source file
  bool exists = false;
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
      return true;
    } else {
      delete module;
      module = NULL;
      return true;
    }
  }

#if 0
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
#endif

  return false;
}

// -------------------------------------------------------------------
// ArchiveImporter

bool ArchiveImporter::load(StringRef qualifiedName, Module *& module) {
  if (!valid_) {
    return false;
  }

  if (archive_ == NULL) {
    SMDiagnostic smErr;
    archive_ = ParseIRFile(path_.str(), smErr, getGlobalContext());
    if (archive_ == NULL) {
      bool exists = false;
      if (fs::exists(path_.str(), exists) == errc::success && exists) {
        diag.error() << "Cannot load library " << path_;
        diag.info() << smErr.getMessage();
      }
      valid_ = false;
      return false;
    }

    archiveSource_ = new ArchiveFile(path_);
  }

  Twine mdName("tart.xdef.");
  NamedMDNode * md = archive_->getNamedMetadata(mdName.concat(qualifiedName));
  if (md != NULL) {
    // Convert the qualified name of the module to a relative path.
    SmallString<128> relpath(path_);
    relpath.reserve(path_.size() + qualifiedName.size() + 1);
    relpath.push_back('#');
    for (size_t i = 0; i < qualifiedName.size(); ++i) {
      if (qualifiedName[i] == '.') {
        relpath.push_back('/');
      } else {
        relpath.push_back(qualifiedName[i]);
      }
    }

    // Create the module and read the header.
    module = new Module(qualifiedName, &Builtins::module);
    module->setModuleSource(new ArchiveEntry(relpath, archiveSource_));
    return MDReader(module, module).read(md);
  }

  return false;
}

// -------------------------------------------------------------------
// PackageMgr

void PackageMgr::addImportPath(StringRef path) {
  bool success = false;
  if (fs::is_directory(path, success) == errc::success && success) {
    importers_.push_back(new DirectoryImporter(path));
  } else if (fs::is_regular_file(path, success) == errc::success && success) {
    if (path::extension(path) == ".bc") {
      importers_.push_back(new ArchiveImporter(path));
    } else {
      diag.error() << "Unsupported path type: " << path;
      exit(-1);
    }
  } else {
    diag.error() << "Unsupported path type: " << path;
    exit(-1);
  }
}

Module * PackageMgr::getCachedModule(StringRef qname) {
  // First, attempt to search the modules already loaded.
  ModuleMap::iterator it = modules_.find(qname);
  if (it != modules_.end()) {
    return it->second;
  }

  return NULL;
}

Module * PackageMgr::loadModule(StringRef qname) {
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
  for (PathList::iterator it = importers_.begin(); it != importers_.end(); ++it) {
    Importer * imp = *it;
    if (imp->load(qname, mod)) {
      // If the module was there but failed to load (NULL) then we're done
      if (mod == NULL) {
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

  // Do this *after* we add the module to the map, because createMembers() might
  // do other module lookups.
  if (mod != NULL) {
    mod->createMembers();
  }
  return mod;
}

void PackageMgr::addModule(Module * mod) {
  assert(modules_.find(mod->qualifiedName()) == modules_.end());
  modules_[mod->qualifiedName()] = mod;
}

void PackageMgr::trace() const {
  for (ModuleMap::const_iterator it = modules_.begin(); it != modules_.end(); ++it) {
    if (it->second != NULL) {
      it->second->mark();
    }
  }

  GC::markList(importers_.begin(), importers_.end());
}

}

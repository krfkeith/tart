/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_PACKAGEMGR_H
#define TART_COMMON_PACKAGEMGR_H

#ifndef TART_DEFN_MODULE_H
#include "tart/Defn/Module.h"
#endif

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

#include <vector>
#include <list>

namespace tart {

class Module;
class Package;

/// -------------------------------------------------------------------
/// Represents a location where modules can be found.

class Importer : public GC {
public:
  virtual bool load(llvm::StringRef qualifiedName, Module *& module) = 0;
  virtual ~Importer() {}
};

/// -------------------------------------------------------------------
/// An import path which points to a directory in the filesystem.

class DirectoryImporter : public Importer {
public:
  DirectoryImporter(llvm::StringRef path) : path_(path) {}

  // Overrides

  bool load(llvm::StringRef qualifiedName, Module *& module);
  void trace() const {}

private:
  llvm::SmallString<128> path_;
};

/// -------------------------------------------------------------------
/// An import path which points to a library archive.

class ArchiveImporter : public Importer {
public:
  ArchiveImporter(llvm::StringRef path)
    : path_(path)
    , archive_(NULL)
    , archiveSource_(NULL)
    , valid_(true) {}

  // Overrides

  bool load(llvm::StringRef qualifiedName, Module *& module);
  void trace() const { safeMark(archiveSource_); }

private:
  llvm::SmallString<128> path_;
  llvm::Module * archive_;
  ProgramSource * archiveSource_;
  bool valid_;
};

/// -------------------------------------------------------------------
/// Keeps track of which modules have been compiled and where they are

class PackageMgr : public GCRootBase {
public:
  /** Add a path to the list of module search paths. The path can either be
      a directory, or a bitcode library file. */
  void addImportPath(llvm::StringRef path);

  /** Add a built-in module. */
  void addModule(Module * mod);

  /** Given a fully-qualified name to a symbol, load the module containing
      that symbol and return it.
  */
  Module * getModuleForImportPath(llvm::StringRef importPath);

  /** Get the module from the module cache using this exact name. */
  Module * getCachedModule(llvm::StringRef moduleName);

  /** Return the singleton instance. */
  static PackageMgr & get() { return instance_; }

  /** Garbage collection for modules. */
  void trace() const;

private:
  typedef llvm::StringMap<Module *> ModuleMap;
  typedef llvm::SmallVector<Importer *, 8> PathList;

  // Set of all modules, arranged by package
  ModuleMap modules_;

  // Set of directories to search for modules.
  PathList importers_;

  // The singleton instance.
  static PackageMgr instance_;
};

} // namespace tart

#endif // TART_COMMON_PACKAGEMGR_H

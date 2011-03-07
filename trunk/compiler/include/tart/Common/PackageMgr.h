/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_PACKAGEMGR_H
#define TART_COMMON_PACKAGEMGR_H

#ifndef TART_DEFN_MODULE_H
#include "tart/Defn/Module.h"
#endif

#include <llvm/ADT/StringMap.h>
#include <vector>
#include <list>

namespace tart {

class Module;
class Package;

/// -------------------------------------------------------------------
/// Container types
typedef llvm::StringMap<Module *> ModuleMap;
typedef std::list<std::string> PathList;

/// -------------------------------------------------------------------
/// Keeps track of which modules have been compiled and where they are
class PackageMgr {
private:
  // Set of all modules, arranged by package
  ModuleMap modules;

  // Set of directories to search for modules.
  PathList importPaths;

  // The singleton instance.
  static PackageMgr instance;

public:

  /** Add a path to the list of module search paths. */
  void addImportPath(const std::string & path) {
      importPaths.push_back(path);
  }

  /** Add a built-in module. */
  void addModule(Module * mod);

  /** Given a fully-qualified name to a symbol, load the module containing
      that symbol and return it. Optionally returns the number of
      prefix characters that were actually used to locate the module.
  */
  Module * getModuleForImportPath(const std::string & importPath);

  /** Get the module from the module cache using this exact name. */
  Module * getCachedModule(const std::string & moduleName);

  /** Get all modules in the specified package. */
  //bool getModulesInPackage(const std::string & qname, ModuleList & out);

  /** Return the singleton instance. */
  static PackageMgr & get() { return instance; }

  /** Garbage collection for modules. */
  void trace();
};

}

#endif

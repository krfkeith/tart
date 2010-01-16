/** LLVM pass to generate link-time reflection data. */

#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"

namespace tart {
using namespace llvm;

//typedef SmallPtrSet<GlobalVariable *, 16> ModuleSet;
typedef SmallVector<GlobalVariable *, 16> ModuleList;
typedef StringMap<GlobalVariable *> GlobalVarMap;

/** Describes a package. */
class Package {
public:

  Package(const StringRef & name) : name_(name) {}

  /** Package name. */
  const std::string & name() const { return name_; }

  /** Global variable for the package reflection object. */
  GlobalVariable * global() const { return global_; }
  void setGlobal(GlobalVariable * value) { global_ = value; }

  /** Add a module to this package. */
  void addModule(GlobalVariable * module) {
    modules_.push_back(module);
  }

private:
  std::string name_;
  GlobalVariable * global_;
  ModuleList modules_;
};

/** Map of package names to packages. */
typedef StringMap<Package *> PackageMap;

/** Reflector pass. */
class ReflectorPass : public ModulePass {
public:
  static char ID;

  ReflectorPass() : ModulePass(&ID) {}
  ~ReflectorPass();

  bool runOnModule(Module & module);
  Package * getOrCreatePackage(const StringRef & pkgName);
  Package * getPackage(const StringRef & pkgName);

private:
  GlobalVarMap modules_;
  PackageMap packages_;
  //GlobalVarMap packages_;
};

}

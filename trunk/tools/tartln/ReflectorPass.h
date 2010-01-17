/** LLVM pass to generate link-time reflection data. */

#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Target/TargetData.h"

#include "ConstantBuilder.h"

namespace tart {
using namespace llvm;

//typedef SmallPtrSet<GlobalVariable *, 16> ModuleSet;
typedef SmallVector<GlobalVariable *, 16> GlobalVarList;
typedef StringMap<GlobalVariable *> GlobalVarMap;

/** Describes a package. */
class Package {
public:

  Package(const StringRef & name)
    : name_(name)
    , global_(NULL)
  {}

  /** Package name. */
  const std::string & name() const { return name_; }

  /** Global variable for the package reflection object. */
  GlobalVariable * global() const { return global_; }
  void setGlobal(GlobalVariable * value) { global_ = value; }

  /** Add a module to this package. */
  void addModule(GlobalVariable * module) {
    modules_.push_back(module);
  }

  GlobalVarList & modules() { return modules_; }
  GlobalVarList & subpackages() { return subpackages_; }

private:
  std::string name_;
  GlobalVariable * global_;
  GlobalVarList modules_;
  GlobalVarList subpackages_;
};

/** Map of package names to packages. */
typedef StringMap<Package *> PackageMap;

/** Reflector pass. */
class ReflectorPass : public ModulePass {
public:
  static char ID;

  ReflectorPass()
    : ModulePass(&ID)
  {}

  ~ReflectorPass();

  bool runOnModule(Module & module);
  Package * getOrCreatePackage(const StringRef & pkgName);
  Package * getPackage(const StringRef & pkgName);

  Constant * createArray(
      Module & module,
      const ConstantList & elements,
      Constant * arrayTypeInfo);

private:
  GlobalVarMap modules_;
  PackageMap packages_;
  ConstantRef packageHeader_;
  ConstantRef moduleArrayPtr_;
  ConstantRef packageArrayPtr_;
  ConstantRef moduleArrayHeader_;
  ConstantRef packageArrayHeader_;
};

}

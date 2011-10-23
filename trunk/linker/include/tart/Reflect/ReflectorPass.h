/** LLVM pass to generate link-time reflection data. */

#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Target/TargetData.h"

#include "tart/Common/ConstantBuilder.h"

namespace tart {
using namespace llvm;

class Package;

//typedef SmallPtrSet<GlobalVariable *, 16> ModuleSet;
typedef SmallVector<GlobalVariable *, 16> GlobalVarList;
typedef StringMap<GlobalVariable *> GlobalVarMap;
typedef SmallVector<Package *, 4> PackageList;

/** Describes a package. */
class Package {
public:

  Package(const StringRef & name, GlobalVariable * global)
    : name_(name)
    , global_(global)
  {}

  /** Package name. */
  const std::string & name() const { return name_; }

  /** Global variable for the package reflection object. */
  GlobalVariable * global() const { return global_; }

  /** Add a module to this package. */
  void addModule(GlobalVariable * module) {
    modules_.push_back(module);
  }

  GlobalVarList & modules() { return modules_; }
  PackageList & subpackages() { return subpackages_; }

private:
  std::string name_;
  GlobalVariable * global_;
  GlobalVarList modules_;
  PackageList subpackages_;
};

/** Map of package names to packages. */
typedef StringMap<Package *> PackageMap;

/** Reflector pass. */
class ReflectorPass : public ModulePass {
public:
  static char ID;

  ReflectorPass()
    : ModulePass(ID)
  {}

  ~ReflectorPass();

  void getAnalysisUsage(AnalysisUsage & AU) const;

  bool runOnModule(Module & module);

  /** Return the package if it already exists, otherwise create it. */
  Package * getOrCreatePackage(const StringRef & pkgName, GlobalVariable * global);

  /** Return the package if it already exists, otherwise create it iff it's a descendant
      of an existing package. */
  Package * getOrCreateSubPackage(const StringRef & pkgName, Module * module);

  Package * createPackage(const StringRef & pkgName, GlobalVariable * global);
  Constant * createArray(
      Module & module,
      const ConstantList & elements,
      Constant * arrayTypeInfo,
      Type * arrayType,
      const StringRef & name);

  Constant * createString(
      Module & module,
      Constant * stringTypeInfo,
      Type * stringType,
      const StringRef & stringVal);

  GlobalVariable * requireGlobal(const StringRef & name, Module & module);

  Type * requireType(const StringRef & name, Module & module);

private:
  GlobalVarMap modules_;
  PackageMap packages_;
};

}

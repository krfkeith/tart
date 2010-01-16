/** LLVM pass to measure the size of reflection data. */

#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Support/raw_ostream.h"

#include "ReflectorPass.h"
#include "ConstantBuilder.h"

namespace tart {
using namespace llvm;

char ReflectorPass::ID = 0;

namespace {

RegisterPass<ReflectorPass> X(
    "reflector", "Generate reflection info",
    false /* Only looks at CFG */,
    false /* Analysis Pass */);
}

ReflectorPass::~ReflectorPass() {
  // TODO: Delete what we created.
}

bool ReflectorPass::runOnModule(llvm::Module & module) {
  using llvm::Module;

  Module::GlobalListType & globals = module.getGlobalList();
  for (Module::GlobalListType::iterator it = globals.begin(); it != globals.end(); ++it) {
    if (GlobalVariable * globalVar = dyn_cast<GlobalVariable>(it)) {
      if (globalVar->getName().startswith(".module")) {
        modules_[globalVar->getName()] = globalVar;
        //errs() << globalVar->getName() << "\n";
      } else if (globalVar->getName().startswith(".package")) {
        std::string packageName(globalVar->getName());
        packageName.erase(0, 9); // remove ".package."
        Package * p = getOrCreatePackage(packageName);
        p->setGlobal(globalVar);
        //errs() << packageName << "\n";
      }
    }
  }

  for (GlobalVarMap::iterator it = modules_.begin(); it != modules_.end(); ++it) {
    GlobalVariable * moduleVar = it->second;
    std::string packageName(moduleVar->getName());
    // remove everything after the last '.'
    packageName.erase(packageName.rfind('.'), packageName.npos);
    packageName.erase(0, 8); // remove ".module."

    Package * p = getPackage(packageName);
    if (p != NULL) {
      p->addModule(moduleVar);
    } else {
      // TODO: Look for ancestor package.
    }

    //GlobalVarMap::iterator pi = packages_.find(packageName);
    //if (pi == packages_.end()) {

    //} else {

    //}

    //errs() << "Module " << moduleVar->getName() << " in package " << packageName << "\n";
  }

  return false;
}

Package * ReflectorPass::getPackage(const StringRef & pkgName) {
  PackageMap::iterator it = packages_.find(pkgName);
  if (it != packages_.end()) {
    return it->second;
  }

  return NULL;
}

Package * ReflectorPass::getOrCreatePackage(const StringRef & pkgName) {
  PackageMap::iterator it = packages_.find(pkgName);
  if (it != packages_.end()) {
    return it->second;
  }

  Package * p = new Package(pkgName);
  packages_[pkgName] = p;
  return p;
}

/*
bool ReflectorPass::createArray(llvm::Module & module, Package * p) {
  Value vals[3];

  ConstantStruct::get(module.getContext(), &vals[0], 3);
}
*/
}

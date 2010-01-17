/** LLVM pass to measure the size of reflection data. */

#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Support/raw_ostream.h"

#include "ReflectorPass.h"

#include <algorithm>

namespace tart {
using namespace llvm;

char ReflectorPass::ID = 0;

namespace {

RegisterPass<ReflectorPass> X(
    "reflector", "Generate reflection info",
    false /* Only looks at CFG */,
    false /* Analysis Pass */);

struct ModuleNameComparator {
  bool operator()(GlobalVariable * m0, GlobalVariable * m1) {
    return m0->getName() < m1->getName();
  }
};
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

        if (packageHeader_.value() == NULL) {
          //errs() << packageName << "\n";
          Constant * init = globalVar->getInitializer();
          assert(init != NULL);
          packageHeader_ = ConstantRef(init).operand(0);
          moduleArrayPtr_ = ConstantRef(init).operand(2, 0);
          moduleArrayHeader_ = moduleArrayPtr_.operand(0);
          packageArrayPtr_ = ConstantRef(init).operand(3, 0);
          packageArrayHeader_ = packageArrayPtr_.operand(0);

//          moduleArrayPtr_.type()->dump(&module);
//          packageArrayPtr_.type()->dump(&module);
//          packageHeader_.type()->dump(&module);
//          moduleArrayHeader_.type()->dump(&module);
//          packageArrayHeader_.type()->dump(&module);

          assert(packageHeader_.type() == moduleArrayHeader_.type());
          assert(packageHeader_.type() == packageArrayHeader_.type());
        }
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
  }

  for (PackageMap::iterator it = packages_.begin(); it != packages_.end(); ++it) {
    Package * p = it->second;
    ConstantRef packageConst = p->global()->getInitializer();

    // List of modules in package
    ConstantList modules;
    std::sort(p->modules().begin(), p->modules().end(), ModuleNameComparator());
    for (GlobalVarList::iterator m = p->modules().begin(); m != p->modules().end(); ++m) {
      modules.push_back(*m);
    }

    Constant * moduleArray = packageConst.operand(2).value();
    if (modules.size() > 0) {
      Constant * moduleArrayVar = createArray(module, modules, moduleArrayHeader_.value());
      moduleArray = llvm::ConstantExpr::getPointerCast(moduleArrayVar, moduleArray->getType());
    }

    // List of subpackages in package
    ConstantList subpackages;
    std::sort(p->subpackages().begin(), p->subpackages().end(), ModuleNameComparator());
    for (GlobalVarList::iterator m = p->subpackages().begin(); m != p->subpackages().end(); ++m) {
      subpackages.push_back(*m);
    }

    if (subpackages.size() > 0) {
      // TODO: List of child packages.
    }

    ConstantBuilder cb(module);
    cb.addField(packageHeader_);            // Object header
    cb.addField(packageConst.operand(1));   // Package name
    cb.addField(moduleArray);               // Module array
    cb.addField(packageConst.operand(3));   // Package array
    p->global()->setInitializer(cb.buildStruct(packageConst.value()->getType()));
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

Constant * ReflectorPass::createArray(
    llvm::Module & module,
    const ConstantList & elements,
    Constant * arrayTypeInfo) {

  TargetData targetData(&module);
  const IntegerType * intptrType = targetData.getIntPtrType(module.getContext());
  const Type * elementType = elements.front()->getType();
  const ArrayType * elementArrayType = ArrayType::get(elementType, elements.size());
  ConstantInt * elementCount = ConstantInt::get(intptrType, elements.size(), false);

  ConstantBuilder builder(module);
  builder.addField(arrayTypeInfo);
  builder.addField(elementCount);
  builder.addField(ConstantArray::get(elementArrayType, elements));
  Constant * arrayStruct = builder.buildStruct();
  return new GlobalVariable(module, arrayStruct->getType(), true,
      GlobalValue::InternalLinkage, arrayStruct, ".modules");
}

}

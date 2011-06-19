/** LLVM pass to generate reflection data for modules and packages. */

#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Support/raw_ostream.h"

#include "tart/Reflect/ReflectorPass.h"

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

struct PackageNameComparator {
  bool operator()(Package * p0, Package * p1) {
    return p0->name() < p1->name();
  }
};

}

ReflectorPass::~ReflectorPass() {
  // TODO: Delete what we created.
}

void ReflectorPass::getAnalysisUsage(AnalysisUsage & AU) const {
  AU.setPreservesCFG();
}

bool ReflectorPass::runOnModule(Module & module) {
  using llvm::Module;

  Module::GlobalListType & globals = module.getGlobalList();
  for (Module::GlobalListType::iterator it = globals.begin(); it != globals.end(); ++it) {
    if (GlobalVariable * globalVar = dyn_cast<GlobalVariable>(it)) {
      if (globalVar->getName().startswith(".module")) {
        //errs() << globalVar->getName() << "\n";
        modules_[globalVar->getName()] = globalVar;
      } else if (globalVar->getName().startswith(".package")) {
        //errs() << globalVar->getName() << "\n";
        std::string packageName(globalVar->getName());
        packageName.erase(0, 9); // remove ".package."
        /*Package * p =*/ getOrCreatePackage(packageName, globalVar);
      }
    }
  }

  if (!packages_.empty()) {
    const Type * packageType = requireType("tart.reflect.Package", module);
    const Type * moduleArrayType = requireType("tart.reflect.Module[]", module);
    const Type * packageArrayType = requireType("tart.reflect.Package[]", module);
    const Type * stringType = requireType("tart.core.String", module);

    Constant * emptyModuleArray = requireGlobal("tart.reflect.Module[].emptyArray", module);
    Constant * emptyPackageArray = requireGlobal("tart.reflect.Package[].emptyArray", module);
    Constant * packageTypeInfo = requireGlobal("tart.reflect.Package.type.tib", module);
    Constant * packageArrayTypeInfo = requireGlobal("tart.reflect.Package[].type.tib", module);
    Constant * moduleArrayTypeInfo = requireGlobal("tart.reflect.Module[].type.tib", module);
    Constant * stringTypeInfo = requireGlobal("tart.core.String.type.tib", module);

    for (GlobalVarMap::iterator it = modules_.begin(); it != modules_.end(); ++it) {
      GlobalVariable * moduleVar = it->second;
      std::string packageName(moduleVar->getName());
      // remove everything after the last '.'
      packageName.erase(packageName.rfind('.'), packageName.npos);
      packageName.erase(0, 8); // remove ".module."

      Package * p = getOrCreateSubPackage(packageName, &module);
      if (p != NULL) {
        //errs() << "Package " << packageName << " found for module " << moduleVar->getName() << "\n";
        p->addModule(moduleVar);
      }
    }

    for (PackageMap::iterator it = packages_.begin(); it != packages_.end(); ++it) {
      Package * p = it->second;

      // Start construction of the updated package structure.

      ConstantBuilder cb(module);
      cb.addObjectHeader(packageTypeInfo);    // Object header
      //cb.addField(packageHeader_);            // Object header

      if (p->global()->hasInitializer()) {
        ConstantRef packageConst = p->global()->getInitializer();
        cb.addField(packageConst.operand(1));   // Package name
      } else {
        cb.addField(createString(module, stringTypeInfo, stringType, p->name()));
      }

      // List of modules in package

      if (!p->modules().empty()) {
        ConstantList modules;
        std::sort(p->modules().begin(), p->modules().end(), ModuleNameComparator());
        for (GlobalVarList::iterator m = p->modules().begin(); m != p->modules().end(); ++m) {
          modules.push_back(*m);
        }

        cb.addField(createArray(module, modules, moduleArrayTypeInfo, moduleArrayType,
            ".modules." + p->name()));
      } else {
        cb.addField(emptyModuleArray);
      }

      // List of subpackages in package

      if (!p->subpackages().empty()) {
        ConstantList subpackages;
        std::sort(p->subpackages().begin(), p->subpackages().end(), PackageNameComparator());
        for (PackageList::iterator m = p->subpackages().begin(); m != p->subpackages().end(); ++m) {
          Package * subPackage = *m;
          assert(subPackage->global() != NULL);
          subpackages.push_back(subPackage->global());
        }

        cb.addField(createArray(module, subpackages, packageArrayTypeInfo, packageArrayType,
            ".subpackages." + p->name()));
      } else {
        cb.addField(emptyPackageArray);
      }

      p->global()->setInitializer(cb.buildStruct(packageType));
    }
  }

  return false;
}

Package * ReflectorPass::getOrCreatePackage(const StringRef & pkgName, GlobalVariable * global) {
  PackageMap::iterator it = packages_.find(pkgName);
  if (it != packages_.end()) {
    return it->second;
  }

  return createPackage(pkgName, global);
}

Package * ReflectorPass::getOrCreateSubPackage(const StringRef & pkgName, Module * module) {
  PackageMap::iterator it = packages_.find(pkgName);
  if (it != packages_.end()) {
    return it->second;
  }

  // Try to find a parent package
  std::string parentPkgName(pkgName);
  std::string::size_type dot = parentPkgName.rfind('.');
  if (dot != parentPkgName.npos) {
    parentPkgName.erase(dot, parentPkgName.npos);
    Package * parent = getOrCreateSubPackage(parentPkgName, module);
    if (parent != NULL) {
      assert(parent->global() != NULL);
      GlobalVariable * global = new GlobalVariable(*module,
          parent->global()->getType()->getContainedType(0), true,
          GlobalValue::ExternalLinkage, NULL, ".package." + pkgName);
      Package * p = createPackage(pkgName, global);
      parent->subpackages().push_back(p);
      return p;
    }
  }

  return NULL;
}

Package * ReflectorPass::createPackage(const StringRef & pkgName, GlobalVariable * global) {
  Package * p = new Package(pkgName, global);
  packages_[pkgName] = p;
  return p;
}

Constant * ReflectorPass::createArray(
    Module & module,
    const ConstantList & elements,
    Constant * arrayTypeInfo,
    const Type * arrayType,
    const StringRef & name) {

  const Type * elementType = elements.front()->getType();
  const ArrayType * elementArrayType = ArrayType::get(elementType, elements.size());

  ConstantBuilder builder(module);
  builder.addObjectHeader(arrayTypeInfo);
  builder.addInt(elements.size());
  builder.addField(ConstantArray::get(elementArrayType, elements));
  Constant * arrayStruct = builder.buildStruct();
  GlobalVariable * arrayVar = new GlobalVariable(module, arrayStruct->getType(), true,
      GlobalValue::InternalLinkage, arrayStruct, name);

  return ConstantExpr::getPointerCast(arrayVar, arrayType->getPointerTo());
}

Constant * ReflectorPass::createString(
    Module & module,
    Constant * stringTypeInfo,
    const Type * stringType,
    const StringRef & stringVal) {

  // Types we'll need
  LLVMContext & context = module.getContext();
  const IntegerType * int32Type = IntegerType::getInt32Ty(context);
  const IntegerType * charType = IntegerType::getInt8Ty(context);
  const Type * charDataType = ArrayType::get(charType, 0);

  // Self-referential member values
  UndefValue * strDataStart = UndefValue::get(charDataType->getPointerTo());
  UndefValue * strSource = UndefValue::get(stringType->getPointerTo());

  ConstantBuilder builder(module);
  builder.addObjectHeader(stringTypeInfo);
  builder.addInt(stringVal.size());
  builder.addField(strSource);
  builder.addField(strDataStart);
  builder.addField(ConstantArray::get(context, stringVal, false));

  Constant * strStruct = builder.buildStruct();
  Constant * strConstant = ConstantExpr::getPointerCast(
      new GlobalVariable(module,
          strStruct->getType(), true, GlobalValue::InternalLinkage, strStruct, ".string"),
      stringType->getPointerTo());

  Constant * indices[2];
  indices[0] = ConstantInt::get(int32Type, 0, false);
  indices[1] = ConstantInt::get(int32Type, 4, false);

  strDataStart->replaceAllUsesWith(
      ConstantExpr::getInBoundsGetElementPtr(strConstant, indices, 2));
  strSource->replaceAllUsesWith(strConstant);

  return ConstantExpr::getPointerCast(strConstant, stringType->getPointerTo());
}

GlobalVariable * ReflectorPass::requireGlobal(const StringRef & name, Module & module) {
  GlobalVariable * global = module.getGlobalVariable(name, true);
  if (global == NULL) {
    errs() << "ReflectorPass: Required global variable " << name << " not found\n";
    abort();
  }

  return global;
}

const Type * ReflectorPass::requireType(const StringRef & name, Module & module) {
  const Type * type = module.getTypeByName(name);
  if (type == NULL) {
    errs() << "ReflectorPass: Required type " << name << " not found\n";
    abort();
  }

  return type;
}

}

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Module.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Objects/Builtins.h"
#include "tart/Common/PackageMgr.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Parse/Parser.h"

namespace tart {

namespace {
SourceFile builtinSource("");
}

Module Builtins::module(&builtinSource, "$builtin", NULL);
Module Builtins::syntheticModule(&builtinSource, "$synthetic", NULL);

Type * Builtins::typeTypeInfoBlock;
Type * Builtins::typeObject;
Type * Builtins::typeString;
Type * Builtins::typeArray;
Type * Builtins::typeRange;
Type * Builtins::typeThrowable;
Type * Builtins::typeUnwindException;
Type * Builtins::typeIterable;
Type * Builtins::typeIterator;

Type * Builtins::typeType;
Type * Builtins::typeClass;
Type * Builtins::typeStruct;
Type * Builtins::typeInterface;
Type * Builtins::typeEnum;

Type * Builtins::typeAttribute;
Type * Builtins::typeIntrinsicAttribute;

TypeAlias Builtins::typeAliasString(NULL);

FunctionDefn * Builtins::funcHasBase;
FunctionDefn * Builtins::funcTypecastError;

NamespaceDefn Builtins::nsOperator(&module, "Operators");

void Builtins::init() {
  // Initialize primitive types.
  for (PrimitiveType * ptype = PrimitiveType::primitiveTypeList; ptype != NULL;
      ptype = ptype->nextType()) {
    ptype->init();
    TypeDefn * de = ptype->typeDefn();
    de->setQualifiedName(de->name());
    if (!ptype->isUnsizedIntType()) {
      de->addTrait(Defn::Singular);
    }
    module.addMember(de);
  }

  // Initialize all intrinsic operators and functions.
  initOperators();

  // Built-in namespaces
  module.addMember(&nsOperator);
  nsOperator.setQualifiedName(nsOperator.name());

  // Other built-in types
  NativePointerType::instance.initBuiltin();
  NativeArrayType::instance.initBuiltin();

  DASSERT(module.module() != NULL);
  ScopeBuilder::createScopeMembers(&module);
}

Module * Builtins::loadSystemModule(const char * name) {
  Module * mod = PackageMgr::get().getModuleForImportPath(name);
  if (mod != NULL) {
    return mod;
  }

  diag.fatal() << "Error: Can't load builtin definition for '" << name << "'";
  abort();
}

Defn * Builtins::loadSystemDef(const char * name) {
  Module * mod = loadSystemModule(name);
  return mod->primaryDefn();
}

Type * Builtins::loadSystemType(const char * name) {
  Type * result = cast<TypeDefn>(loadSystemDef(name))->typeValue();
  DASSERT(result != NULL);
  return result;
}

Defn * Builtins::getSingleDefn(Type * type, const char * name) {
  DefnList defs;
  if (CompositeType * ctype = dyn_cast<CompositeType>(type)) {
    AnalyzerBase::analyzeTypeDefn(ctype->typeDefn(), Task_PrepMemberLookup);
    if (!ctype->lookupMember(name, defs, false)) {
      DFAIL("Couldn't find system definition");
    }

    if (defs.size() > 1) {
      DFAIL("Couldn't find system definition");
    }
  }

  return defs.front();
}

void Builtins::loadSystemClasses() {
  typeArray = loadSystemType("tart.core.Array");
  typeAttribute = loadSystemType("tart.core.Attribute");
  typeType = loadSystemType("tart.reflect.Type");
  typeTypeInfoBlock = loadSystemType("tart.core.TypeInfoBlock");
  typeObject = loadSystemType("tart.core.Object");
  typeString = loadSystemType("tart.core.String");
  typeThrowable = loadSystemType("tart.core.Throwable");

  typeIntrinsicAttribute = loadSystemType("tart.core.Intrinsic");

  typeClass = loadSystemType("tart.reflect.Class");
  typeStruct = loadSystemType("tart.reflect.Struct");
  typeInterface = loadSystemType("tart.reflect.Interface");
  typeEnum = loadSystemType("tart.reflect.Enum");

  // Make sure object class gets analyzed
  AnalyzerBase::analyzeTypeDefn(typeObject->typeDefn(), Task_PrepCodeGeneration);

  // Get the function that tests for a type
  funcHasBase = cast_or_null<FunctionDefn>(getSingleDefn(typeTypeInfoBlock, "hasBase"));
  funcTypecastError = cast_or_null<FunctionDefn>(getSingleDefn(typeTypeInfoBlock, "typecastError"));

  // Get the low-level exception structure
  typeUnwindException = cast<TypeDefn>(
      getSingleDefn(typeThrowable, "UnwindException"))->typeValue();

  // Set the aliases
  typeAliasString.setValue(typeString);
}

bool Builtins::compileBuiltins(ProgramSource & source) {
  Parser parser(&source, &module);
  return parser.parse();
}

}

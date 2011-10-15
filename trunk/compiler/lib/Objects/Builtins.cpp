/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Module.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/NamespaceDefn.h"

#include "tart/Objects/TargetSelection.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Common/PackageMgr.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ScopeBuilder.h"

#include "tart/Parse/Parser.h"

namespace tart {

namespace {
  SourceFile builtinSource("");

  struct EssentialType {
    const char * name;
    Type ** type;
  };

  EssentialType annexTypes[] = {
    { "tart.core.Iterable", &Builtins::typeIterable },
    { "tart.core.Iterator", &Builtins::typeIterator },
  };
}

Module Builtins::module(&builtinSource, "$builtin");
Module Builtins::syntheticModule(&builtinSource, "$synthetic");

SystemClass Builtins::typeTypeInfoBlock("tart.core.TypeInfoBlock");
SystemClass Builtins::typeObject("tart.core.Object");
SystemClass Builtins::typeString("tart.core.String");
SystemClass Builtins::typeArray("tart.core.Array");
SystemClass Builtins::typeRange("tart.core.Range");
SystemClass Builtins::typeThrowable("tart.core.Throwable");
SystemClass Builtins::typeFunction("tart.core.Function");
Type * Builtins::typeIterable;
Type * Builtins::typeIterator;

SystemClass Builtins::typeType("tart.reflect.Type");
SystemClass Builtins::typePrimitiveType("tart.reflect.PrimitiveType");
SystemClass Builtins::typeFunctionType("tart.reflect.FunctionType");
SystemClass Builtins::typeCompositeType("tart.reflect.CompositeType");
SystemClass Builtins::typeEnumType("tart.reflect.EnumType");
SystemClass Builtins::typeDerivedType("tart.reflect.DerivedType");
SystemClass Builtins::typeModule("tart.reflect.Module");
SystemClass Builtins::typeNameTable("tart.reflect.NameTable");
SystemClass Builtins::typePackage("tart.reflect.Package");
SystemClass Builtins::typeMethod("tart.reflect.Method");
SystemClass Builtins::typeProperty("tart.reflect.Property");
SystemClass Builtins::typeField("tart.reflect.Field");
SystemClass Builtins::typeDataMember("tart.reflect.DataMember");
SystemClass Builtins::typeMember("tart.reflect.Member");
SystemClass Builtins::typeTypeList("tart.reflect.TypeList");
SystemClass Builtins::typeAttributeList("tart.reflect.AttributeList");
SystemClass Builtins::typeMethodList("tart.reflect.MethodList");
SystemClass Builtins::typePropertyList("tart.reflect.PropertyList");
SystemClass Builtins::typeFieldList("tart.reflect.FieldList");

SystemClass Builtins::typeAttribute("tart.core.Attribute");
SystemClass Builtins::typeIntrinsicAttribute("tart.annex.Intrinsic");

SystemClass Builtins::typeStaticRoot("tart.gc.StaticRoot");
SystemClass Builtins::typeTraceAction("tart.gc.TraceAction");
SystemClass Builtins::typeTraceDescriptor("tart.gc.TraceDescriptor");

SystemClass Builtins::typeRef("tart.core.Ref");
SystemClass Builtins::typeValueRef("tart.core.ValueRef");
SystemClass Builtins::typeMutableRef("tart.core.MutableRef");
SystemNamespace Builtins::nsRefs("tart.core.Refs");
SystemNamespace Builtins::nsGC("tart.gc.GC");

SystemNamespaceMember<FunctionDefn> gc_allocContext(Builtins::nsGC, "allocContext");
SystemNamespaceMember<FunctionDefn> gc_alloc(Builtins::nsGC, "alloc");

Type * Builtins::typeUnwindException;

TypeAlias Builtins::typeAliasString(NULL, NULL);

FunctionDefn * Builtins::funcHasBase;
FunctionDefn * Builtins::funcTypecastError;
FunctionDefn * Builtins::funcTypecastErrorExt;
FunctionDefn * Builtins::funcDispatchError;
FunctionDefn * Builtins::funcUndefinedMethod;

void Builtins::init() {
  static GCPointerRoot moduleRoot(&module);
  static GCPointerRoot syntheticModuleRoot(&syntheticModule);

  // Initialize primitive types
  PrimitiveType::initPrimitiveTypes(&module);

  // Initialize all intrinsic operators and functions.
  initOperators();

  // Other built-in types
  AddressType::initBuiltin();
  NativeArrayType::initBuiltin();
  FlexibleArrayType::initBuiltin();
  TypeLiteralType::initBuiltin();

  DASSERT(module.module() != NULL);
  ScopeBuilder::createScopeMembers(&module);
  module.passes().finish(Module::ScopeCreationPass);
}

Module * Builtins::loadSystemModule(const char * name) {
  Module * mod = PackageMgr::get().loadModule(name);
  if (mod != NULL) {
    return mod;
  }

  diag.fatal() << "Can't locate builtin module for '" << name << "'";
  abort();
}

Defn * Builtins::loadSystemDef(const char * name) {
  Module * mod = loadSystemModule(name);
  if (mod->primaryDefn() == NULL) {
    diag.fatal(mod) << "Can't load builtin definition for '" << name << "'";
    abort();
  }
  return mod->primaryDefn();
}

Type * Builtins::loadSystemType(const char * name) {
  Type * result = cast<TypeDefn>(loadSystemDef(name))->mutableTypePtr();
  DASSERT(result != NULL);
  return result;
}

Defn * Builtins::getSingleMember(const Scope * scope, const char * name) {
  DefnList defs;
  if (!scope->lookupMember(name, defs, false)) {
    diag.info() << "Couldn't find system definition for " << name;
    DFAIL("Couldn't find system definition");
  }

  if (defs.size() > 1) {
    DFAIL("Couldn't find system definition");
  }

  return defs.front();
}

Defn * Builtins::getSingleDefn(Type * type, const char * name) {
  AnalyzerBase::analyzeType(type, Task_PrepMemberLookup);
  return getSingleMember(type->memberScope(), name);
}

Defn * Builtins::getSingleDefn(NamespaceDefn * ns, const char * name) {
  AnalyzerBase::analyzeNamespace(ns, Task_PrepMemberLookup);
  return getSingleMember(&ns->memberScope(), name);
}

void Builtins::loadSystemClasses() {
  typeArray.get();
  typeAttribute.get();
  typeTypeInfoBlock.get();
  typeType.get();
  typeObject.get();
  typeString.get();
  typeThrowable.get();
  typeIntrinsicAttribute.get();
  typeStaticRoot.get();
  typeTraceAction.get();
  typeTraceDescriptor.get();

  // Analyze class Object.
  AnalyzerBase::analyzeType(typeObject.get(), Task_PrepMemberLookup);

  // Get the function that tests for a type
  funcHasBase = getMember<FunctionDefn>(typeTypeInfoBlock.get(), "hasBase");
  funcTypecastError = getMember<FunctionDefn>(typeTypeInfoBlock.get(), "typecastError");
  funcTypecastErrorExt = getMember<FunctionDefn>(typeTypeInfoBlock.get(), "typecastErrorExt");
  funcDispatchError = getMember<FunctionDefn>(typeTypeInfoBlock.get(), "dispatchError");

  // Get the function that throws an undefined method error
  funcUndefinedMethod = getMember<FunctionDefn>(typeObject.get(), "__undefinedMethod");

  // Get the low-level exception structure
  typeUnwindException = getMember<TypeDefn>(
      typeThrowable.get(), "UnwindException")->mutableTypePtr();

  // Set the aliases
  typeAliasString.setValue(typeString.get());
}

bool Builtins::compileBuiltins(ProgramSource & source) {
  Parser parser(&source, &module);
  return parser.parse();
}

#define elementsof(x)   (sizeof(x)/sizeof(x[0]))

void Builtins::registerEssentialType(const Type * type) {
  for (size_t i = 0; i < elementsof(annexTypes); ++i) {
    EssentialType * atype = &annexTypes[i];
    if (type->typeDefn()->qualifiedName() == atype->name) {
      *atype->type = const_cast<Type *>(type);
      break;
    }
  }
}

FunctionDefn * Builtins::objectCoerceFn() {
  if (!AnalyzerBase::analyzeType(Builtins::typeObject, Task_PrepConversion)) {
    return NULL;
  }

  // Get the coerce function that's a template.
  FunctionDefn * coerceFn = NULL;
  const MethodList & coercers = Builtins::typeObject->coercers();
  for (MethodList::const_iterator it = coercers.begin(); it != coercers.end(); ++it) {
    if ((*it)->isTemplate()) {
      coerceFn = *it;
      break;
    }
  }

  return coerceFn;
}

}

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

  struct EssentialType {
    const char * name;
    Type ** type;
  };

  EssentialType annexTypes[] = {
    "tart.core.Iterable", &Builtins::typeIterable,
    "tart.core.Iterator", &Builtins::typeIterator,
  };
}

Module Builtins::module(&builtinSource, "$builtin");
Module Builtins::syntheticModule(&builtinSource, "$synthetic");

Type * Builtins::typeTypeInfoBlock;
SystemClass Builtins::typeObject("tart.core.Object");
Type * Builtins::typeString;
Type * Builtins::typeArray;
Type * Builtins::typeRange;
Type * Builtins::typeThrowable;
Type * Builtins::typeUnwindException;
Type * Builtins::typeIterable;
Type * Builtins::typeIterator;
Type * Builtins::typeUnsupportedOperationException;

Type * Builtins::typeTypeDescriptor;
Type * Builtins::typeType;
Type * Builtins::typeTypeLiteral;
Type * Builtins::typeSimpleType;
Type * Builtins::typeComplexType;
Type * Builtins::typeEnumType;
Type * Builtins::typeFunctionType;
Type * Builtins::typeDerivedType;
Type * Builtins::typeMember;
Type * Builtins::typeField;
Type * Builtins::typeProperty;
Type * Builtins::typeMethod;
Type * Builtins::typeModule;

Type * Builtins::typeAttribute;
Type * Builtins::typeIntrinsicAttribute;

SystemClass Builtins::typeRef("tart.core.Ref");
SystemClass Builtins::typeValueRef("tart.core.ValueRef");

//SystemTypeRef Builtins::typeComplexType("tart.reflect.ComplexType");
//SystemTypeRef Builtins::typeEnumType("tart.reflect.EnumType");

TypeAlias Builtins::typeAliasString = NULL;

FunctionDefn * Builtins::funcHasBase;
FunctionDefn * Builtins::funcTypecastError;

void Builtins::init() {
  // Initialize primitive types
  PrimitiveType::initPrimitiveTypes(&module);

  // Initialize all intrinsic operators and functions.
  initOperators();

  // Other built-in types
  AddressType::initBuiltin();
  PointerType::initBuiltin();
  NativeArrayType::initBuiltin();

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
    AnalyzerBase::analyzeType(ctype, Task_PrepMemberLookup);
    if (!ctype->lookupMember(name, defs, false)) {
      diag.info() << "Couldn't find system definition for " << name;
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
  typeTypeDescriptor = loadSystemType("tart.reflect.TypeDescriptor");
  typeTypeInfoBlock = loadSystemType("tart.core.TypeInfoBlock");
  typeType = loadSystemType("tart.reflect.Type");
  typeObject.get();
  //typeObject = loadSystemType("tart.core.Object");
  typeString = loadSystemType("tart.core.String");
  typeThrowable = loadSystemType("tart.core.Throwable");
  //typeTypeLiteral = loadSystemType("tart.core.TypeLiteral");
  typeUnsupportedOperationException = loadSystemType("tart.core.UnsupportedOperationException");
  typeIntrinsicAttribute = loadSystemType("tart.annex.Intrinsic");

  // Analyze class Object.
  //AnalyzerBase::analyzeType(typeObject, Task_PrepCodeGeneration);
  AnalyzerBase::analyzeType(typeObject, Task_PrepMemberLookup);

  // Get the function that tests for a type
  funcHasBase = getMember<FunctionDefn>(typeTypeInfoBlock, "hasBase");
  funcTypecastError = getMember<FunctionDefn>(typeTypeInfoBlock, "typecastError");

  // Get the low-level exception structure
  typeUnwindException = getMember<TypeDefn>(typeThrowable, "UnwindException")->typeValue();

  // Set the aliases
  typeAliasString.setValue(typeString);
}

void Builtins::loadReflectionClasses() {
  if (typeModule == NULL) {
    typeSimpleType = loadSystemType("tart.reflect.SimpleType");
    typeComplexType = loadSystemType("tart.reflect.ComplexType");
    typeEnumType = loadSystemType("tart.reflect.EnumType");
    typeFunctionType = loadSystemType("tart.reflect.FunctionType");
    typeDerivedType = loadSystemType("tart.reflect.DerivedType");
    typeMember = loadSystemType("tart.reflect.Member");
    typeField = loadSystemType("tart.reflect.Field");
    typeProperty = loadSystemType("tart.reflect.Property");
    typeMethod = loadSystemType("tart.reflect.Method");
    typeModule = loadSystemType("tart.reflect.Module");

    // Analyzing these types should analyze the rest.
    AnalyzerBase::analyzeType(typeModule, Task_PrepCodeGeneration);
    AnalyzerBase::analyzeType(typeComplexType, Task_PrepCodeGeneration);
    AnalyzerBase::analyzeType(typeEnumType, Task_PrepCodeGeneration);
    AnalyzerBase::analyzeType(typeFunctionType, Task_PrepCodeGeneration);
    AnalyzerBase::analyzeType(typeDerivedType, Task_PrepCodeGeneration);
  }
}

bool Builtins::compileBuiltins(ProgramSource & source) {
  Parser parser(&source, &module);
  return parser.parse();
}

#define elementsof(x)   (sizeof(x)/sizeof(x[0]))

void Builtins::registerEssentialType(const Type * type) {
  for (int i = 0; i < elementsof(annexTypes); ++i) {
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

CompositeType * SystemClass::get() const {
  if (type_ == NULL) {
    type_ = cast<CompositeType>(Builtins::loadSystemType(typeName_));
  }

  return type_;
}

}

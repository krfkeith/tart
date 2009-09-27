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
Type * Builtins::typeUnsupportedOperationException;

Type * Builtins::typeType;
Type * Builtins::typeTypeDescriptor;
Type * Builtins::typeMember;
Type * Builtins::typeField;
Type * Builtins::typeProperty;
Type * Builtins::typeMethod;
Type * Builtins::typeModule;

Type * Builtins::typeAttribute;
Type * Builtins::typeIntrinsicAttribute;

TypeAlias Builtins::typeAliasString(NULL);

FunctionDefn * Builtins::funcHasBase;
FunctionDefn * Builtins::funcTypecastError;

ReflectTypeDescriptor Builtins::rfTypeDescriptor;
ReflectMember Builtins::rfMember;
ReflectModule Builtins::rfModule;

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
    AnalyzerBase::analyzeType(ctype, Task_PrepMemberLookup);
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
  typeTypeDescriptor = loadSystemType("tart.reflect.TypeDescriptor");
  typeTypeInfoBlock = loadSystemType("tart.core.TypeInfoBlock");
  typeObject = loadSystemType("tart.core.Object");
  typeString = loadSystemType("tart.core.String");
  typeThrowable = loadSystemType("tart.core.Throwable");
  typeUnsupportedOperationException = loadSystemType("tart.core.UnsupportedOperationException");

  typeIntrinsicAttribute = loadSystemType("tart.annex.Intrinsic");

  typeMember = loadSystemType("tart.reflect.Member");
  typeField = loadSystemType("tart.reflect.Field");
  typeProperty = loadSystemType("tart.reflect.Property");
  typeMethod = loadSystemType("tart.reflect.Method");
  typeModule = loadSystemType("tart.reflect.Module");

  // Analyze class Object.
  AnalyzerBase::analyzeType(typeObject, Task_PrepCodeGeneration);

  // Get the function that tests for a type
  funcHasBase = getMember<FunctionDefn>(typeTypeInfoBlock, "hasBase");
  funcTypecastError = getMember<FunctionDefn>(typeTypeInfoBlock, "typecastError");

  // Get the low-level exception structure
  typeUnwindException = getMember<TypeDefn>(typeThrowable, "UnwindException")->typeValue();

  // Set the aliases
  typeAliasString.setValue(typeString);

  // Get references to members of class tart.reflect.TypeDescriptor
  AnalyzerBase::analyzeType(typeTypeDescriptor, Task_PrepMemberLookup);
  rfTypeDescriptor.memberTypeKind = getMember<VariableDefn>(typeTypeDescriptor, "typeKind");
  rfTypeDescriptor.memberSupertype = getMember<VariableDefn>(typeTypeDescriptor, "supertype");
  rfTypeDescriptor.memberInterfaces = getMember<VariableDefn>(typeTypeDescriptor, "interfaces");
  rfTypeDescriptor.memberTypeParams = getMember<VariableDefn>(typeTypeDescriptor, "typeParams");
  rfTypeDescriptor.memberAttributes = getMember<VariableDefn>(typeTypeDescriptor, "attributes");
  rfTypeDescriptor.memberFields = getMember<VariableDefn>(typeTypeDescriptor, "fields");
  rfTypeDescriptor.memberProperties = getMember<VariableDefn>(typeTypeDescriptor, "properties");
  rfTypeDescriptor.memberConstructors = getMember<VariableDefn>(typeTypeDescriptor, "constructors");
  rfTypeDescriptor.memberMethods = getMember<VariableDefn>(typeTypeDescriptor, "methods");

  // Get references to members of class tart.reflect.Member
  AnalyzerBase::analyzeType(typeMember, Task_PrepMemberLookup);
  rfMember.memberName = getMember<VariableDefn>(typeMember, "name");
  rfMember.memberMemberType = getMember<VariableDefn>(typeMember, "memberType");
  rfMember.memberKind = getMember<VariableDefn>(typeMember, "kind");
  rfMember.memberDeclaringType = getMember<VariableDefn>(typeMember, "declaringType");
  rfMember.memberAccess = getMember<VariableDefn>(typeMember, "access");
  rfMember.memberTraits = getMember<VariableDefn>(typeMember, "traits");
  rfMember.memberAttributes = getMember<VariableDefn>(typeMember, "attributes");

  // Get references to members of class tart.reflect.Module
  AnalyzerBase::analyzeType(typeModule, Task_PrepMemberLookup);
  rfModule.memberName = getMember<VariableDefn>(typeModule, "moduleName");
  rfModule.memberTypes = getMember<VariableDefn>(typeModule, "typeList");
  rfModule.memberMethods = getMember<VariableDefn>(typeModule, "methodList");
}

bool Builtins::compileBuiltins(ProgramSource & source) {
  Parser parser(&source, &module);
  return parser.parse();
}

#define elementsof(x)   (sizeof(x)/sizeof(x[0]))

void Builtins::registerEssentialType(Type * type) {
  for (int i = 0; i < elementsof(annexTypes); ++i) {
    EssentialType * atype = &annexTypes[i];
    if (type->typeDefn()->qualifiedName() == atype->name) {
      *atype->type = type;
      break;
    }
  }
}

}

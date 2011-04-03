/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_OBJECTS_BUILTINS_H
#define TART_OBJECTS_BUILTINS_H

#ifndef TART_DEFN_SCOPE_H
#include "tart/Defn/Scope.h"
#endif

#ifndef TART_TYPE_TYPEALIAS_H
#include "tart/Type/TypeAlias.h"
#endif

namespace tart {

class Module;
class Defn;
class TypeDefn;
class NamespaceDefn;
class ValueDefn;
class SystemClass;
class SystemNamespace;

/// -------------------------------------------------------------------
/// Class to hold references to all builtin types and methods.
class Builtins {
private:
  static Module * loadSystemModule(const char * name);
  static Defn * getSingleMember(Scope * scope, const char * name);
  static Defn * getSingleDefn(Type * tdef, const char * name);
  static Defn * getSingleDefn(NamespaceDefn * ns, const char * name);

  static bool compileBuiltins(ProgramSource & source);

public:

  template<class T> static T * getMember(Type * tdef, const char * name);
  template<class T> static T * getMember(NamespaceDefn * ns, const char * name);

  // The module for builtins
  static Module module;
  static Module syntheticModule;

  // System types - core
  static SystemClass typeTypeInfoBlock;
  static SystemClass typeObject;
  static SystemClass typeString;
  static SystemClass typeArray;
  static SystemClass typeRange;
  static SystemClass typeThrowable;
  static SystemClass typeFunction;
  static Type * typeUnwindException;
  static Type * typeIterable;
  static Type * typeIterator;

  // System types - reflection
  static SystemClass typeType;
  static SystemClass typeFunctionType;
  static SystemClass typePrimitiveType;
  static SystemClass typeCompositeType;
  static SystemClass typeEnumType;
  static SystemClass typeDerivedType;
  static SystemClass typeModule;
  static SystemClass typeNameTable;
  static SystemClass typePackage;
  static SystemClass typeMethod;
  static SystemClass typeProperty;
  static SystemClass typeField;
  static SystemClass typeDataMember;
  static SystemClass typeMember;
  static SystemClass typeTypeList;
  static SystemClass typeAttributeList;
  static SystemClass typeMethodList;
  static SystemClass typePropertyList;
  static SystemClass typeFieldList;

  // System types - attributes
  static SystemClass typeAttribute;
  static SystemClass typeIntrinsicAttribute;

  // System types - gc
  static SystemClass typeTraceAction;

  // System types - lazily loaded
  static SystemClass typeRef;
  static SystemClass typeValueRef;
  static SystemClass typeMutableRef;
  static SystemNamespace nsRefs;
  static SystemNamespace nsGC;

  // Global aliases - used to create static references to types that are dynamically loaded.
  static TypeAlias typeAliasString;

  // System functions
  static FunctionDefn * funcHasBase;
  static FunctionDefn * funcTypecastError;
  static FunctionDefn * funcUndefinedMethod;

  /** Initialization function for builtins. */
  static void init();

  /** Load a system type. */
  static Type * loadSystemType(const char * name);

  /** Load a system definition. */
  static Defn * loadSystemDef(const char * name);

  /** Load classes known to the compiler. */
  static void loadSystemClasses();

  /** Define built-in operators. */
  static void initOperators();

  /** Register an annex type. */
  static void registerEssentialType(const Type * type);

  /** Return the 'coerce' function of class Object. Needs to be handled specially
      because there are multiple overloads, and we want the one that is a template. */
  static FunctionDefn * objectCoerceFn();
};

template<class T> T * Builtins::getMember(Type * tdef, const char * name) {
  return cast_or_null<T>(getSingleDefn(tdef, name));
}

template<class T> T * Builtins::getMember(NamespaceDefn * ns, const char * name) {
  return cast_or_null<T>(getSingleDefn(ns, name));
}

}

#endif

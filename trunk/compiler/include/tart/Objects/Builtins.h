/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_OBJECTS_BUILTINS_H
#define TART_OBJECTS_BUILTINS_H

#ifndef TART_CFG_SCOPE_H
#include "tart/CFG/Scope.h"
#endif

namespace tart {

class Module;
class Defn;
class TypeDefn;
class ValueDefn;

/// -------------------------------------------------------------------
/// Class to hold references to members of tart.reflect.TypeDescriptor.
struct ReflectTypeDescriptor {
  VariableDefn * memberTypeInfo;
  VariableDefn * memberTypeKind;
  VariableDefn * memberSupertype;
  VariableDefn * memberInterfaces;
  VariableDefn * memberTypeParams;
  VariableDefn * memberAttributes;
  VariableDefn * memberFields;
  VariableDefn * memberProperties;
  VariableDefn * memberConstructors;
  VariableDefn * memberMethods;
};

/// -------------------------------------------------------------------
/// Class to hold references to members of tart.reflect.Member.
struct ReflectMember {
  VariableDefn * memberName;
  VariableDefn * memberMemberType;
  VariableDefn * memberKind;
  VariableDefn * memberDeclaringType;
  VariableDefn * memberAccess;
  VariableDefn * memberTraits;
  VariableDefn * memberAttributes;
};

/// -------------------------------------------------------------------
/// Class to hold references to members of tart.reflect.Member.
struct ReflectModule {
  VariableDefn * memberName;
  VariableDefn * memberTypes;
  VariableDefn * memberMethods;
};

/// -------------------------------------------------------------------
/// Class to hold references to all builtin types and methods.
class Builtins {
private:
  static Module * loadSystemModule(const char * name);
  static Defn * loadSystemDef(const char * name);
  static Type * loadSystemType(const char * name);
  static Defn * getSingleDefn(Type * tdef, const char * name);

  template<class T> static T * getMember(Type * tdef, const char * name);

  static bool compileBuiltins(ProgramSource & source);

public:

  // The module for builtins
  static Module module;
  static Module syntheticModule;

  // System types - core
  static Type * typeTypeInfoBlock;
  static Type * typeObject;
  static Type * typeString;
  static Type * typeArray;
  static Type * typeRange;
  static Type * typeThrowable;
  static Type * typeUnwindException;
  static Type * typeIterable;
  static Type * typeIterator;
  static Type * typeUnsupportedOperationException;

  // System types - reflection
  static Type * typeTypeDescriptor;
  static Type * typeType;
  static Type * typeMember;
  static Type * typeField;
  static Type * typeProperty;
  static Type * typeMethod;
  static Type * typeModule;

  // System types - attributes
  static Type * typeAttribute;
  static Type * typeIntrinsicAttribute;

  // Global aliases - used to create static references to types that are dynamically loaded.
  static TypeAlias typeAliasString;

  // System functions
  static FunctionDefn * funcHasBase;
  static FunctionDefn * funcTypecastError;

  // Information about reflection classes
  static ReflectTypeDescriptor rfTypeDescriptor;
  static ReflectMember rfMember;
  static ReflectModule rfModule;

  // Namespaces
  static NamespaceDefn nsOperator;

  /** Initialization function for builtins. */
  static void init();

  /** Load classes known to the compiler. */
  static void loadSystemClasses();

  /** Define built-in operators. */
  static void initOperators();

  /** Register an annex type. */
  static void registerEssentialType(Type * type);
};

template<class T> T * Builtins::getMember(Type * tdef, const char * name) {
  return cast_or_null<T>(getSingleDefn(tdef, name));
}

}

#endif

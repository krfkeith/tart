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
/// Class to hold references to all builtin types and methods.
class Builtins {
private:
  static Module * loadSystemModule(const char * name);
  static Defn * loadSystemDef(const char * name);
  static Type * loadSystemType(const char * name);
  static Defn * getSingleDefn(Type * tdef, const char * name);
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

  // System types - reflection
  static Type * typeType;
  static Type * typeClass;
  static Type * typeStruct;
  static Type * typeInterface;
  static Type * typeEnum;

  // System types - attributes
  static Type * typeAttribute;
  static Type * typeIntrinsicAttribute;

  // System types - implementation

#if 0
  // System types
  static const CompositeType * typeInterfaceTable;
  static const CompositeType * typeArrayType;

  // System attribute types
  static const CompositeType * typeAllowUnsafeAttribute;
  static const CompositeType * typeCommutativeAttribute;
#endif

  // Global aliases - used to create static references to types that are dynamically loaded.
  static TypeAlias typeAliasString;

  // System functions
  static FunctionDefn * funcHasBase;
  static FunctionDefn * funcTypecastError;

  // Namespaces
  static NamespaceDefn nsOperator;

  /** Initialization function for builtins. */
  static void init();

  /** Load classes known to the compiler. */
  static void loadSystemClasses();

  /** Define built-in operators. */
  static void initOperators();
};

}

#endif

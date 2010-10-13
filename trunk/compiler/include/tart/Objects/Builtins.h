/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_OBJECTS_BUILTINS_H
#define TART_OBJECTS_BUILTINS_H

#ifndef TART_CFG_SCOPE_H
#include "tart/CFG/Scope.h"
#endif

#ifndef TART_CFG_TYPEALIAS_H
#include "tart/CFG/TypeAlias.h"
#endif

namespace tart {

class Module;
class Defn;
class TypeDefn;
class ValueDefn;
class SystemClass;

/// -------------------------------------------------------------------
/// Class to hold references to all builtin types and methods.
class Builtins {
private:
  static Module * loadSystemModule(const char * name);
  static Defn * loadSystemDef(const char * name);
  static Defn * getSingleDefn(Type * tdef, const char * name);

  static bool compileBuiltins(ProgramSource & source);

public:

  template<class T> static T * getMember(Type * tdef, const char * name);

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
  static SystemClass typeUnsupportedOperationError;
  static Type * typeUnwindException;
  static Type * typeIterable;
  static Type * typeIterator;

  // System types - reflection
  static SystemClass typeType;
  static SystemClass typeCompositeType;
  static SystemClass typeEnumInfoBlock;
  static SystemClass typeEnumType;
  static SystemClass typeFunctionType;
  static SystemClass typeDerivedType;
  static SystemClass typePrimitiveType;
  static SystemClass typeMember;
  static SystemClass typeField;
  static SystemClass typeParameter;
  static SystemClass typeProperty;
  static SystemClass typeMethod;
  static SystemClass typeModule;
  static SystemClass typeNameTable;
  static SystemClass typePackage;
  static SystemClass typeReflectionMetadata;

  // System types - attributes
  static SystemClass typeAttribute;
  static SystemClass typeIntrinsicAttribute;

  // System types - gc
  static SystemClass typeTraceAction;

  // System types - lazily loaded
  static SystemClass typeRef;
  static SystemClass typeValueRef;

  // Global aliases - used to create static references to types that are dynamically loaded.
  static TypeAlias typeAliasString;

  // System functions
  static FunctionDefn * funcHasBase;
  static FunctionDefn * funcTypecastError;

  /** Initialization function for builtins. */
  static void init();

  /** Load a system type. */
  static Type * loadSystemType(const char * name);

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

/// -------------------------------------------------------------------
/// Contains a lazy reference to a system class
class SystemClass {
public:
  SystemClass(const char * typeName)
    : typeName_(typeName)
    , type_(NULL)
    {}

  CompositeType * get() const;
  CompositeType * peek() const { return type_; }
  CompositeType * operator->() const {
    return get();
  }

  operator CompositeType *() const {
    return get();
  }

  const llvm::Type * irType() const;
  const llvm::Type * irEmbeddedType() const;
  const llvm::Type * irParameterType() const;
  const llvm::Type * irReturnType() const;

  TypeDefn * typeDefn() const;

private:
  const char * typeName_;
  mutable CompositeType * type_;
};

/// -------------------------------------------------------------------
/// Contains a lazy reference to a member of a system type
template <class T>
class SystemClassMember {
public:
  SystemClassMember(SystemClass & definingClass, const char * fieldName)
    : definingClass_(definingClass)
    , fieldName_(fieldName)
    , member_(NULL)
  {}

  T * get() const {
    if (member_ == NULL) {
      member_ = Builtins::getMember<T>(definingClass_.get(), fieldName_);
    }

    return member_;
  }

  T * operator->() const {
    return get();
  }

  operator T *() const {
    return get();
  }

  const Type * type() const {
    return get()->type();
  }

private:
  SystemClass & definingClass_;
  const char * fieldName_;
  mutable T * member_;
};

}

#endif

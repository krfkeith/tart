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
  static Type * typeTypeInfoBlock;
  static SystemClass typeObject;
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
  static Type * typeTypeRef;
  static Type * typeTypeLiteral;
  static Type * typeSimpleType;
  static Type * typeComplexType;
  static Type * typeEnumType;
  static Type * typeFunctionType;
  static Type * typeDerivedType;
  static Type * typeMember;
  static Type * typeField;
  static Type * typeProperty;
  static Type * typeMethod;
  static Type * typeModule;

  // System types - attributes
  static Type * typeAttribute;
  static Type * typeIntrinsicAttribute;

  // System types - lazily loaded
  static SystemClass typeRef;
  static SystemClass typeValueRef;

  //static SystemTypeRef typeComplexType;
  //static SystemTypeRef typeEnumType;

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

  /** Load classes needed for reflection. */
  static void loadReflectionClasses();

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

  CompositeType * operator->() const {
    return get();
  }

  operator CompositeType *() const {
    return get();
  }

private:
  const char * typeName_;
  mutable CompositeType * type_;
};

/// -------------------------------------------------------------------
/// Contains a lazy reference to a member of a system type
template <class T>
class BuiltinMemberRef {
public:
  BuiltinMemberRef(Type *& definingType, const char * fieldName)
    : definingType_(definingType)
    , fieldName_(fieldName)
    , member_(NULL)
    {}

  T * get() const {
    if (member_ == NULL) {
      member_ = Builtins::getMember<T>(definingType_, fieldName_);
    }

    return member_;
  }

  T * operator->() const {
    return get();
  }

  operator T *() const {
    return get();
  }

  TypeRef type() const {
    return get()->type();
  }

private:
  Type *& definingType_;
  const char * fieldName_;
  mutable T * member_;
};

}

#endif

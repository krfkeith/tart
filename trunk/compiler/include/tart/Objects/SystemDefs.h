/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_OBJECTS_SYSTEMDEFS_H
#define TART_OBJECTS_SYSTEMDEFS_H

#ifndef TART_OBJECTS_BUILTINS_H
#include "tart/Objects/Builtins.h"
#endif

namespace tart {

class EnumType;

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

  operator Qualified<CompositeType>() const {
    return Qualified<CompositeType>(get());
  }

  llvm::Type * irType() const;
  llvm::Type * irEmbeddedType() const;
  llvm::Type * irParameterType() const;
  llvm::Type * irReturnType() const;

  TypeDefn * typeDefn() const;

private:
  const char * typeName_;
  mutable CompositeType * type_;
};

/// -------------------------------------------------------------------
/// Contains a lazy reference to a system enumeration
class SystemEnum {
public:
  SystemEnum(SystemClass & definingClass, const char * typeName)
    : definingClass_(definingClass)
    , typeName_(typeName)
    , type_(NULL)
    {}

  EnumType * get() const;
  EnumType * peek() const { return type_; }
  EnumType * operator->() const {
    return get();
  }

  operator EnumType *() const {
    return get();
  }

  llvm::Type * irType() const;
  llvm::Type * irEmbeddedType() const;
  llvm::Type * irParameterType() const;
  llvm::Type * irReturnType() const;

  TypeDefn * typeDefn() const;

private:
  SystemClass & definingClass_;
  const char * typeName_;
  mutable EnumType * type_;
};

/// -------------------------------------------------------------------
/// Contains a lazy reference to a member of a system type
template <class T>
class SystemClassMember {
public:
  SystemClassMember(const SystemClass & definingClass, const char * fieldName)
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
    return get()->type().type();
  }

private:
  const SystemClass & definingClass_;
  const char * fieldName_;
  mutable T * member_;
};

/// -------------------------------------------------------------------
/// Contains a lazy reference to a system namespace
class SystemNamespace {
public:
  SystemNamespace(const char * typeName)
    : nsName_(typeName)
    , ns_(NULL)
    {}

  NamespaceDefn * get() const;
  NamespaceDefn * peek() const { return ns_; }
  NamespaceDefn * operator->() const {
    return get();
  }

  operator NamespaceDefn *() const {
    return get();
  }

private:
  const char * nsName_;
  mutable NamespaceDefn * ns_;
};

/// -------------------------------------------------------------------
/// Contains a lazy reference to a member of a system namespace
template <class T>
class SystemNamespaceMember {
public:
  SystemNamespaceMember(const SystemNamespace & ns, const char * fieldName)
    : ns_(ns)
    , fieldName_(fieldName)
    , member_(NULL)
  {}

  T * get() const {
    if (member_ == NULL) {
      member_ = Builtins::getMember<T>(ns_.get(), fieldName_);
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
  const SystemNamespace & ns_;
  const char * fieldName_;
  mutable T * member_;
};

/// -------------------------------------------------------------------
/// Contains a lazy reference to an enumeration constant
class SystemEnumConstant {
public:
  SystemEnumConstant(const SystemEnum & definingEnum, const char * name)
    : definingEnum_(definingEnum)
    , name_(name)
    , value_(NULL)
  {}

  VariableDefn * get() const;
  VariableDefn * operator->() const;
  operator VariableDefn *() const;
  int asInt() const;

  const Type * type() const;

private:
  const SystemEnum & definingEnum_;
  const char * name_;
  mutable VariableDefn * value_;
};

}

#endif

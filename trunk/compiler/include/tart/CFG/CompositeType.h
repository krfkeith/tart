/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_CFG_COMPOSITETYPE_H
#define TART_CFG_COMPOSITETYPE_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#include <list>
#include <llvm/ADT/SetVector.h>

namespace tart {

// -------------------------------------------------------------------
// Forward declarations
class ASTTypeDecl;
class CompositeType;

// -------------------------------------------------------------------
// Container declarations
typedef llvm::SmallVector<CompositeType *, 4> ClassList;
typedef llvm::SetVector<CompositeType *> ClassSet;

/// -------------------------------------------------------------------
/// Composite types - class, struct, interface
class CompositeType : public DeclaredType {
  friend class CodeGenerator;
  friend class RuntimeTypeInfo;
  friend class ClassAnalyzer;
public:
  enum ClassFlags {
    ArrayType    = (1 << 0), // This is an array type
  };
  
  /** A table of methods that together implement a given interface. */
  struct InterfaceTable {
    /** The base class. */
    CompositeType * interfaceType;

    /** The list of methods that are overridden for this type. For the direct
        superclass this includes all methods defined in this class. */
    MethodList methods;

    /** The constant array of compiled method pointers. */
    llvm::GlobalVariable * methodTable;

    /** Constructor */
    InterfaceTable(CompositeType * itype)
      : interfaceType(itype)
      , methodTable(NULL)
    {}
  };

  typedef std::list<InterfaceTable> InterfaceList;

private:
  ClassList bases_;
  CompositeType * super_;
  int classFlags;

  // The list of instance methods for this type
  MethodList instanceMethods;

  // The interface override list
  InterfaceList interfaces;

public:
  CompositeType(Type::TypeClass tcls, TypeDefn * de, Scope * parentScope)
    : DeclaredType(tcls, de, parentScope)
    , super_(NULL)
    , classFlags(0)
  {}

  bool getClassFlag(ClassFlags flg) const {
    return (classFlags & flg) != 0;
  }

  void setClassFlag(ClassFlags flg, bool enabled) {
    if (enabled) classFlags |= flg;
    else classFlags &= ~flg;
  }
  
  /** The list of base classes or interfaces. */
  const ClassList & bases() const { return bases_; }
  ClassList & bases() { return bases_; }

  /** The super class of this type. */
  const CompositeType * super() const { return super_; }
  CompositeType * super() { return super_; }
  void setSuper(CompositeType * s) { super_ = s; }

  /** Return true if this class is the same as, or is a subclass of,
      the class 'base'. */
  bool isSubclassOf(const CompositeType * base) const;

  /** Return the default (no-arg) constructor for this type. */
  FunctionDefn * getDefaultConstructor();
  
  /** Return true if this is a reference type. */
  bool isReferenceType() const;

  /** Return the set of all ancestor classes. */
  void getAncestorClasses(ClassSet & out) const;

  /** Add all of the methods that are referred to by this class's TypeInfoBlock as
      definitions to this module. This is used for template instances. */
  void addMethodXDefs(Module * module);

  /** Add all of the static member variables of this class as definitions to this module. */
  void addStaticXDefs(Module * module);

  /** Add all of the ancestor classes as references to this module. */
  void addBaseXRefs(Module * module);

  // Overrides

  bool lookupMember(const char * name, DefnList & defs, bool inherit) const;
  void dumpHierarchy(bool full) const;
  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  void trace() const;
  bool isSubtype(const Type * other) const;
  bool isSingular() const;
  bool includes(const Type * other) const;

  static inline bool classof(const CompositeType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() >= Type::Class &&
           t->typeClass() <= Type::Protocol;
  }
};

}

#endif

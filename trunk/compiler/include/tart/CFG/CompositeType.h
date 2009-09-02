/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_COMPOSITETYPE_H
#define TART_CFG_COMPOSITETYPE_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#ifndef TART_CFG_ATTRIBUTE_H
#include "tart/CFG/Attribute.h"
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
    //ArrayType    = (1 << 0), // This is an array type
    Attribute = (1<<0),         // This class is an attribute.
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

  CompositeType(Type::TypeClass tcls, TypeDefn * de, Scope * parentScope)
    : DeclaredType(tcls, de, parentScope)
    , super_(NULL)
    , classFlags_(0)
  {}

  bool getClassFlag(ClassFlags flg) const {
    return (classFlags_ & flg) != 0;
  }

  void setClassFlag(ClassFlags flg, bool enabled) {
    if (enabled) classFlags_ |= flg;
    else classFlags_ &= ~flg;
  }

  /** The list of base classes or interfaces. */
  const ClassList & bases() const { return bases_; }
  ClassList & bases() { return bases_; }

  /** The super class of this type. */
  const CompositeType * super() const { return super_; }
  CompositeType * super() { return super_; }
  void setSuper(CompositeType * s) { super_ = s; }

  /** List of data members which have storage type Storage_Instance. */
  const DefnList & instanceFields() const { return instanceFields_; }

  /** List of data members which have storage type Storage_Static. */
  const DefnList & staticFields() const { return staticFields_; }

  /** Return the number of instance fields, not including the instance slot for
      the superclass. */
  int instanceFieldCount() const {
    int count = instanceFields_.size();
    return super_ != NULL ? count - 1 : count;
  }

  /** Return the number of instance fields in this class and all superclasses. */
  int instanceFieldCountRecursive() const {
    int count = 0;
    for (const CompositeType * c = this; c != NULL; c = c->super_) {
      count += instanceFieldCount();
    }

    return count;
  }

  /** True if this class is an attribute. */
  bool isAttribute() const { return (classFlags_ & Attribute) != 0; }

  const AttributeInfo & attributeInfo() const { return attributeInfo_; }
  AttributeInfo & attributeInfo() { return attributeInfo_; }

  /** Return true if this class is the same as, or is a subclass of, the class 'base'. */
  bool isSubclassOf(const CompositeType * base) const;

  /** Similar to 'isSubclassOf', except also takes into account template specializations and
      implicit protocol implementations. */
  bool implements(const CompositeType * interface) const;

  /** Return the default (no-arg) constructor for this type. */
  FunctionDefn * defaultConstructor();

  /** Return true if this is a reference type. */
  bool isReferenceType() const;

  /** Return the set of all ancestor classes. */
  void ancestorClasses(ClassSet & out) const;

  /** Add all of the methods that are referred to by this class's TypeInfoBlock as
      definitions to this module. This is used for template instances. */
  void addMethodDefsToModule(Module * module);

  /** Add all of the static member variables of this class as definitions to this module. */
  void addStaticDefsToModule(Module * module);

  /** Add all of the ancestor classes as references to this module. */
  void addBaseXRefs(Module * module);

  // Overrides

  bool lookupMember(const char * name, DefnList & defs, bool inherit) const;
  void dumpHierarchy(bool full) const;
  const llvm::Type * createIRType() const;
  const llvm::Type * irEmbeddedType() const;
  const llvm::Type * irParameterType() const;
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

private:
  ClassList bases_;
  CompositeType * super_;
  int classFlags_;

  // The list of instance methods for this type
  MethodList instanceMethods_;

  // The interface override list
  InterfaceList interfaces_;

  DefnList instanceFields_;
  DefnList staticFields_;

  // For classes which are attributes
  AttributeInfo attributeInfo_;

  bool implementsImpl(const CompositeType * interface) const;
};

}

#endif

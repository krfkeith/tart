/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_COMPOSITETYPE_H
#define TART_TYPE_COMPOSITETYPE_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_TYPE_ATTRIBUTE_H
#include "tart/Type/Attribute.h"
#endif

#ifndef TART_COMMON_PASSMGR_H
#include "tart/Common/PassMgr.h"
#endif

#include "llvm/ADT/SetVector.h"
#include "llvm/DerivedTypes.h"

#include <list>

namespace tart {

// -------------------------------------------------------------------
// Forward declarations
class ASTTypeDecl;
class CompositeType;
class Module;

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
    Attribute = (1<<0),         // This class is an attribute.
    Abstract = (1<<1),          // This class cannot be instantiated
    Final = (1<<2),             // This class cannot be subclassed
    Closure = (1<<3),           // This class is a closure environment
    HasErrors = (1<<4),         // This class has errors
  };

  /** A table of methods that together implement a given interface. */
  struct InterfaceTable {
    /** The base class. */
    CompositeType * interfaceType;

    /** List of interfaces that can use this dispatch table. */
    ClassSet ifaces;

    /** The list of methods that are overridden for this type. For the direct
        superclass this includes all methods defined in this class. */
    MethodList methods;

    /** Constructor */
    InterfaceTable(CompositeType * itype)
      : interfaceType(itype)
    {}
  };

  typedef std::list<InterfaceTable> InterfaceList;

  enum AnalysisPass {
    ScopeCreationPass,
    BaseTypesPass,
    AttributePass,
    NamingConflictPass,
    ImportPass,
    CoercerPass,
    ConstructorPass,
    MemberTypePass,
    FieldPass,
    MethodPass,
    OverloadingPass,
    RecursiveFieldTypePass,
    CompletionPass,
    PassCount
  };

  typedef tart::PassMgr<AnalysisPass, PassCount> PassMgr;
  typedef PassMgr::PassSet PassSet;

  CompositeType(Type::TypeClass tcls, TypeDefn * de, Scope * parentScope, uint32_t flags = 0);

  bool getClassFlag(ClassFlags flg) const {
    return (classFlags_ & flg) != 0;
  }

  void setClassFlag(ClassFlags flg, bool enabled = true) {
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

  /** List of methods which have storage type Storage_Instance. */
  const MethodList & instanceMethods() const { return instanceMethods_; }

  /** Return the number of instance methods. */
  size_t instanceMethodCount() const { return instanceMethods_.size(); }

  /** Return the number of instance fields, not including the instance slot for
      the superclass. */
  int instanceFieldCount() const;

  /** Return the number of instance fields in this class and all superclasses. */
  int instanceFieldCountRecursive() const;

  /** Return the list of custom trace methods for this class. */
  const MethodList & traceMethods() const { return traceMethods_; }
  MethodList & traceMethods() { return traceMethods_; }

  /** True if this class is an attribute. */
  bool isAttribute() const { return (classFlags_ & Attribute) != 0; }
  bool isFinal() const { return (classFlags_ & Final) != 0; }
  bool isAbstract() const { return (classFlags_ & Abstract) != 0; }
  bool hasErrors() const { return (classFlags_ & HasErrors) != 0; }

  const AttributeInfo & attributeInfo() const { return attributeInfo_; }
  AttributeInfo & attributeInfo() { return attributeInfo_; }

  /** Return true if this class is the same as, or is a subclass of, the class 'base'. */
  bool isSubclassOf(const CompositeType * base) const;

  /** Similar to 'isSubclassOf', except also takes into account template specializations and
      implicit protocol implementations. */
  bool implements(const CompositeType * interface) const;

  /** The implicit protocol test - returns true if 'type' fulfills all of the
      requirements of this protocol. */
  bool isSupportedBy(const Type * type) const;

  /** True if this object has any mutable fields. (Not counting garbage collector state) */
  bool isMutable() const;

  /** Return the default constructor for this type. */
  FunctionDefn * defaultConstructor() const;

  /** Return the no-arg constructor for this type. */
  FunctionDefn * noArgConstructor() const;

  /** The list of implicit converters for this class */
  const MethodList & coercers() const { return coercers_; }

  /** The current passes state. */
  const PassMgr & passes() const { return passes_; }
  PassMgr & passes() { return passes_; }

  /** Return the set of all ancestor classes. */
  void ancestorClasses(ClassSet & out) const;

  /** Search all base classes for an interface table entry that either matches 'type'
      or is a subclass of 'type. */
  const InterfaceTable * findBaseImplementationOf(CompositeType * type) const;

  /** Search all base classes for one that is a specialization of the type 'templateType'. */
  const CompositeType * findBaseSpecializing(const CompositeType * templateType) const;

  /** Add all of the definitions that are referred to by this class's TypeInfoBlock or
      this class's CompositeType definitions to this module. This is used for template
      instances. */
  void addClassExportsToModule(Module * module) const;

  /** Add all of the ancestor classes as references to this module. */
  void addBaseXRefs(Module * module);

  /** Fill in the body for the IR StructType. */
  void createIRTypeFields() const;

  // Overrides

  bool lookupMember(StringRef name, DefnList & defs, bool inherit) const;
  void dumpHierarchy(bool full) const;
  llvm::Type * irType() const;
  llvm::Type * irTypeComplete() const;
  llvm::Type * createIRType() const;
  llvm::Type * irEmbeddedType() const;
  llvm::Type * irParameterType() const;
  llvm::Type * irReturnType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  void format(FormatStream & out) const;
  void trace() const;
  bool isSingular() const;
  bool isReferenceType() const;
  TypeShape typeShape() const;
  Expr * nullInitValue() const;
  bool containsReferenceType() const;

  static inline bool classof(const CompositeType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() >= Type::CompositesBegin &&
           t->typeClass() <= Type::CompositesEnd;
  }

private:
  ClassList bases_;
  CompositeType * super_;
  int classFlags_;
  MethodList coercers_;
  PassMgr passes_;
  mutable bool recursionCheck_;

  // The list of instance methods for this type
  MethodList instanceMethods_;
  MethodList traceMethods_;

  // The interface override list
  InterfaceList interfaces_;

  DefnList instanceFields_;
  DefnList staticFields_;

  // For classes which are attributes
  AttributeInfo attributeInfo_;

  // Cache of types that fulfill the requirements of this protocol.
  typedef llvm::DenseMap<const Type *, bool, Type::KeyInfo> ProtocolCache;
  ProtocolCache fulfillments_;

  bool implementsImpl(const CompositeType * interface) const;
};

}

#endif

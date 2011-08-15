/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/Module.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/AnalyzerBase.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"
#include "tart/Objects/TargetSelection.h"

#include "llvm/DerivedTypes.h"

namespace tart {

// Given a definition from some base class, find a method in this class that can override it.
namespace {
  const Defn * findOverride(const Type * searchContext, Defn * baseDef, bool inherit) {
    if (searchContext->memberScope() == NULL) {
      return NULL;
    }

    DefnList defs;
    if (!searchContext->memberScope()->lookupMember(baseDef->name(), defs, inherit)) {
      return NULL;
    }

    if (FunctionDefn * baseFn = dyn_cast<FunctionDefn>(baseDef)) {
      AnalyzerBase::analyzeFunction(baseFn, Task_PrepTypeComparison);
    } else {
      // TODO: Implement overrides of properties.
    }

    for (DefnList::const_iterator it = defs.begin(); it != defs.end(); ++it) {
      Defn * de = *it;
      if (de->defnType() == baseDef->defnType() && de != baseDef) {
        if (FunctionDefn * fn = dyn_cast<FunctionDefn>(de)) {
          AnalyzerBase::analyzeFunction(fn, Task_PrepTypeComparison);
          if (fn->canOverride(static_cast<const FunctionDefn *>(baseDef))) {
            return fn;
          }
        } else {
          // TODO: Implement overrides of properties.
        }
      }
    }

    return NULL;
  }
}

/// -------------------------------------------------------------------
/// CompositeType

CompositeType::CompositeType(Type::TypeClass tcls, TypeDefn * de, Scope * parentScope,
    uint32_t flags)
  : DeclaredType(tcls, de, parentScope, Shape_Unset)
  , super_(NULL)
  , classFlags_(0)
  , recursionCheck_(false)
{
  if (flags & tart::Final) {
    classFlags_ |= CompositeType::Final;
  }

  if (flags & tart::Abstract) {
    classFlags_ |= CompositeType::Abstract;
  }
}

/** Return the number of instance fields, not including the instance slot for
    the superclass. */
int CompositeType::instanceFieldCount() const {
  int count = instanceFields_.size();
  return super_ != NULL ? count - 1 : count;
}

/** Return the number of instance fields in this class and all superclasses. */
int CompositeType::instanceFieldCountRecursive() const {
  int count = 0;
  for (const CompositeType * c = this; c != NULL; c = c->super_) {
    count += c->instanceFieldCount();
  }

  return count;
}

FunctionDefn * CompositeType::defaultConstructor() const {
  const SymbolTable::Entry * ctors = findSymbol("construct");
  if (ctors == NULL) {
    return NULL;
  }

  // Look for a constructor that has zero required parameters.
  for (DefnList::const_iterator it = ctors->begin(); it != ctors->end(); ++it) {
    FunctionDefn * ctor = dyn_cast<FunctionDefn> (*it);
    if (ctor != NULL) {
      ParameterList & params = ctor->params();
      int requiredArgCount = 0;
      for (ParameterList::iterator p = params.begin(); p != params.end(); ++p) {
        if ((*p)->initValue() == NULL) {
          ++requiredArgCount;
        }
      }

      if (requiredArgCount == 0) {
        return ctor;
      }
    }
  }

  // Try creators
  ctors = findSymbol("create");
  if (ctors == NULL) {
    return NULL;
  }

  // Look for a creator that has zero required parameters.
  for (DefnList::const_iterator it = ctors->begin(); it != ctors->end(); ++it) {
    FunctionDefn * ctor = dyn_cast<FunctionDefn> (*it);
    if (ctor != NULL) {
      ParameterList & params = ctor->params();
      int requiredArgCount = 0;
      for (ParameterList::iterator p = params.begin(); p != params.end(); ++p) {
        if ((*p)->initValue() == NULL) {
          ++requiredArgCount;
        }
      }

      if (requiredArgCount == 0) {
        return ctor;
      }
    }
  }

  return NULL;
}

FunctionDefn * CompositeType::noArgConstructor() const {
  const SymbolTable::Entry * ctors = findSymbol("construct");
  if (ctors == NULL) {
    return NULL;
  }

  // Look for a constructor that has zero required parameters.
  for (DefnList::const_iterator it = ctors->begin(); it != ctors->end(); ++it) {
    if (FunctionDefn * ctor = dyn_cast<FunctionDefn> (*it)) {
      if (ctor->params().empty()) {
        return ctor;
      }
    }
  }

  // Try creators
  ctors = findSymbol("create");
  if (ctors == NULL) {
    return NULL;
  }

  // Look for a creator that has zero required parameters.
  for (DefnList::const_iterator it = ctors->begin(); it != ctors->end(); ++it) {
    if (FunctionDefn * ctor = dyn_cast<FunctionDefn> (*it)) {
      if (ctor->params().empty()) {
        return ctor;
      }
    }
  }

  return NULL;
}

bool CompositeType::lookupMember(StringRef name, DefnList & defs, bool inherit) const {
  if (DeclaredType::lookupMember(name, defs, inherit)) {
    return true;
  }

  if (inherit) {
    if (defn_ != NULL && name == defn_->name()) {
      defs.push_back(defn_);
      return true;
    }

    DASSERT_OBJ(passes_.isFinished(BaseTypesPass), this);
    for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
      if ((*it)->lookupMember(name, defs, inherit)) {
        return true;
      }
    }
  }

  return false;
}

void CompositeType::dumpHierarchy(bool full) const {
  DeclaredType::dumpHierarchy(full);
  if (full) {
    diag.indent();
    for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
      (*it)->dumpHierarchy(true);
    }
    diag.unindent();
  }
}

llvm::Type * CompositeType::irType() const {
  if (irType_ == NULL) {
    DASSERT(!defn_->linkageName().empty());
    irType_ = llvm::StructType::createNamed(llvm::getGlobalContext(), defn_->linkageName());
  }
  return irType_;
}

llvm::Type * CompositeType::irTypeComplete() const {
  createIRTypeFields();
  return irType();
}

llvm::Type * CompositeType::createIRType() const {
  DINVALID;
  return NULL;
}

void CompositeType::createIRTypeFields() const {
  using namespace llvm;

  StructType * structType = cast<StructType>(irType());
  if (!structType->isOpaque()) {
    return;
  }

  if (recursionCheck_) {
    return;
  }

  recursionCheck_ = true;

  DASSERT_OBJ(isSingular(), this);
  DASSERT_OBJ(passes_.isFinished(BaseTypesPass), this);
  DASSERT_OBJ(passes_.isFinished(FieldPass), this);

  // Members of the class
  llvm::SmallVector<llvm::Type *, 32> fieldTypes;

  // Handle inheritance
  if (super_ != NULL) {
    // Base class
    super_->createIRTypeFields();
    fieldTypes.push_back(super_->irType());
  }

  if (typeClass() == Type::Interface) {
    fieldTypes.push_back(Builtins::typeTypeInfoBlock.irType()->getPointerTo());
  }

  for (DefnList::const_iterator it = instanceFields_.begin(); it != instanceFields_.end(); ++it) {
    if (*it) {
      VariableDefn * var = static_cast<VariableDefn *>(*it);
      DASSERT_OBJ(var->passes().isFinished(VariableDefn::VariableTypePass), var);
      const Type * varType = var->type();
      llvm::Type * memberType =
          (var->isSharedRef() ? var->sharedRefType() : varType)->irEmbeddedType();
      fieldTypes.push_back(memberType);
    }
  }

  recursionCheck_ = false;

  structType->setBody(fieldTypes);
  DASSERT_OBJ(structType->isSized(), this);
}

llvm::Type * CompositeType::irEmbeddedType() const {
  llvm::Type * type = irType();
  if (isReferenceType()) {
    return type->getPointerTo();
  } else {
    createIRTypeFields();
    return type;
  }
}

llvm::Type * CompositeType::irParameterType() const {
  llvm::Type * type = irType();
  if (isReferenceType() || typeShape() == Shape_Large_Value) {
    return type->getPointerTo();
  } else {
    createIRTypeFields();
    return type;
  }
}

llvm::Type * CompositeType::irReturnType() const {
  llvm::Type * type = irType();
  if (isReferenceType() || typeShape() == Shape_Large_Value) {
    return type->getPointerTo();
  } else {
    createIRTypeFields();
    return type;
  }
}

bool CompositeType::isSubclassOf(const CompositeType * base) const {
  if (this == base) {
    return true;
  }

  if (TypeRelation::isEqual(this, base)) {
    return true;
  }

  for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
    if ((*it)->isSubclassOf(base)) {
      return true;
    }
  }

  if (cls == Interface && base == Builtins::typeObject.get()) {
    return true;
  }

  return false;
}

bool CompositeType::implements(const CompositeType * interface) const {
  return implementsImpl(interface);
}

bool CompositeType::implementsImpl(const CompositeType * interface) const {
  if (defn_->ast() == interface->typeDefn()->ast()) {
    return true;
  }

  for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
    if ((*it)->implementsImpl(interface)) {
      return true;
    }
  }

  return false;
}

/** The implicit protocol test - returns true if 'type' fulfills all of the
    requirements of this protocol. */
bool CompositeType::isSupportedBy(const Type * type) const {
  DASSERT(typeClass() == Protocol);

  if (type->typeDefn() == NULL) {
    return false;
  }

  // First do a direct subclass test.
  if (const CompositeType * ctype = dyn_cast<CompositeType>(type)) {
    if (ctype->isSubclassOf(this)) {
      return true;
    }
  }

  // See if the answer has been cached.
  ProtocolCache::const_iterator it = fulfillments_.find(type);
  if (it != fulfillments_.end()) {
    return it->second;
  }

  //DASSERT_OBJ(passes_.isFinished(BaseTypesPass), this);
  AnalyzerBase::analyzeType(this, Task_PrepMemberLookup);
  AnalyzerBase::analyzeType(type, Task_PrepMemberLookup);
  for (Defn * pm = firstMember(); pm != NULL; pm = pm->nextInScope()) {
    if (pm->isSingular()) {
      switch (pm->defnType()) {
        // case Defn::Indexer:
        case Defn::Function:
        case Defn::Property: {
          const Defn * de = findOverride(type, pm, true);
          if (de != NULL && de->visibility() == Public) {
            return true;
          }

          return false;
        }

        default:
          break;
      }
    }
  }

  // Check inherited protocols as well.
  for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
    if (!(*it)->isSupportedBy(type)) {
      return false;
    }
  }

  return true;
}

bool CompositeType::isMutable() const {
  if (this == Builtins::typeObject.get()) {
    return false;
  }

  for (DefnList::const_iterator it = instanceFields().begin(); it != instanceFields().end(); ++it) {
    if (*it != NULL) {
      VariableDefn * var = cast<VariableDefn>(*it);
      if (var->defnType() == Defn::Var) {
        return true;
      }
    }
  }

  if (super_) {
    return super_->isMutable();
  }

  return false;
}

bool CompositeType::isSingular() const {
  return defn_->isSingular();
}

bool CompositeType::isReferenceType() const {
  return (typeClass() == Type::Class || typeClass() == Type::Interface);
}

TypeShape CompositeType::typeShape() const {
  if (shape_ == Shape_Unset) {
    // Determine the shape of the type.
    switch (typeClass()) {
      case Type::Class:
      case Type::Interface:
        shape_ = Shape_Reference;
        break;

      case Type::Struct: {
        createIRTypeFields();
        uint64_t size = TargetSelection::instance.targetData()->getTypeSizeInBits(irType_);
        if (size == 0) {
          // An empty struct
          shape_ = Shape_ZeroSize;
        } else if (size <= 2 * TargetSelection::instance.targetData()->getPointerSizeInBits()) {
          shape_ = Shape_Small_LValue;
        } else {
          shape_ = Shape_Large_Value;
        }
        break;
      }

      case Type::Protocol:
        shape_ = Shape_None;
        break;

      default:
        DFAIL("Invalid composite");
    }
  }

  return shape_;
}

Expr * CompositeType::nullInitValue() const {
  if (typeClass() == Class || typeClass() == Interface) {
    return ConstantNull::get(SourceLocation(), this);
  }

  return NULL;
}

bool CompositeType::containsReferenceType() const {
  if (typeClass() == Class || typeClass() == Interface) {
    return true;
  }

  if (typeClass() == Struct) {
    for (DefnList::const_iterator it = instanceFields_.begin(); it != instanceFields_.end(); ++it) {
      if (const VariableDefn * var = cast_or_null<VariableDefn>(*it)) {
        if (var->type() != NULL && var->type()->containsReferenceType()) {
          return true;
        }
      }
    }
  }

  return false;
}

void CompositeType::ancestorClasses(ClassSet & out) const {
  for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
    CompositeType * baseType = *it;
    if (out.insert(baseType)) {
      baseType->ancestorClasses(out);
    }
  }
}

const CompositeType::InterfaceTable * CompositeType::findBaseImplementationOf(CompositeType * type)
    const {
  for (InterfaceList::const_iterator it = interfaces_.begin(); it != interfaces_.end(); ++it) {
    if (it->interfaceType == type) {
      return &*it;
    }
  }

  for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
    if (const InterfaceTable * itable = (*it)->findBaseImplementationOf(type)) {
      return itable;
    }
  }

  return NULL;
}

const CompositeType * CompositeType::findBaseSpecializing(const CompositeType * templateType)
    const {
  TemplateInstance * ti = typeDefn()->templateInstance();
  if (ti != NULL && ti->templateDefn() == templateType->typeDefn()) {
    return this;
  }

  for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
    const CompositeType * result = (*it)->findBaseSpecializing(templateType);
    if (result != NULL) {
      return result;
    }
  }

  return NULL;
}

void CompositeType::addClassExportsToModule(Module * module) const {
  DASSERT_OBJ(defn_->isSynthetic(), defn_);

  // Make certain that every method that is referred to from the TIB is XRef'd.
  for (MethodList::const_iterator m = instanceMethods_.begin(); m != instanceMethods_.end(); ++m) {
    FunctionDefn * method = *m;
    if (method->hasBody() && module->addSymbol(method->mergeTo() ? method->mergeTo() : method)) {
      //diag.info() << Format_Verbose << "Added method " << method;
    }
  }

  // Do the same for the trace table.
  for (MethodList::const_iterator m = traceMethods_.begin(); m != traceMethods_.end(); ++m) {
    FunctionDefn * method = *m;
    if (method->hasBody() && module->addSymbol(method->mergeTo() ? method->mergeTo() : method)) {
      //diag.info() << Format_Verbose << "Added method " << method;
    }
  }

  // Add references to all interfaces and interface methods.
  for (InterfaceList::const_iterator it = interfaces_.begin(); it != interfaces_.end(); ++it) {
    if (it->interfaceType->typeDefn()->isSynthetic()) {
      module->addSymbol(it->interfaceType->typeDefn());
    }

    for (MethodList::const_iterator m = it->methods.begin(); m != it->methods.end(); ++m) {
      FunctionDefn * method = *m;
      if (method->hasBody()) {
        module->addSymbol(method->mergeTo() ? method->mergeTo() : method);
      }
    }
  }

  for (DefnList::const_iterator it = staticFields_.begin(); it != staticFields_.end(); ++it) {
    module->addSymbol(*it);
  }
}

void CompositeType::addBaseXRefs(Module * module) {
  module->addSymbol(typeDefn());
  if (typeDefn()->isSynthetic()) {
    for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
      (*it)->addBaseXRefs(module);
    }
  }
}

void CompositeType::format(FormatStream & out) const {
  if (classFlags_ & Closure) {
    DeclaredType::format(out);
    out << ".";
    bases_[1]->format(out);
  } else {
    DeclaredType::format(out);
  }
}

void CompositeType::trace() const {
  DeclaredType::trace();
  markList(bases_.begin(), bases_.end());
  safeMark(super_);
  for (ProtocolCache::const_iterator it = fulfillments_.begin(); it != fulfillments_.end(); ++it) {
    it->first->mark();
  }
}

} // namespace tart

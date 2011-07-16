/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/Module.h"

#include "tart/Common/InternedString.h"
#include "tart/Common/Diagnostics.h"

#include "tart/Sema/AnalyzerBase.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

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
  const SymbolTable::Entry * ctors = findSymbol(istrings.idConstruct);
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
  ctors = findSymbol(istrings.idCreate);
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
  const SymbolTable::Entry * ctors = findSymbol(istrings.idConstruct);
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
  ctors = findSymbol(istrings.idCreate);
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

bool CompositeType::lookupMember(llvm::StringRef name, DefnList & defs, bool inherit) const {
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

const llvm::Type * CompositeType::irType() const {
  return irTypeSafe();
}

const llvm::Type * CompositeType::createIRType() const {
  using namespace llvm;

  DASSERT_OBJ(isSingular(), this);
  DASSERT_OBJ(passes_.isFinished(BaseTypesPass), this);
  DASSERT_OBJ(passes_.isFinished(FieldPass), this);

  // Members of the class
  std::vector<const llvm::Type *> fieldTypes;

  // Handle inheritance
  if (super_ != NULL) {
    // Base class
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
      const llvm::Type * memberType = varType->irEmbeddedType();
      if (var->isSharedRef()) {
        memberType = var->sharedRefType()->irEmbeddedType();
      }
      fieldTypes.push_back(memberType);
    }
  }

  // This is not the normal legal way to do type refinement in LLVM.
  // Normally irType_ would have to be a PATypeHolder as well, however we can't use that
  // because sometimes irType_ will be NULL which PATypeHolder cannot be. However, it is
  // assumed that no one is keeping a persistent copy of irType_ around in raw pointer form.
  llvm::Type * finalType = StructType::get(llvm::getGlobalContext(), fieldTypes);

  // Determine the shape of the type.
  switch (typeClass()) {
    case Type::Class:
    case Type::Interface:
      shape_ = Shape_Reference;
      break;

    case Type::Struct:
      if (fieldTypes.empty()) {
        // An empty struct
        shape_ = Shape_ZeroSize;
      } else {
        shape_ = isLargeIRType(finalType) ? Shape_Large_Value : Shape_Small_LValue;
      }
      break;

    case Type::Protocol:
      shape_ = Shape_None;
      break;

    default:
      DFAIL("Invalid composite");
  }

  return finalType;

//  cast<OpaqueType>(irTypeHolder_.get())->refineAbstractTypeTo(finalType);
//  return irTypeHolder_.get();
}

const llvm::Type * CompositeType::irEmbeddedType() const {
  const llvm::Type * type = irType();
  if (isReferenceType()) {
    return type->getPointerTo();
  } else {
    return type;
  }
}

const llvm::Type * CompositeType::irParameterType() const {
  const llvm::Type * type = irType();
  if (isReferenceType() || typeShape() == Shape_Large_Value) {
    return type->getPointerTo();
  } else {
    return type;
  }
}

const llvm::Type * CompositeType::irReturnType() const {
  const llvm::Type * type = irType();
  if (isReferenceType() || typeShape() == Shape_Large_Value) {
    return type->getPointerTo();
  } else {
    return type;
  }
}

bool CompositeType::isSubclassOf(const CompositeType * base) const {
  if (this == base) {
    return true;
  }

  if (Type::equivalent(this, base)) {
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

ConversionRank CompositeType::convertImpl(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (const CompositeType * fromClass = dyn_cast_or_null<CompositeType>(fromType)) {
    /*if (!fromClass->passes().isFinished(BaseTypesPass)) {
      diag.error() << "While converting to " << this;
      diag.info() << "Class " << fromClass << " has not been properly analyzed.";
    }*/
    DASSERT_OBJ(passes_.isFinished(BaseTypesPass), this);
    DASSERT_OBJ(fromClass->passes().isFinished(BaseTypesPass), fromClass);
    if (fromClass == this) {
      if (cn.fromValue && cn.resultValue) {
        *cn.resultValue = cn.fromValue;
      }

      return IdenticalTypes;
    } else if (typeClass() != Type::Struct && fromClass->isSubclassOf(this)) {
      if (cn.fromValue && cn.resultValue) {
        *cn.resultValue = CastExpr::upCast(cn.fromValue, this)->at(cn.fromValue->location());
      }

      return ExactConversion;
    }
/*  } else if (fromType == &NullType::instance) {
    // Conversion from 'null'.
    if (this->isReferenceType()) {
      if (cn.fromValue && cn.resultValue) {
        *cn.resultValue = new ConstantNull(
            cn.fromValue->location(), this);
      }

      return ExactConversion;
    }*/

    // Check dynamic casts.
    if ((cn.options & Conversion::Checked)
        && isReferenceType() && fromClass->isReferenceType()) {
      if (cn.fromValue && cn.resultValue) {
        *cn.resultValue = CastExpr::tryCast(cn.fromValue, this)->at(cn.fromValue->location());
      }

      return NonPreferred;
    }
  } else if (const FunctionType * ftype = dyn_cast<FunctionType>(fromType)) {
    // See if this class implements the Function interface.
    const CompositeType * functionInterface = findBaseSpecializing(Builtins::typeFunction);
    if (functionInterface != NULL) {
      if (!equivalent(ftype->returnType(), functionInterface->typeParam(0))) {
        return Incompatible;
      }

      if (!equivalent(ftype->paramTypes(), functionInterface->typeParam(1))) {
        return Incompatible;
      }

      if (ftype->isStatic()) {
        // We currently only support bound methods.
        DFAIL("Implement conversion of non-method functions");
      }

      if (cn.fromValue && cn.resultValue) {
        if (LValueExpr * lval = dyn_cast<LValueExpr>(cn.fromValue)) {
          if (FunctionDefn * method = dyn_cast<FunctionDefn>(lval->value())) {
            if (method->isIntrinsic()) {
              diag.error(lval) << "Intrinsic methods cannot be called indirectly.";
              return Incompatible;
            } else if (method->isCtor()) {
              diag.error(lval) << "Constructors cannot be called indirectly.";
              return Incompatible;
            }

            DASSERT(lval->base() != NULL);
            *cn.resultValue = new BoundMethodExpr(
                lval->location(), lval->base(), method, functionInterface);
            return NonPreferred;
          }
        }

        DFAIL("Implement conversion of non-lvalue functions");
      }

      return NonPreferred;
    }

    return Incompatible;
  }

  return Incompatible;
}

bool CompositeType::isSingular() const {
  return defn_->isSingular();
}

bool CompositeType::isReferenceType() const {
  return (typeClass() == Type::Class || typeClass() == Type::Interface);
}

TypeShape CompositeType::typeShape() const {
  if (shape_ == Shape_Unset) {
    irType();
  }

  return shape_;
}

bool CompositeType::isSubtypeOf(const Type * other) const {
  if (const CompositeType * otherCls = dyn_cast<CompositeType>(other)) {
    return otherCls == this || isSubclassOf(otherCls) ||
        (otherCls->typeClass() == Type::Protocol && otherCls->isSupportedBy(this));
  }

  return false;
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

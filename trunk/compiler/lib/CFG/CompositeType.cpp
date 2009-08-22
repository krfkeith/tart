/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/CompositeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Module.h"
#include "tart/Common/InternedString.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Objects/Builtins.h"

#include <llvm/DerivedTypes.h>

namespace tart {

/// -------------------------------------------------------------------
/// CompositeType

FunctionDefn * CompositeType::defaultConstructor() {
  const SymbolTable::Entry * ctors = findSymbol(istrings.idConstruct);
  if (ctors == NULL) {
    return NULL;
  }

  // Look for a constructor that has zero required parameters.
  for (DefnList::const_iterator it = ctors->begin(); it != ctors->end(); ++it) {
    FunctionDefn * ctor = dyn_cast<FunctionDefn>(*it);
    if (ctor != NULL) {
      ParameterList & params = ctor->functionType()->params();
      int requiredArgCount = 0;
      for (ParameterList::iterator p = params.begin(); p != params.end();++p) {
        if ((*p)->defaultValue() == NULL) {
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
    FunctionDefn * ctor = dyn_cast<FunctionDefn>(*it);
    if (ctor != NULL) {
      ParameterList & params = ctor->functionType()->params();
      int requiredArgCount = 0;
      for (ParameterList::iterator p = params.begin(); p != params.end();++p) {
        if ((*p)->defaultValue() == NULL) {
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

bool CompositeType::lookupMember(const char * name, DefnList & defs, bool inherit) const {
  if (DeclaredType::lookupMember(name, defs, inherit)) {
    return true;
  }

  if (inherit) {
    DASSERT_OBJ(typeDefn()->isPassFinished(Pass_ResolveBaseTypes), this);
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

const llvm::Type * CompositeType::createIRType() const {
  using namespace llvm;

  DASSERT_OBJ(isSingular(), this);
  DASSERT_OBJ(typeDefn()->isPassFinished(Pass_ResolveBaseTypes), this);
  DASSERT_OBJ(typeDefn()->isPassFinished(Pass_AnalyzeFields), this);
  DASSERT(irType_ == NULL);

  // Temporarily get an opaque type to use if this type is self-referential
  OpaqueType * tempIRType = OpaqueType::get(llvm::getGlobalContext());
  irType_ = tempIRType;

  // Members of the class
  std::vector<const llvm::Type *> fieldTypes;

  // Handle inheritance
  if (super_ != NULL) {
    // Base class
    fieldTypes.push_back(super_->getIRType());
  }

  if (typeClass() == Type::Interface) {
    fieldTypes.push_back(PointerType::get(Builtins::typeTypeInfoBlock->getIRType(), 0));
  }

  for (DefnList::const_iterator it = instanceFields_.begin(); it != instanceFields_.end(); ++it) {
    if (*it) {
      VariableDefn * var = static_cast<VariableDefn *>(*it);
      DASSERT_OBJ(var->isPassFinished(Pass_ResolveVarType), var);
      Type * varType = var->getType();
      const llvm::Type * memberType = var->getType()->getIRType();
      if (varType->isReferenceType()) {
        memberType = PointerType::get(memberType, 0);
      }

      fieldTypes.push_back(memberType);
    }
  }

#if 0
  // Handle member fields...
  for (Defn * field = firstMember(); field != NULL; field = field->nextInScope()) {
    switch (field->defnType()) {
    case Defn::Var: {
      // A class member variable.
      VariableDefn * var = static_cast<VariableDefn *>(field);
      Type * varType = var->getType();
      DASSERT_OBJ(varType != NULL, var);

      if (var->storageClass() == Storage_Instance) {
        var->setMemberIndex((int)fieldTypes.size());
        const llvm::Type * memberType = var->getType()->getIRType();
        if (varType->isReferenceType()) {
          memberType = PointerType::get(memberType, 0);
        }

        fieldTypes.push_back(memberType);
      }
      break;
    }

    case Defn::Let: {
      VariableDefn * let = static_cast<VariableDefn *>(field);
      Type * varType = let->getType();
      DASSERT(varType != NULL);

      // Only if it's not a constant.
      if (let->storageClass() == Storage_Instance) {
        assert(false && "implement instance lets");
      } else if (let->initValue() == NULL) {
        diag.debug(let) << "Unimplemented non-constant let " << let;
      } else if (!let->initValue()->isConstant()) {
        diag.debug(let) << "Unimplemented non-constant let " << let;
      }
      break;
    }

    case Defn::Typedef:
    case Defn::Property:
    case Defn::Indexer:
    case Defn::Namespace:
    case Defn::Function:
    //case Expr::Template:
      break;

    default:
      DFAIL("IllegalState");
    }
  }
#endif

  // This is not the normal legal way to do type refinement in LLVM.
  // Normally one would use a PATypeHolder. However, in this case it is assumed
  // that no one is keeping a raw pointer to irType_, except via an LLVM
  // "User" class.
  irType_ = StructType::get(llvm::getGlobalContext(), fieldTypes);
  tempIRType->refineAbstractTypeTo(irType_);
  return irType_;
}

bool CompositeType::isSubclassOf(const CompositeType * base) const {
  if (this == base) {
    return true;
  }

  for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
    if ((*it)->isSubclassOf(base)) {
      return true;
    }
  }

  return false;
}

ConversionRank CompositeType::convertImpl(const Conversion & cn) const {
  const Type * fromType = cn.getFromType();
  if (const CompositeType * fromClass =
      dyn_cast_or_null<CompositeType>(fromType)) {
    if (fromClass == this) {
      if (cn.fromValue && cn.resultValue) {
        *cn.resultValue = cn.fromValue;
      }

      return IdenticalTypes;
    } else if (fromClass->isSubclassOf(this)) {
      if (cn.fromValue && cn.resultValue) {
        *cn.resultValue = new CastExpr(Expr::UpCast,
            cn.fromValue->getLocation(), const_cast<CompositeType *>(this),
            cn.fromValue);
      }

      //return StrengthenRequired;
      return ExactConversion;
    }
  } else if (fromType == &NullType::instance) {
    // Conversion from 'null'.
    if (this->isReferenceType()) {
      if (cn.fromValue && cn.resultValue) {
        *cn.resultValue = new ConstantNull(
            cn.fromValue->getLocation(), const_cast<CompositeType *>(this));
      }

      return ExactConversion;
    }
  }

  return Incompatible;
}

bool CompositeType::isSingular() const {
  return defn_->isSingular();
}

bool CompositeType::isReferenceType() const {
  // TODO: Not if it's a static interface...
  return (typeClass() == Type::Class ||
          typeClass() == Type::Interface);
}

bool CompositeType::isSubtype(const Type * other) const {
  if (const CompositeType * otherCls = dyn_cast<CompositeType>(other)) {
    return otherCls == this || otherCls->isSubclassOf(this);
  }

  return false;
}

bool CompositeType::includes(const Type * other) const {
  if (const CompositeType * otherCls = dyn_cast<CompositeType>(other)) {
    return otherCls == this || isSubclassOf(otherCls);
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

void CompositeType::addMethodDefsToModule(Module * module) {
  DASSERT_OBJ(defn_->isSynthetic(), defn_);

  // Make certain that every method that is referred to from the TIB is XRef'd.
  for (MethodList::iterator m = instanceMethods_.begin(); m != instanceMethods_.end(); ++m) {
    FunctionDefn * method = *m;
    module->addSymbol(method);
  }

  for (InterfaceList::iterator it = interfaces_.begin(); it != interfaces_.end(); ++it) {
    for (MethodList::iterator m = it->methods.begin(); m != it->methods.end(); ++m) {
      FunctionDefn * method = *m;
      module->addSymbol(method);
    }
  }
}

void CompositeType::addStaticDefsToModule(Module * module) {
  for (DefnList::iterator it = staticFields_.begin(); it != staticFields_.end(); ++it) {
    module->addSymbol(*it);
  }

/*  for (Defn * field = firstMember(); field != NULL; field = field->nextInScope()) {
    switch (field->defnType()) {
      case Defn::Var: {
        VariableDefn * var = static_cast<VariableDefn *>(field);
        if (var->storageClass() == Storage_Static) {
          module->addSymbol(var);
        }

        break;
      }
    }
  }*/
}

void CompositeType::addBaseXRefs(Module * module) {
  if (module->addSymbol(typeDefn())) {
    for (ClassList::const_iterator it = bases_.begin(); it != bases_.end(); ++it) {
      (*it)->addBaseXRefs(module);
    }
  }
}

void CompositeType::trace() const {
  DeclaredType::trace();
  markList(bases_.begin(), bases_.end());
  safeMark(super_);
}

} // namespace tart

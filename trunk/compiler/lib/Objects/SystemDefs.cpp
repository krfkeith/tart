/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Module.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/TypeLiteral.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/NamespaceDefn.h"

#include "tart/Objects/TargetSelection.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Common/PackageMgr.h"
#include "tart/Common/Diagnostics.h"

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ScopeBuilder.h"

#include "tart/Parse/Parser.h"

namespace tart {

// -------------------------------------------------------------------
// SystemClass

CompositeType * SystemClass::get() const {
  if (type_ == NULL) {
    type_ = cast<CompositeType>(Builtins::loadSystemType(typeName_));
  }

  return type_;
}

const llvm::Type * SystemClass::irType() const {
  return get()->irType();
}

const llvm::Type * SystemClass::irEmbeddedType() const {
  return get()->irEmbeddedType();
}

const llvm::Type * SystemClass::irParameterType() const {
  return get()->irParameterType();
}

const llvm::Type * SystemClass::irReturnType() const {
  return get()->irReturnType();
}

TypeDefn * SystemClass::typeDefn() const {
  return get()->typeDefn();
}

// -------------------------------------------------------------------
// SystemEnum

EnumType * SystemEnum::get() const {
  if (type_ == NULL) {
    TypeDefn * enumDef = Builtins::getMember<TypeDefn>(definingClass_.get(), typeName_);
    type_ = cast<EnumType>(enumDef->typeValue());
  }

  return type_;
}

const llvm::Type * SystemEnum::irType() const {
  return get()->irType();
}

const llvm::Type * SystemEnum::irEmbeddedType() const {
  return get()->irEmbeddedType();
}

const llvm::Type * SystemEnum::irParameterType() const {
  return get()->irParameterType();
}

const llvm::Type * SystemEnum::irReturnType() const {
  return get()->irReturnType();
}

TypeDefn * SystemEnum::typeDefn() const {
  return get()->typeDefn();
}

// -------------------------------------------------------------------
// SystemNamespace

NamespaceDefn * SystemNamespace::get() const {
  if (ns_ == NULL) {
    ns_ = cast<NamespaceDefn>(Builtins::loadSystemDef(nsName_));
  }

  return ns_;
}

// -------------------------------------------------------------------
// SystemEnumConstant

VariableDefn * SystemEnumConstant::get() const {
  if (value_ == NULL) {
    value_ = Builtins::getMember<VariableDefn>(definingEnum_.get(), name_);
  }

  return value_;
}

VariableDefn * SystemEnumConstant::operator->() const {
  return get();
}

SystemEnumConstant::operator VariableDefn *() const {
  return get();
}

const Type * SystemEnumConstant::type() const {
  return get()->type();
}

int SystemEnumConstant::asInt() const {
  VariableDefn * var = get();
  ConstantInteger * cint = dyn_cast<ConstantInteger>(var->initValue());
  return int(cint->intValue().getSExtValue());
}

}

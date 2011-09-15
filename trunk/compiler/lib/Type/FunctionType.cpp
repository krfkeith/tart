/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/TupleType.h"

#include "tart/Sema/ParameterAssignments.h"
#include "tart/Sema/TypeAnalyzer.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/Hashing.h"

#include "tart/Objects/Builtins.h"

#include "llvm/DerivedTypes.h"

namespace tart {

// -------------------------------------------------------------------
// FunctionType

FunctionType::FunctionType(QualifiedType rtype, ParameterList & plist)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(NULL)
  , paramTypes_(NULL)
  , irType_(NULL)
  , isCreatingType(false)
  , isStructReturn_(false)
  , isInvocable_(false)
{
  for (ParameterList::iterator it = plist.begin(); it != plist.end(); ++it) {
    addParam(*it);
  }
}

FunctionType::FunctionType(QualifiedType rtype, ParameterDefn ** plist, size_t pcount)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(NULL)
  , paramTypes_(NULL)
  , irType_(NULL)
  , isCreatingType(false)
  , isStructReturn_(false)
  , isInvocable_(false)
{
  for (size_t i = 0; i < pcount; ++i) {
    addParam(plist[i]);
  }
}

FunctionType::FunctionType(
    QualifiedType rtype, ParameterDefn * selfParam, ParameterDefn ** plist, size_t pcount)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(selfParam)
  , paramTypes_(NULL)
  , irType_(NULL)
  , isCreatingType(false)
  , isStructReturn_(false)
  , isInvocable_(false)
{
  for (size_t i = 0; i < pcount; ++i) {
    addParam(plist[i]);
  }
}

void FunctionType::addParam(ParameterDefn * param) {
  params_.push_back(param);
}

ParameterDefn * FunctionType::addParam(StringRef name, const Type * ty) {
  ParameterDefn * param = new ParameterDefn(NULL, name, ty, 0);
  addParam(param);
  return param;
}

int FunctionType::paramNameIndex(StringRef name) const {
  for (size_t i = 0; i < params_.size(); i++) {
    ParameterDefn * param = params_[i];
    if (param->name() == name)
      return i;
  }

  return -1;
}

const Type * FunctionType::paramType(int index) const {
  return params_[index]->type();
}

TupleType * FunctionType::paramTypes() const {
  if (paramTypes_ == NULL) {
    QualifiedTypeList typeRefs;
    for (size_t i = 0; i < params_.size(); i++) {
      ParameterDefn * param = params_[i];
      typeRefs.push_back(param->internalType());
    }

    paramTypes_ = TupleType::get(typeRefs);
  }

  return paramTypes_;
}

bool FunctionType::isStructReturn() const {
  DASSERT(irType_ != NULL) << "Getting isStructReturn before irType has been settled.";

  return isStructReturn_;
}

llvm::Type * FunctionType::irType() const {
  if (irType_ == NULL) {
    if (!isCreatingType) {
      irType_ = createIRType();
    }
  }

  return irType_;
}

llvm::Type * FunctionType::createIRType() const {
  using namespace llvm;

  DASSERT_OBJ(isSingular(), this);
//  DASSERT_OBJ(passes_.isFinished(BaseTypesPass), this);
//  DASSERT_OBJ(passes_.isFinished(FieldPass), this);

  // Prevent recursive types from entering this function while type is being
  // created.
  isCreatingType = true;

  // Insert the 'self' parameter if it's an instance method
  const Type * selfType = NULL;
  if (selfParam_ != NULL && !isStatic()) {
    selfType = selfParam_->type();
  }

  // Create the function type
  return createIRFunctionType(selfType, params_, returnType_);
}

llvm::FunctionType * FunctionType::createIRFunctionType(
    const Type * selfType, const ParameterList & params, QualifiedType returnType) const {
  using namespace llvm;

  // Types of the function parameters.
  std::vector<llvm::Type *> parameterTypes;

  // See if we need to use a struct return.
  llvm::Type * rType = returnType->irReturnType();
  if (returnType->typeShape() == Shape_Large_Value) {
    parameterTypes.push_back(rType);
    rType = llvm::Type::getVoidTy(llvm::getGlobalContext());
    isStructReturn_ = true;
  }

  // Insert the 'self' parameter if it's an instance method
  if (selfType != NULL) {
    llvm::Type * argType = selfType->irType();
    if (!isa<PrimitiveType>(selfType) && selfType->typeClass() != Type::Enum) {
      argType = argType->getPointerTo();
    }

    parameterTypes.push_back(argType);
  }

  // Generate the argument signature
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    const ParameterDefn * param = *it;
    const Type * paramType = param->internalType();
    DASSERT_OBJ(paramType != NULL, param);
    parameterTypes.push_back(paramType->irParameterType());
  }

  // Create the function type
  return llvm::FunctionType::get(rType, parameterTypes, false);
}

void FunctionType::trace() const {
  safeMark(returnType_.unqualified());
  safeMark(selfParam_);
  markList(params_.begin(), params_.end());
  safeMark(paramTypes_);
}

llvm::Type * FunctionType::irEmbeddedType() const {
  return irType()->getPointerTo();
//  if (isStatic()) {
//  } else {
//    DFAIL("Plain function type cannot be embedded");
//  }
}

llvm::Type * FunctionType::irParameterType() const {
  if (isStatic()) {
    return irType()->getPointerTo();
  } else {
    DFAIL("Plain function type cannot be passed as a parameter");
  }
}

bool FunctionType::isReferenceType() const {
  return true;
}

Expr * FunctionType::nullInitValue() const {
  return ConstantNull::get(SourceLocation(), this);
}

TypeShape FunctionType::typeShape() const {
  // It's a primitive when used as a function pointer
  return Shape_Primitive;
}

void FunctionType::format(FormatStream & out) const {
  out << "fn (";
  formatParameterList(out, params_);
  out << ")";
  if (returnType_ && !returnType_->isVoidType()) {
    out << " -> " << returnType_;
  }
}

bool FunctionType::isSingular() const {
  if (!returnType_ || !returnType_->isSingular()) {
    return false;
  }

  if (selfParam_ != NULL && (selfParam_->type() == NULL || !selfParam_->type()->isSingular())) {
    return false;
  }

  for (ParameterList::const_iterator it = params_.begin(); it != params_.end(); ++it) {
    const ParameterDefn * param = *it;
    if (param->type() == NULL || !param->type()->isSingular()) {
      return false;
    }
  }

  return true;
}

void FunctionType::whyNotSingular() const {
  if (!returnType_) {
    diag.info() << "Function has unspecified return type.";
  } else if (!returnType_->isSingular()) {
    diag.info() << "Function has non-singular return type.";
  }

  if (selfParam_ != NULL) {
    if (selfParam_->type() == NULL) {
      diag.info() << "Parameter 'self' has unspecified type.";
    } else if (!selfParam_->type()->isSingular()) {
      diag.info() << "Parameter 'self' has non-singular type.";
    }
  }

  for (ParameterList::const_iterator it = params_.begin(); it != params_.end(); ++it) {
    const ParameterDefn * param = *it;
    if (param->type() == NULL) {
      diag.info() << "Parameter '" << param->name() << "' parameter has unspecified type.";
    } else if (!param->type()->isSingular()) {
      diag.info() << "Parameter '" << param->name() << "' parameter has non-singular type.";
    }
  }
}

StringRef FunctionType::invokeName() const {
  if (invokeName_.empty()) {
    if (isStatic_) {
      invokeName_ += ".invoke_static.";
    } else {
      invokeName_ += ".invoke.";
    }
    typeLinkageName(invokeName_, paramTypes());
    if (!returnType_->isVoidType()) {
      invokeName_ += "->";
      typeLinkageName(invokeName_, returnType_);
    }
  }

  return invokeName_;
}

bool FunctionType::hasErrors() const {
  for (ParameterList::const_iterator it = params_.begin(); it != params_.end(); ++it) {
    const ParameterDefn * param = *it;
    if (param->type() == &BadType::instance) {
      return true;
    }
  }

  return returnType_->isErrorType();
}

}

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

#include <llvm/DerivedTypes.h>

namespace tart {

// -------------------------------------------------------------------
// FunctionType

FunctionType::FunctionType(const Type * rtype, ParameterList & plist)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(NULL)
  , paramTypes_(NULL)
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
  , isCreatingType(false)
  , isStructReturn_(false)
  , isInvocable_(false)
{
  for (ParameterList::iterator it = plist.begin(); it != plist.end(); ++it) {
    addParam(*it);
  }
}

FunctionType::FunctionType(const Type * rtype, ParameterDefn ** plist, size_t pcount)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(NULL)
  , paramTypes_(NULL)
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
  , isCreatingType(false)
  , isStructReturn_(false)
  , isInvocable_(false)
{
  for (size_t i = 0; i < pcount; ++i) {
    addParam(plist[i]);
  }
}

FunctionType::FunctionType(
    const Type * rtype, ParameterDefn * selfParam, ParameterDefn ** plist, size_t pcount)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(selfParam)
  , paramTypes_(NULL)
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
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

ParameterDefn * FunctionType::addParam(const char * name, const Type * ty) {
  ParameterDefn * param = new ParameterDefn(NULL, name, ty, NULL);
  addParam(param);
  return param;
}

int FunctionType::paramNameIndex(const char * name) const {
  for (size_t i = 0; i < params_.size(); i++) {
    ParameterDefn * param = params_[i];
    if (param->name() != NULL && strcmp(param->name(), name) == 0)
      return i;
  }

  return -1;
}

bool FunctionType::isSubtype(const Type * other) const {
  // TODO: For self param as well.

  if (const FunctionType * otherFunc = dyn_cast<FunctionType>(other)) {
    (void)otherFunc;
    DFAIL("Implement");
  }

  return false;
}

const Type * FunctionType::paramType(int index) const {
  return params_[index]->type();
}

TupleType * FunctionType::paramTypes() const {
  if (paramTypes_ == NULL) {
    TypeList typeRefs;
    for (size_t i = 0; i < params_.size(); i++) {
      ParameterDefn * param = params_[i];
      typeRefs.push_back(const_cast<Type *>(param->internalType()));
    }

    paramTypes_ = TupleType::get(typeRefs);
  }

  return paramTypes_;
}

bool FunctionType::isStructReturn() const {
  DASSERT_MSG(!isa<llvm::OpaqueType>(irType_.get()),
      "Getting isStructReturn before irType has been settled.");

  return isStructReturn_;
}

const llvm::Type * FunctionType::irType() const {
  if (llvm::OpaqueType * otype = dyn_cast<llvm::OpaqueType>(irType_.get())) {
    if (!isCreatingType) {
      otype->refineAbstractTypeTo(createIRType());
    }
  }

  return irType_;
}

const llvm::Type * FunctionType::createIRType() const {
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

const llvm::FunctionType * FunctionType::createIRFunctionType(
    const Type * selfType, const ParameterList & params, const Type * returnType) const {
  using namespace llvm;

  // Types of the function parameters.
  std::vector<const llvm::Type *> parameterTypes;

  // See if we need to use a struct return.
  const llvm::Type * rType = returnType->irReturnType();
  if (returnType->typeShape() == Shape_Large_Value) {
    parameterTypes.push_back(rType);
    rType = llvm::Type::getVoidTy(llvm::getGlobalContext());
    isStructReturn_ = true;
  }

  // Insert the 'self' parameter if it's an instance method
  if (selfType != NULL) {
    const llvm::Type * argType = selfType->irType();
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
  safeMark(returnType_);
  safeMark(selfParam_);
  markList(params_.begin(), params_.end());
  safeMark(paramTypes_);
}

const llvm::Type * FunctionType::irEmbeddedType() const {
  return irType()->getPointerTo();
//  if (isStatic()) {
//  } else {
//    DFAIL("Plain function type cannot be embedded");
//  }
}

const llvm::Type * FunctionType::irParameterType() const {
  if (isStatic()) {
    return irType()->getPointerTo();
  } else {
    DFAIL("Plain function type cannot be passed as a parameter");
  }
}

bool FunctionType::isEqual(const Type * other) const {
  if (const FunctionType * ft = dyn_cast<FunctionType>(other)) {
    if (ft->params().size() != params_.size()) {
      return false;
    }

    // Note that selfParam types are not compared. I *think* that's right, but
    // I'm not sure.

    if (ft->isStatic() != isStatic()) {
      return false;
    }

    DASSERT(ft->returnType() != NULL);
    if (!ft->returnType()->isEqual(returnType())) {
      return false;
    }

    size_t numParams = params_.size();
    for (size_t i = 0; i < numParams; ++i) {
      if (!params_[i]->type()->isEqual(ft->params_[i]->type())) {
        return false;
      }
    }

    return true;
  }

  return false;
}

bool FunctionType::isReferenceType() const {
  return true;
}

unsigned FunctionType::getHashValue() const {
  IncrementalHash hash;
  hash.add(returnType_->getHashValue());
  hash.add(paramTypes()->getHashValue());
  hash.add(isStatic());
  return hash.end();
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
  if (returnType_ != NULL && !returnType_->isVoidType()) {
    out << " -> " << returnType_;
  }
}

bool FunctionType::isSingular() const {
  if (returnType_ == NULL || !returnType_->isSingular()) {
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
  if (returnType_ == NULL) {
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

ConversionRank FunctionType::convertImpl(const Conversion & cn) const {
  const Type * fromType = dealias(cn.fromType);
  if (isEqual(fromType)) {
    if (cn.resultValue != NULL) {
      *cn.resultValue = cn.fromValue;
    }
    return IdenticalTypes;
  }
  return Incompatible;
}

llvm::StringRef FunctionType::invokeName() const {
  if (invokeName_.empty()) {
    if (isStatic_) {
      invokeName_.append(".invoke_static.");
    } else {
      invokeName_.append(".invoke.");
    }
    typeLinkageName(invokeName_, paramTypes());
    if (!returnType_->isVoidType()) {
      invokeName_.append("->");
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

  return returnType_ == &BadType::instance;
}

}

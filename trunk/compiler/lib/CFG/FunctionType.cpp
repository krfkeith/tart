/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Sema/ParameterAssignments.h"
#include "tart/Common/Diagnostics.h"
#include <llvm/DerivedTypes.h>

namespace tart {

// -------------------------------------------------------------------
// FunctionType

FunctionType::FunctionType(Type * rtype, ParameterList & plist)
  : Type(Function)
  , returnType_(rtype)
  , selfParam_(NULL)
  , irType(llvm::OpaqueType::get())
  , isCreatingType(false)

{
  for (ParameterList::iterator it = plist.begin(); it != plist.end(); ++it) {
    addParam(*it);
  }
}

FunctionType::FunctionType(Type * rtype, ParameterDefn ** plist, size_t pcount)
  : Type(Function)
  , returnType_(rtype)
  , selfParam_(NULL)
  , irType(llvm::OpaqueType::get())
{
  for (size_t i = 0; i < pcount; ++i) {
    addParam(plist[i]);
  }
}

FunctionType::FunctionType(
    Type * rtype, ParameterDefn * selfParam, ParameterDefn ** plist, size_t pcount)
  : Type(Function)
  , returnType_(rtype)
  , selfParam_(selfParam)
  , irType(llvm::OpaqueType::get())
{
  for (size_t i = 0; i < pcount; ++i) {
    addParam(plist[i]);
  }
}

void FunctionType::addParam(ParameterDefn * param) {
  params_.push_back(param);
}

ParameterDefn * FunctionType::addParam(const char * name, Type * ty) {
  ParameterDefn * param = new ParameterDefn(NULL, name, ty, NULL);
  addParam(param);
  return param;
}

int FunctionType::getParamNameIndex(const char * name) const {
  for (size_t i = 0; i < params_.size(); i++) {
    ParameterDefn * param = params_[i];
    if (param->getName() != NULL && strcmp(param->getName(), name) == 0)
      return i;
  }

  return -1;
}

bool FunctionType::isSubtype(const Type * other) const {
  // TODO: For self param as well.
  
  if (const FunctionType * otherFunc = dyn_cast<FunctionType>(other)) {
    DFAIL("Implement");
  }
  
  return false;
}

const llvm::Type * FunctionType::getIRType() const {
  if (!isCreatingType && irType.get()->getTypeID() == llvm::Type::OpaqueTyID) {
    return createIRType();
  }

  return irType;
}

const llvm::Type * FunctionType::createIRType() const {
  using namespace llvm;

  // Prevent recursive types from entering this function while type is being
  // created.
  isCreatingType = true;

  // Types of the function parameters.
  std::vector<const llvm::Type *> parameterTypes;

  // Insert the 'self' parameter if it's an instance method
  if (selfParam_ != NULL) {
    const ParameterDefn * param = selfParam_;
    const llvm::Type * argType = param->getType()->getIRType();
    if (!isa<PrimitiveType>(param->getType())) {
      // TODO: Also don't do this for enums.
      argType = PointerType::getUnqual(argType);
    }
    parameterTypes.push_back(argType);
  }

  // Generate the argument signature
  for (ParameterList::const_iterator it = params_.begin(); it != params_.end();
      ++it) {
    const ParameterDefn * param = *it;
    const llvm::Type * argType = param->getType()->getIRType();
    if (param->getType()->isReferenceType()
      || param->getFlag(ParameterDefn::Reference)) {
      argType = PointerType::getUnqual(argType);
    }

    parameterTypes.push_back(argType);
  }

  // Get the return type
  Type * retType = returnType_;
  if (retType == NULL) {
    retType = &VoidType::instance;
  }

  // Wrap return type in pointer type if needed.
  const llvm::Type * rType = retType->getIRType();
  if (retType->isReferenceType()) {
    rType = PointerType::get(rType, 0);
    //} else if (!rType->isFirstClassType() && rType != llvm::Type::VoidTy) {
    //diag.debug("%s", retType->getCanonicalType()->toString().c_str());
  }

  // Create the function type
  cast<OpaqueType>(irType.get())->refineAbstractTypeTo(
    llvm::FunctionType::get(rType, parameterTypes, false));
  return irType.get();
}

void FunctionType::trace() const {
  safeMark(returnType_);
  markList(params_.begin(), params_.end());
}

bool FunctionType::isReferenceType() const {
  return true;
}

void FunctionType::format(FormatStream & out) const {
  out << "fn (";
  formatParameterList(out, params_);
  out << ")";
  if (returnType_) {
    out << " -> " << returnType_;
  }
}

bool FunctionType::isSingular() const {
  if (returnType_ == NULL || !returnType_->isSingular()) {
    return false;
  }
  
  if (selfParam_ != NULL && (selfParam_->getType() == NULL || !selfParam_->getType()->isSingular())) {
    return false;
  }
  
  for (ParameterList::const_iterator it = params_.begin(); it != params_.end(); ++it) {
    const ParameterDefn * param = *it;
    if (param->getType() == NULL || !param->getType()->isSingular()) {
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
    if (selfParam_->getType() == NULL) {
      diag.info() << "Parameter 'self' has unspecified type.";
    } else if (!selfParam_->getType()->isSingular()) {
      diag.info() << "Parameter 'self' has non-singular type.";
    }
  }
  
  for (ParameterList::const_iterator it = params_.begin(); it != params_.end(); ++it) {
    const ParameterDefn * param = *it;
    if (param->getType() == NULL) {
      diag.info() << "Parameter '" << param->getName() <<
          "' parameter has unspecified type.";
    } else if (!param->getType()->isSingular()) {
      diag.info() << "Parameter '" << param->getName() <<
          "' parameter has non-singular type.";
    }
  }
}

ConversionRank FunctionType::convertImpl(const Conversion & conversion) const {
  DFAIL("Implement");
}

}

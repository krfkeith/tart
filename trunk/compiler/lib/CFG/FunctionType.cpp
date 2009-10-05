/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/FunctionType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Sema/ParameterAssignments.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Common/Diagnostics.h"
#include <llvm/DerivedTypes.h>

namespace tart {

// -------------------------------------------------------------------
// FunctionType

FunctionType::FunctionType(Type * rtype, ParameterList & plist)
  : Type(Function)
  , returnType_(rtype)
  , selfParam_(NULL)
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
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
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
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
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
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
    DFAIL("Implement");
  }

  return false;
}

const llvm::Type * FunctionType::irType() const {
  if (!isCreatingType && irType_.get()->getTypeID() == llvm::Type::OpaqueTyID) {
    return createIRType();
  }

  return irType_;
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
    const llvm::Type * argType = param->type().type()->irType();
    if (!isa<PrimitiveType>(param->type().type())) {
      // TODO: Also don't do this for enums.
      argType = PointerType::getUnqual(argType);
    }
    parameterTypes.push_back(argType);
  }

  // Generate the argument signature
  for (ParameterList::const_iterator it = params_.begin(); it != params_.end(); ++it) {
    const ParameterDefn * param = *it;
    TypeRef paramType = param->internalType();
    DASSERT_OBJ(paramType.isDefined(), param);

    const llvm::Type * argType = paramType.irType();
    if (paramType.isReferenceType() || param->getFlag(ParameterDefn::Reference)) {
      argType = PointerType::getUnqual(argType);
    }

    parameterTypes.push_back(argType);
  }

  // Get the return type
  const Type * retType = returnType_.type();
  if (retType == NULL) {
    retType = &VoidType::instance;
  }

  // Wrap return type in pointer type if needed.
  const llvm::Type * rType = retType->irParameterType();

  // Create the function type
  cast<OpaqueType>(irType_.get())->refineAbstractTypeTo(
    llvm::FunctionType::get(rType, parameterTypes, false));
  return irType_.get();
}

void FunctionType::trace() const {
  returnType_.trace();
  markList(params_.begin(), params_.end());
}

const llvm::Type * FunctionType::irEmbeddedType() const {
  return llvm::PointerType::get(irType(), 0);
}

const llvm::Type * FunctionType::irParameterType() const {
  return llvm::PointerType::get(irType(), 0);
}

bool FunctionType::isReferenceType() const {
  return true;
}

void FunctionType::format(FormatStream & out) const {
  out << "fn (";
  formatParameterList(out, params_);
  out << ")";
  if (returnType_.isDefined()) {
    out << " -> " << returnType_;
  }
}

bool FunctionType::isSingular() const {
  if (!returnType_.isDefined() || !returnType_.isSingular()) {
    return false;
  }

  if (selfParam_ != NULL && (!selfParam_->type().isDefined() || !selfParam_->type().isSingular())) {
    return false;
  }

  for (ParameterList::const_iterator it = params_.begin(); it != params_.end(); ++it) {
    const ParameterDefn * param = *it;
    if (!param->type().isDefined() || !param->type().isSingular()) {
      return false;
    }
  }

  return true;
}

void FunctionType::whyNotSingular() const {
  if (!returnType_.isDefined()) {
    diag.info() << "Function has unspecified return type.";
  } else if (!returnType_.isSingular()) {
    diag.info() << "Function has non-singular return type.";
  }

  if (selfParam_ != NULL) {
    if (!selfParam_->type().isDefined()) {
      diag.info() << "Parameter 'self' has unspecified type.";
    } else if (!selfParam_->type().isSingular()) {
      diag.info() << "Parameter 'self' has non-singular type.";
    }
  }

  for (ParameterList::const_iterator it = params_.begin(); it != params_.end(); ++it) {
    const ParameterDefn * param = *it;
    if (!param->type().isDefined()) {
      diag.info() << "Parameter '" << param->name() << "' parameter has unspecified type.";
    } else if (!param->type().isSingular()) {
      diag.info() << "Parameter '" << param->name() << "' parameter has non-singular type.";
    }
  }
}

ConversionRank FunctionType::convertImpl(const Conversion & conversion) const {
  DFAIL("Implement");
}

}

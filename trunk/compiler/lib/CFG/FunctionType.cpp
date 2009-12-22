/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/FunctionType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TupleType.h"

#include "tart/Sema/ParameterAssignments.h"
#include "tart/Sema/TypeAnalyzer.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"

#include <llvm/DerivedTypes.h>

namespace tart {

// -------------------------------------------------------------------
// FunctionType

FunctionType::FunctionType(Type * rtype, ParameterList & plist)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(NULL)
  , paramTypes_(NULL)
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
  , isCreatingType(false)
  , isStructReturn_(false)
{
  for (ParameterList::iterator it = plist.begin(); it != plist.end(); ++it) {
    addParam(*it);
  }
}

FunctionType::FunctionType(Type * rtype, ParameterDefn ** plist, size_t pcount)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(NULL)
  , paramTypes_(NULL)
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
  , isCreatingType(false)
  , isStructReturn_(false)
{
  for (size_t i = 0; i < pcount; ++i) {
    addParam(plist[i]);
  }
}

FunctionType::FunctionType(
    Type * rtype, ParameterDefn * selfParam, ParameterDefn ** plist, size_t pcount)
  : Type(Function)
  , isStatic_(false)
  , returnType_(rtype)
  , selfParam_(selfParam)
  , paramTypes_(NULL)
  , irType_(llvm::OpaqueType::get(llvm::getGlobalContext()))
  , isCreatingType(false)
  , isStructReturn_(false)
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

  // Prevent recursive types from entering this function while type is being
  // created.
  isCreatingType = true;

  // Insert the 'self' parameter if it's an instance method
  const Type * selfType = NULL;
  if (selfParam_ != NULL && !isStatic()) {
    selfType = selfParam_->type();
  }

  // Get the return type
  const Type * retType = returnType_;
  if (retType == NULL) {
    retType = &VoidType::instance;
  }

  // Create the function type
  return createIRFunctionType(selfType, params_, returnType_);
}

const llvm::FunctionType * FunctionType::createIRFunctionType(
    const Type * selfType, const ParameterList & params, const Type * returnType) const {
  using namespace llvm;

  // Types of the function parameters.
  std::vector<const llvm::Type *> parameterTypes;

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
      argType = PointerType::getUnqual(argType);
    }

    parameterTypes.push_back(argType);
  }

  // Generate the argument signature
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    const ParameterDefn * param = *it;
    const Type * paramType = param->internalType();
    DASSERT_OBJ(paramType != NULL, param);

    const llvm::Type * argType;
    if (paramType->isReferenceType()) {
      argType = PointerType::get(paramType->irType(), 0);
    } else {
      argType = paramType->irParameterType();
    }

    parameterTypes.push_back(argType);
  }

  // Create the function type
  return llvm::FunctionType::get(rType, parameterTypes, false);
}

void FunctionType::trace() const {
  safeMark(returnType_);
  markList(params_.begin(), params_.end());
  safeMark(paramTypes_);
}

const llvm::Type * FunctionType::irEmbeddedType() const {
  return llvm::PointerType::get(irType(), 0);
//  if (isStatic()) {
//  } else {
//    DFAIL("Plain function type cannot be embedded");
//  }
}

const llvm::Type * FunctionType::irParameterType() const {
  if (isStatic()) {
    return llvm::PointerType::get(irType(), 0);
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
    // I'm not sure - if not, we'll need to revise BoundMethodType to tell it
    // not to take selfParam into account.

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
  return Incompatible;
}

const std::string & FunctionType::invokeName() const {
  if (invokeName_.empty()) {
    if (isStatic_) {
      invokeName_.append(".invoke_static.(");
    } else {
      invokeName_.append(".invoke.(");
    }
    typeLinkageName(invokeName_, paramTypes());
    invokeName_.append(")");
    if (!returnType_->isVoidType()) {
      invokeName_.append("->");
      typeLinkageName(invokeName_, returnType_);
    }
  }

  return invokeName_;
}

// -------------------------------------------------------------------
// Type that represents a reference to a 'bound' method.

const llvm::Type * BoundMethodType::irType() const {
  if (llvm::OpaqueType * ty = dyn_cast<llvm::OpaqueType>(irType_.get())) {
    ty->refineAbstractTypeTo(createIRType());
  }

  return irType_.get();
}

const llvm::Type * BoundMethodType::createIRType() const {
  const llvm::FunctionType * irFnType = fnType_->createIRFunctionType(
      Builtins::typeObject, fnType_->params(), fnType_->returnType());

  std::vector<const llvm::Type *> fieldTypes;
  fieldTypes.push_back(llvm::PointerType::get(irFnType, 0));
  fieldTypes.push_back(Builtins::typeObject->irEmbeddedType());
  return llvm::StructType::get(llvm::getGlobalContext(), fieldTypes);
}

ConversionRank BoundMethodType::convertImpl(const Conversion & cn) const {
  if (const BoundMethodType * btFrom = dyn_cast<BoundMethodType>(cn.fromType)) {
    if (fnType_->isEqual(btFrom->fnType())) {
      if (cn.resultValue != NULL) {
        *cn.resultValue = cn.fromValue;
      }

      return IdenticalTypes;
    }

    return Incompatible;
  } else if (const FunctionType * fnFrom = dyn_cast<FunctionType>(cn.fromType)) {
    if (fnType_->isEqual(fnFrom)) {
      if (cn.resultValue != NULL) {
        if (LValueExpr * lval = dyn_cast<LValueExpr>(cn.fromValue)) {
          Expr * base = lval->base();
          if (FunctionDefn * fnVal = dyn_cast<FunctionDefn>(lval->value())) {
            DASSERT(fnFrom->selfParam() != NULL);
            BoundMethodType * bmType = new BoundMethodType(fnFrom);
            BoundMethodExpr * boundMethod = new BoundMethodExpr(lval->location(),
                base, fnVal, bmType);
            *cn.resultValue = boundMethod;
            return ExactConversion;
          } else {
            return Incompatible;
          }
        } else {
          return Incompatible;
        }
      }
    }

    return Incompatible;
  }

  return Incompatible;
}

bool BoundMethodType::isEqual(const Type * other) const {
  if (const BoundMethodType * bmOther = dyn_cast<BoundMethodType>(other)) {
    return fnType_->isEqual(bmOther->fnType());
  }

  return false;
}

bool BoundMethodType::isSubtype(const Type * other) const {
  return isEqual(other);
}

bool BoundMethodType::isReferenceType() const { return false; }
bool BoundMethodType::isSingular() const { return fnType_->isSingular(); }

void BoundMethodType::trace() const {
  fnType_->mark();
}

void BoundMethodType::format(FormatStream & out) const {
  out << fnType_;
}

}

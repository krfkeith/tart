/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Common/Diagnostics.h"
#include <llvm/Support/CommandLine.h>

namespace tart {

// -------------------------------------------------------------------
// CallCandidate

CallCandidate::CallCandidate(CallExpr * call, Expr * baseExpr, FunctionDefn * m,
    const ParameterAssignments & params)
  : callExpr_(call)
  , base_(baseExpr)
  , method_(m)
  , pruningDepth_(0)
  , paramAssignments_(params)
  , bindingEnv_(m->templateSignature())
  , resultType_(m->functionType()->returnType())
  , isTemplate_(false)
{
  if (m->isCtor()) {
    resultType_ = m->functionType()->selfParam()->type();
  }

  ParameterList & methodParams = m->functionType()->params();
  for (ParameterList::iterator p = methodParams.begin(); p != methodParams.end(); ++p) {
    paramTypes_.push_back((*p)->type());
  }

  if (m->isTemplate() || m->isTemplateMember()) {
    // Normalize the return type and parameter types, replacing all pattern variables
    // with a pattern value which will eventually contain the inferred type for that
    // variable.
    for (Defn * def = m; def != NULL && !def->isSingular(); def = def->parentDefn()) {
      TemplateSignature * ts = def->templateSignature();
      if (ts != NULL) {
        isTemplate_ = true;
        size_t numParams = ts->params().size();
        if (numParams > 0) {
          // For each template parameter, create a PatternValue instance.
          for (size_t i = 0; i < numParams; ++i) {
            PatternVar * var = ts->patternVar(i);
            Type * value = bindingEnv_.get(var);
            if (value == NULL) {
              bindingEnv_.bind(var, new PatternValue(&bindingEnv_, var));
            }
          }

          // Substitute all occurances of pattern vars in the result type
          // the corresponding pattern value.
          resultType_ = bindingEnv_.subst(resultType_);

          // Same with function parameter types.
          for (TypeList::iterator pt = paramTypes_.begin(); pt != paramTypes_.end(); ++pt) {
            *pt = bindingEnv_.subst(*pt);
          }
        }
      }
    }

    // Clear all definitions in the environment.
    bindingEnv_.reset();
  }
}

Type * CallCandidate::paramType(int argIndex) const {
  return paramTypes_[parameterIndex(argIndex)];
}

bool CallCandidate::isEqual(const CallCandidate * other) const {
  if (paramAssignments_.size() != other->paramAssignments_.size()) {
    return false;
  }

  if (resultType()->isEqual(other->resultType())) {
    return false;
  }

  size_t argCount = paramAssignments_.size();
  for (size_t i = 0; i < argCount; ++i) {
    Type * t0 = paramType(i);
    Type * t1 = paramType(i);

    if (!t0->isEqual(t1)) {
      return false;
    }
  }

  return true;
}

bool CallCandidate::isMoreSpecific(const CallCandidate * other) const {
  bool same = true;

  if (paramAssignments_.size() != other->paramAssignments_.size()) {
    diag.info() << "different number of args.";
    return false;
  }

  // TODO: Factor in return type.

  /*if (!resultType()->isEqual(other->resultType())) {
    if (!resultType()->isSubtype(other->resultType())) {
      return false;
    }

    same = false;
  }*/

  size_t argCount = paramAssignments_.size();
  for (size_t i = 0; i < argCount; ++i) {
    Type * t0 = paramType(i);
    Type * t1 = other->paramType(i);

    if (!t0->isEqual(t1)) {
      if (!t0->isSubtype(t1)) {
        return false;
      }

      same = false;
    } else {
      bool isEQ = t1->isEqual(t0);
      if (!isEQ) {
        DASSERT(t1->isEqual(t0));
      }
    }
  }

  //if (!same) {
  //  diag.debug(method_) << method_ << " is more specific than " << other->method_;
  //} else {
  //  diag.debug(method_) << method_ << " is the same as " << other->method_;
  //}

  if (same) {
    // If one is a template, than it is less specific than the other.
    // TODO: If they are both templates, choose the one with the smaller number
    // of template parameters. Although explicitly bound parameters should not count, only
    // deduced parameters.
    if (!method_->isTemplate() && other->method()->isTemplate()) {
      return true;
    }
  }

  // Return true if they are not the same.
  return !same;
}

ConversionRank CallCandidate::updateConversionRank() {
  conversionRank_ = IdenticalTypes;
  size_t argCount = callExpr_->argCount();
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr_->arg(argIndex);
    Type * paramType = this->paramType(argIndex);
    conversionRank_ = std::min(conversionRank_, paramType->convert(argExpr));
  }

  Type * expectedReturnType = callExpr_->expectedReturnType();
  if (expectedReturnType != NULL && callExpr_->exprType() != Expr::Construct) {
    conversionRank_ = std::min(conversionRank_, expectedReturnType->convert(resultType_));
  }

  return conversionRank_;
}

bool CallCandidate::unify(CallExpr * callExpr) {
  if (!isTemplate_) {
    return true;
  }

  bindingEnv_.reset();

  // Now, for each parameter attempt unification.
  SourceContext callSite(callExpr, NULL, callExpr);
  SourceContext candidateSite(method_, &callSite, method_, Format_Type);
  size_t argCount = callExpr_->argCount();
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr_->arg(argIndex);
    Type * argType = argExpr->type();
    Type * paramType = this->paramType(argIndex);

    // Skip unsized type integers for now, we'll bind them on the second pass.
    if (!argType->isEqual(&UnsizedIntType::instance)) {
      if (!bindingEnv_.unify(&candidateSite, paramType, argType, Contravariant)) {
        return false;
      }
    }
  }

  // Unify the return type (Pass 1)
  Type * expectedReturnType = callExpr_->expectedReturnType();
  if (expectedReturnType != NULL && !resultType_->isUnsizedIntType()) {
    if (!bindingEnv_.unify(&candidateSite, resultType_, expectedReturnType, Covariant)) {
      return false;
    }
  }

  // Pass 2 for constant integer arguments - choose the size of the integers.
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr_->arg(argIndex);
    Type * argType = argExpr->type();
    Type * paramType = this->paramType(argIndex);

    // Do the unsized types - use the information about previously bound types to determine
    // whether or not to make the integer type signed or unsigned.
    if (argType->isEqual(&UnsizedIntType::instance)) {
      ConstantInteger * cint = cast<ConstantInteger>(argExpr);
      const llvm::APInt & intVal = cint->value()->getValue();
      bool isUnsigned = false;

      // See if the parameter type is an unsigned integer type.
      if (PrimitiveType * ptype = dyn_cast<PrimitiveType>(dealias(paramType))) {
        if (isUnsignedIntegerType(ptype->typeId())) {
          isUnsigned = true;
        }
      }

      // Calculate the sized argument type based on the value of the integer.
      unsigned bitsRequired = isUnsigned ? intVal.getActiveBits() : intVal.getMinSignedBits();
      argType = PrimitiveType::fitIntegerType(bitsRequired, isUnsigned);

      // Try to bind the integer type.
      if (!bindingEnv_.unify(&candidateSite, paramType, argType, Contravariant)) {
        return false;
      }
    }
  }

  // Unify the return type (Pass 2, for unsized integer types.)
  if (expectedReturnType != NULL && resultType_->isUnsizedIntType()) {
    // TODO: Determine the size of the constant integer here.
    if (!bindingEnv_.unify(&candidateSite, resultType_, expectedReturnType, Covariant)) {
      return false;
    }
  }

  return true;
}

bool CallCandidate::isSingular() const {
  return method_->isSingular() && (base_ == NULL || base_->isSingular());
}

void CallCandidate::trace() const {
  safeMark(base_);
  bindingEnv_.trace();
  method_->mark();
}

FormatStream & operator<<(FormatStream & out, const CallCandidate & cc) {
  out << cc.method()->name() << "(";
  for (size_t i = 0; i < cc.callExpr()->argCount(); ++i) {
    if (i != 0) {
      out << ", ";
    }

    out << ":" << cc.paramType(i);
  }

  out << ") -> " << cc.resultType();
}

} // namespace tart

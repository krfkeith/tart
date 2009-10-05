/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/AnalyzerBase.h"
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
  , fnType_(m->functionType())
  , resultType_(m->functionType()->returnType())
  , isTemplate_(false)
{
  if (m->isCtor()) {
    resultType_ = TypeRef(m->functionType()->selfParam()->type());
  }

  ParameterList & methodParams = m->functionType()->params();
  for (ParameterList::iterator p = methodParams.begin(); p != methodParams.end(); ++p) {
    paramTypes_.push_back((*p)->type());
  }

  if (m->isTemplate() || m->isTemplateMember() || m->isPartialInstantiation()) {
    // Normalize the return type and parameter types, replacing all pattern variables
    // with a pattern value which will eventually contain the inferred type for that
    // variable.
    for (Defn * def = m; def != NULL && !def->isSingular(); def = def->parentDefn()) {
      TemplateSignature * ts = def->templateSignature();
      if (ts != NULL) {
        isTemplate_ = true;
        size_t numParams = ts->params().size();
        // For each template parameter, create a PatternValue instance.
        for (size_t i = 0; i < numParams; ++i) {
          PatternVar * var = ts->patternVar(i);
          Type * value = bindingEnv_.get(var);
          if (value == NULL) {
            bindingEnv_.addSubstitution(var, new PatternValue(&bindingEnv_, var));
          }
        }
      }
    }

    // Substitute all occurances of pattern vars in the result type
    // the corresponding pattern value.
    resultType_ = bindingEnv_.relabel(resultType_);
    AnalyzerBase::analyzeType(resultType_, Task_PrepTypeComparison);

    // Same with function parameter types.
    for (TypeRefList::iterator pt = paramTypes_.begin(); pt != paramTypes_.end(); ++pt) {
      *pt = bindingEnv_.relabel(*pt);
      AnalyzerBase::analyzeType(pt->type(), Task_PrepTypeComparison);
    }

    // Clear all definitions in the environment.
    bindingEnv_.reset();
  }
}

CallCandidate::CallCandidate(CallExpr * call, Expr * fnExpr, FunctionType * fnType,
    const ParameterAssignments & params)
  : callExpr_(call)
  , base_(fnExpr)
  , method_(NULL)
  , pruningDepth_(0)
  , paramAssignments_(params)
  , bindingEnv_(NULL)
  , fnType_(fnType)
  , resultType_(fnType->returnType())
  , isTemplate_(false)
{
  DASSERT(fnExpr->isSingular());
  DASSERT(fnType->isSingular());
  ParameterList & methodParams = fnType->params();
  for (ParameterList::iterator p = methodParams.begin(); p != methodParams.end(); ++p) {
    paramTypes_.push_back((*p)->type());
  }
}

TypeRef CallCandidate::paramType(int argIndex) const {
  return paramTypes_[parameterIndex(argIndex)];
}

bool CallCandidate::isEqual(const CallCandidate * other) const {
  if (paramAssignments_.size() != other->paramAssignments_.size()) {
    return false;
  }

  if (resultType().isEqual(other->resultType())) {
    return false;
  }

  size_t argCount = paramAssignments_.size();
  for (size_t i = 0; i < argCount; ++i) {
    TypeRef t0 = paramType(i);
    TypeRef t1 = paramType(i);

    if (!t0.isEqual(t1)) {
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
    TypeRef t0 = paramType(i);
    TypeRef t1 = other->paramType(i);

    if (!t0.isEqual(t1)) {
      if (!t0.type()->isSubtype(t1.type())) {
        return false;
      }

      same = false;
    } else {
      // Ensure that equality is symmetrical.
      DASSERT(t1.isEqual(t0));

      // Variadic parameters are less specific than non-variadic parameters.
      ParameterDefn * p0 = fnType_->param(parameterIndex(i));
      ParameterDefn * p1 = other->fnType_->param(other->parameterIndex(i));
      if (p0->isVariadic()) {
        if (!p1->isVariadic()) {
          return false;
        }
      } else if (p1->isVariadic()) {
        same = false;
      }
    }
  }

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
    TypeRef paramType = this->paramType(argIndex);
    Conversion cn(argExpr);
    cn.matchPatterns = true;
    conversionRank_ = std::min(conversionRank_, paramType.convert(cn));
  }

  Type * expectedReturnType = callExpr_->expectedReturnType();
  if (expectedReturnType != NULL && callExpr_->exprType() != Expr::Construct) {
    Conversion cn(resultType_.type());
    cn.matchPatterns = true;
    conversionRank_ = std::min(conversionRank_, expectedReturnType->convert(cn));
  }

  return conversionRank_;
}

bool CallCandidate::unify(CallExpr * callExpr) {
  if (!isTemplate_) {
    return true;
  }

  DASSERT(method_ != NULL);
  bindingEnv_.reset();

  // Now, for each parameter attempt unification.
  SourceContext callSite(callExpr, NULL, callExpr);
  SourceContext candidateSite(method_, &callSite, method_, Format_Type);
  size_t argCount = callExpr_->argCount();
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr_->arg(argIndex);
    Type * argType = argExpr->type();
    TypeRef paramType = this->paramType(argIndex);

    // Skip unsized type integers for now, we'll bind them on the second pass.
    if (!argType->isEqual(&UnsizedIntType::instance)) {
      if (!bindingEnv_.unify(&candidateSite, paramType.type(), argType, Contravariant)) {
        return false;
      }
    }
  }

  // Unify the return type (Pass 1)
  Type * expectedReturnType = callExpr_->expectedReturnType();
  if (expectedReturnType != NULL && !resultType_.isUnsizedIntType()) {
    if (!bindingEnv_.unify(&candidateSite, resultType_.type(), expectedReturnType, Covariant)) {
      return false;
    }
  }

  // Pass 2 for constant integer arguments - choose the size of the integers.
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr_->arg(argIndex);
    Type * argType = argExpr->type();
    TypeRef paramType = this->paramType(argIndex);

    // Do the unsized types - use the information about previously bound types to determine
    // whether or not to make the integer type signed or unsigned.
    if (argType->isEqual(&UnsizedIntType::instance)) {
      ConstantInteger * cint = cast<ConstantInteger>(argExpr);
      const llvm::APInt & intVal = cint->value()->getValue();
      bool isUnsigned = false;

      // See if the parameter type is an unsigned integer type.
      if (PrimitiveType * ptype = dyn_cast<PrimitiveType>(paramType.dealias())) {
        if (isUnsignedIntegerType(ptype->typeId())) {
          isUnsigned = true;
        }
      }

      // Calculate the sized argument type based on the value of the integer.
      unsigned bitsRequired = isUnsigned ? intVal.getActiveBits() : intVal.getMinSignedBits();
      argType = PrimitiveType::fitIntegerType(bitsRequired, isUnsigned);

      // Try to bind the integer type.
      if (!bindingEnv_.unify(&candidateSite, paramType.type(), argType, Contravariant)) {
        return false;
      }
    }
  }

  // Unify the return type (Pass 2, for unsized integer types.)
  if (expectedReturnType != NULL && resultType_.isUnsizedIntType()) {
    // TODO: Determine the size of the constant integer here.
    if (!bindingEnv_.unify(&candidateSite, resultType_.type(), expectedReturnType, Covariant)) {
      return false;
    }
  }

  // A proper unification requires that each template parameter be bound to something.
  for (Defn * def = method_; def != NULL && !def->isSingular(); def = def->parentDefn()) {
    TemplateSignature * ts = def->templateSignature();
    if (ts != NULL) {
      size_t numParams = ts->params().size();
      // For each template parameter, create a PatternValue instance.
      for (size_t i = 0; i < numParams; ++i) {
        PatternVar * var = ts->patternVar(i);
        Type * value = bindingEnv_.get(var);
        if (value == NULL) {
          return false;
        }
      }
    }
  }

  return true;
}

bool CallCandidate::isSingular() const {
  return (method_ == NULL || method_->isSingular()) && (base_ == NULL || base_->isSingular());
}

void CallCandidate::trace() const {
  safeMark(base_);
  bindingEnv_.trace();
  safeMark(method_);
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

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/TemplateConditions.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Sema/TypeTransform.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Common/Diagnostics.h"
#include "llvm/Support/CommandLine.h"

namespace tart {

// -------------------------------------------------------------------
// CallCandidate

CallCandidate::CallCandidate(CallExpr * call, Expr * baseExpr, FunctionDefn * m,
    const ParameterAssignments & params, SpCandidate * spCandidate)
  : callExpr_(call)
  , base_(baseExpr)
  , method_(m)
  , pruningDepth_(0)
  , paramAssignments_(params)
  , fnType_(m->functionType())
  , resultType_(m->functionType()->returnType())
  , typeParams_(NULL)
  , typeArgs_(NULL)
  , spCandidate_(spCandidate)
  , isTemplate_(false)
{
  if (m->isCtor()) {
    resultType_ = m->functionType()->selfParam()->type();
  }

  if (spCandidate != NULL) {
    bindingEnv_.setSubstitutions(spCandidate->env().substitutions());
  }

  ParameterList & methodParams = m->params();
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
        size_t numParams = ts->typeParams()->size();
        // For each template parameter, create a PatternValue instance.
        for (size_t i = 0; i < numParams; ++i) {
          TypeVariable * var = ts->patternVar(i);
          Type * value = bindingEnv_.get(var);
          if (value == NULL) {
            bindingEnv_.addSubstitution(var, new PatternValue(&bindingEnv_, var));
          }
        }
      }
    }

    if (spCandidate_ != NULL) {
      typeArgs_ = spCandidate_->args();
//      for (TupleType::const_iterator it = ts->typeParams()->begin(); it != ts->typeParams()->end();
//          ++it) {
//        templateParams_.push_back(bindingEnv_.subst(*it));
//      }
    }

    // If there are any explicit type arguments, then we want to relabel the template
    // parameters themselves so that we can unify the type arguments with them.
    TemplateSignature * ts = m->templateSignature();
    if (ts != NULL) {
      if (typeArgs_ != NULL) {
        typeParams_ = cast<TupleType>(bindingEnv_.subst(ts->typeParams()));
      } else {
        typeParams_ = ts->typeParams();
      }
    }

    // Substitute all occurrences of pattern vars in the result type
    // the corresponding pattern value.
    resultType_ = bindingEnv_.subst(resultType_);
    AnalyzerBase::analyzeType(resultType_, Task_PrepTypeComparison);

    // Same with function parameter types.
    for (ConstTypeList::iterator pt = paramTypes_.begin(); pt != paramTypes_.end(); ++pt) {
      *pt = bindingEnv_.subst(*pt);
      AnalyzerBase::analyzeType(*pt, Task_PrepTypeComparison);
    }

    if (ts != NULL) {
      for (TemplateConditionList::const_iterator it = ts->conditions().begin();
          it != ts->conditions().end(); ++it) {
        TemplateCondition * condition = *it;
        SubstitutionTransform subst(bindingEnv_);
        conditions_.push_back(condition->transform(subst));
      }
    }

    // Clear all definitions in the environment.
    bindingEnv_.reset();
  }
}

CallCandidate::CallCandidate(CallExpr * call, Expr * fnExpr, const FunctionType * fnType,
    const ParameterAssignments & params)
  : callExpr_(call)
  , base_(fnExpr)
  , method_(NULL)
  , pruningDepth_(0)
  , paramAssignments_(params)
  , fnType_(fnType)
  , resultType_(fnType->returnType())
  , typeParams_(NULL)
  , typeArgs_(NULL)
  , spCandidate_(NULL)
  , isTemplate_(false)
{
  DASSERT(fnExpr->isSingular());
  DASSERT(fnType->isSingular());
  const ParameterList & methodParams = fnType->params();
  for (ParameterList::const_iterator p = methodParams.begin(); p != methodParams.end(); ++p) {
    paramTypes_.push_back((*p)->type());
  }
}

const Type * CallCandidate::paramType(int argIndex) const {
  return paramTypes_[parameterIndex(argIndex)];
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
    const Type * t0 = paramType(i);
    const Type * t1 = other->paramType(i);

    if (!t0->isEqual(t1)) {
      if (!t0->isSubtype(t1)) {
        return false;
      }

      same = false;
    } else {
      // Ensure that equality is symmetrical.
      DASSERT(t1->isEqual(t0));

      // Variadic parameters are less specific than non-variadic parameters.
      const ParameterDefn * p0 = fnType_->param(parameterIndex(i));
      const ParameterDefn * p1 = other->fnType_->param(other->parameterIndex(i));
      if (p0->isVariadic()) {
        if (!p1->isVariadic()) {
          return false;
        }
      } else if (p1->isVariadic()) {
        same = false;
      }
    }
  }

//  if (same) {
//    if (typeParams_ != NULL && other->typeParams_ != NULL) {
//      ComparisonResult result = compareSpecificity(typeParams_, other->typeParams_);
//      diag.info() << "Comparison result " << result;
//    }
//  }

  if (same) {
    // If one is a template, than it is less specific than the other.
    // TODO: If they are both templates, choose the one with the smaller number
    // of template parameters. Although explicitly bound parameters should not count, only
    // deduced parameters.
    if (!method_->isTemplate() && other->method()->isTemplate()) {
      return true;
    }

    if (method_->isTemplate() && !other->method()->isTemplate()) {
      return false;
    }

    if (typeParams_ == NULL && other->typeParams_ != NULL) {
      return true;
    }

    if (typeParams_ != NULL && other->typeParams_ != NULL) {
    }

    // TODO: This is a temporary kludge - should really compare the two template parameter
    // lists and see which one is more tightly bound.
    if (!method_->hasUnboundTypeParams() && other->method()->hasUnboundTypeParams()) {
      return true;
    }

    if (conditions_.size() > other->conditions_.size()) {
      return true;
    }
  }

  // Return true if they are not the same.
  return !same;
}

ConversionRank CallCandidate::updateConversionRank() {
  conversionRank_ = IdenticalTypes;

  for (TemplateConditionList::const_iterator it = conditions_.begin();
      it != conditions_.end(); ++it) {
    if (!(*it)->eval()) {
      conversionRank_ = Incompatible;
      return Incompatible;
    }
  }

  size_t argCount = callExpr_->argCount();
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr_->arg(argIndex);
    const Type * paramType = this->paramType(argIndex);
    conversionRank_ = std::min(conversionRank_, paramType->canConvert(argExpr, Conversion::Coerce));
  }

  const Type * expectedReturnType = callExpr_->expectedReturnType();
  if (expectedReturnType != NULL && callExpr_->exprType() != Expr::Construct) {
    conversionRank_ = std::min(
        conversionRank_,
        expectedReturnType->canConvert(resultType_, Conversion::Coerce));
  }

  // If there are explicit specializations, then check those too.
  // Note that these must be an exact match.
  if (typeArgs_ != NULL) {
    size_t typeArgCount = typeArgs_->size();
    for (size_t i = 0; i < typeArgCount; ++i) {
      const Type * typeArg = (*typeArgs_)[i];
      const Type * typeParam = (*typeParams_)[i];
      ConversionRank rank = typeParam->canConvert(typeArg);
      if (rank < IdenticalTypes) {
        conversionRank_ = Incompatible;
        break;
      }
    }
  }

  return conversionRank_;
}

bool CallCandidate::unify(CallExpr * callExpr) {
  if (!isTemplate_) {
    return true;
  }

  DASSERT(method_ != NULL);
  if (spCandidate_ != NULL) {
    // TODO: This could equally well be achieved by unifying typeArgs_ with typeParams_.
    bindingEnv_.setSubstitutions(spCandidate_->env().substitutions());
  } else {
    bindingEnv_.setSubstitutions(NULL);
  }

#if 0
  // TODO: An alternate way to do the above
  bindingEnv_.setSubstitutions(NULL);

  // If there are explicit type arguments, then unify them with the template's type parameters.
  if (typeArgs_ != NULL && !bindingEnv_.unify(&candidateSite, typeParams_, typeArgs_, Invariant)) {
    return false;
  }
#endif

  // Now, for each parameter attempt unification.
  SourceContext callSite(callExpr, NULL, callExpr);
  SourceContext candidateSite(method_, &callSite, method_, Format_Type);
  size_t argCount = callExpr_->argCount();
  bool hasUnsizedArgs = false;
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr_->arg(argIndex);
    const Type * argType = argExpr->type();
    const Type * paramType = this->paramType(argIndex);

    // Skip unsized type integers for now, we'll bind them on the second pass.
    if (argType->isEqual(&UnsizedIntType::instance)) {
      hasUnsizedArgs = true;
    } else if (!bindingEnv_.unify(&candidateSite, paramType, argType, Contravariant)) {
      return false;
    }
  }

  // Unify the return type (Pass 1)
  const Type * expectedReturnType = callExpr_->expectedReturnType();
  if (expectedReturnType != NULL) {
    if (resultType_->isUnsizedIntType()) {
      hasUnsizedArgs = true;
    } else if (!bindingEnv_.unify(&candidateSite, resultType_, expectedReturnType, Covariant)) {
      return false;
    }
  }

  // See if any of the variables are bound to an unsized integer. If so, find a sized
  // integer that will work.
  // Pass 2 for constant integer arguments - choose the size of the integers.
  if (hasUnsizedArgs) {
    for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
      Expr * argExpr = callExpr_->arg(argIndex);
      const Type * argType = argExpr->type();
      const Type * paramType = this->paramType(argIndex);

      // Do the unsized types - use the information about previously bound types to determine
      // whether or not to make the integer type signed or unsigned.
      if (argType->isEqual(&UnsizedIntType::instance)) {
        ConstantInteger * cint = cast<ConstantInteger>(argExpr);
        const llvm::APInt & intVal = cint->value()->getValue();
        bool isUnsigned = false;

        // See if the parameter type is a pattern variable.
        if (const PatternValue * pvar = dyn_cast<PatternValue>(dealias(paramType))) {
          if (pvar->value() == NULL) {
            // There are no constraints, so bind it to an int.
            bindingEnv_.addSubstitution(pvar->var(), &IntType::instance);
            continue;
          }
        }

        // See if the parameter type is an unsigned integer type.
        if (const PrimitiveType * ptype = dyn_cast<PrimitiveType>(dealias(paramType))) {
          if (isUnsignedIntegerTypeId(ptype->typeId())) {
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
      if (!bindingEnv_.unify(
          &candidateSite, resultType_, expectedReturnType, Covariant)) {
        return false;
      }
    }
  }

  // A proper unification requires that each template parameter be bound to something.
  for (Defn * def = method_; def != NULL && !def->isSingular(); def = def->parentDefn()) {
    TemplateSignature * ts = def->templateSignature();
    if (ts != NULL) {
      size_t numParams = ts->typeParams()->size();
      // For each template parameter, create a PatternValue instance.
      for (size_t i = 0; i < numParams; ++i) {
        TypeVariable * var = ts->patternVar(i);
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
  return out;
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/SpecializeCandidate.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

SpecializeCandidate::SpecializeCandidate(Expr * base, Defn * tdef)
  : templateDefn_(tdef)
  , base_(base)
{
#if 0
  if (tdef->templateSignature() != NULL) {
    // Normalize the patter variables, replacing all pattern variables with a pattern value
    // which will eventually contain the bound type for that variable.
    TemplateSignature * ts = tdef->templateSignature();
    size_t numParams = ts->params().size();
    if (numParams > 0) {
      // For each template parameter, create a PatternValue instance.
      for (size_t i = 0; i < numParams; ++i) {
        PatternVar * var = ts->patternVar(i);
        Type * value = env_.get(var);
        if (value == NULL) {
          env_.bind(var, new PatternValue(&env_, var));
        }
      }

      // Substitute all occurances of pattern vars in the result type
      // the corresponding pattern value.
      //resultType_ = bindingEnv_.subst(resultType_);

      // Same with function parameter types.
      //for (TypeList::iterator pt = paramTypes_.begin(); pt != paramTypes_.end(); ++pt) {
      //  *pt = bindingEnv_.subst(*pt);
      //}

      // Clear all definitions in the environment.
      env_.reset();
    }
  }
#endif
}

bool SpecializeCandidate::unify(SourceContext * source, const TypeList & args) {
  const TemplateSignature * tsig = templateDefn_->templateSignature();
  DASSERT(tsig->params().size() == args.size());
  for (size_t i = 0; i < args.size(); ++i) {
    Type * pattern = tsig->params()[i];
    Type * value = args[i];
    if (!env_.unify(source, pattern, value, Invariant)) {
      return false;
    }
  }

  const ExprList & reqs = tsig->requirements();
  for (ExprList::const_iterator it = reqs.begin(); it != reqs.end(); ++it) {
    DFAIL("Implement");
  }

  return true;
}

ConversionRank SpecializeCandidate::updateConversionRank(const TypeList & argList) {
  conversionRank_ = IdenticalTypes;

  const TemplateSignature * tsig = templateDefn_->templateSignature();
  for (size_t i = 0; i < argList.size(); ++i) {
    Type * pattern = tsig->params()[i];
    Type * value = argList[i];
    conversionRank_ = std::min(conversionRank_, pattern->canConvert(value));
  }

  return conversionRank_;
}

#if 0
Type * SpecializeCandidate::getParamType(int argIndex) const {
  ParameterList & params = method->functionType()->params();
  return params[getParameterIndex(argIndex)]->type();
}

Type * SpecializeCandidate::getResultType() const {
  return method->functionType()->returnType();
}

bool SpecializeCandidate::isEqual(const SpecializeCandidate * other) const {
  if (paramAssignments.size() != other->paramAssignments.size()) {
    return false;
  }

  if (getResultType()->isEqual(other->getResultType())) {
    return false;
  }

  size_t argCount = paramAssignments.size();
  for (size_t i = 0; i < argCount; ++i) {
    Type * t0 = getParamType(i);
    Type * t1 = getParamType(i);

    if (!t0->isEqual(t1)) {
      return false;
    }
  }

  return true;
}

bool SpecializeCandidate::isMoreSpecific(const SpecializeCandidate * other) const {
  bool same = true;

  if (paramAssignments.size() != other->paramAssignments.size()) {
    diag.info() << "different number of args.";
    return false;
  }

  /*if (!getResultType()->isEqual(other->getResultType())) {
    if (!getResultType()->isSubtype(other->getResultType())) {
      return false;
    }

    same = false;
  }*/

  size_t argCount = paramAssignments.size();
  for (size_t i = 0; i < argCount; ++i) {
    Type * t0 = getParamType(i);
    Type * t1 = other->getParamType(i);

    if (!t0->isEqual(t1)) {
      if (!t0->isSubtype(t1)) {
        return false;
      }

      same = false;
    }
  }

  // Return true if they are not the same.
  return !same;
}

ConversionRank SpecializeCandidate::updateConversionRank(CallExpr * callExpr) {
  conversionRank = IdenticalTypes;
  size_t argCount = callExpr->getInputArgCount();
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr->getInputArg(argIndex);
    Type * paramType = getParamType(argIndex);
    conversionRank = std::min(conversionRank, paramType->canConvert(argExpr));
  }

  return conversionRank;
}
#endif

void SpecializeCandidate::trace() const {
  templateDefn_->mark();
}

} // namespace tart

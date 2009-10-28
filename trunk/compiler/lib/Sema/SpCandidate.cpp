/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

SpCandidate::SpCandidate(Expr * base, Defn * tdef, const TypeVector * args)
  : templateDefn_(tdef)
  , base_(base)
  , args_(args)
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

bool SpCandidate::unify(SourceContext * source) {
  const TemplateSignature * tsig = templateDefn_->templateSignature();
  DASSERT(tsig->params().size() == args_->size());
  for (size_t i = 0; i < args_->size(); ++i) {
    Type * pattern = tsig->params()[i];
    TypeRef value = (*args_)[i];
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

ConversionRank SpCandidate::updateConversionRank() {
  conversionRank_ = IdenticalTypes;

  const TemplateSignature * tsig = templateDefn_->templateSignature();
  for (size_t i = 0; i < args_->size(); ++i) {
    Type * pattern = tsig->params()[i];
    TypeRef value = (*args_)[i];
    conversionRank_ = std::min(conversionRank_, pattern->canConvert(value.type()));
  }

  return conversionRank_;
}

#if 0
bool SpCandidate::isEqual(const SpCandidate * other) const {
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

bool SpCandidate::isMoreSpecific(const SpCandidate * other) const {
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

#endif

void SpCandidate::trace() const {
  templateDefn_->mark();
}

FormatStream & operator<<(FormatStream & out, const SpCandidate & sp) {
  out << sp.templateDefn()->name() << "[";
  for (TypeVector::iterator it = sp.args()->begin(); it != sp.args()->end(); ++it) {
    if (it != sp.args()->begin()) {
      out << ", ";
    }

    out << *it;
  }

  out << "]";
  return out;
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Sema/TypeTransform.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

SpCandidate::SpCandidate(Expr * base, Defn * tdef, TypeVector * args)
  : def_(tdef)
  , base_(base)
  , args_(args)
  , params_(NULL)
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
  const TemplateSignature * tsig = def_->templateSignature();
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
  const TemplateSignature * tsig = def_->templateSignature();
  if (params_ == NULL) {
    TypeRefList typeParams;
    if (def_->hasUnboundTypeParams()) {
      RelabelTransform rt(env_);
      for (TypeList::const_iterator it = tsig->params().begin(); it != tsig->params().end(); ++it) {
        typeParams.push_back(rt.transform(*it));
      }
    } else {
      for (TypeList::const_iterator it = tsig->params().begin(); it != tsig->params().end(); ++it) {
        TypeRef tf = *it;
        typeParams.push_back(*it);
      }
    }

    params_ = TypeVector::get(typeParams);
  }

  conversionRank_ = IdenticalTypes;
  for (size_t i = 0; i < args_->size(); ++i) {
    Type * pattern = (*params_)[i].type();
    TypeRef value = (*args_)[i];
    conversionRank_ = std::min(conversionRank_, pattern->canConvert(value.type()));
  }

  return conversionRank_;
}

bool SpCandidate::isMoreSpecific(const SpCandidate * other) const {
  const TemplateSignature * tsig = def_->templateSignature();
  const TemplateSignature * otsig = other->def_->templateSignature();

  if (tsig->params().size() != otsig->params().size()) {
    return false;
  }

  bool same = true;
  size_t numParams = tsig->params().size();
  for (size_t i = 0; i < numParams; ++i) {
    TypeRef param = tsig->params()[i];
    TypeRef oparam = otsig->params()[i];

    if (!param.isEqual(oparam)) {
      same = false;
      if (!param.isSubtype(oparam)) {
        return false;
      }
    }
  }

  return !same;
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

#endif

void SpCandidate::trace() const {
  def_->mark();
}

FormatStream & operator<<(FormatStream & out, const SpCandidate & sp) {
  out << sp.def()->name() << "[";
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

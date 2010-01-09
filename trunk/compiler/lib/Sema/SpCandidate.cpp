/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/TemplateConditions.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Sema/TypeTransform.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

SpCandidate::SpCandidate(Expr * base, Defn * tdef, TupleType * args)
  : def_(tdef)
  , base_(base)
  , args_(args)
  , params_(NULL)
{
}

bool SpCandidate::unify(SourceContext * source) {
  const TemplateSignature * tsig = def_->templateSignature();
  DASSERT(args_->size() <= tsig->typeParams()->size());
  size_t i;
  for (i = 0; i < args_->size(); ++i) {
    const Type * pattern = tsig->typeParam(i);
    const Type * value = (*args_)[i];
    if (!env_.unify(source, pattern, value, Invariant)) {
      return false;
    }
  }

  for (; i < tsig->typeParams()->size(); ++i) {
    const Type * pattern = tsig->typeParam(i);
    const Type * value = tsig->typeParamDefaults()[i];
    if (!env_.unify(source, pattern, value, Invariant)) {
      return false;
    }
  }

  conditions_.clear();
  for (TemplateConditionList::const_iterator it = tsig->conditions().begin();
      it != tsig->conditions().end(); ++it) {
    TemplateCondition * condition = *it;
    SubstitutionTransform subst(env_);
    conditions_.push_back(condition->transform(subst));
  }

  return true;
}

ConversionRank SpCandidate::updateConversionRank() {
  const TemplateSignature * tsig = def_->templateSignature();
  if (params_ == NULL) {
    TypeList typeParams;
    if (def_->hasUnboundTypeParams()) {
      RelabelTransform rt(env_);
      params_ = cast<TupleType>(rt.transform(tsig->typeParams()));
    } else {
      params_ = tsig->typeParams();
    }
  }

  conversionRank_ = IdenticalTypes;
  for (TemplateConditionList::const_iterator it = conditions_.begin();
      it != conditions_.end(); ++it) {
    if (!(*it)->eval()) {
      conversionRank_ = Incompatible;
      return Incompatible;
    }
  }

  for (size_t i = 0; i < args_->size(); ++i) {
    const Type * pattern = (*params_)[i];
    const Type * value = (*args_)[i];
    conversionRank_ = std::min(conversionRank_, pattern->canConvert(value));
  }

  return conversionRank_;
}

bool SpCandidate::isMoreSpecific(const SpCandidate * other) const {
  const TemplateSignature * tsig = def_->templateSignature();
  const TemplateSignature * otsig = other->def_->templateSignature();

  if (tsig->typeParams()->size() != otsig->typeParams()->size()) {
    return false;
  }

  bool same = true;
  size_t numParams = tsig->typeParams()->size();
  for (size_t i = 0; i < numParams; ++i) {
    const Type * param = tsig->typeParam(i);
    const Type * oparam = otsig->typeParam(i);

    if (!param->isEqual(oparam)) {
      same = false;
      if (!param->isSubtype(oparam)) {
        if (oparam->typeClass() != Type::Pattern) {
          return false;
        }

        // TODO: CanBind check here...
      }
    }
  }

  if (same) {
    // TODO A temporary kludge.
    if (!def_->hasUnboundTypeParams() && other->def_->hasUnboundTypeParams()) {
      return true;
    }
  }

  return !same;
}

void SpCandidate::trace() const {
  def_->mark();
}

FormatStream & operator<<(FormatStream & out, const SpCandidate & sp) {
  out << sp.def()->name() << "[";
  for (TupleType::const_iterator it = sp.args()->begin(); it != sp.args()->end(); ++it) {
    if (it != sp.args()->begin()) {
      out << ", ";
    }

    out << *it;
  }

  out << "]";
  return out;
}

} // namespace tart

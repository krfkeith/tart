/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Template.h"
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
  DASSERT(tsig->typeParams()->size() == args_->size());
  for (size_t i = 0; i < args_->size(); ++i) {
    TypeRef pattern = tsig->typeParam(i);
    TypeRef value = (*args_)[i];
    if (!env_.unify(source, pattern, value, Invariant)) {
      return false;
    }
  }

  const TemplateConditionList & reqs = tsig->conditions();
  for (TemplateConditionList::const_iterator it = reqs.begin(); it != reqs.end(); ++it) {
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
      params_ = cast<TupleType>(rt.transform(tsig->typeParams()));
    } else {
      params_ = tsig->typeParams();
    }
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

  if (tsig->typeParams()->size() != otsig->typeParams()->size()) {
    return false;
  }

  bool same = true;
  size_t numParams = tsig->typeParams()->size();
  for (size_t i = 0; i < numParams; ++i) {
    TypeRef param = tsig->typeParam(i);
    TypeRef oparam = otsig->typeParam(i);


    if (!param.isEqual(oparam)) {
      same = false;
      if (!param.isSubtype(oparam)) {
        if (oparam.type()->typeClass() != Type::Pattern) {
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

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Defn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/TemplateConditions.h"

#include "tart/Type/FunctionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeConversion.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Sema/TypeTransform.h"
#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

#include <algorithm>

namespace tart {

SpCandidate::SpCandidate(Expr * base, Defn * tdef, const TupleType * args)
  : def_(tdef)
  , base_(base)
  , args_(args)
  , params_(NULL)
{
}

void SpCandidate::relabelTypeVars(BindingEnv & env) {
  DASSERT(params_ == NULL);
  TypeList typeParams;
  const Template * tm = def_->templateSignature();
  const TypeList & tmDefaults = def_->templateSignature()->typeParamDefaults();

  if (def_->hasUnboundTypeParams()) {
    size_t numParams = tm->typeParams()->size();
    QualifiedTypeVarMap assignments;

    // For each template parameter, create a TypeAssignment instance.
    for (size_t i = 0; i < numParams; ++i) {
      assignments[tm->patternVar(i)] = env.assign(tm->patternVar(i), NULL, this);
    }

    // Transform the parameters into type assignments.
    RelabelTransform rt(assignments);
    params_ = cast<TupleType>(rt(tm->typeParams()));
    typeParamDefaults_.resize(tmDefaults.size());
    std::transform(tmDefaults.begin(), tmDefaults.end(), typeParamDefaults_.begin(), rt);
  } else {
    typeParamDefaults_.append(tmDefaults.begin(), tmDefaults.end());
    params_ = tm->typeParams();
  }
}

void SpCandidate::relabelTypeVars(RelabelTransform & rt) {
  DASSERT(params_ == NULL);
  const Template * tm = def_->templateSignature();
  params_ = cast<TupleType>(rt(tm->typeParams()));
  const TypeList & tmDefaults = tm->typeParamDefaults();
  typeParamDefaults_.resize(tmDefaults.size());
  std::transform(tmDefaults.begin(), tmDefaults.end(), typeParamDefaults_.begin(), rt);
}

bool SpCandidate::unify(SourceContext * source, BindingEnv & env) {
  const Template * tm = def_->templateSignature();
  DASSERT(args_->size() <= tm->typeParams()->size());
  size_t i;
  for (i = 0; i < args_->size(); ++i) {
    QualifiedType pattern = params_->member(i);
    QualifiedType value = (*args_)[i];
    // We need to do more than unify here.
    if (!env.unify(source, pattern, value, Constraint::EXACT)) {
      return false;
    }
  }

  for (; i < tm->typeParams()->size(); ++i) {
    QualifiedType pattern = params_->member(i);
    QualifiedType value = typeParamDefaults_[i];
    if (!env.unify(source, pattern, value, Constraint::EXACT)) {
      return false;
    }
  }

  env.updateAssignments(SourceLocation(), this);

  // Transform the condition list based on the unification result.
  conditions_.clear();
  if (!tm->conditions().empty()) {
    QualifiedTypeVarMap vars;
    env.toTypeVarMap(vars, this);
    SubstitutionTransform subst(vars);
    for (TemplateConditionList::const_iterator it = tm->conditions().begin();
        it != tm->conditions().end(); ++it) {
      TemplateCondition * condition = *it;
      conditions_.push_back(condition->transform(subst));
    }
  }

  return true;
}

Type * SpCandidate::toType(SourceContext * source, BindingEnv & env) {
  TypeDefn * tdef = cast<TypeDefn>(def_);
  if (AnalyzerBase::analyzeTypeDefn(tdef, Task_PrepMemberLookup)) {
    SourceContext candidateSite(tdef->location(), source, tdef, Format_Type);
    relabelTypeVars(env);
    if (unify(&candidateSite, env)) {
      env.updateAssignments(source->location());
      QualifiedTypeVarMap vars;
      env.toTypeVarMap(vars, this);
      return const_cast<Type *>(tdef->templateSignature()->instantiateType(source->location(), vars));
    } else {
      unify(&candidateSite, env);
      DFAIL("Unify failed...");
    }
  }

  return NULL;
}

ConversionRank SpCandidate::updateConversionRank() {
  DASSERT(params_ != NULL);
  conversionRank_ = IdenticalTypes;
  for (TemplateConditionList::const_iterator it = conditions_.begin();
      it != conditions_.end(); ++it) {
    if (!(*it)->eval()) {
      conversionRank_ = Incompatible;
      return Incompatible;
    }
  }

  for (size_t i = 0; i < args_->size(); ++i) {
    QualifiedType pattern = (*params_)[i];
    QualifiedType value = (*args_)[i];
    AnalyzerBase::analyzeType(value, Task_PrepTypeComparison);
    conversionRank_ = std::min(conversionRank_, TypeConversion::check(value, pattern));
  }

  return conversionRank_;
}

bool SpCandidate::isMoreSpecific(const SpCandidate * other) const {
  const Template * tm = def_->templateSignature();
  const Template * otm = other->def_->templateSignature();

  if (tm->typeParams()->size() != otm->typeParams()->size()) {
    return false;
  }

  bool same = true;
  size_t numParams = tm->typeParams()->size();
  for (size_t i = 0; i < numParams; ++i) {
    QualifiedType param = tm->typeParam(i);
    QualifiedType oparam = otm->typeParam(i);

    if (!TypeRelation::isEqual(param, oparam)) {
      same = false;
      if (!TypeRelation::isSubtype(param, oparam)) {
        if (oparam->typeClass() != Type::TypeVar) {
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

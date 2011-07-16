/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Defn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/TemplateConditions.h"
#include "tart/Defn/FunctionDefn.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/FunctionType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeAlias.h"

#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Sema/TypeTransform.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/Infer/TypeAssignment.h"

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
  , primaryProvision_(NULL)
  , paramAssignments_(params)
  , fnType_(m->functionType())
  , resultType_(m->functionType()->returnType())
  , typeParams_(NULL)
  , typeArgs_(NULL)
  , spCandidate_(spCandidate)
  , isTemplate_(false)
  , trace_(AnalyzerBase::isTraceEnabled(m))
{
  if (m->isCtor()) {
    resultType_ = m->functionType()->selfParam()->type();
  }

  if (spCandidate != NULL) {
    spCandidate_= new SpCandidate(spCandidate->base(), spCandidate->def(), spCandidate->args());
  }

  ParameterList & methodParams = m->params();
  for (ParameterList::iterator p = methodParams.begin(); p != methodParams.end(); ++p) {
    paramTypes_.push_back((*p)->type());
  }
}

CallCandidate::CallCandidate(CallExpr * call, Expr * fnExpr, const FunctionType * fnType,
    const ParameterAssignments & params)
  : callExpr_(call)
  , base_(fnExpr)
  , method_(NULL)
  , pruningDepth_(0)
  , primaryProvision_(NULL)
  , paramAssignments_(params)
  , fnType_(fnType)
  , resultType_(fnType->returnType())
  , typeParams_(NULL)
  , typeArgs_(NULL)
  , spCandidate_(NULL)
  , isTemplate_(false)
  , trace_(false)
{
  DASSERT(fnExpr->isSingular());
  DASSERT(fnType->isSingular());
  const ParameterList & methodParams = fnType->params();
  for (ParameterList::const_iterator p = methodParams.begin(); p != methodParams.end(); ++p) {
    paramTypes_.push_back((*p)->type());
  }
}

void CallCandidate::relabelTypeVars(BindingEnv & env) {
  if (method_ == NULL) {
    DASSERT(spCandidate_ == NULL);
    return;
  }

  // The precondition for all type assignments is that this candidate not be culled.
  if (callExpr_ != NULL && callExpr_->candidates().size() > 1) {
    primaryProvision_ = new CandidateNotCulledProvision(this);
  }

  if (method_->isTemplate() || method_->isTemplateMember() || method_->isPartialInstantiation()) {
    // Normalize the return type and parameter types, replacing all type variables
    // with type assignments which will eventually contain the inferred type for that
    // variable.
    TypeVarMap assignments;
    for (Defn * def = method_; def != NULL && !def->isSingular(); def = def->parentDefn()) {
      Template * ts = def->templateSignature();
      if (ts != NULL) {
        isTemplate_ = true;
        if (typeParams_ == NULL) {
          typeParams_ = ts->typeParams();
        }
        size_t numParams = ts->typeParams()->size();
        // For each template parameter, create a TypeAssignment instance.
        for (size_t i = 0; i < numParams; ++i) {
          TypeVariable * var = ts->patternVar(i);
          if (assignments.count(var) == 0) {
            TypeAssignment * ta = env.assign(var, NULL, this);
            ta->setPrimaryProvision(primaryProvision_);
            assignments[var] = ta;
          }
        }
      }
    }

    // If there are any explicit type arguments, then we want to relabel the template
    // parameters themselves so that we can unify the type arguments with them.
    RelabelTransform relabel(assignments);

    Template * ts = method_->templateSignature();
    if (typeParams_ == NULL) {
      // typeParams_ should have been set above *unless* method_ is a template instance with
      // no unbound params.
      typeParams_ = ts->typeParams();
    }
    if (typeParams_ != NULL) {
      typeParams_ = cast<TupleType>(relabel(typeParams_));
    }

    if (spCandidate_ != NULL) {
      if (spCandidate_->def() != method_) {
        spCandidate_->relabelTypeVars(relabel);
      } else {
        DASSERT(typeParams_ != NULL);
        spCandidate_->setParams(typeParams_);
      }
    }

    // Substitute all occurrences of pattern vars in the result type
    // the corresponding pattern value.
    resultType_ = relabel(resultType_);
    if (!resultType_->isScaffold()) {
      AnalyzerBase::analyzeType(resultType_, Task_PrepTypeComparison);
    }

    // Same with function parameter types.
    for (ConstTypeList::iterator pt = paramTypes_.begin(); pt != paramTypes_.end(); ++pt) {
      const Type * paramType = relabel.transform(*pt);
      if (AnalyzerBase::analyzeType(paramType, Task_PrepTypeComparison)) {
        *pt = paramType;
      }
    }

    if (ts != NULL) {
      for (TemplateConditionList::const_iterator it = ts->conditions().begin();
          it != ts->conditions().end(); ++it) {
        TemplateCondition * condition = *it;
        conditions_.push_back(condition->transform(relabel));
      }
    }
  }
}

const Type * CallCandidate::paramType(int argIndex) const {
  return paramTypes_[parameterIndex(argIndex)];
}

bool CallCandidate::unify(CallExpr * callExpr, BindingEnv & env, FormatStream * errStrm) {
  size_t argCount = callExpr_->argCount();
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    const Type * paramType = this->paramType(argIndex);
    AnalyzerBase::analyzeType(paramType, Task_PrepConversion);
  }

  if (!isTemplate_) {
    return true;
  }

  // Now, for each parameter attempt unification.
  SourceContext callSite(callExpr, NULL, callExpr);
  SourceContext candidateSite(method_, &callSite, method_, Format_Type);

  // Unify explicit template arguments with template parameters.
  if (spCandidate_ != NULL) {
    if (!spCandidate_->unify(&candidateSite, env)) {
      return false;
    }
  }

  //bool hasUnsizedArgs = false;
  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    Expr * argExpr = callExpr_->arg(argIndex);
    const Type * argType = argExpr->type();
    const Type * paramType = this->paramType(argIndex);

    // Skip unsized type integers for now, we'll bind them on the second pass.
    if (!env.unify(&candidateSite, paramType, argType, Constraint::LOWER_BOUND)) {
      if (errStrm) {
        *errStrm << "Argument #" << argIndex + 1 << " type " << paramType <<
            " failed to unify with " << argType;
      }
      return false;
    }
  }

  // Unify the return type
  const Type * expectedReturnType = callExpr_->expectedReturnType();
  if (expectedReturnType != NULL) {
    if (!env.unify(&candidateSite, resultType_, expectedReturnType, Constraint::UPPER_BOUND)) {
      if (errStrm) {
        *errStrm << "Return type " << expectedReturnType << " failed to unify with " << resultType_;
      }
      return false;
    }
  }

  // A proper unification requires that each template parameter be bound to something.
  for (Defn * def = method_; def != NULL && !def->isSingular(); def = def->parentDefn()) {
    Template * ts = def->templateSignature();
    if (ts != NULL) {
      size_t numParams = ts->typeParams()->size();
      // For each template parameter, create a TypeAssignment instance.
      for (size_t i = 0; i < numParams; ++i) {
        TypeVariable * var = ts->patternVar(i);
        const TypeAssignment * ta = env.getAssignment(var, this);
        if (ta->constraints().empty()) {
          if (errStrm) {
            *errStrm << "No binding for template parameter " << var << " in " << env;
          }
          return false;
        }
      }
    }
  }

  return true;
}

ConversionRank CallCandidate::updateConversionRank() {
  conversionRank_ = IdenticalTypes;
  conversionCount_ = 0;

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
    combineConversionRanks(paramType->canConvert(argExpr, Conversion::Coerce));
  }

  const Type * expectedReturnType = callExpr_->expectedReturnType();
  if (expectedReturnType != NULL && callExpr_->exprType() != Expr::Construct) {
    AnalyzerBase::analyzeType(resultType_, Task_PrepTypeComparison);
    combineConversionRanks(expectedReturnType->canConvert(resultType_, Conversion::Coerce));
  }

  // If there are explicit specializations, then check those too.
  // Note that these must be an exact match.
  if (spCandidate_ != NULL) {
    combineConversionRanks(spCandidate_->updateConversionRank());
  }

#if 0
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
#endif

  return conversionRank_;
}

bool CallCandidate::isMoreSpecific(const CallCandidate * other) const {
  bool same = true;

  if (paramAssignments_.size() != other->paramAssignments_.size()) {
    diag.info() << "different number of args.";
    return false;
  }

  // TODO: Factor in return type.

  /*if (!resultType()->isEqual(other->resultType())) {
    if (!resultType()->isSubtypeOf(other->resultType())) {
      return false;
    }

    same = false;
  }*/

  // Note - I think we want to compare candidates in their *unbound* state.
  // So other than type aliases, we don't want to dereference any types.
  size_t argCount = paramAssignments_.size();
  for (size_t i = 0; i < argCount; ++i) {
    const Type * t0 = paramType(i);
    const Type * t1 = other->paramType(i);

    RelativeSpecificity rspec = isMoreSpecific(t0, t1);
    if (rspec == NOT_MORE_SPECIFIC) {
      return false;
    } else if (rspec == MORE_SPECIFIC) {
      same = false;
    } else {
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

  if (same) {
    // If this method has fewer default params than the other, then it is more
    // specific.
    if (method_->params().size() < other->method()->params().size()) {
      return true;
    }

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

CallCandidate::RelativeSpecificity CallCandidate::isMoreSpecific(
    const Type * lhs, const Type * rhs) {
  while (const TypeAlias * alias = dyn_cast<TypeAlias>(lhs)) {
    lhs = alias->value();
    DASSERT_OBJ(lhs != NULL, alias);
  }

  while (const TypeAlias * alias = dyn_cast<TypeAlias>(rhs)) {
    rhs = alias->value();
    DASSERT_OBJ(rhs != NULL, alias);
  }

  DASSERT(!isa<TypeConstraint>(lhs));
  DASSERT(!isa<TypeConstraint>(rhs));

  if (const TypeAssignment * lta = dyn_cast<TypeAssignment>(lhs)) {
    const TypeVariable * ltv = lta->target();
    if (const TypeAssignment * rta = dyn_cast<TypeAssignment>(rhs)) {
      // Both sides are type variables. Check upper bounds.
      const TypeVariable * rtv = rta->target();
      if (ltv->upperBound() != NULL) {
        if (rtv->upperBound() != NULL) {
          return isMoreSpecific(ltv->upperBound(), rtv->upperBound());
        }
        // Both sides are type vars, but only lhs has an upper bound, so lhs is more specific.
        return MORE_SPECIFIC;
      } else if (rtv->upperBound() != NULL) {
        // Both sides are type vars, but only rhs has an upper bound, so lhs is less specific.
        return NOT_MORE_SPECIFIC;
      } else {
        // Neither side has an upper bound, so they rank the same.
        return EQUAL_SPECIFICITY;
      }
    } else {
      if (ltv->upperBound() != NULL &&
          ltv->upperBound()->isSubtypeOf(rhs) &&
          !ltv->upperBound()->isEqual(rhs)) {
        // lhs is a type var whose upper bound is more specific than rhs, so lhs counts as
        // more specific.
        return MORE_SPECIFIC;
      } else {
        // A type variable is less specific than a concrete type.
        return NOT_MORE_SPECIFIC;
      }
    }
  } else if (const TypeAssignment * rta = dyn_cast<TypeAssignment>(rhs)) {
    const TypeVariable * rtv = rta->target();
    if (rtv->upperBound() != NULL &&
        rtv->upperBound()->isSubtypeOf(lhs) &&
        !rtv->upperBound()->isEqual(lhs)) {
      // rhs is a type var whose upper bound is more specific than lhs, so lhs counts as
      // less specific.
      return NOT_MORE_SPECIFIC;
    } else {
      // A type variable is less specific than a concrete type.
      return MORE_SPECIFIC;
    }
  }

  switch (lhs->typeClass()) {
    case Type::Class:
    case Type::Interface:
      if (rhs->typeClass() != Type::Class &&
          rhs->typeClass() != Type::Interface &&
          rhs->typeClass() != Type::Protocol) {
        return NOT_MORE_SPECIFIC;
      }
      break;

    case Type::Struct:
      if (rhs->typeClass() != Type::Struct &&
          rhs->typeClass() != Type::Protocol) {
        return NOT_MORE_SPECIFIC;
      }
      break;

    case Type::Protocol:
      if (rhs->typeClass() != Type::Class &&
          rhs->typeClass() != Type::Interface &&
          rhs->typeClass() != Type::Struct &&
          rhs->typeClass() != Type::Protocol) {
        return NOT_MORE_SPECIFIC;
      }
      break;

    case Type::NAddress:
    case Type::NArray:
    case Type::FlexibleArray:
      if (lhs->typeClass() != rhs->typeClass()) {
        return NOT_MORE_SPECIFIC;
      }

      return isMoreSpecific(lhs->typeParam(0), rhs->typeParam(0));

    default:
      if (lhs->typeClass() != rhs->typeClass()) {
        return NOT_MORE_SPECIFIC;
      }

      break;
  }

  if (lhs->isEqual(rhs)) {
    // Ensure that equality is symmetrical.
    DASSERT_OBJ(rhs->isEqual(lhs), lhs);
    return EQUAL_SPECIFICITY;
  } else if (lhs->isSubtypeOf(rhs)) {
    return MORE_SPECIFIC;
  } else {
    return NOT_MORE_SPECIFIC;
  }
}

void CallCandidate::combineConversionRanks(ConversionRank newRank) {
  // conversionCount_ is the number of conversions that took place
  // at the specified ranking level.
  if (conversionRank_ == newRank) {
    ++conversionCount_;
  } else if (newRank < conversionRank_) {
    conversionRank_ = newRank;
    conversionCount_ = 1;
  }
}

bool CallCandidate::isSingular() const {
  return (method_ == NULL || method_->isSingular()) && (base_ == NULL || base_->isSingular());
}

void CallCandidate::trace() const {
  safeMark(callExpr_);
  safeMark(base_);
  safeMark(method_);
  safeMark(method_);
  safeMark(fnType_);
  safeMark(resultType_);
  safeMark(typeParams_);
  safeMark(spCandidate_);
  markList(paramTypes_.begin(), paramTypes_.end());
  markList(conditions_.begin(), conditions_.end());
}

void CallCandidate::dumpTypeParams() const {
  if (typeParams_ != NULL) {
    for (TupleType::const_iterator it = typeParams_->begin(); it != typeParams_->end(); ++it) {
      if (const TypeAssignment * ta = dyn_cast<TypeAssignment>(*it)) {
        if (ta->constraints().empty()) {
          diag.debug() << "where " << ta << " == {} (empty set)";
        } else if (ta->constraints().size() == 1) {
          Constraint * s = ta->constraints().front();
          diag.debug() << "where " << ta << " " << s->kind() << " " << s->value();
        } else {
          diag.debug() << "where " << ta << ":";
          diag.indent();
          for (ConstraintSet::const_iterator si = ta->begin(); si != ta->end(); ++si) {
            diag.debug() << "" << (*si)->kind() << " " << (*si)->value() <<
                ((*si)->checkProvisions() ? "" : " [culled]");
          }
          diag.unindent();
        }
      }
    }
  }
}

void CallCandidate::format(FormatStream & out) const {
  out << method_->name();
  if (typeParams_ != NULL) {
    out << "[";
    for (TupleType::const_iterator it = typeParams_->begin(); it != typeParams_->end(); ++it) {
      if (it != typeParams_->begin()) {
        out << ", ";
      }
      out << *it;
    }
    out << "]";
  }
  out << "(";
  for (size_t i = 0; i < callExpr_->argCount(); ++i) {
    if (i != 0) {
      out << ", ";
    }

    out << ":" << paramType(i);
  }

  out << ") -> " << resultType();
}

} // namespace tart

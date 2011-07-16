/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Defn/FunctionDefn.h"

#include "tart/Type/AmbiguousParameterType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/CallCandidate.h"

namespace tart {

// -------------------------------------------------------------------
// AmbiguousParameterType

void AmbiguousParameterType::expand(TypeExpansion & out) const {
  const Candidates & cd = callExpr_->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    (*it)->paramType(argIndex_)->expand(out);
  }
}

const Candidates & AmbiguousParameterType::candidates() const {
  return callExpr_->candidates();
}

const Type * AmbiguousParameterType::candidateParamType(const CallCandidate * cc) const {
  return cc->paramType(argIndex_);
}

void AmbiguousParameterType::trace() const {
  Type::trace();
  callExpr_->mark();
}

void AmbiguousParameterType::format(FormatStream & out) const {
  out << "{" << callExpr_->candidates().front()->method()->name() << ".param[" <<
      argIndex_ << "]: ";
  TypeSetConstraint::format(out);
  out << "}";
}

} // namespace tart

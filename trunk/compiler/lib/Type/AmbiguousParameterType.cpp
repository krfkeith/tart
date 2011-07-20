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

void AmbiguousParameterType::listProspects(ProspectList & out, const ProvisionSet & add) const {
  const Candidates & cd = callExpr_->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    CallCandidate * cc = *it;
    ProvisionSet ccProvisions(add);
    ccProvisions.insertIfValid(cc->primaryProvision());
    if (ccProvisions.isConsistent()) {
      AmbiguousType::listProspects(out, cc->paramType(argIndex_), ccProvisions);
    }
  }
}

void AmbiguousParameterType::expand(TypeExpansion & out) const {
  const Candidates & cd = callExpr_->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    (*it)->paramType(argIndex_)->expand(out);
  }
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

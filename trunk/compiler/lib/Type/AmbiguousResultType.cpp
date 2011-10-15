/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Defn/FunctionDefn.h"

#include "tart/Type/AmbiguousResultType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/CallCandidate.h"

namespace tart {

// -------------------------------------------------------------------
// AmbiguousResultType

void AmbiguousResultType::listProspects(ProspectList & out, const ProvisionSet & add) const {
  const Candidates & cd = callExpr_->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    CallCandidate * cc = *it;
    ProvisionSet ccProvisions(add);
    ccProvisions.insertIfValid(cc->primaryProvision());
    if (ccProvisions.isConsistent()) {
      AmbiguousType::listProspects(out, candidateResultType(cc), ccProvisions);
    }
  }
}

void AmbiguousResultType::expandImpl(QualifiedTypeSet & out, unsigned qualifiers) const {
  const Candidates & cd = callExpr_->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    candidateResultType(*it).expand(out, qualifiers);
  }
}

QualifiedType AmbiguousResultType::candidateResultType(const CallCandidate * cc) const {
//  if (callExpr_->exprType() == Expr::Construct && cc->method()->isCtor()) {
//    return cc->method()->functionType()->selfParam()->type();
//  }

  return cc->resultType();
}

void AmbiguousResultType::trace() const {
  Type::trace();
  callExpr_->mark();
}

void AmbiguousResultType::format(FormatStream & out) const {
  QualifiedType singularType = callExpr_->singularResultType();
  if (singularType) {
    out << singularType;
    return;
  }

  out << "{Result: ";
  TypeSetConstraint::format(out);
  out << "}";
}

} // namespace tart

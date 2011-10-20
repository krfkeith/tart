/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Type/AmbiguousPhiType.h"
#include "tart/Type/TupleType.h"

#include "tart/Sema/Infer/ConversionSite.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/Support/CommandLine.h"

namespace tart {

// -------------------------------------------------------------------
// AssignmentSite

void AssignmentSite::update() {
  const AssignmentExpr * assign = static_cast<const AssignmentExpr *>(expr_);
  rank_ = TypeConversion::check(assign->fromExpr(), assign->toExpr()->type());
}

// -------------------------------------------------------------------
// TupleCtorSite

void TupleCtorSite::update() {
  rank_ = IdenticalTypes;
  TupleCtorExpr * tct = static_cast<TupleCtorExpr *>(expr_);
  const TupleType * tt = dyn_cast<TupleType>(tct->type().unqualified());
  size_t size = tt->size();
  for (size_t i = 0; i < size; ++i) {
    rank_ = std::min(rank_, TypeConversion::check(tct->arg(i), tt->member(i)));
  }
}

// -------------------------------------------------------------------
// PHISite

void PHISite::update() {
  Qualified<AmbiguousPhiType> phiType = expr_->type().cast<AmbiguousPhiType>();
  QualifiedTypeSet types;
  QualifiedType solution = NULL;

  phiType.expand(types);
  if (phiType->expected()) {
    solution = phiType->expected();
  } else {
    // Try to find a common type that encompasses all input types.
    // TODO: Use coercions if needed.
    for (QualifiedTypeSet::iterator it = types.begin(); it != types.end(); ++it) {
      QualifiedType ty = *it;
      if (!solution) {
        solution = ty;
      } else {
        solution = Type::commonBase(solution, ty);
        if (!solution) {
          break;
        }
      }
    }
  }

  phiType->setCommon(solution);
  if (!solution) {
    // Compute the lowest conversion ranking of all input types to the solution.
    rank_ = IdenticalTypes;
    for (QualifiedTypeSet::iterator it = types.begin(); it != types.end(); ++it) {
      rank_ = std::min(rank_, TypeConversion::check(*it, solution, TypeConversion::COERCE));
    }
  }
}

void PHISite::report() {
  Qualified<AmbiguousPhiType> phiType = expr_->type().cast<AmbiguousPhiType>();
  QualifiedTypeSet types;
  phiType.expand(types);
  diag.info() << "expression: " << expr_;
  if (phiType->expected()) {
    diag.info() << "expected result type: " << phiType->expected();
  }
  for (QualifiedTypeSet::iterator it = types.begin(); it != types.end(); ++it) {
    diag.info() << "expression type: " << *it;
  }
}

} // namespace tart

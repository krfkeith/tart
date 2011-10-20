/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_CONVERSIONSITE_H
#define TART_SEMA_INFER_CONVERSIONSITE_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

#ifndef TART_SEMA_INFER_CALLSITE_H
#include "tart/Sema/Infer/CallSite.h"
#endif

#ifndef TART_SEMA_BINDINGENV_H
#include "tart/Sema/BindingEnv.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A conversion site represents a point in the expression tree where
/// some type conversion takes place, but which does not involve a
/// function call.
///
/// Unlike call sites, there are no candidates or template parameters
/// to be determined, in other words there are no decisions to be made.
/// Conversion sites are only used to measure how good the current
/// type inference solution is - the ranking for each site is factored
/// in to the overall ranking for the solution.

struct ConversionSite {
public:
  ConversionSite()
    : expr_(NULL)
    , rank_(IdenticalTypes)
  {}

  ConversionSite(Expr * ex)
    : expr_(ex)
    , rank_(IdenticalTypes)
  {}

  ConversionSite(const ConversionSite & src)
    : expr_(src.expr_)
    , rank_(src.rank_)
  {}

  virtual ~ConversionSite() {}

  const ConversionSite & operator=(const ConversionSite & src) {
    expr_ = src.expr_;
    rank_ = src.rank_;
    return *this;
  }

  // The expression which is the source of the constraint.
  const Expr * expr() const { return expr_; }

  // Update conversion rankings
  virtual void update() = 0;

  // Report errors
  //virtual void reportErrors() const {}

  /** Conversion ranking for this constraint. */
  ConversionRank rank() const { return rank_; }

protected:
  Expr * expr_;              // Expression that defines the constraint.
  ConversionRank rank_;
};

typedef llvm::SmallVector<ConversionSite *, 16> ConversionSites;

/// -------------------------------------------------------------------
/// A conversion site for assignment statements.

class AssignmentSite : public ConversionSite {
public:
  AssignmentSite(AssignmentExpr * in) : ConversionSite(in) {}

  void update();
};

/// -------------------------------------------------------------------
/// A conversion site for tuple constructors. This ranks each member
/// of the tuple separately.

class TupleCtorSite : public ConversionSite {
public:
  TupleCtorSite(TupleCtorExpr * in) : ConversionSite(in) {}

  void update();
};

/// -------------------------------------------------------------------
/// A conversion site for PHI-class expressions, that is expressions which
/// choose one of several alternate values to return. (Examples being
/// 'if' and 'switch' expressions.)

class PHISite : public ConversionSite {
public:
  PHISite(Expr * in) : ConversionSite(in) {}

  void update();
  void report();
};

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_CALLSITE_H
#define TART_SEMA_INFER_CALLSITE_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

#ifndef TART_TYPE_TYPECONVERSION_H
#include "tart/Type/TypeConversion.h"
#endif

#ifndef TART_EXPR_EXPRS_H
#include "tart/Expr/Exprs.h"
#endif

namespace tart {

class BindingEnv;
class ProvisionSet;

/// -------------------------------------------------------------------
/// Represents a pointer in the expression tree where a function call
/// takes place.
///
/// Each call site may have one or more 'candidate' functions which
/// were selected as a result of the function name lookup. For each
/// site, the type inference algorithm will  determine (a) which candidate
/// is the best match against the calling arguments, and (b) what type
/// expressions should be bound to the candidate's template parameters,
/// if it has any.

class CallSite {
public:
  typedef Candidates::iterator iterator;
  typedef Candidates::const_iterator const_iterator;

  CallSite()
    : callExpr_(NULL)
    , remaining_(0)
    , lowestRank_(IdenticalTypes)
    , best_(NULL)
  {}

  CallSite(CallExpr * call)
    : callExpr_(call)
    , remaining_(0)
    , lowestRank_(IdenticalTypes)
    , best_(NULL)
  {}

  /** The call expression. */
  const CallExpr * expr() const { return callExpr_; }

  /** Return the number of candidate functions. */
  size_t count() const {
    return callExpr_->candidates().size();
  }

  /** Iterate through the candidates. */
  iterator begin() { return callExpr_->candidates().begin(); }
  const_iterator begin() const { return callExpr_->candidates().begin(); }
  iterator end() { return callExpr_->candidates().end(); }
  const_iterator end() const { return callExpr_->candidates().end(); }

  /** Return the Nth candidate. */
  CallCandidate * candidate(unsigned index) {
    return callExpr_->candidates()[index];
  }

  /** Update current conversion rankings. */
  void update();

  /** Return the current worst-case conversion rank. */
  ConversionRank rank() const { return lowestRank_; }

  /** Return the number of viable choices remaining after culling. */
  int remaining() const { return remaining_; }

  /** Replace all type variables in this expression with unique variables, so that we
      can distinguish between multiple occurances of the same template in an expression. */
  void relabelVars(BindingEnv & env);

  /** Do unification of the operation with its arguments. */
  bool unify(BindingEnv & env, int cullingDepth);

  /** Return true if the specified choice has been culled. */
  bool isCulled(int ch) const;

  /** Cull a specific numbered choice. */
  void cull(int choice, int searchDepth);

  /** Cull all choices lower than a given conversion rank. */
  int cullByConversionRank(ConversionRank lowerLimit, int searchDepth);

  /** Cull all choices which are less specific than others. */
  void cullBySpecificity(int searchDepth);

  /** Cull all choices except for the numbered choice. */
  void cullAllExcept(int choice, int searchDepth);

  /** Cull all choices except for the best choice. */
  void cullAllExceptBest(int searchDepth);

  /** Undo all cullings of greater depth than 'searchDepth'. */
  void backtrack(int searchDepth);

  /** Return true if choice 'choice' depends on any provision in 'p'. */
  bool dependsOn(int choice, const ProvisionSet & pset) const;

  /** True if any of the expressions in this call site have errors. */
  bool hasErrors() const;

  /** Eliminate culled candidates. */
  void finish();

  /** Generate a printable version of the input calling signature. */
  void formatCallSignature(FormatStream & out);

  /** Report the given error message, along with the calling signature and the candidates. */
  void reportErrors(const char * msg);

  /** Print debugging info. */
  void reportRanks();

  /** Save the single best candidate. */
  void saveBest();

private:
  /** The overloaded call. */
  CallExpr * callExpr_;

  /** Number of possible overload choices remaining after pruning. */
  int remaining_;

  /** The lowest compatibility score of all choices. */
  ConversionRank lowestRank_;

  /** The best candidate for the current solution being tested. */
  CallCandidate * best_;
};

typedef llvm::SmallVector<CallSite *, 16> CallSites;

} // namespace tart

#endif

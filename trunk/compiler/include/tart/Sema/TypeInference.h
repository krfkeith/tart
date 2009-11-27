/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_TYPEINFERENCE_H
#define TART_SEMA_TYPEINFERENCE_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

#ifndef TART_SEMA_BINDINGENV_H
#include "tart/Sema/BindingEnv.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Represents a call site within an expression.
struct CallSite {
  /** The overloaded call. */
  CallExpr * callExpr;

  /** Number of possible overload choices remaining after pruning. */
  int numRemaining;

  /** The lowest compatibility score of all choices. */
  ConversionRank lowestRank;

  /** The best candidate for the current solution being tested. */
  CallCandidate * best;

  CallSite()
    : callExpr(NULL)
    , numRemaining(0)
    , lowestRank(IdenticalTypes)
    , best(NULL)
  {}

  CallSite(CallExpr * call)
    : callExpr(call)
    , numRemaining(0)
    , lowestRank(IdenticalTypes)
    , best(NULL)
  {}

  CallSite(const CallSite & src)
    : callExpr(src.callExpr)
    , numRemaining(src.numRemaining)
    , lowestRank(src.lowestRank)
    , best(src.best)
  {}

  const CallSite & operator=(const CallSite & src) {
    callExpr = src.callExpr;
    numRemaining = src.numRemaining;
    lowestRank = src.lowestRank;
    best = src.best;
    return *this;
  }

  // Update conversion rankings
  void update();

  // Eliminate from consideration any culld candidates
  void removeCulled();

  // Eliminate culled candidates
  void finish();

  // Generate a printable version of the input calling signature.
  void formatCallSignature(FormatStream & out);

  // Report the given error message, along with the calling signature and the candidates.
  void reportErrors(const char * msg);
};

typedef llvm::SmallVector<CallSite, 16> CallSiteList;
typedef llvm::SmallSet<const Expr *, 16> ExprSet;

/// -------------------------------------------------------------------
/// Represents an additional type constraint that is not a call.
struct ConstraintSite {
public:
  Expr * expr;              // Expression that defines the constraint.
  ConversionRank rank;      // Conversion ranking for this constraint.

  ConstraintSite()
    : expr(NULL)
    , rank(IdenticalTypes)
  {}

  ConstraintSite(Expr * ex)
    : expr(ex)
    , rank(IdenticalTypes)
  {}

  ConstraintSite(const ConstraintSite & src)
    : expr(src.expr)
    , rank(src.rank)
  {}

  const ConstraintSite & operator=(const ConstraintSite & src) {
    expr = src.expr;
    rank = src.rank;
    return *this;
  }

  // Update conversion rankings
  void update();
};

typedef llvm::SmallVector<ConstraintSite, 16> ConstraintSiteList;

/// -------------------------------------------------------------------
/// Pass to collect all of the constraints in the expression tree.
class GatherConstraintsPass : public CFGPass {
public:
  GatherConstraintsPass(CallSiteList & callSites, ConstraintSiteList & cstrSites)
    : callSites_(callSites)
    , cstrSites_(cstrSites)
  {}

private:
  CallSiteList & callSites_;
  ConstraintSiteList & cstrSites_;
  ExprSet visited_;

  Expr * visitCall(CallExpr * in);
  Expr * visitAssign(AssignmentExpr * in);
  Expr * visitPostAssign(AssignmentExpr * in);
};

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class TypeInferencePass {
public:
  /** Run this pass on the specified expression. */
  static Expr * run(Expr * in, const Type * expected, bool strict = true);

private:
  Expr * rootExpr_;
  const Type * expectedType_;
  CallSiteList callSites_;
  ConstraintSiteList cstrSites_;
  ConversionRank lowestRank_;
  ConversionRank bestSolutionRank_;
  int bestSolutionCount_;
  int cullCount_;
  int searchDepth_;
  bool underconstrained_;
  bool overconstrained_;
  bool strict_;

  // Map of template parameter substitutions
  //BindingMap substitutions_;

  TypeInferencePass(Expr * root, const Type * expected, bool strict = true)
    : rootExpr_(root)
    , expectedType_(expected)
    , searchDepth_(0)
    , strict_(strict)
  {}

  Expr * runImpl();

  void reportRanks(bool final);
  void update();
  void checkSolution();

  bool unifyCalls();

  // Various pruning strategies.

  /** Remove from consideration overloads that have a low conversion rank. */
  void cullByConversionRank();
  void cullByConversionRank(ConversionRank lowerLimit);
  void cullByConversionRank(ConversionRank lowerLimit, CallSite & site);

  /** Remove from consideration overloads that are less specific. */
  void cullBySpecificity();
  void cullBySpecificity(CallSite & site);

  /** Try removing each from consideration and re-evaluate all conversion
      ranks for the entire AST. Choose the one with the best rank. */
  void cullByElimination();
  void cullByElimination(CallSiteList::iterator first, CallSiteList::iterator last);
  void cullByElimination(CallSite & site);

  /** Cull all candidates for a given site except for cc. */
  void cullAllButOne(CallSite & site, CallCandidate * cc);

  // Undo the last pruning
  void backtrack();
};

} // namespace tart

#endif

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
};

typedef llvm::SmallVector<CallSite, 16> CallSiteList;
typedef llvm::SmallSet<CallExpr *, 16> CallExprSet;

/// -------------------------------------------------------------------
/// Pass to collect all of the constraints in the expression tree.
class GatherConstraintsPass : public CFGPass {
public:
  GatherConstraintsPass(CallSiteList & oll) : callSites(oll) {}

private:
  CallSiteList & callSites;
  CallExprSet calls;

  Expr * visitCall(CallExpr * in);
};

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class TypeInferencePass {
public:
  /** Run this pass on the specified expression. */
  static Expr * run(Expr * in, Type * expected, bool strict = true);

private:
  Expr * rootExpr;
  Type * expectedType;
  CallSiteList callSites;
  ConversionRank lowestRank;
  ConversionRank bestSolutionRank;
  int bestSolutionCount;
  int cullCount;
  int searchDepth;
  bool underconstrained;
  bool overconstrained;
  bool strict_;

  // Map of template parameter substitutions
  //BindingMap substitutions_;

  TypeInferencePass(Expr * root, Type * expected, bool strict = true)
    : rootExpr(root)
    , expectedType(expected)
    , searchDepth(0)
    , strict_(strict)
  {}

  Expr * runImpl();

  void reportRanks();
  void update();
  void checkSolution();

  void unifyCalls();

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

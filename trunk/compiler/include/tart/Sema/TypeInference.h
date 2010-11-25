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

class CullableChoicePoint;

/// -------------------------------------------------------------------
/// Represents a point in the expression hierarchy where the AST node
/// maps to multiple possible items. The type inferencer attempts to
/// narrow the possibilities to a single choice for each point.
class ChoicePoint {
public:

  virtual ~ChoicePoint() {}

  // Update conversion rankings
  virtual void update() = 0;

  /** Return the total number of choices. */
  virtual size_t count() const = 0;

  /** Return the number of viable choices remaining after culling. */
  virtual int remaining() const = 0;

  /** Return the current worst-case conversion rank. */
  virtual ConversionRank rank() const = 0;

  /** Do unification of the operation with its arguments. */
  virtual bool unify(int cullingDepth) = 0;

  /** Generate a printable version of this choice point's expression. */
  virtual const Expr * expr() const = 0;

  // Eliminate culled candidates
  virtual void finish() = 0;

  // Report the given error message, along with the calling signature and the candidates.
  virtual void reportErrors(const char * msg) = 0;

  // Print debugging info
  virtual void reportRanks() = 0;

  /** Downcast. */
  virtual CullableChoicePoint * asCullable() { return NULL; }
};

/// -------------------------------------------------------------------
class CullableChoicePoint : public ChoicePoint {
public:
  /** Return true if the specified choice has been culled. */
  virtual bool isCulled(int index) const = 0;

  /** Cull all choices lower than a given conversion rank. */
  virtual int cullByConversionRank(ConversionRank lowerLimit, int searchDepth) = 0;

  /** Cull all choices which are less specific than others. */
  virtual void cullBySpecificity(int searchDepth) = 0;

  /** Cull all choices except for the numbered choice. */
  virtual void cullAllExcept(int choice, int searchDepth) = 0;

  /** Cull all choices except for the best choice. */
  virtual void cullAllExceptBest(int searchDepth) = 0;

  /** Eliminate from consideration any culled candidates. */
  virtual void removeCulled() = 0;

  /** Save the single best candidate. */
  virtual void saveBest() = 0;

  /** Undo all cullings of greater than search depth. */
  virtual void backtrack(int searchDepth) = 0;
};

/// -------------------------------------------------------------------
/// Represents a call site within an expression.
class CallSite : public CullableChoicePoint {
public:
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

  void update();
  size_t count() const;
  ConversionRank rank() const { return lowestRank_; }
  int remaining() const { return remaining_; }
  bool unify(int cullingDepth);
  bool isCulled(int ch) const;
  int cullByConversionRank(ConversionRank lowerLimit, int searchDepth);
  void cullBySpecificity(int searchDepth);
  void cullAllExcept(int choice, int searchDepth);
  void cullAllExceptBest(int searchDepth);
  void removeCulled();
  void backtrack(int searchDepth);
  const Expr * expr() const { return callExpr_; }
  CullableChoicePoint * asCullable() { return this; }

  // Eliminate culled candidates
  void finish();

  // Generate a printable version of the input calling signature.
  void formatCallSignature(FormatStream & out);

  // Report the given error message, along with the calling signature and the candidates.
  void reportErrors(const char * msg);
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

typedef llvm::SmallVector<ChoicePoint *, 16> CallSiteList;
typedef llvm::SmallSet<const Expr *, 16> ExprSet;

/// -------------------------------------------------------------------
/// A choice point for constant integers.
class ConstantIntegerSite : public ChoicePoint {
public:
  // Update conversion rankings
  void update();
  size_t count() const;
  int remaining() const;
  ConversionRank rank() const;
  bool isCulled(int index) const;
  int cullByConversionRank(ConversionRank lowerLimit, int searchDepth);
  void cullBySpecificity(int searchDepth);
  void cullAllExcept(int choice, int searchDepth);
  void cullAllExceptBest(int searchDepth);
  void removeCulled() {}
  const Expr * expr() const { return expr_; }

private:
  ConstantInteger * expr_;
  int remaining_;
  ConversionRank lowestRank_;
};

/// -------------------------------------------------------------------
/// A choice point for tuple constructors.
class TupleCtorSite : public ChoicePoint {
public:
  TupleCtorSite(TupleCtorExpr * in);

  // Update conversion rankings
  void update();
  size_t count() const { return 1; }
  int remaining() const { return 1; }
  ConversionRank rank() const { return rank_; }
  bool unify(int cullingDepth);
  void removeCulled() {}
  void finish() {}
  void reportErrors(const char * msg) {}
  void reportRanks();
  const Expr * expr() const { return expr_; }

private:
  TupleCtorExpr * expr_;
  ConversionRank rank_;
};

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
  Expr * visitTupleCtor(TupleCtorExpr * in);
};

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class TypeInferencePass {
public:
  /** Run this pass on the specified expression. */
  static Expr * run(Module * module, Expr * in, const Type * expected, bool strict = true);

private:
  Module * module_;
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

  TypeInferencePass(Module * module, Expr * root, const Type * expected, bool strict = true)
    : module_(module)
    , rootExpr_(root)
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

  /** Remove from consideration overloads that are less specific. */
  void cullBySpecificity();
  void cullBySpecificity(CallSite * site);

  /** Try removing each from consideration and re-evaluate all conversion
      ranks for the entire AST. Choose the one with the best rank. */
  void cullByElimination();
  void cullByElimination(CallSiteList::iterator first, CallSiteList::iterator last);
  void cullByElimination(CallSite * site);

  /** Cull all candidates for a given site except for ch. */
  void cullAllButOne(CallSite * site, int choice);

  // Undo the last pruning
  void backtrack();
};

} // namespace tart

#endif

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

  /** Replace all type variables in this expression with unique variables, so that we
      can distinguish between multiple occurances of the same template in an expression. */
  virtual void relabelVars(BindingEnv & env) = 0;

  /** Do unification of the operation with its arguments. */
  virtual bool unify(BindingEnv & env, int cullingDepth) = 0;

  /** Generate a printable version of this choice point's expression. */
  virtual const Expr * expr() const = 0;

  // Eliminate culled candidates
  virtual void finish() = 0;

  // Report the given error message, along with the calling signature and the candidates.
  virtual void reportErrors(const char * msg) = 0;

  // Print debugging info
  virtual void reportRanks() = 0;

  /** Return true if the specified choice has been culled. */
  virtual bool isCulled(int index) const = 0;

  /** Cull a specific numbered choice. */
  virtual void cull(int choice, int searchDepth) = 0;

  /** Cull all choices lower than a given conversion rank. */
  virtual int cullByConversionRank(ConversionRank lowerLimit, int searchDepth) = 0;

  /** Cull all choices which are less specific than others. */
  virtual void cullBySpecificity(int searchDepth) = 0;

  /** Cull all choices except for the numbered choice. */
  virtual void cullAllExcept(int choice, int searchDepth) = 0;

  /** Cull all choices except for the best choice. */
  virtual void cullAllExceptBest(int searchDepth) = 0;

  /** Return true if choice 'choice' depends on any provision in 'p'. */
  virtual bool dependsOn(int choice, const ProvisionSet & pset) const = 0;

  /** Save the single best candidate. */
  virtual void saveBest() = 0;

  /** Undo all cullings of greater than search depth. */
  virtual void backtrack(int searchDepth) = 0;

  /** Generate a printable version of the input expression. */
  virtual void formatExpression(FormatStream & out) = 0;
};

typedef llvm::SmallVector<ChoicePoint *, 16> ChoicePointList;
typedef llvm::SmallSet<const Expr *, 16> ExprSet;

/// -------------------------------------------------------------------
/// Represents a site where a function is called. There are two
/// kinds of choice: Selecting overloads, and binding template params.

class CallSite : public ChoicePoint {
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
  void relabelVars(BindingEnv & env);
  bool unify(BindingEnv & env, int cullingDepth);
  bool isCulled(int ch) const;
  void cull(int choice, int searchDepth);
  int cullByConversionRank(ConversionRank lowerLimit, int searchDepth);
  void cullBySpecificity(int searchDepth);
  void cullAllExcept(int choice, int searchDepth);
  void cullAllExceptBest(int searchDepth);
  void removeCulled();
  void backtrack(int searchDepth);
  const Expr * expr() const { return callExpr_; }
  bool dependsOn(int choice, const ProvisionSet & pset) const;
  void formatExpression(FormatStream & out);

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

/// -------------------------------------------------------------------
/// Represents an additional type constraint that is not a choice
/// point, meaning that it has an influence on the final
struct ConstraintSite {
public:
  ConstraintSite()
    : expr_(NULL)
    , rank_(IdenticalTypes)
  {}

  ConstraintSite(Expr * ex)
    : expr_(ex)
    , rank_(IdenticalTypes)
  {}

  ConstraintSite(const ConstraintSite & src)
    : expr_(src.expr_)
    , rank_(src.rank_)
  {}

  virtual ~ConstraintSite() {}

  const ConstraintSite & operator=(const ConstraintSite & src) {
    expr_ = src.expr_;
    rank_ = src.rank_;
    return *this;
  }

  // The expression which is the source of the constraint.
  const Expr * expr() const { return expr_; }

  // Update conversion rankings
  virtual void update() = 0;

  /** Conversion ranking for this constraint. */
  ConversionRank rank() const { return rank_; }

protected:
  Expr * expr_;              // Expression that defines the constraint.
  ConversionRank rank_;
};

typedef llvm::SmallVector<ConstraintSite *, 16> ConstraintSiteSet;

/// -------------------------------------------------------------------
/// A constraint for assignment statements.

class AssignmentSite : public ConstraintSite {
public:
  AssignmentSite(AssignmentExpr * in) : ConstraintSite(in) {}

  // Update conversion rankings
  void update();
};

/// -------------------------------------------------------------------
/// A constraint for tuple constructors.

class TupleCtorSite : public ConstraintSite {
public:
  TupleCtorSite(TupleCtorExpr * in) : ConstraintSite(in) {}

  // Update conversion rankings
  void update();
};

/// -------------------------------------------------------------------
/// A constraint for PHI-class expressions, that is expressions which
/// choose one of several alternate values to return. (Examples being
/// 'if' and 'switch' expressions.)

class PHISite : public ConstraintSite {
public:
  PHISite(Expr * in) : ConstraintSite(in) {}

  // Update conversion rankings
  void update();
  void report();
};

/// -------------------------------------------------------------------
/// Pass to collect all of the constraints in the expression tree.
class GatherConstraintsPass : public CFGPass {
public:
  GatherConstraintsPass(ChoicePointList & callSites, ConstraintSiteSet & cstrSites)
    : choicePoints_(callSites)
    , constraints_(cstrSites)
  {}

private:
  ChoicePointList & choicePoints_;
  ConstraintSiteSet & constraints_;
  ExprSet visited_;

  Expr * visitCall(CallExpr * in);
  Expr * visitAssign(AssignmentExpr * in);
  Expr * visitPostAssign(AssignmentExpr * in);
  Expr * visitTupleCtor(TupleCtorExpr * in);
//  Expr * visitConstantInteger(ConstantInteger * in);
  Expr * visitIf(IfExpr * in);
  Expr * visitSwitch(SwitchExpr * in);
  Expr * visitMatch(MatchExpr * in);

  void visitPHI(Expr * in);
};

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class TypeInferencePass {
public:
  /** Run this pass on the specified expression. */
  static Expr * run(Module * module, Expr * in, BindingEnv & env, const Type * expected,
      bool strict = true);

private:
  enum ReportLabel {
    INITIAL,
    INTERMEDIATE,
    FINAL,
  };

  Module * module_;
  Expr * rootExpr_;
  BindingEnv & env_;
  const Type * expectedType_;
  ChoicePointList choicePoints_;
  ConstraintSiteSet cstrSites_;
  ConversionRank lowestRank_;
  ConversionRank bestSolutionRank_;
  int bestSolutionCount_;
  int cullCount_;
  int searchDepth_;
  bool underconstrained_;
  bool overconstrained_;
  bool strict_;

  TypeInferencePass(Module * module, Expr * root, BindingEnv & env, const Type * expected,
      bool strict = true)
    : module_(module)
    , rootExpr_(root)
    , env_(env)
    , expectedType_(expected)
    , searchDepth_(0)
    , strict_(strict)
  {}

  Expr * runImpl();

  /** Re-label all type variables. */
  void relabelVars();

  /** Run unification algorithm on all expressions containing type variables (which should
      be relabeled. */
  bool unify();

  /** Simplify the set of constraints generated during the unification pass. */
  void simplifyConstraints();
  void selectConstantIntegerTypes();

  /** Update the type conversion rankings of all constraints and determine whether the
      current solution is over or under constrained. */
  void update();

  void reportRanks(ReportLabel label);
  void checkSolution();

  // Various pruning strategies.

  /** Remove from consideration overloads that have unsolvable constraints. */
  bool cullByContradiction();
  bool checkContradictoryConstraints();

  /** Remove from consideration overloads that have a low conversion rank. */
  bool cullByConversionRank();
  void cullByConversionRank(ConversionRank lowerLimit);
  bool cullEachSiteByConversionRank();

  /** Remove from consideration overloads that are less specific. */
  void cullBySpecificity();
  void cullBySpecificity(CallSite * site);

  /** Try removing each from consideration and re-evaluate all conversion
      ranks for the entire AST. Choose the one with the best rank. */
  void cullByElimination();
  void cullByElimination(ChoicePointList::iterator first, ChoicePointList::iterator last);
  void cullByElimination(CallSite * site);

  // Undo the last pruning
  void backtrack();

  /** Find a common type which encompasses both type1 and type2. */
  const Type * selectLessSpecificType(const Type * type1, const Type * type2);
  const Type * lessSpecificIntType(const UnsizedIntType * uint, const Type * ty);
};

} // namespace tart

#endif

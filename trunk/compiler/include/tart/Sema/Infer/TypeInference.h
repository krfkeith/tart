/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_TYPEINFERENCE_H
#define TART_SEMA_INFER_TYPEINFERENCE_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

#ifndef TART_SEMA_INFER_CALLSITE_H
#include "tart/Sema/Infer/CallSite.h"
#endif

#ifndef TART_SEMA_INFER_CONVERSIONSITE_H
#include "tart/Sema/Infer/ConversionSite.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class TypeInferencePass {
public:
  /** Run this pass on the specified expression. */
  static Expr * run(Module * module, Expr * in, BindingEnv & env, QualifiedType expected,
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
  QualifiedType expectedType_;
  CallSites calls_;
  ConversionSites cstrSites_;
  ConversionRank lowestRank_;
  ConversionRank bestSolutionRank_;
  int bestSolutionCount_;
  int cullCount_;
  int searchDepth_;
  bool underconstrained_;
  bool overconstrained_;
  bool strict_;

  TypeInferencePass(Module * module, Expr * root, BindingEnv & env, QualifiedType expected,
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
  void cullByElimination(CallSites::iterator first, CallSites::iterator last);
  void cullByElimination(CallSite * site);

  // Undo the last pruning
  void backtrack();

  /** Find a common type which encompasses both type1 and type2. */
  const Type * selectLessSpecificType(const Type * type1, const Type * type2);
  const Type * lessSpecificIntType(const UnsizedIntType * uint, const Type * ty);
};

} // namespace tart

#endif

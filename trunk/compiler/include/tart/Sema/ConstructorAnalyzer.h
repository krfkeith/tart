/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_CONSTRUCTORANALYZER_H
#define TART_SEMA_CONSTRUCTORANALYZER_H

#ifndef TART_CFG_VARIABLEDEFN_H
#include "tart/CFG/VariableDefn.h"
#endif

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"

namespace tart {

class CompositeType;
class FunctionDefn;
class ConstructorAnalyzer;

typedef llvm::DenseMap<VariableDefn *, int> VarIndexMap;

/// -------------------------------------------------------------------
/// Analyzer for constructors - determines which instance vars have
/// been properly initialized.

class CtorInitState : public CFGPass {
public:
  CtorInitState(ConstructorAnalyzer & analyzer);

  Expr * visitAssign(AssignmentExpr * in);
  Expr * visitPostAssign(AssignmentExpr * in);
  Expr * visitFnCall(FnCallExpr * in);

  Expr * visitIf(IfExpr * in);
  Expr * visitWhile(WhileExpr * in);
  Expr * visitDoWhile(WhileExpr * in);
  Expr * visitFor(ForExpr * in);
  Expr * visitForEach(ForEachExpr * in);
  Expr * visitSwitch(SwitchExpr * in);
  Expr * visitMatch(MatchExpr * in);
  Expr * visitTry(TryExpr * in);
  Expr * visitReturn(ReturnExpr * in);

private:
  void checkAssign(AssignmentExpr * in);

  ConstructorAnalyzer & analyzer_;
  llvm::BitVector set_;
  llvm::BitVector maybeSet_;
};

/// -------------------------------------------------------------------
/// Analyzer for constructors - determines which instance vars have
/// been properly initialized.
class ConstructorAnalyzer {
  friend class CtorInitState;
public:
  ConstructorAnalyzer(CompositeType * cls_);

  void run(FunctionDefn * ctor);

  void putReturnState(llvm::BitVector & set, llvm::BitVector & maybeSet);

private:
  ConstructorAnalyzer(ConstructorAnalyzer &); // DO NOT IMPLEMENT

  CompositeType * cls_;
  FunctionDefn * ctor_;
  VarIndexMap varIndices_;
  int varCount_;
  bool isReturnVisited_;
  llvm::BitVector set_;
  llvm::BitVector maybeSet_;
};

}

#endif

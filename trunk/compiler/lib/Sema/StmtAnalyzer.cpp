/* ================================================================ *
 TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Closure.h"

#include "tart/AST/Stmt.h"

#include "tart/Sema/StmtAnalyzer.h"
#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/FinalizeTypesPass.h"
#include "tart/Sema/MacroExpansionPass.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/EvalPass.h"

#include "tart/Objects/Builtins.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"

#define IMPLICIT_SELF 1

namespace tart {

namespace {
  void visitSuccessors(Block * blk, llvm::SmallPtrSet<Block *, 128> & visited) {
    if (visited.insert(blk)) {
      for (BlockList::iterator it = blk->succs().begin(); it != blk->succs().end(); ++it) {
        visitSuccessors(*it, visited);
      }

      if (blk->unwindTarget()) {
        visitSuccessors(blk->unwindTarget(), visited);
      }
    }
  }
}

// A scope which allows definitions in the enclosing class to be looked up
// via the 'self' parameter.
class SelfScope: public DelegatingScope {
  ParameterDefn * selfParam;
  Expr * selfExpr;

public:
  SelfScope(Scope * delegate, Scope * parent) :
    DelegatingScope(delegate, parent), selfParam(NULL), selfExpr(NULL) {
  }

  void setSelfParam(ParameterDefn * self) {
    selfParam = self;
  }

  Expr * baseExpr() {
    if (selfExpr == NULL) {
      selfExpr = LValueExpr::get(selfParam->location(), NULL, selfParam);
    }

    return selfExpr;
  }
};

/// -------------------------------------------------------------------
/// StmtAnalyzer

StmtAnalyzer::StmtAnalyzer(FunctionDefn * func)
  : ExprAnalyzer(func->module(), &func->parameterScope(), func, func)
  , function(func)
  //, returnType_(NULL)
  //, yieldType_(NULL)
  , blocks(func->blocks())
  , currentBlock_(NULL)
  , continueTarget_(NULL)
  , breakTarget_(NULL)
  , unwindTarget_(NULL)
  , cleanups_(NULL)
  , loopCleanups_(NULL)
  , macroReturnVal_(NULL)
  , macroReturnTarget_(NULL)
{
  insertPos_ = blocks.end();
  returnType_ = function->returnType();
}

bool StmtAnalyzer::buildCFG() {
  if (function->functionDecl() && function->functionDecl()->body() != NULL) {

    // Create a temporary scope to allow lookup of the function parameters.
    DelegatingScope parameterScope(&function->parameterScope(), function->definingScope());
    setActiveScope(&parameterScope);

    // If this is an instance method, then set up the implicit 'self'
    // scope as well. This scope searches the type of the self parameter,
    // and is always searched immediately after the parameter scope.
    if (function->storageClass() == Storage_Instance) {
      ParameterDefn * selfParam = function->functionType()->selfParam();
      DASSERT_OBJ(selfParam != NULL, function);
      DASSERT_OBJ(selfParam->type() != NULL, function);
      TypeDefn * selfType = selfParam->type()->typeDefn();
      DASSERT_OBJ(selfType != NULL, function);

#if IMPLICIT_SELF
      // Uncomment to allow 'self' to be searched implicitly.
      SelfScope * selfScope =
          new SelfScope(selfType->typeValue()->memberScope(), function->definingScope());
      selfScope->setSelfParam(selfParam);
      parameterScope.setParentScope(selfScope);
#endif
    } else if (function->storageClass() == Storage_Local) {
      ParameterDefn * selfParam = function->functionType()->selfParam();
      DASSERT_OBJ(selfParam != NULL, function);
      DASSERT_OBJ(selfParam->type() != NULL, function);
      DASSERT_OBJ(selfParam->initValue() != NULL, function);
      if (ClosureEnvExpr * env = dyn_cast<ClosureEnvExpr>(selfParam->initValue())) {
        parameterScope.setParentScope(env);
      }
    }

    // Create the initial block.
    setInsertPos(createBlock("entry"));
    const Stmt * body = function->functionDecl()->body();
    if (!buildStmtCFG(body)) {
      return false;
    }

    // Now flatten all local returns.
    flattenLocalProcedureCalls();
    optimizeBranches();
    removeDeadBlocks();

    // Add a return statement at the end if it is needed.
    // Note that this may be removed during dead code deletion if there is no way to
    // get to the block.
    if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
      if (returnType_ != NULL && !returnType_->isVoidType()) {
        diag.error(body->finalLocation()) <<
            "Missing return statement at end of non-void function.";
      }
      currentBlock_->exitReturn(body->finalLocation(), NULL);
    }

    return true;
  }

  return true;
}

bool StmtAnalyzer::buildStmtCFG(const Stmt * st) {
  switch (st->nodeType()) {
    case ASTNode::Block:
      return buildBlockStmtCFG(static_cast<const BlockStmt *>(st));

    case ASTNode::Expression:
      return buildExprStmtCFG(static_cast<const ExprStmt *>(st));

    case ASTNode::If:
      return buildIfStmtCFG(static_cast<const IfStmt *>(st));

    case ASTNode::While:
      return buildWhileStmtCFG(static_cast<const WhileStmt *>(st));

    case ASTNode::DoWhile:
      return buildDoWhileStmtCFG(static_cast<const DoWhileStmt *>(st));

    case ASTNode::For:
      return buildForStmtCFG(static_cast<const ForStmt *>(st));

    case ASTNode::ForEach:
      return buildForEachStmtCFG(static_cast<const ForEachStmt *>(st));

    case ASTNode::Switch:
      return buildSwitchStmtCFG(static_cast<const SwitchStmt *>(st));

    case ASTNode::Classify:
      return buildClassifyStmtCFG(static_cast<const ClassifyStmt *>(st));

    case ASTNode::Throw:
      return buildThrowStmtCFG(static_cast<const ThrowStmt *>(st));

    case ASTNode::Try:
      return buildTryStmtCFG(static_cast<const TryStmt *>(st));

    case ASTNode::Return:
      return buildReturnStmtCFG(static_cast<const ReturnStmt *>(st));

    case ASTNode::Yield:
      return buildYieldStmtCFG(static_cast<const YieldStmt *>(st));

    case ASTNode::Break:
      return buildBreakStmtCFG(st);

    case ASTNode::Continue:
      return buildContinueStmtCFG(st);

    case ASTNode::LocalDecl:
      return buildLocalDeclStmtCFG(static_cast<const DeclStmt *>(st));

    //case Catch:
    default:
      diag.fatal(st->location()) << "Invalid statement type '" << nodeTypeName(st->nodeType());
      return false;
  }
}

bool StmtAnalyzer::buildBlockStmtCFG(const BlockStmt * st) {
  bool success = true;
  LocalScope * blockScope = createLocalScope("block-scope");
  Scope * savedScope = setActiveScope(blockScope);

  // Now process all of the statements
  const StmtList & stlist = st->stmts();
  for (StmtList::const_iterator it = stlist.begin(); it != stlist.end(); ++it) {
    // No need to continue if the block already has a terminator.
    if (currentBlock_ == NULL || currentBlock_->hasTerminator()) {
      break;
    }

    if (!buildStmtCFG(*it)) {
      success = false;
      break;
    }
  }

  setActiveScope(savedScope);
  return success;
}

bool StmtAnalyzer::buildExprStmtCFG(const ExprStmt * st) {
  const ASTNode * value = st->value();
  Expr * expr = inferTypes(astToAssignExpr(value, NULL), NULL);
  if (!isErrorResult(expr)) {
    if (expr->isSideEffectFree() && value->nodeType() != ASTNode::Call) {
      diag.warn(expr) << "Statement '" << st << "' has no effect";
    }

    currentBlock_->append(expr);
    return true;
  }

  return false;
}

bool StmtAnalyzer::buildIfStmtCFG(const IfStmt * st) {
  Scope * savedScope = activeScope;
  Expr * testExpr = astToTestExpr(st->testExpr());
  if (isErrorResult(testExpr)) {
    return false;
  }

  DASSERT(testExpr->type() != NULL);
  bool hasElse = (st->elseSt() != NULL);

  // Create the set of basic blocks. We don't know yet
  // if we need an else block or endif block.
  Block * blkThen = createBlock("then");
  Block * blkElse = NULL;
  Block * blkDone = NULL;

  if (st->elseSt() != NULL) {
    blkElse = createBlock("else");
    currentBlock_->condBranchTo(st->testExpr()->location(), testExpr, blkThen, blkElse);
  } else {
    blkDone = createBlock("endif");
    currentBlock_->condBranchTo(st->testExpr()->location(), testExpr, blkThen, blkDone);
  }

  // Generate the contents of the 'then' block.
  setInsertPos(blkThen);
  buildStmtCFG(st->thenSt());

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
    if (blkDone == NULL)
    blkDone = createBlock("endif");
    currentBlock_->branchTo(st->thenSt()->finalLocation(), blkDone);
  }

  // Generate the contents of the 'else' block
  if (blkElse != NULL) {
    setInsertPos(blkElse);
    buildStmtCFG(st->elseSt());

    // Only generate a branch if we haven't returned or thrown
    if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
      if (blkDone == NULL)
      blkDone = createBlock("endif");
      currentBlock_->branchTo(st->finalLocation(), blkDone);
    }
  }

  // Continue at the 'done' block if there was one.
  setInsertPos(blkDone);
  setActiveScope(savedScope);
  return true;
}

bool StmtAnalyzer::buildWhileStmtCFG(const WhileStmt * st) {
  Scope * savedScope = activeScope;

  // Create the set of basic blocks. We don't know yet
  // if we need an else block or endif block.
  Block * blkTest = createBlock("test");
  Block * blkBody = createBlock("loopbody");
  Block * blkDone = createBlock("endwhile");

  // Start by branching to the 'test' block.
  currentBlock_->branchTo(st->location(), blkTest);

  // Generate the test expression.
  Expr * testExpr = astToTestExpr(st->testExpr());
  if (isErrorResult(testExpr)) {
    return false;
  }

  DASSERT(testExpr != NULL);
  DASSERT_OBJ(testExpr->type() != NULL, testExpr);

  // Generate the 'test' block with the conditional branch.
  blkTest->condBranchTo(st->testExpr()->location(),
      testExpr, blkBody, blkDone);

  // Generate the 'body' block.
  Block * saveBreakTarget = breakTarget_;
  Block * saveContinueTarget = continueTarget_;
  CleanupHandler * saveLoopEH = loopCleanups_;
  setInsertPos(blkBody);
  breakTarget_ = blkDone;
  loopCleanups_ = cleanups_;
  continueTarget_ = blkTest;
  buildStmtCFG(st->body());
  breakTarget_ = saveBreakTarget;
  continueTarget_ = saveContinueTarget;
  loopCleanups_ = saveLoopEH;

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->testExpr()->location(), blkTest);
  }

  setInsertPos(blkDone);
  setActiveScope(savedScope);
  return true;
}

bool StmtAnalyzer::buildDoWhileStmtCFG(const DoWhileStmt * st) {
  Scope * savedScope = activeScope;

  // Create the set of basic blocks. We don't know yet
  // if we need an else block or endif block.
  Block * blkBody = createBlock("loopbody");
  Block * blkDone = createBlock("endwhile");

  // Start by branching to the loop body block.
  currentBlock_->branchTo(st->location(), blkBody);

  // Generate the 'body' block.
  Block * saveBreakTarget = breakTarget_;
  Block * saveContinueTarget = continueTarget_;
  CleanupHandler * saveLoopEH = loopCleanups_;
  setInsertPos(blkBody);
  breakTarget_ = blkDone;
  loopCleanups_ = cleanups_;
  continueTarget_ = blkBody;
  buildStmtCFG(st->body());
  breakTarget_ = saveBreakTarget;
  continueTarget_ = saveContinueTarget;
  loopCleanups_ = saveLoopEH;

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
    // Generate the test expression at the end of the body.
    Expr * testExpr = astToTestExpr(st->testExpr());
    if (isErrorResult(testExpr)) {
      return false;
    }

    DASSERT(testExpr != NULL);
    DASSERT_OBJ(testExpr->type() != NULL, testExpr);

    // Generate the 'test' block with the conditional branch.
    blkBody->condBranchTo(st->testExpr()->location(),
        testExpr, blkBody, blkDone);
  }

  setInsertPos(blkDone);
  setActiveScope(savedScope);
  return true;
}

bool StmtAnalyzer::buildForStmtCFG(const ForStmt * st) {
  Scope * savedScope = activeScope;
  LocalScope * forScope = createLocalScope("for-scope");
  setActiveScope(forScope);

  // Evaluate the initialization expression
  const ASTNode * initExpr = st->initExpr();
  if (initExpr != NULL) {
    if (initExpr->nodeType() == ASTNode::Var) {
      const ASTVarDecl * initDecl = static_cast<const ASTVarDecl *>(initExpr);
      VariableDefn * initDefn = cast<VariableDefn>(astToDefn(initDecl));
      if (!analyzeValueDefn(initDefn, Task_PrepTypeGeneration)) {
        return false;
      }

      DASSERT(initDefn->initValue() != NULL);
      Expr * initValue = initDefn->initValue();
      initDefn->setInitValue(NULL);
      currentBlock_->append(new InitVarExpr(st->location(), initDefn, initValue));
      //if (initDefn == NULL) {
      //  return NULL;
      //}
    } else if (initExpr->nodeType() == ASTNode::VarList) {
      const ASTVarDecl * varList = static_cast<const ASTVarDecl *>(initExpr);
      //Expr * initExpr = ea.analyze(varList->value(), target->type());

      DASSERT(varList->value() != NULL);
      //Expr * initValue = initDefn->initValue();
      //initDefn->setInitValue(NULL);

      //currentBlock_->append(new InitVarExpr(st->location(), initDefn, initValue));

      DefnList vars;
      if (!astToDefnList(varList, vars)) {
        return false;
      }

      //VariableDefn * initDefn = cast<VariableDefn>(astToDefn(initDecl));
      DFAIL("Implement");
    }
  }

  // Evaluate the test expression
  Expr * testExpr = NULL;
  if (st->testExpr() != NULL) {
    testExpr = inferTypes(astToExpr(st->testExpr(), &BoolType::instance), &BoolType::instance);
  }

  Block * blkTest = NULL;
  if (testExpr) {
    //testExpr = analyzer.reduceExpressionToType(
    //    testExpr, &PrimitiveType::BoolType);
    //DASSERT(testExpr->getCanonicalType() != NULL);
    blkTest = createBlock("test");
  }

  Block * blkBody = createBlock("loopbody");

  Expr * incrExpr = NULL;
  if (st->incrExpr() != NULL) {
    incrExpr = inferTypes(astToExpr(st->incrExpr(), NULL), NULL);
    if (incrExpr != NULL && incrExpr->isSideEffectFree()) {
      diag.warn(incrExpr) << "Loop increment expression has no effect";
    }
  }

  Block * blkIncr = NULL;
  if (incrExpr) {
    //incrExpr = analyzer.reduceExpression(incrExpr);
    //DASSERT(testExpr != NULL);
    blkIncr = createBlock("incr");
    blkIncr->append(incrExpr);
  }

  Block * blkDone = createBlock("endfor");

  // Generate the 'test' block with the conditional branch.
  if (blkTest) {
    currentBlock_->branchTo(st->testExpr()->location(), blkTest);
    blkTest->condBranchTo(st->testExpr()->location(), testExpr, blkBody, blkDone);
  } else {
    currentBlock_->branchTo(st->location(), blkBody);
  }

  // Generate the 'body' block.
  Block * saveBreakTarget = breakTarget_;
  Block * saveContinueTarget = continueTarget_;

  breakTarget_ = blkDone;
  continueTarget_ = blkIncr ? blkIncr : (blkTest ? blkTest : blkBody);

  setInsertPos(blkBody);
  buildStmtCFG(st->body());

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->location(), continueTarget_);
  }

  breakTarget_ = saveBreakTarget;
  continueTarget_ = saveContinueTarget;

  if (blkIncr) {
    blkIncr->branchTo(st->location(), blkTest ? blkTest : blkBody);
  }

  // Continue at the 'done' block.
  setInsertPos(blkDone);
  setActiveScope(savedScope);
  return true;
}

bool StmtAnalyzer::buildForEachStmtCFG(const ForEachStmt * st) {
  Scope * savedScope = activeScope;
  LocalScope * forScope = createLocalScope("for-scope");
  setActiveScope(forScope);

  Expr * iterExpr = inferTypes(astToExpr(st->iterExpr(), NULL), NULL);
  if (isErrorResult(iterExpr)) {
    return false;
  }

  if (!isa<CompositeType>(iterExpr->type())) {
    diag.error(iterExpr) << "Invalid iterator type: " << iterExpr->type();
    return false;
  }

  const CompositeType * iterType = static_cast<const CompositeType *>(iterExpr->type());
  AnalyzerBase::analyzeType(iterType, Task_PrepMemberLookup);
  FunctionDefn * next = findInterfaceMethod(iterType, Builtins::typeIterator, "next");
  if (next == NULL) {
    // If it's not an Iterator, see if it's an Iterable.
    FunctionDefn * iterate = findInterfaceMethod(iterType, Builtins::typeIterable, "iterate");
    if (iterate == NULL) {
      diag.error(iterExpr) << "Invalid iterator type: " << iterExpr->type();
      return false;
    }

    LValueExpr * iterMethod = LValueExpr::get(iterate->location(), iterExpr, iterate);
    iterExpr = inferTypes(callExpr(st->location(), iterMethod, ASTNodeList(), NULL), NULL);
    if (iterExpr == NULL) {
      return false;
    }

    if (!isa<CompositeType>(iterExpr->type())) {
      diag.error(iterExpr) << "Invalid iterator type: " << iterExpr->type();
      return false;
    }

    iterType = static_cast<const CompositeType *>(iterExpr->type());
    AnalyzerBase::analyzeType(iterType, Task_PrepMemberLookup);
    next = findInterfaceMethod(iterType, Builtins::typeIterator, "next");
    if (next == NULL) {
      diag.error(iterExpr) << "Invalid iterator type: " << iterExpr->type();
      return false;
    }
  }

  iterExpr = createTempVar(".iterator", iterExpr, false);
  LValueExpr * nextMethod = LValueExpr::get(next->location(), iterExpr, next);
  Expr * nextCall = inferTypes(
      callExpr(iterExpr->location(), nextMethod, ASTNodeList(), NULL), NULL);
  if (nextCall == NULL) {
    return false;
  }

  Block * blkTest = createBlock("test");
  currentBlock_->branchTo(st->location(), blkTest);
  setInsertPos(blkTest);

  LValueExpr * nextVal = createTempVar(".iterval", nextCall, true);

  const UnionType * utype = dyn_cast<UnionType>(nextVal->type());
  DASSERT(utype != NULL);
  DASSERT(utype->members().size() == 2);
  const Type * iterVarType;
  for (TupleType::const_iterator it = utype->members().begin(); it != utype->members().end();
      ++it) {
    const Type * ty = *it;
    if (!ty->isVoidType()) {
      iterVarType = ty;
    }
  }

  Expr * testExpr = new InstanceOfExpr(st->location(), nextVal, &VoidType::instance);
  Expr * iterValue = new CastExpr(Expr::UnionMemberCast, st->location(), iterVarType, nextVal);

  // Generate the 'body' block.
  Block * blkBody = createBlock("loopbody");
  Block * blkDone = createBlock("endfor");
  Block * saveBreakTarget = breakTarget_;
  Block * saveContinueTarget = continueTarget_;

  // Assign the next value to the iteration variable.
  if (st->loopVars()->nodeType() == ASTNode::Var) {
    const ASTVarDecl * initDecl = static_cast<const ASTVarDecl *>(st->loopVars());
    VariableDefn * initDefn = cast<VariableDefn>(astToDefn(initDecl));
    initDefn->setType(iterVarType);
    if (!analyzeValueDefn(initDefn, Task_PrepTypeGeneration)) {
      return false;
    }

    blkBody->append(new InitVarExpr(st->location(), initDefn, iterValue));
  } else if (st->loopVars()->nodeType() == ASTNode::VarList) {
    const ASTVarDecl * varList = static_cast<const ASTVarDecl *>(st->loopVars());
    DefnList vars;
    if (!astToDefnList(varList, vars)) {
      return false;
    }

    DFAIL("Implement");
  } else {
    DFAIL("Invalid loop variable");
  }

  breakTarget_ = blkDone;
  continueTarget_ = blkTest;

  setInsertPos(blkBody);
  buildStmtCFG(st->body());

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->location(), continueTarget_);
  }

  breakTarget_ = saveBreakTarget;
  continueTarget_ = saveContinueTarget;

  blkTest->condBranchTo(testExpr->location(), testExpr, blkDone, blkBody);

  // Continue at the 'done' block.
  setInsertPos(blkDone);
  setActiveScope(savedScope);
  return true;
}

bool StmtAnalyzer::buildSwitchStmtCFG(const SwitchStmt * st) {
  Scope * savedScope = activeScope;
  Scope * caseValScope = activeScope;
  Expr * testExpr = astToTestExpr(st->testExpr(), false);
  if (isErrorResult(testExpr)) {
    return false;
  }

  DASSERT(testExpr->type() != NULL);
  if (testExpr->isConstant()) {
    // TODO: See if the test expression is a constant.
    // Check for bool and int types, not floats.
  }

  const Type * testType = testExpr->type();

  if (const PrimitiveType * ptype = dyn_cast<PrimitiveType>(testType)) {
    if (isIntegerTypeId(ptype->typeId())) {
      // TODO: Implement
    } else {
      diag.error(st) << "Invalid expression type for switch statement: " << testType;
    }
  } else if (const EnumType * etype = dyn_cast<EnumType>(testType)) {
    // If it's an enum, allow unqualified enum constants to be used.
    caseValScope = new DelegatingScope(const_cast<IterableScope *>(etype->memberScope()), activeScope);
    // TODO: Implement
  } else if (testType == Builtins::typeString.get()) {
    // TODO: Implement
    DFAIL("Implement");
  } else {
    diag.error(st) << "Invalid expression type for switch statement: " << testType;
  }

  Block * testBlock = currentBlock_;
  testBlock->setTerminator(st->location(), BlockTerm_Switch);
  testBlock->addCase(testExpr, NULL);
  Block * endBlock = NULL;

  ConstantExprList usedCaseVals;

  const Stmt * elseSt = NULL;
  const StmtList & cases = st->caseList();
  for (StmtList::const_iterator it = cases.begin(); it != cases.end(); ++it) {
    const Stmt * s = *it;
    if (s->nodeType() == ASTNode::Case) {
      const CaseStmt * caseSt = static_cast<const CaseStmt *>(s);
      ConstantExprList caseValList;
      Scope * saveCaseScope = setActiveScope(caseValScope);

      for (ASTNodeList::const_iterator it = caseSt->caseExprs().begin();
          it != caseSt->caseExprs().end(); ++it) {
        ConstantExpr * caseVal = astToCaseValueExpr(*it, testType);
        if (caseVal == NULL) {
          continue;
        }

        caseValList.push_back(caseVal);
      }

      setActiveScope(saveCaseScope);
      if (caseValList.empty()) {
        continue;
      }

      Block * caseBody = createBlock("casebody");
      for (ConstantExprList::iterator ci = caseValList.begin(); ci != caseValList.end(); ++ci) {
        ConstantExpr * caseVal = *ci;
        testBlock->addCase(caseVal, caseBody);
        for (ConstantExprList::iterator ce = usedCaseVals.begin(); ce != usedCaseVals.end(); ++ce) {
          if (caseVal->isEqual(*ce)) {
            diag.error(caseVal) << "Duplicate case value in switch: " << caseVal;
          }
        }
        usedCaseVals.push_back(caseVal);
      }

      // Build the body of the case.
      setInsertPos(caseBody);
      buildStmtCFG(caseSt->body());

      // Only generate a branch if we haven't returned or thrown.
      if (currentBlock_ && !currentBlock_->hasTerminator()) {
        if (endBlock == NULL) {
          endBlock = new Block("endswitch");
        }

        currentBlock_->branchTo(st->location(), endBlock);
      }
    } else if (s->nodeType() == ASTNode::Block) {
      if (elseSt != NULL) {
        diag.error(elseSt) << "Only one 'else' clause is allowed.";
      } else {
        elseSt = s;
      }
    } else {
      DFAIL("Bad case statement");
    }
  }

  if (elseSt != NULL) {
    Block * elseBlock = createBlock("else");
    testBlock->succs()[0] = elseBlock;
    setInsertPos(elseBlock);
    buildStmtCFG(elseSt);

    // Only generate a branch if we haven't returned or thrown.
    if (currentBlock_ && !currentBlock_->hasTerminator()) {
      if (endBlock == NULL) {
        endBlock = new Block("endswitch");
      }

      currentBlock_->branchTo(st->location(), endBlock);
    }
  } else {
    if (endBlock == NULL) {
      endBlock = new Block("endswitch");
    }

    testBlock->succs()[0] = endBlock;
  }

  if (endBlock != NULL) {
    endBlock->setUnwindTarget(unwindTarget_);
    blocks.push_back(endBlock);
    setInsertPos(endBlock);
  } else {
    setInsertPos(NULL);
  }

  setActiveScope(savedScope);
  return true;
}

ConstantExpr * StmtAnalyzer::astToCaseValueExpr(const ASTNode * ast, const Type * testType) {
  Expr * caseVal = astToExpr(ast, testType);
  Expr * result = NULL;
  Conversion cn(caseVal, &result);
  ConversionRank rank = testType->convert(cn);
  if (rank == Incompatible) {
    diag.error(ast) << "Case value type '" << caseVal->type() <<
        " is incompatible with switch expression type '" << testType << "'";
    return NULL;
  } else if (rank == Truncation) {
    diag.error(ast) << "Case value '" << caseVal <<
        " can never equal switch expression of type '" << testType << "'";
  }

  result = EvalPass::eval(result, false);
  if (result == NULL) {
    return NULL;
  }

  return cast<ConstantExpr>(result);
}

bool StmtAnalyzer::buildClassifyStmtCFG(const ClassifyStmt * st) {
  Scope * savedScope = activeScope;
  Expr * testExpr = astToTestExpr(st->testExpr(), false);
  if (isErrorResult(testExpr)) {
    return false;
  }

  DASSERT(testExpr->type() != NULL);
  if (testExpr->isConstant()) {
    // TODO: See if the test expression is a constant.
  }

  // TODO: There are lots of optimizations that could be done here.
  const Type * fromType = testExpr->type();
  Expr::ExprType castType = Expr::BitCast;
  bool testIsLValue = true;
  if (fromType->isReferenceType()) {
    testIsLValue = false;
  } else if (const UnionType * utype = dyn_cast<UnionType>(fromType)) {
    castType = Expr::UnionMemberCast;
  } else {
    diag.warn(testExpr) << "Classify expression's type is already known: " << fromType;
  }

  // Create the temporary variable which is going to hold the test expression.
  LValueExpr * testLVal = createTempVar("classify-expr", testExpr, testIsLValue);

  Block * endBlock = NULL;
  Block * prevTestBlock = NULL;
  llvm::SmallSet<const Type *, 16> typesSeen;
  const Stmt * elseSt = NULL;
  const StmtList & cases = st->caseList();
  for (StmtList::const_iterator it = cases.begin(); it != cases.end(); ++it) {
    const Stmt * s = *it;
    if (s->nodeType() == ASTNode::Case) {
      const ClassifyAsStmt * asSt = static_cast<const ClassifyAsStmt *>(s);
      if (const ASTDecl * asDecl = asSt->asDecl()) {
        SourceLocation stLoc = asDecl->location();
        LocalScope * caseScope = createLocalScope("as-scope");
        Scope * prevScope = setActiveScope(caseScope);

        Defn * asDefn = astToDefn(asDecl);
        if (asDefn == NULL || !analyzeDefn(asDefn, Task_PrepCodeGeneration)) {
          return NULL;
        }

        VariableDefn * asValueDefn = cast<VariableDefn>(asDefn);
        const Type * toType = asValueDefn->type();
        if (!analyzeType(toType, Task_PrepTypeComparison)) {
          return NULL;
        }

        if (typesSeen.count(toType)) {
          diag.error(asDecl) << "Duplicate type test '" << toType << "'.";
          setActiveScope(prevScope);
          continue;
        }

        typesSeen.insert(toType);

        // Create the block containing the type test
        Block * testBlock = NULL;
        if (prevTestBlock != NULL) {
          // The previous test's failure target should jump to the test.
          testBlock = createBlock("as-test-", toType->typeDefn()->name());
          prevTestBlock->succs().push_back(testBlock);
        } else {
          testBlock = currentBlock_;
        }

        // Create the block containing the case body.
        Block * caseBlock = createBlock("as-", toType->typeDefn()->name());

        prevTestBlock = testBlock;

        // Set up the type test.
        Expr * typeTestExpr = new InstanceOfExpr(stLoc, testLVal, toType);
        testBlock->setTerminator(stLoc, BlockTerm_Conditional);
        testBlock->termExprs().push_back(typeTestExpr);
        testBlock->succs().push_back(caseBlock);

        // Set up the variable
        setInsertPos(caseBlock);
        Expr * asValueExpr = new CastExpr(castType, stLoc, toType, testLVal);
        currentBlock_->append(new InitVarExpr(stLoc, asValueDefn, asValueExpr));

        // Build the body of the case.
        buildStmtCFG(asSt->body());

        // Only generate a branch if we haven't returned or thrown.
        if (currentBlock_ && !currentBlock_->hasTerminator()) {
          if (endBlock == NULL) {
            endBlock = new Block("endclassify");
          }

          currentBlock_->branchTo(st->location(), endBlock);
        }

        setActiveScope(prevScope);
      } else {
        DFAIL("Bad case expression");
      }
    } else if (s->nodeType() == ASTNode::Block) {
      if (elseSt != NULL) {
        diag.error(elseSt) << "Only one 'else' clause is allowed.";
      } else {
        elseSt = s;
      }
    } else {
      DFAIL("Bad case statement");
    }
  }

  if (elseSt != NULL) {
    Block * elseBlock = createBlock("else");
    prevTestBlock->succs().push_back(elseBlock);
    setInsertPos(elseBlock);
    buildStmtCFG(elseSt);

    // Only generate a branch if we haven't returned or thrown.
    if (currentBlock_ && !currentBlock_->hasTerminator()) {
      if (endBlock == NULL) {
        endBlock = new Block("endclassify");
      }

      currentBlock_->branchTo(st->location(), endBlock);
    }
  } else {
    if (endBlock == NULL) {
      endBlock = new Block("endclassify");
    }

    prevTestBlock->succs().push_back(endBlock);
  }

  if (endBlock != NULL) {
    endBlock->setUnwindTarget(unwindTarget_);
    blocks.push_back(endBlock);
    setInsertPos(endBlock);
  } else {
    setInsertPos(NULL);
  }

  setActiveScope(savedScope);
  return true;
}

bool StmtAnalyzer::buildThrowStmtCFG(const ThrowStmt * st) {
  Expr * resultVal = NULL;
  if (st->value() != NULL) {
    //if (funcDef->isGenerator()) {
    //  diag.fatal(retSt->location(),
    //      "Return value not allowed in generator function");
    //}

    resultVal = inferTypes(
        astToExpr(st->value(), Builtins::typeThrowable),
        Builtins::typeThrowable);

    if (isErrorResult(resultVal)) {
      return false;
    }

    //if (targets.empty()) {
    //  DFAIL("Implement finally before throw ??");
    //}
  }

  currentBlock_->exitThrow(st->location(), resultVal);
  return true;
}

bool StmtAnalyzer::canCatch(TypeList & catchTypes, const CompositeType * exceptionType) {
  for (TypeList::iterator it = catchTypes.begin(); it != catchTypes.end(); ++it) {
    if (exceptionType->isSubclassOf(cast<CompositeType>(*it))) {
      return true;
    }
  }

  return false;
}

bool StmtAnalyzer::buildTryStmtCFG(const TryStmt * st) {
  const StmtList & catchList = st->catchList();
  Block * tryBlock = createBlock("try");
  currentBlock_->branchTo(st->location(), tryBlock);

  // Create a block for the 'finally' statement if any.
  Block * finallyBlock = NULL;
  if (st->finallySt()) {
    finallyBlock = createBlock("finally");
  }

  // Create the next block for after the try/catch statement.
  Block * endTry = new Block("endTry");
  endTry->setUnwindTarget(unwindTarget_);

  // Set the current exception handler context
  CleanupHandler cleanup(cleanups_, finallyBlock);
  cleanups_ = &cleanup;

  // Create the 'catch' statement blocks.
  Block * unwindBlock = NULL;
  if (!catchList.empty() || st->finallySt()) {
    TypeList catchTypes;
    bool catchAll = false;

    setInsertPos(tryBlock);
    unwindBlock = createBlock("catch");
    unwindBlock->setTerminator(
        catchList.empty()
        ? st->finallySt()->location()
        : (catchList.front()->location() | catchList.back()->location()),
        BlockTerm_Catch);

    // The active throwable object, returned from the unwind code.
    Expr * activeThrowable = new IRValueExpr(unwindBlock->termLocation(), Builtins::typeThrowable);
    unwindBlock->termExprs().push_back(activeThrowable);

    // Generate the catch cases
    for (StmtList::const_iterator it = catchList.begin(); it != catchList.end(); ++it) {
      CatchStmt * cst = static_cast<CatchStmt *>(*it);
      const SourceLocation & loc = cst->location();

      // Create a local scope in which the exception expression will be defined.
      LocalScope * catchScope = createLocalScope("catch-scope");

      // Define the exception variable in the catch scope.
      ASTDecl * exceptDecl = cst->exceptDecl();
      VariableDefn * exceptDefn = cast<VariableDefn>(
          ScopeBuilder::createLocalDefn(catchScope, function, exceptDecl));
      if (!analyzeValueDefn(exceptDefn, Task_PrepCodeGeneration)) {
        return false;
      }

      if (exceptDefn->hasTrait(Defn::RequestStackTrace)) {
        unwindBlock->setTerminator(unwindBlock->termLocation(), BlockTerm_TraceCatch);
      }

      // Get the exception type and determine if it is valid.
      const CompositeType * exceptType = dyn_cast<CompositeType>(dealias(exceptDefn->type()));
      if (isErrorResult(exceptType)) {
        continue;
      }

      // Analyze the exception type definition
      AnalyzerBase::analyzeType(exceptType, Task_PrepTypeComparison);
      if (exceptType == NULL || !exceptType->isSubclassOf(Builtins::typeThrowable.get())) {
        diag.fatal(exceptDecl) << "'" << exceptDecl << "' is not a subtype of Throwable";
        return false;
      }

      // See if any of the previous exceptions are superclasses of this one.
      if (canCatch(catchTypes, exceptType)) {
        diag.warn(loc) << "Exception handler for type " << exceptType << " can never be reached";
        continue;
      }

      // If we're catching type "Throwable", then that catches everything.
      if (exceptType->isEqual(Builtins::typeThrowable.get())) {
        catchAll = true;
      }

      exceptDefn->setType(exceptType);
      module->addSymbol(exceptDefn);

      catchTypes.push_back(const_cast<CompositeType *>(exceptType));

      // Create the catch block.
      Block * catchBody = createBlock("catch-", exceptType->typeDefn()->name());

      // Add the catch block to the switch statement.
      unwindBlock->addCase(new TypeLiteralExpr(cst->location(), exceptType), catchBody);

      // Make the catch scope the current scope for generating the block contents.
      Scope * savedScope = setActiveScope(catchScope);

      // Generate the assignment of the exception to the variable.
      setInsertPos(catchBody);
      if (exceptDefn->name() != NULL) {
        const SourceLocation & loc = cst->location();
        Expr * initExpr = new InitVarExpr(cst->location(), exceptDefn,
            CastExpr::bitCast(activeThrowable, exceptType)->at(loc));
        currentBlock_->append(initExpr);
      }

      // Generate the catch body
      buildStmtCFG(cst->body());
      setActiveScope(savedScope);

      // If the catch body fell through, then jump to finally or end
      if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
        currentBlock_->branchTo(cst->body()->finalLocation(), endTry);
        if (finallyBlock != NULL) {
          currentBlock_->append(new LocalCallExpr(finallyBlock));
        }
      }
    }

    // Whether we need a catch-all block.
    if ((finallyBlock != NULL || unwindTarget_ != NULL) && !catchAll) {
      // Create the catch block.
      Block * catchBody = createBlock("catch-all");
      if (finallyBlock != NULL) {
        catchBody->append(new LocalCallExpr(finallyBlock));
      }

      // Add it as the default case
      unwindBlock->addCase(NULL, catchBody);

      // Resume the exception.
      catchBody->exitResumeUnwind(st->location(), activeThrowable);
    }
  }

  // Note: If there's no catch block, then tryBlock might have an unwind
  // target inherited from some outer exception context, so unwindTarget might
  // not be null in this case.
  if (unwindBlock) {
    tryBlock->setUnwindTarget(unwindBlock);
  }

  // Set the current unwind target to be the unwind block.
  //cleanup.unwindTarget = unwindBlock;
  Block * saveUnwind = setUnwindTarget(unwindBlock);

  // Generate the code inside the body of the try block.
  setInsertPos(tryBlock);
  buildStmtCFG(st->body());

  // Catch statements no longer in effect at this point, but finally still is.
  //cleanup.unwindTarget = NULL;
  setUnwindTarget(saveUnwind);

  // Generate the 'else' statement.
  if (st->elseSt() != NULL) {
    // If the body had a terminator, then there's no way to reach the
    // 'else' statement.
    if (currentBlock_ == NULL || currentBlock_->hasTerminator()) {
      diag.warn(st->elseSt()) << "'else' block is unreachable.";
    }

    // Generate the 'else' statement. We don't need to add a new block for this,
    // just append it to the try body.
    buildStmtCFG(st->elseSt());
  }

  // If the body (op 'else' block) doesn't have a terminator, then branch
  // to the block after the try/catch.
  if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->finalLocation(), endTry);
    // Execute the finally block before exiting.
    if (finallyBlock != NULL) {
      currentBlock_->append(new LocalCallExpr(finallyBlock));
      currentBlock_ = NULL;
    }
  }

  // Generate the finally block.
  if (finallyBlock != NULL) {
    setInsertPos(finallyBlock);
    if (!buildStmtCFG(st->finallySt())) {
      return false;
    }

    // The finallyBlock exits with a "local return" which we'll convert into
    // a branch later.
    currentBlock_->setTerminator(
        st->finallySt()->finalLocation(),
        BlockTerm_LocalReturn);

    defineLocalProcedure(finallyBlock, currentBlock_);
  }

  cleanups_ = cleanup.prev;
  blocks.push_back(endTry);
  setInsertPos(endTry);
  return true;
}

bool StmtAnalyzer::buildReturnStmtCFG(const ReturnStmt * st) {
  Expr * resultVal = NULL;
  if (st->value() != NULL) {
    //if (funcDef->isGenerator()) {
    //  diag.fatal(retSt->location(),
    //      "Return value not allowed in generator function");
    //}

    analyzeType(returnType_, Task_PrepTypeComparison);
    resultVal = inferTypes(astToExpr(st->value(), returnType_), returnType_);
    if (isErrorResult(resultVal)) {
      return false;
    }

    // If the return type is an unsized int, and there's no explicit return
    // type declared, then choose an integer type.
    const Type * exprType = resultVal->type();
    if (exprType->isUnsizedIntType() && returnType_ == NULL) {
      if (Int32Type::instance.canConvert(resultVal) >= ExactConversion) {
        resultVal->setType(&Int32Type::instance);
      } else if (Int64Type::instance.canConvert(resultVal) >= ExactConversion) {
        resultVal->setType(&Int64Type::instance);
      }
    }

    if (returnType_ != NULL) {
      analyzeType(exprType, Task_PrepTypeComparison);
      resultVal = doImplicitCast(resultVal, returnType_);
    }
  } else if (returnType_->typeClass() == Type::Union) {
    // Converting a void to a union.
    const UnionType * utype = static_cast<const UnionType *>(returnType_);
    if (utype->hasVoidType()) {
      int typeIndex = utype->getTypeIndex(&VoidType::instance);
      CastExpr * voidValue = new CastExpr(
          Expr::UnionCtorCast,
          st->location(),
          utype,
          ConstantNull::get(st->location(), &VoidType::instance));
      voidValue->setTypeIndex(typeIndex);
      resultVal = voidValue;
    } else {
      diag.error(st) << "Return value required for non-void function";
    }
  } else if (!returnType_->isVoidType()) {
    diag.error(st) << "Return value required for non-void function";
  }


  if (macroReturnTarget_ != NULL) {
    // We are inside a macro expansion, which means that 'return' doesn't
    // actually return, it assigns to the macro result and then branches.

    // TODO: Skip this assignment if it's void.
    DASSERT(returnType_->isEqual(macroReturnVal_->type()));
    Expr * exp = new AssignmentExpr(st->location(), macroReturnVal_, resultVal);
    currentBlock_->append(exp);

    if (macroReturnTarget_ != NULL) {
      currentBlock_->branchTo(st->location(), macroReturnTarget_);

      // If there are any active exception handlers in the macro, make sure
      // that we run their finally blocks.
      for (CleanupHandler * eh = cleanups_; eh != NULL; eh = eh->prev) {
        if (eh->target != NULL) {
          currentBlock_->append(new LocalCallExpr(eh->target));
        }
      }
    }

    return true;
  }

  // If there's an active exception handler context, then we need to
  // make sure we call the finally block before returning.
  for (CleanupHandler * eh = cleanups_; eh != NULL; eh = eh->prev) {
    if (eh->target != NULL) {
      currentBlock_->append(new LocalCallExpr(eh->target));
    }
  }

  currentBlock_->exitReturn(st->location(), resultVal);
  return true;
}

bool StmtAnalyzer::buildYieldStmtCFG(const YieldStmt * st) {
  DFAIL("Unimplemented");
  return true;
}

bool StmtAnalyzer::buildBreakStmtCFG(const Stmt * st) {
  if (breakTarget_ == NULL) {
    diag.error(st) << "Break statement outside of loop body";
    return false;
  } else {
    currentBlock_->branchTo(st->location(), breakTarget_);
    return true;
  }
  return true;
}

bool StmtAnalyzer::buildContinueStmtCFG(const Stmt * st) {
  if (continueTarget_ == NULL) {
    diag.error(st) << "Continue statement outside of loop body";
    return false;
  } else {
    currentBlock_->branchTo(st->location(), continueTarget_);
    return true;
  }
}

bool StmtAnalyzer::buildLocalDeclStmtCFG(const DeclStmt * st) {
  // Handle multiple vars.
  if (st->decl()->nodeType() == ASTNode::VarList) {
    DefnList vars;
    const ASTVarDecl * varList = static_cast<const ASTVarDecl *>(st->decl());
    if (!astToDefnList(varList, vars)) {
      return false;
    }

    // Gather up the types of each var. If a var has no type, then use the 'any' type.
    ConstTypeList varTypes;
    for (DefnList::const_iterator it = vars.begin(); it != vars.end(); ++it) {
      VariableDefn * var = static_cast<VariableDefn *>(*it);
      const ASTVarDecl * varDecl = static_cast<const ASTVarDecl *>(var->ast());
      DASSERT(varDecl->value() == NULL);
      if (varDecl->type() != NULL) {
        VarAnalyzer va(var, activeScope, module, function, function);
        if (!va.analyze(Task_PrepTypeComparison)) {
          return false;
        }

        varTypes.push_back(var->type());
      } else {
        varTypes.push_back(&AnyType::instance);
      }
    }

    if (varList->value() != NULL) {
      const TupleType * tt = TupleType::get(varTypes.begin(), varTypes.end());
      ExprAnalyzer ea(module, activeScope, function, function);
      Expr * initExpr = inferTypes(ea.analyze(varList->value(), tt), tt);
      if (initExpr == NULL) {
        return false;
      }

      DASSERT_OBJ(initExpr->canonicalType()->typeClass() == Type::Tuple, initExpr);
      LValueExpr * initVar = createTempVar(".packed-value", initExpr, false);

      if (const TupleType * ttActual = dyn_cast_or_null<TupleType>(initVar->type())) {
        tt = ttActual;
      }

      // Now extract members from the tuple and assign to individual vars.
      int memberIndex = 0;
      for (DefnList::const_iterator it = vars.begin(); it != vars.end(); ++it, ++memberIndex) {
        VariableDefn * var = static_cast<VariableDefn *>(*it);
        Expr * initVal = new BinaryExpr(
            Expr::ElementRef, var->ast()->location(), tt->member(memberIndex),
            initVar, ConstantInteger::getUInt32(memberIndex));
        currentBlock_->append(new InitVarExpr(var->location(), var, initVal));
        if (var->type() == NULL) {
          DASSERT(initVal->canonicalType() != &AnyType::instance);
          var->setType(initVal->canonicalType());
        }
      }
    }

    return true;
  }

  Defn * de = astToDefn(st->decl());
  if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
    VarAnalyzer va(var, activeScope, module, function, function);
    if (!va.analyze(Task_PrepConstruction)) {
      return false;
    }

    DASSERT_OBJ(var->isSingular(), var);

    // TODO: Should we check for true constants here?
    if (var->initValue() != NULL) {
      Expr * initVal = MacroExpansionPass::run(*this, var->initValue());
      if (!isErrorResult(initVal)) {
        var->setInitValue(NULL);
        currentBlock_->append(new InitVarExpr(st->location(), var, initVal));
      }
    }
  }

  return true;
}

Expr * StmtAnalyzer::inferTypes(Expr * expr, const Type * expectedType) {
  expr = ExprAnalyzer::inferTypes(function, expr, expectedType);
  if (isErrorResult(expr)) {
    return expr;
  }

  expr = MacroExpansionPass::run(*this, expr);
  if (expr->type()->isEqual(&VoidType::instance)) {
    return expr;
  }

  DASSERT_OBJ(expr->isSingular(), expr);
  return expr;
}

Expr * StmtAnalyzer::astToAssignExpr(const ASTNode * ast, Type * expectedType) {
  if (ast->nodeType() == ASTNode::Assign) {
    return reduceAssign(static_cast<const ASTOper *>(ast));
  } else {
    return astToExpr(ast, expectedType);
  }
}

Expr * StmtAnalyzer::astToTestExpr(const ASTNode * test, bool castToBool) {
  Expr * testExpr;

  if (const ASTDecl * testDecl = dyn_cast<ASTDecl>(test)) {
    LocalScope * testScope = createLocalScope("test-scope");
    setActiveScope(testScope);

    Defn * testDefn = astToDefn(testDecl);
    if (testDefn == NULL || !analyzeDefn(testDefn, Task_PrepConstruction)) {
      return NULL;
    }

    ValueDefn * testValueDefn = cast<ValueDefn>(testDefn);

    //DASSERT(testDefn->initValue() != NULL);
    //currentBlock_->append(new InitVarExpr(st->location(), testDefn,
    //    testDefn->initValue()));

    // TODO: We need to cast the defn to a boolean as well.
    testExpr = LValueExpr::get(test->location(), NULL, testValueDefn);
  } else {
    testExpr = astToExpr(test, &BoolType::instance);
    if (isErrorResult(testExpr)) {
      return &Expr::ErrorVal;
    }

    if (!testExpr->isSingular()) {
      testExpr = TypeInferencePass::run(testExpr, &BoolType::instance, false);
    }

    testExpr = FinalizeTypesPass::run(function, testExpr);
    testExpr = MacroExpansionPass::run(*this, testExpr);
    DASSERT_OBJ(testExpr->isSingular(), testExpr);
  }

  if (isErrorResult(testExpr)) {
    return &Expr::ErrorVal;
  }

  if (testExpr->type()->isEqual(&VoidType::instance)) {
    diag.error(test) << "invalid use of void return result: " << test;
    return &Expr::ErrorVal;
  }

  if (!castToBool) {
    return testExpr;
  }

  // Compare reference type with null.
  if (testExpr->type()->isReferenceType()) {
    return new CompareExpr(test->location(),
        llvm::CmpInst::ICMP_NE, testExpr,
        ConstantNull::get(test->location(), testExpr->type()));
  } else if (const AddressType * mat = dyn_cast<AddressType>(testExpr->type())) {
    return new CompareExpr(test->location(),
        llvm::CmpInst::ICMP_NE, testExpr,
        ConstantNull::get(test->location(), testExpr->type()));
  }

  // Cast to boolean.
  return BoolType::instance.implicitCast(test->location(), testExpr);
}

Expr * StmtAnalyzer::astToExpr(const ASTNode * ast, const Type * expectedType) {
  return reduceExpr(ast, expectedType);
}

Defn * StmtAnalyzer::astToDefn(const ASTDecl * ast) {
  if (ast->nodeType() == ASTNode::VarList) {
    diag.error(ast) << "Multiple variable declarations not allowed here";
  }

  return ScopeBuilder::createLocalDefn(activeScope, function, ast);
}

bool StmtAnalyzer::astToDefnList(const ASTVarDecl * ast, DefnList & vars) {
  for (ASTDeclList::const_iterator it = ast->members().begin(); it != ast->members().end(); ++it) {
    Defn * var = ScopeBuilder::createLocalDefn(activeScope, function, *it);
    vars.push_back(var);
  }

  return true;
}

LValueExpr * StmtAnalyzer::setMacroReturnVal(LValueExpr * retVal) {
  LValueExpr * oldVal = macroReturnVal_;
  macroReturnVal_ = retVal;
  return oldVal;
}

Block * StmtAnalyzer::setMacroReturnTarget(Block * blk) {
  Block * oldBlock = macroReturnTarget_;
  macroReturnTarget_ = blk;
  return oldBlock;
}

/** Set the return type - used when doing macro expansion. */
const Type * StmtAnalyzer::setReturnType(const Type * returnType) {
  const Type * oldType = returnType_;
  returnType_ = returnType;
  return oldType;
}

LocalScope * StmtAnalyzer::createLocalScope(const char * scopeName) {
  DASSERT(activeScope != NULL);
  LocalScope * newScope = new LocalScope(activeScope);
  newScope->setScopeName(scopeName);
  DASSERT(newScope->parentScope() != NULL);

  function->localScopes().push_back(newScope);
  return newScope;
}

void StmtAnalyzer::setInsertPos(Block * blk) {
  currentBlock_ = blk;
  insertPos_ = std::find(blocks.begin(), blocks.end(), blk);
  if (insertPos_ != blocks.end()) {
    ++insertPos_;
  }
}

Block * StmtAnalyzer::createBlock(const char * name) {
  Block * block = new Block(name);

  // If a throw statement should occur within this block, here is where it will go.
  block->setUnwindTarget(unwindTarget_);
  insertPos_ = blocks.insert(insertPos_, block) + 1;
  return block;
}

Block * StmtAnalyzer::createBlock(const char * prefix, const std::string & suffix) {
  const char * blockName = istrings.intern(std::string(prefix) + suffix);
  return createBlock(blockName);
}

LValueExpr * StmtAnalyzer::createTempVar(const char * name, Expr * value, bool isMutable) {
  VariableDefn * var = new VariableDefn(isMutable ? Defn::Var : Defn::Let, module, name);
  var->setLocation(value->location());
  var->setType(value->type());
  var->setStorageClass(Storage_Local);
  activeScope->addMember(var);
  currentBlock_->append(new InitVarExpr(value->location(), var, value));
  return LValueExpr::get(value->location(), NULL, var);
}

void StmtAnalyzer::defineLocalProcedure(Block * first, Block * last) {
  LocalProcedure & proc = localProcedures_[first];
  if (proc.first == NULL) {
    proc.first = first;
    proc.last = last;
  }
}

StmtAnalyzer::LocalProcedure & StmtAnalyzer::findLocalProcedure(Block * first) {
  LocalProcedureMap::iterator it = localProcedures_.find(first);
  if (it != localProcedures_.end()) {
    DASSERT(it->second.first != NULL);
    DASSERT(it->second.last != NULL);
    return it->second;
  }

  DFAIL("Block is not the start of a local procedure.");
}

int StmtAnalyzer::LocalProcedure::addFollowingBlock(Block * b) {
  BlockList::iterator it = std::find(followingBlocks.begin(), followingBlocks.end(), b);
  if (it != followingBlocks.end()) {
    return it - followingBlocks.begin();
  }

  followingBlocks.push_back(b);
  return followingBlocks.size() - 1;
}

FunctionDefn * StmtAnalyzer::findInterfaceMethod(const CompositeType * type, const Type * interface,
    const char * method) {

  // Analyze the type.
  if (!AnalyzerBase::analyzeType(type, Task_PrepMemberLookup)) {
    return NULL;
  }

  // See if the type even has any members with that name.
  DefnList defns;
  if (!type->memberScope()->lookupMember(method, defns, true)) {
    return NULL;
  }

  // Analyze the interface.
  if (!AnalyzerBase::analyzeType(interface, Task_PrepMemberLookup)) {
    return NULL;
  }

  const CompositeType * interfaceClass = cast<CompositeType>(interface);
  if (!type->implements(interfaceClass)) {
    return NULL;
  }

  DefnList interfaceMethods;
  if (!type->memberScope()->lookupMember(method, interfaceMethods, true)) {
    diag.error(interface->typeDefn()) << "Interface '" << interface <<
        "' does not define method '" << method << "'";
    return NULL;
  } else if (interfaceMethods.size() != 1) {
    diag.error(interface->typeDefn()) << "Interface '" << interface <<
        "' has multiple definitions of method '" << method << "'";
  }

  FunctionDefn * interfaceMethod = cast<FunctionDefn>(interfaceMethods.front());
  for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
    if (FunctionDefn * fn = dyn_cast<FunctionDefn>(*it)) {
      if (fn->isOverrideOf(interfaceMethod)) {
        return fn;
      }
    }
  }

  return NULL;
}

void StmtAnalyzer::flattenLocalProcedureCalls() {

  // If no local procedures have been defined, then there's nothing to do here.
  if (localProcedures_.empty()) {
    return;
  }

  // Find all local procedure calls and determine how many unique return targets there are
  // for each procedure.
  for (BlockList::iterator bi = blocks.begin(); bi != blocks.end(); ++bi) {
    Block * blk = *bi;
    ExprList::iterator eiEnd = blk->exprs().end();
    for (ExprList::iterator ei = blk->exprs().begin(); ei != eiEnd;) {
      if (LocalCallExpr * localCall = dyn_cast<LocalCallExpr>(*ei++)) {
        if (ei == eiEnd) {
          LocalProcedure & proc = findLocalProcedure(localCall->target());
          if (blk->terminator() == BlockTerm_Branch) {
            localCall->setReturnState(proc.addFollowingBlock(blk->succs().front()));
          } else if (blk->terminator() == BlockTerm_Return
              || blk->terminator() == BlockTerm_ResumeUnwind) {
            Block * newBlock = blk->split();
            if (currentBlock_ == blk) {
              currentBlock_ = newBlock;
            }
            bi = blocks.insert(bi + 1, newBlock);
            blk->branchTo(blk->termLocation(), newBlock);
            localCall->setReturnState(proc.addFollowingBlock(newBlock));
          } else {
            DFAIL("Implement");
          }
        } else {
          DFAIL("Implement splitting");
        }
      }
    }
  }

  // Allocate a state variable for each local procedure.
  // TODO: Decide whether the local procedure is simple enough that it would be cheaper to
  // inline it rather than setting a state variable.
  LocalScope * stateVarScope = NULL;
  for (LocalProcedureMap::iterator it = localProcedures_.begin(); it != localProcedures_.end();
      ++it) {
    LocalProcedure & proc = it->second;

    // Clear out the old terminator
    DASSERT(proc.last->terminator() == BlockTerm_LocalReturn);
    proc.last->clearTerminator();

    if (proc.followingBlocks.size() == 1) {
      // If there's only one block, then do an unconditional branch to it.
      proc.last->branchTo(SourceLocation(), proc.followingBlocks.front());
    } else {
      // Otherwise, create a state variable and do a conditional branch.
      proc.stateVar = new VariableDefn(Defn::Var, NULL, "__state");
      proc.stateVar->setType(&Int32Type::instance);
      proc.stateVar->setStorageClass(Storage_Local);
      proc.stateVar->addTrait(Defn::Singular);
      proc.stateExpr = LValueExpr::get(proc.stateVar);

      // Add it to the local scope
      if (stateVarScope == NULL) {
        stateVarScope = new LocalScope(activeScope);
        stateVarScope->setScopeName("proc-state");
        function->localScopes().push_back(stateVarScope);
      }
      stateVarScope->addMember(proc.stateVar);

      // Now, use the state variable to branch back to the call location.
      if (proc.followingBlocks.size() == 2) {
        // If there are two possible return targets, then do a conditional branch.
        proc.stateVar->setType(&BoolType::instance);
        proc.stateExpr->setType(&BoolType::instance);
        proc.last->condBranchTo(SourceLocation(), proc.stateExpr, proc.followingBlocks[1],
            proc.followingBlocks[0]);
      } else {
        // Otherwise, do a multi-way switch instruction.
        DFAIL("Implement switch statement");
      }
    }
  }

  // Replace all local calls with branches.
  for (BlockList::iterator bi = blocks.begin(); bi != blocks.end(); ++bi) {
    Block * blk = *bi;
    if (!blk->exprs().empty()) {
      // At this point, the local call can only be the last expression in the block, since
      // we've already split each block immediately after a local call.
      ExprList::iterator ei = blk->exprs().end() - 1;
      if (LocalCallExpr * localCall = dyn_cast<LocalCallExpr>(*ei)) {
        LocalProcedure & proc = findLocalProcedure(localCall->target());

        // Insure that the terminator is a branch, and then replace it with a branch
        // to the first block of the procedure.
        DASSERT(blk->terminator() == BlockTerm_Branch);
        blk->succs()[0] = proc.first;

        if (proc.stateExpr == NULL) {
          // Erase the local call
          blk->exprs().erase(ei);
        } else {
          // Replace the local call with a load of the state variable.
          Expr * stateValue = ConstantInteger::get(SourceLocation(), proc.stateVar->type(),
              localCall->returnState());
          *ei = new AssignmentExpr(SourceLocation(), proc.stateExpr, stateValue);
        }
      }
    }
  }

  //function->dumpBlocks();
}

void StmtAnalyzer::optimizeBranches() {
  for (BlockList::iterator b = blocks.begin(); b != blocks.end(); ++b) {
    Block * blk = *b;
    if (blk->terminator() == BlockTerm_Conditional) {
      Expr * testExpr = blk->termValue();
      bool isConstTrue = false;
      bool isConstFalse = false;
      if (ConstantInteger * cint = dyn_cast<ConstantInteger>(testExpr)) {
        if (cint->value()->isZero()) {
          isConstFalse = true;
        } else {
          isConstTrue = true;
        }
      }

      if (isConstTrue) {
        blk->setTerminator(blk->termLocation(), BlockTerm_Branch);
        blk->succs().pop_back();
      } else if (isConstFalse) {
        blk->setTerminator(blk->termLocation(), BlockTerm_Branch);
        blk->succs().erase(blk->succs().begin());
      }
    }
  }
}

void StmtAnalyzer::removeDeadBlocks() {
  llvm::SmallPtrSet<Block *, 128> visited;
  visitSuccessors(blocks.front(), visited);
  for (BlockList::iterator bi = blocks.begin(); bi != blocks.end();) {
    Block * blk = *bi;
    if (!visited.count(blk)) {
      if (currentBlock_ == blk) {
        currentBlock_ = NULL;
      }

      bi = blocks.erase(bi);
      //++bi;
    } else {
      ++bi;
    }
  }
}

} // namespace tart

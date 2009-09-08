/* ================================================================ *
 TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/Module.h"

#include "tart/AST/Stmt.h"

#include "tart/Sema/StmtAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/FinalizeTypesPass.h"
#include "tart/Sema/MacroExpansionPass.h"
#include "tart/Sema/ScopeBuilder.h"

#include "tart/Objects/Builtins.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"

#define IMPLICIT_SELF 1

namespace tart {

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
      selfExpr = new LValueExpr(selfParam->location(), NULL, selfParam);
    }

    return selfExpr;
  }
};

/// -------------------------------------------------------------------
/// StmtAnalyzer

StmtAnalyzer::StmtAnalyzer(FunctionDefn * func) :
  AnalyzerBase(func->module(), &func->parameterScope()), function(func), returnType(NULL),
      yieldType_(NULL), blocks(func->blocks()), currentBlock_(NULL), continueTarget_(NULL),
      breakTarget(NULL), unwindTarget_(NULL), cleanups_(NULL), loopCleanups_(NULL),
      macroReturnVal_(NULL), macroReturnTarget_(NULL) {
  insertPos_ = blocks.end();
  returnType = function->returnType();
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
    }

    // Create the initial block.
    setInsertPos(createBlock("entry"));
    const Stmt * body = function->functionDecl()->body();
    if (!buildStmtCFG(body)) {
      return false;
    }

    if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
      DASSERT_OBJ(macroReturnTarget_ == NULL, function);
      //if (macroReturnTarget_ != NULL) {
      //currentBlock->branchTo(body->finalLocation(), macroReturnTarget_);
      //} else {
      currentBlock_->exitReturn(body->finalLocation(), NULL);
      //}
    }

    // Now flatten all local returns.
    flattenLocalProcedureCalls();

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
  if (testExpr->isConstant()) {
    // TODO: See if the test expression is a constant.
    // Check for bool and int types, not floats.
  }

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
  Expr * testExpr = astToTestExpr(st->testExpr());
  if (isErrorResult(testExpr)) {
    return false;
  }

  DASSERT(testExpr != NULL);
  DASSERT_OBJ(testExpr->type() != NULL, testExpr);
  if (testExpr->isConstant()) {
    // If it's a constant, it may affect whether things fall through
    // or not.
  }

  // Create the set of basic blocks. We don't know yet
  // if we need an else block or endif block.
  Block * blkTest = createBlock("test");
  Block * blkBody = createBlock("loopbody");
  Block * blkDone = createBlock("endwhile");

  // Start by branching to the 'test' block.
  currentBlock_->branchTo(st->location(), blkTest);

  // Generate the 'test' block with the conditional branch.
  blkTest->condBranchTo(st->testExpr()->location(),
      testExpr, blkBody, blkDone);

  // Generate the 'body' block.
  Block * saveBreakTarget = breakTarget;
  Block * saveContinueTarget = continueTarget_;
  CleanupHandler * saveLoopEH = loopCleanups_;
  setInsertPos(blkBody);
  breakTarget = blkDone;
  loopCleanups_ = cleanups_;
  continueTarget_ = blkTest;
  buildStmtCFG(st->body());
  breakTarget = saveBreakTarget;
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
      if (!analyzeValueDefn(initDefn, Task_PrepCodeGeneration)) {
        return false;
      }
      DASSERT(initDefn->initValue() != NULL);
      currentBlock_->append(new InitVarExpr(st->location(), initDefn, initDefn->initValue()));
      //if (initDefn == NULL) {
      //  return NULL;
      //}
    } else if (initExpr->nodeType() == ASTNode::Tuple) {
      const ASTOper * initTuple = static_cast<const ASTOper *>(initExpr);
      DFAIL("Implement");
    }
  }

#if 0
  if (initExpr) {
    if (initExpr->exprType() == Expr::Let ||
        initExpr->exprType() == Expr::Var) {
      ValueDef * initDecl =
      static_cast<ValueDef *>(const_cast<Expr *>(initExpr));
      if (!analyzeLocalDecl(initDecl)) {
        return false;
      }

      currentBlock_->append(DeclStatement::get(initDecl));
    } else if (initExpr->getClass() == Expr::Assign) {
      const OpExpr * op = static_cast<const OpExpr *>(initExpr);
      const Expr * to = op->getOperands()[0];
      if (const ValueDef * initDecl = dyn_cast<ValueDef>(to)) {
        DASSERT(false && "shouldn't happen - decls are inited, not assigned");
        /*initDecl->setValue(op->getOperands()[1]);
         if (!analyzeLocalDecl(loopAnalyzer, initDecl)) {
         return false;
         }*/
      } else if (to->getClass() == Expr::Tuple) {
        DASSERT(false && "deal with tuples.");
      } else {
        DASSERT(false && "eh?");
      }

      initExpr = analyzer.reduceExpression(initExpr);
      DASSERT(initExpr != NULL);
      currentBlock_->append(ExprStatement::get(initExpr));
    } else {
      DASSERT(false);
    }
  }
#endif

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
  Block * saveBreakTarget = breakTarget;
  Block * saveContinueTarget = continueTarget_;

  breakTarget = blkDone;
  continueTarget_ = blkIncr ? blkIncr : (blkTest ? blkTest : blkBody);

  setInsertPos(blkBody);
  buildStmtCFG(st->body());

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->location(), continueTarget_);
  }

  breakTarget = saveBreakTarget;
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

  CompositeType * iterType = static_cast<CompositeType *>(iterExpr->type());
  AnalyzerBase::analyzeTypeDefn(iterType->typeDefn(), Task_PrepMemberLookup);
  FunctionDefn * next = findInterfaceMethod(iterType, Builtins::typeIterator, "next");
  if (next == NULL) {
    // If it's not an Iterator, see if it's an Iterable.
    FunctionDefn * iterate = findInterfaceMethod(iterType, Builtins::typeIterable, "iterate");
    if (iterate == NULL) {
      diag.error(iterExpr) << "Invalid iterator type: " << iterExpr->type();
      return false;
    }

    LValueExpr * iterMethod = new LValueExpr(iterate->location(), iterExpr, iterate);
    iterExpr = inferTypes(ExprAnalyzer(module, activeScope, function).callExpr(
        st->location(), iterMethod, ASTNodeList(), NULL), NULL);
    if (iterExpr == NULL) {
      return false;
    }

    if (!isa<CompositeType>(iterExpr->type())) {
      diag.error(iterExpr) << "Invalid iterator type: " << iterExpr->type();
      return false;
    }

    iterType = static_cast<CompositeType *>(iterExpr->type());
    AnalyzerBase::analyzeTypeDefn(iterType->typeDefn(), Task_PrepMemberLookup);
    next = findInterfaceMethod(iterType, Builtins::typeIterator, "next");
    if (next == NULL) {
      diag.error(iterExpr) << "Invalid iterator type: " << iterExpr->type();
      return false;
    }
  }

  iterExpr = createTempVar(".iterator", iterExpr, false);
  LValueExpr * nextMethod = new LValueExpr(next->location(), iterExpr, next);
  Expr * nextCall = inferTypes(ExprAnalyzer(module, activeScope, function).callExpr(
      iterExpr->location(), nextMethod, ASTNodeList(), NULL), NULL);
  if (nextCall == NULL) {
    return false;
  }

  Block * blkTest = createBlock("test");
  currentBlock_->branchTo(st->location(), blkTest);
  setInsertPos(blkTest);

  LValueExpr * nextVal = createTempVar(".iterval", nextCall, true);

  UnionType * utype = dyn_cast<UnionType>(nextVal->type());
  DASSERT(utype != NULL);
  DASSERT(utype->members().size() == 2);
  Type * iterVarType = NULL;
  for (TypeList::const_iterator it = utype->members().begin(); it != utype->members().end(); ++it) {
    Type * ty = *it;
    if (!ty->isVoidType()) {
      iterVarType = ty;
    }
  }

  Expr * testExpr = new InstanceOfExpr(st->location(), nextVal, &VoidType::instance);
  Expr * iterValue = new CastExpr(Expr::UnionMemberCast, st->location(), iterVarType, nextVal);

  // Generate the 'body' block.
  Block * blkBody = createBlock("loopbody");
  Block * blkDone = createBlock("endfor");
  Block * saveBreakTarget = breakTarget;
  Block * saveContinueTarget = continueTarget_;

  // Assign the next value to the iteration variable.
  if (st->loopVars()->nodeType() == ASTNode::Var) {
    const ASTVarDecl * initDecl = static_cast<const ASTVarDecl *>(st->loopVars());
    VariableDefn * initDefn = cast<VariableDefn>(astToDefn(initDecl));
    initDefn->setType(iterVarType);
    if (!analyzeValueDefn(initDefn, Task_PrepCodeGeneration)) {
      return false;
    }

    DASSERT(initDefn->initValue() == NULL);
    blkBody->append(new InitVarExpr(st->location(), initDefn, iterValue));
  } else {
    DFAIL("Implement multiple loop variables");
  }

  breakTarget = blkDone;
  continueTarget_ = blkTest;

  setInsertPos(blkBody);
  buildStmtCFG(st->body());

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->location(), continueTarget_);
  }

  breakTarget = saveBreakTarget;
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

  Type * testType = testExpr->type();

  if (PrimitiveType * ptype = dyn_cast<PrimitiveType>(testType)) {
    if (isIntegerType(ptype->typeId())) {
      // TODO: Implement
    } else {
      diag.error(st) << "Invalid expression type for switch statement: " << testType;
    }
  } else if (EnumType * etype = dyn_cast<EnumType>(testType)) {
    // If it's an enum, allow unqualified enum constants to be used.
    caseValScope = new DelegatingScope(etype->memberScope(), activeScope);
    // TODO: Implement
  } else if (testType == Builtins::typeString) {
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
      CaseStmt * caseSt = static_cast<const CaseStmt *>(s);
      ASTNode * caseValAst = caseSt->caseExpr();
      ConstantExprList caseValList;
      Scope * saveCaseScope = setActiveScope(caseValScope);
      if (caseValAst->nodeType() == ASTNode::Tuple) {
        const ASTOper * caseVals = static_cast<const ASTOper *>(caseValAst);
        for (ASTNodeList::const_iterator it = caseVals->args().begin();
            it != caseVals->args().end(); ++it) {
          ConstantExpr * caseVal = astToCaseValueExpr(caseSt->caseExpr(), testType);
          if (caseVal == NULL) {
            continue;
          }

          caseValList.push_back(caseVal);
        }
      } else {
        ConstantExpr * caseVal = astToCaseValueExpr(caseSt->caseExpr(), testType);
        if (caseVal == NULL) {
          continue;
        }

        caseValList.push_back(caseVal);
      }

      setActiveScope(saveCaseScope);
      DASSERT(!caseValList.empty());

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

ConstantExpr * StmtAnalyzer::astToCaseValueExpr(const ASTNode * ast, Type * testType) {
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

  if (!result->isConstant()) {
    diag.error(ast) << "Case expression '" << ast << "' is not a constant.";
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
  Type * fromType = testExpr->type();
  Expr::ExprType castType = Expr::BitCast;
  if (fromType->isReferenceType()) {
  } else if (UnionType * utype = dyn_cast<UnionType>(fromType)) {
    castType = Expr::UnionMemberCast;
  } else {
    diag.warn(testExpr) << "Classify expression's type is already known: " << fromType;
  }

  // Create the temporary variable which is going to hold the test expression.
  VariableDefn * testVar = new VariableDefn(Defn::Var, module, "classify-expr", testExpr);
  testVar->setStorageClass(Storage_Local);
  testVar->setType(testExpr->type());
  activeScope->addMember(testVar);
  currentBlock_->append(new InitVarExpr(st->location(), testVar, testExpr));

  // After this we will refer to the variable instead of referring to its value.
  LValueExpr * testLVal = new LValueExpr(testExpr->location(), NULL, testVar);

  Block * endBlock = NULL;
  Block * prevTestBlock = currentBlock_;
  prevTestBlock->setTerminator(st->location(), BlockTerm_Branch);

  llvm::SmallSet<Type *, 16> typesSeen;
  const Stmt * elseSt = NULL;
  const StmtList & cases = st->caseList();
  for (StmtList::const_iterator it = cases.begin(); it != cases.end(); ++it) {
    const Stmt * s = *it;
    if (s->nodeType() == ASTNode::Case) {
      const CaseStmt * caseSt = static_cast<const CaseStmt *>(s);
      if (const ASTDecl * asDecl = dyn_cast<ASTDecl>(caseSt->caseExpr())) {
        SourceLocation stLoc = asDecl->location();
        LocalScope * caseScope = createLocalScope("as-scope");
        Scope * prevScope = setActiveScope(caseScope);

        Defn * asDefn = astToDefn(asDecl);
        if (asDefn == NULL || !analyzeDefn(asDefn, Task_PrepCodeGeneration)) {
          return NULL;
        }

        VariableDefn * asValueDefn = cast<VariableDefn>(asDefn);
        Type * toType = asValueDefn->type();
        if (!analyzeType(toType, Task_PrepCallOrUse)) {
          return NULL;
        }

        if (typesSeen.count(toType)) {
          diag.error(asDecl) << "Duplicate type test '" << toType << "'.";
          setActiveScope(prevScope);
          continue;
        }

        typesSeen.insert(toType);

        // Create the block containing the type test
        Block * testBlock = createBlock("as-test-", toType->typeDefn()->name());

        // Create the block containing the case body.
        Block * caseBlock = createBlock("as-", toType->typeDefn()->name());

        // The previous test's failure target should jump to the test.
        prevTestBlock->succs().push_back(testBlock);
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
        buildStmtCFG(caseSt->body());

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

bool StmtAnalyzer::canCatch(TypeList & catchTypes, CompositeType * exceptionType) {
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

      // Get the exception type and determine if it is valid.
      CompositeType * exceptType = dyn_cast<CompositeType>(dealias(exceptDefn->type()));
      if (isErrorResult(exceptType)) {
        continue;
      }

      // Analyze the exception type definition
      AnalyzerBase::analyzeTypeDefn(exceptType->typeDefn(), Task_PrepMemberLookup);
      if (exceptType == NULL ||
          !exceptType->isSubclassOf(cast<CompositeType>(Builtins::typeThrowable))) {
        diag.fatal(exceptDecl) << "'" << exceptDecl << "' is not a subtype of Throwable";
        return false;
      }

      // See if any of the previous exceptions are superclasses of this one.
      if (canCatch(catchTypes, exceptType)) {
        diag.warn(loc) << "Exception handler for type " << exceptType << " can never be reached";
        continue;
      }

      // If we're catching type "Throwable", then that catches everything.
      if (exceptType->isEqual(cast<CompositeType>(Builtins::typeThrowable))) {
        catchAll = true;
      }

      exceptDefn->setType(exceptType);
      module->addSymbol(exceptDefn);

      catchTypes.push_back(exceptType);

      // Create the catch block.
      Block * catchBody = createBlock("catch-", exceptType->typeDefn()->name());

      // Add the catch block to the switch statement.
      unwindBlock->addCase(new ConstantType(cst->location(), exceptType), catchBody);

      // Make the catch scope the current scope for generating the block contents.
      Scope * savedScope = setActiveScope(catchScope);

      // Generate the assignment of the exception to the variable.
      setInsertPos(catchBody);
      if (exceptDefn->name() != NULL) {
        const SourceLocation & loc = cst->location();
        Expr * initExpr = new InitVarExpr(cst->location(), exceptDefn,
            new CastExpr(Expr::BitCast, loc, exceptType, activeThrowable));
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

    resultVal = inferTypes(astToExpr(st->value(), returnType), returnType);
    if (isErrorResult(resultVal)) {
      return false;
    }

    Type * exprType = resultVal->type();

    // If the return type is an unsized int, and there's no explicit return
    // type declared, then choose an integer type.
    if (exprType->isUnsizedIntType() && returnType == NULL) {
      if (IntType::instance.canConvert(resultVal) >= ExactConversion) {
        resultVal->setType(&IntType::instance);
      } else if (LongType::instance.canConvert(resultVal) >= ExactConversion) {
        resultVal->setType(&LongType::instance);
      }
    }
  }

  if (macroReturnTarget_ != NULL) {
    // We are inside a macro expansion, which means that 'return' doesn't
    // actually return, it assigns to the macro result andt then branches.

    // TODO: Skip this assignment if it's void.
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
  DFAIL("Unimplemented");
  return true;
}

bool StmtAnalyzer::buildContinueStmtCFG(const Stmt * st) {
  DFAIL("Unimplemented");
  return true;
}

bool StmtAnalyzer::buildLocalDeclStmtCFG(const DeclStmt * st) {
  Defn * de = astToDefn(st->decl());
  if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
    if (!analyzeValueDefn(var, Task_PrepCodeGeneration)) {
      return false;
    }

    DASSERT_OBJ(var->isSingular(), var);

    // TODO: Should we insure that Let has an initializer?
    // TODO: Should we check for true constants here?
    if (var->initValue() != NULL) {
      Expr * initVal = inferTypes(var->initValue(), var->type());
      if (!isErrorResult(initVal)) {
        currentBlock_->append(new InitVarExpr(st->location(), var, initVal));
      }
    }
  }

  return true;
}

Expr * StmtAnalyzer::inferTypes(Expr * expr, Type * expectedType) {
  expr = ExprAnalyzer::inferTypes(expr, expectedType);
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
    ExprAnalyzer ea(function->module(), activeScope, function);
    return ea.reduceAssign(static_cast<const ASTOper *>(ast));
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
    if (testDefn == NULL || !analyzeDefn(testDefn, Task_PrepCodeGeneration)) {
      return NULL;
    }

    ValueDefn * testValueDefn = cast<ValueDefn>(testDefn);

    //DASSERT(testDefn->initValue() != NULL);
    //currentBlock_->append(new InitVarExpr(st->location(), testDefn,
    //    testDefn->initValue()));

    // TODO: We need to cast the defn to a boolean as well.
    testExpr = new LValueExpr(test->location(), NULL, testValueDefn);
  } else {
    testExpr = astToExpr(test, &BoolType::instance);
    if (isErrorResult(testExpr)) {
      return &Expr::ErrorVal;
    }

    if (!testExpr->isSingular()) {
      testExpr = TypeInferencePass::run(testExpr, &BoolType::instance, false);
    }

    testExpr = FinalizeTypesPass::run(testExpr);
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
        llvm::CmpInst::ICMP_EQ, testExpr,
        ConstantNull::get(test->location(), testExpr->type()));
  }

  // Cast to boolean.
  return BoolType::instance.implicitCast(test->location(), testExpr);
}

Expr * StmtAnalyzer::astToExpr(const ASTNode * ast, Type * expectedType) {
  ExprAnalyzer ea(function->module(), activeScope, function);
  return ea.reduceExpr(ast, expectedType);
}

Defn * StmtAnalyzer::astToDefn(const ASTDecl * ast) {
  return ScopeBuilder::createLocalDefn(activeScope, function, ast);
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
  const char * blockName = istrings.intern(std::string("prefix") + suffix);
  return createBlock(blockName);
}

LValueExpr * StmtAnalyzer::createTempVar(const char * name, Expr * value, bool isMutable) {
  VariableDefn * var = new VariableDefn(isMutable ? Defn::Var : Defn::Let, module, name, value);
  var->setLocation(value->location());
  var->setType(value->type());
  var->setStorageClass(Storage_Local);
  activeScope->addMember(var);
  currentBlock_->append(new InitVarExpr(value->location(), var, value));
  return new LValueExpr(value->location(), NULL, var);
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

FunctionDefn * StmtAnalyzer::findInterfaceMethod(CompositeType * type, Type * interface,
    const char * method) {

  // Analyze the type.
  if (!AnalyzerBase::analyzeTypeDefn(type->typeDefn(), Task_PrepMemberLookup)) {
    return NULL;
  }

  // See if the type even has any members with that name.
  DefnList defns;
  if (!type->memberScope()->lookupMember(method, defns, true)) {
    return NULL;
  }

  // Analyze the interface.
  if (!AnalyzerBase::analyzeTypeDefn(type->typeDefn(), Task_PrepMemberLookup)) {
    return NULL;
  }

  CompositeType * interfaceClass = cast<CompositeType>(interface);
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
      proc.stateVar->setType(&IntType::instance);
      proc.stateVar->setStorageClass(Storage_Local);
      proc.stateVar->addTrait(Defn::Singular);
      proc.stateExpr = new LValueExpr(SourceLocation(), NULL, proc.stateVar);

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

} // namespace tart

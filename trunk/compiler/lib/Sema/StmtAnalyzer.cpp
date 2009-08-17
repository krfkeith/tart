/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Defn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
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

namespace tart {

// A scope which allows definitions in the enclosing class to be looked up
// via the 'self' parameter.
class SelfScope : public DelegatingScope {
  ParameterDefn * selfParam;
  Expr * selfExpr;
  
public:
  SelfScope(Scope * delegate, Scope * parent)
    : DelegatingScope(delegate, parent)
    , selfParam(NULL), selfExpr(NULL)
  {}
  
  void setSelfParam(ParameterDefn * self) { selfParam = self; }

  Expr * getBaseExpr() {
    if (selfExpr == NULL) {
      selfExpr = new LValueExpr(selfParam->getLocation(), NULL, selfParam);
    }

    return selfExpr;
  }
};

/// -------------------------------------------------------------------
/// StmtAnalyzer

StmtAnalyzer::StmtAnalyzer(FunctionDefn * func)
  : AnalyzerBase(func->module(), &func->parameterScope())
  , function(func)
  , returnType(NULL)
  , yieldType_(NULL)
  , blocks(func->blocks())
  , currentBlock_(NULL)
  , continueTarget_(NULL)
  , breakTarget(NULL)
  , unwindTarget_(NULL)
  , cleanups_(NULL)
  , loopCleanups_(NULL)
  , macroReturnVal_(NULL)
  , macroReturnTarget_(NULL)
{
  insertPos_ = blocks.end();
  returnType = function->returnType();
}

bool StmtAnalyzer::buildCFG() {
  if (function->getFunctionDecl() &&
      function->getFunctionDecl()->getBody() != NULL) {

    // Create a temporary scope to allow lookup of the function parameters.
    DelegatingScope parameterScope(&function->parameterScope(), function->definingScope());
    setActiveScope(&parameterScope);

    // If this is an instance method, then set up the implicit 'self'
    // scope as well. This scope searches the type of the self parameter,
    // and is always searched immediately after the parameter scope.
    if (function->storageClass() == Storage_Instance) {
      ParameterDefn * selfParam = function->functionType()->selfParam();
      DASSERT_OBJ(selfParam != NULL, function);
      DASSERT_OBJ(selfParam->getType() != NULL, function);
      TypeDefn * selfType = selfParam->getType()->typeDefn();
      DASSERT_OBJ(selfType != NULL, function);

      // Uncomment to allow 'self' to be searched implicitly.
      //SelfScope * selfScope = new SelfScope(
      //    selfType->getTypeValue()->memberScope(), function->definingScope());
      //selfScope->setSelfParam(selfParam);
      //parameterScope.setParentScope(selfScope);
    }

    // Create the initial block.
    setInsertPos(createBlock("entry"));
    const Stmt * body = function->getFunctionDecl()->getBody();
    if (!buildStmtCFG(body)) {
      return false;
    }
    
    if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
      DASSERT_OBJ(macroReturnTarget_ == NULL, function);
      //if (macroReturnTarget_ != NULL) {
        //currentBlock->branchTo(body->getFinalLocation(), macroReturnTarget_);
      //} else {
      currentBlock_->exitReturn(body->getFinalLocation(), NULL);
      //}
    }
    
    // Now flatten all local returns.
    flattenLocalProcedureCalls();
    
    return true;
  }
  
  return true;
}

bool StmtAnalyzer::buildStmtCFG(const Stmt * st) {
  switch (st->getNodeType()) {
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
      diag.fatal(st->getLocation()) << "Invalid statement type '" <<
          getNodeTypeName(st->getNodeType());
      return false;
  }
}

bool StmtAnalyzer::buildBlockStmtCFG(const BlockStmt * st) {
  bool success = true;
  LocalScope * blockScope = createLocalScope("block-scope");
  Scope * savedScope = setActiveScope(blockScope);

  // Now process all of the statements
  const StmtList & stlist = st->getStmts();
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
  const ASTNode * value = st->getValue();
  Expr * expr = inferTypes(astToAssignExpr(value, NULL), NULL);
  if (!isErrorResult(expr)) {
    if (expr->isSideEffectFree() && value->getNodeType() != ASTNode::Call) {
      diag.warn(expr) << "Statement '" << st << "' has no effect";
    }

    currentBlock_->append(expr);
    return true;
  }
  
  return false;
}

bool StmtAnalyzer::buildIfStmtCFG(const IfStmt * st) {
  Scope * savedScope = activeScope;
  Expr * testExpr = astToTestExpr(st->getTestExpr());
  if (isErrorResult(testExpr)) {
    return false;
  }

  DASSERT(testExpr != NULL);
  DASSERT(testExpr->getType() != NULL);
  if (testExpr->isConstant()) {
    // TODO: See if the test expression is a constant.
    // Check for bool and int types, not floats.
  }

  bool hasElse = (st->getElseSt() != NULL);

  // Create the set of basic blocks. We don't know yet
  // if we need an else block or endif block.
  Block * blkThen = createBlock("then");
  Block * blkElse = NULL;
  Block * blkDone = NULL;

  if (st->getElseSt() != NULL) {
    blkElse = createBlock("else");
    currentBlock_->condBranchTo(st->getTestExpr()->getLocation(), testExpr, blkThen, blkElse);
  } else {
    blkDone = createBlock("endif");
    currentBlock_->condBranchTo(st->getTestExpr()->getLocation(), testExpr, blkThen, blkDone);
  }

  // Generate the contents of the 'then' block.
  setInsertPos(blkThen);
  buildStmtCFG(st->getThenSt());

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
    if (blkDone == NULL)
      blkDone = createBlock("endif");
    currentBlock_->branchTo(st->getThenSt()->getFinalLocation(), blkDone);
  }

  // Generate the contents of the 'else' block
  if (blkElse != NULL) {
    setInsertPos(blkElse);
    buildStmtCFG(st->getElseSt());

    // Only generate a branch if we haven't returned or thrown
    if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
      if (blkDone == NULL)
        blkDone = createBlock("endif");
      currentBlock_->branchTo(st->getFinalLocation(), blkDone);
    }
  }

  // Continue at the 'done' block if there was one.
  setInsertPos(blkDone);
  setActiveScope(savedScope);
  return true;
}

bool StmtAnalyzer::buildWhileStmtCFG(const WhileStmt * st) {
  Scope * savedScope = activeScope;
  Expr * testExpr = astToTestExpr(st->getTestExpr());
  if (isErrorResult(testExpr)) {
    return false;
  }

  DASSERT(testExpr != NULL);
  DASSERT_OBJ(testExpr->getType() != NULL, testExpr);
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
  currentBlock_->branchTo(st->getLocation(), blkTest);

  // Generate the 'test' block with the conditional branch.
  blkTest->condBranchTo(st->getTestExpr()->getLocation(),
      testExpr, blkBody, blkDone);

  // Generate the 'body' block.
  Block * saveBreakTarget = breakTarget;
  Block * saveContinueTarget = continueTarget_;
  CleanupHandler * saveLoopEH = loopCleanups_;
  setInsertPos(blkBody);
  breakTarget = blkDone;
  loopCleanups_ = cleanups_;
  continueTarget_ = blkTest;
  buildStmtCFG(st->getLoopBody());
  breakTarget = saveBreakTarget;
  continueTarget_ = saveContinueTarget;
  loopCleanups_ = saveLoopEH;

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->getTestExpr()->getLocation(), blkTest);
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
  const ASTNode * initExpr = st->getInitExpr();
  if (initExpr != NULL) {
    if (initExpr->getNodeType() == ASTNode::Var) {
      const ASTVarDecl * initDecl = static_cast<const ASTVarDecl *>(initExpr);
      VariableDefn * initDefn = cast<VariableDefn>(astToDefn(initDecl));
      if (!analyzeValueDefn(initDefn, Task_PrepCodeGeneration)) {
        return false;
      }
      DASSERT(initDefn->initValue() != NULL);
      currentBlock_->append(new InitVarExpr(st->getLocation(), initDefn,
          initDefn->initValue()));
      //if (initDefn == NULL) {
      //  return NULL;
      //}
    } else if (initExpr->getNodeType() == ASTNode::Tuple) {
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
  if (st->getTestExpr() != NULL) {
    testExpr = inferTypes(astToExpr(st->getTestExpr(), &BoolType::instance), &BoolType::instance);
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
  if (st->getIncrExpr() != NULL) {
    incrExpr = inferTypes(astToExpr(st->getIncrExpr(), NULL), NULL);
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
    currentBlock_->branchTo(st->getTestExpr()->getLocation(), blkTest);
    blkTest->condBranchTo(st->getTestExpr()->getLocation(),
        testExpr, blkBody, blkDone);
  } else {
    currentBlock_->branchTo(st->getLocation(), blkBody);
  }

  // Generate the 'body' block.
  Block * saveBreakTarget = breakTarget;
  Block * saveContinueTarget = continueTarget_;

  breakTarget = blkDone;
  continueTarget_ = blkIncr ? blkIncr : (blkTest ? blkTest : blkBody);

  setInsertPos(blkBody);
  buildStmtCFG(st->getLoopBody());

  // Only generate a branch if we haven't returned or thrown.
  if (currentBlock_ && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->getLocation(), continueTarget_);
  }

  breakTarget = saveBreakTarget;
  continueTarget_ = saveContinueTarget;

  if (blkIncr) {
    blkIncr->branchTo(st->getLocation(), blkTest ? blkTest : blkBody);
  }

  // Continue at the 'done' block.
  setInsertPos(blkDone);
  setActiveScope(savedScope);
  return true;
}

bool StmtAnalyzer::buildForEachStmtCFG(const ForEachStmt * st) {
  DFAIL("Unimplemented");
  return true;
}

bool StmtAnalyzer::buildThrowStmtCFG(const ThrowStmt * st) {
  Expr * resultVal = NULL;
  if (st->getValue() != NULL) {
    //if (funcDef->isGenerator()) {
    //  diag.fatal(retSt->getLocation(),
    //      "Return value not allowed in generator function");
    //}

    resultVal = inferTypes(
        astToExpr(st->getValue(), Builtins::typeThrowable),
        Builtins::typeThrowable);

    if (isErrorResult(resultVal)) {
      return false;
    }
    
    //if (targets.empty()) {
    //  DFAIL("Implement finally before throw ??");
    //}
  }

  currentBlock_->exitThrow(st->getLocation(), resultVal);
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
  const StmtList & catchList = st->getCatchList();
  Block * tryBlock = createBlock("try");
  currentBlock_->branchTo(st->getLocation(), tryBlock);  

  // Create a block for the 'finally' statement if any.
  Block * finallyBlock = NULL;
  if (st->getFinallySt()) {
    finallyBlock = createBlock("finally");
  }

  // Create the next block for after the try/catch statement.
  Block * endTry = new Block("endTry");

  // Set the current exception handler context
  CleanupHandler cleanup(cleanups_, finallyBlock);
  cleanups_ = &cleanup;

  // Create the 'catch' statement blocks.
  Block * unwindBlock = NULL;
  if (!catchList.empty() || st->getFinallySt()) {
    TypeList catchTypes;
    bool catchAll = false;

    setInsertPos(tryBlock);
    unwindBlock = createBlock("catch");
    unwindBlock->setTerminator(
        catchList.empty()
            ? st->getFinallySt()->getLocation()
            : (catchList.front()->getLocation() | catchList.back()->getLocation()),
        BlockTerm_Catch);

    // The active throwable object, returned from the unwind code.
    Expr * activeThrowable = new IRValueExpr(unwindBlock->termLocation(), Builtins::typeThrowable);
    unwindBlock->termExprs().push_back(activeThrowable);

    // Generate the catch cases
    for (StmtList::const_iterator it = catchList.begin(); it != catchList.end(); ++it) {
      CatchStmt * cst = static_cast<CatchStmt *>(*it);
      const SourceLocation & loc = cst->getLocation();

      // Create a local scope in which the exception expression will be defined.
      LocalScope * catchScope = createLocalScope("catch-scope");

      // Define the exception variable in the catch scope.
      ASTDecl * exceptDecl = cst->getExceptDecl();
      VariableDefn * exceptDefn = cast<VariableDefn>(
          ScopeBuilder::createLocalDefn(catchScope, function, exceptDecl));
      if (!analyzeValueDefn(exceptDefn, Task_PrepCodeGeneration)) {
        return false;
      }

      // Get the exception type and determine if it is valid.
      CompositeType * exceptType = dyn_cast<CompositeType>(dealias(exceptDefn->getType()));
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
      module->addXRef(exceptDefn);

      catchTypes.push_back(exceptType);

      // Create the catch block.
      const char * catchBlockName =
          istrings.intern(std::string("catch-") + exceptType->typeDefn()->getName());
      Block * catchBody = createBlock(catchBlockName);

      // Add the catch block to the switch statement.
      unwindBlock->addCase(
          new ConstantType(cst->getLocation(), exceptType), catchBody);

      // Make the catch scope the current scope for generating the block contents.
      Scope * savedScope = setActiveScope(catchScope);

      // Generate the assignment of the exception to the variable.
      setInsertPos(catchBody);
      if (exceptDefn->getName() != NULL) {
        const SourceLocation & loc = cst->getLocation();
        Expr * initExpr = new InitVarExpr(cst->getLocation(), exceptDefn,
            new CastExpr(Expr::BitCast, loc, exceptType, activeThrowable));
        currentBlock_->append(initExpr);
      }

      // Generate the catch body
      buildStmtCFG(cst->getBodySt());
      setActiveScope(savedScope);

      // If the catch body fell through, then jump to finally or end
      if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
        currentBlock_->branchTo(cst->getBodySt()->getFinalLocation(), endTry);
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
      catchBody->exitResumeUnwind(st->getLocation(), activeThrowable);
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
  buildStmtCFG(st->getBodySt());

  // Catch statements no longer in effect at this point, but finally still is.
  //cleanup.unwindTarget = NULL;
  setUnwindTarget(saveUnwind);

  // Generate the 'else' statement.
  if (st->getElseSt() != NULL) {
    // If the body had a terminator, then there's no way to reach the
    // 'else' statement.
    if (currentBlock_ == NULL || currentBlock_->hasTerminator()) {
      diag.warn(st->getElseSt()) << "'else' block is unreachable.";
    }

    // Generate the 'else' statement. We don't need to add a new block for this,
    // just append it to the try body.
    buildStmtCFG(st->getElseSt());
  }

  // If the body (op 'else' block) doesn't have a terminator, then branch
  // to the block after the try/catch.
  if (currentBlock_ != NULL && !currentBlock_->hasTerminator()) {
    currentBlock_->branchTo(st->getFinalLocation(), endTry);
    // Execute the finally block before exiting.
    if (finallyBlock != NULL) {
      currentBlock_->append(new LocalCallExpr(finallyBlock));
      currentBlock_ = NULL;
    }
  }

  // Generate the finally block.
  if (finallyBlock != NULL) {
    setInsertPos(finallyBlock);
    if (!buildStmtCFG(st->getFinallySt())) {
      return false;
    }

    // The finallyBlock exits with a "local return" which we'll convert into
    // a branch later.
    currentBlock_->setTerminator(
        st->getFinallySt()->getFinalLocation(),
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
  if (st->getValue() != NULL) {
    //if (funcDef->isGenerator()) {
    //  diag.fatal(retSt->getLocation(),
    //      "Return value not allowed in generator function");
    //}

    resultVal = inferTypes(astToExpr(st->getValue(), returnType), returnType);
    if (isErrorResult(resultVal)) {
      return false;
    }
    
    Type * exprType = resultVal->getType();
    
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
    Expr * exp = new AssignmentExpr(st->getLocation(), macroReturnVal_, resultVal);
    currentBlock_->append(exp);
    
    if (macroReturnTarget_ != NULL) {
      currentBlock_->branchTo(st->getLocation(), macroReturnTarget_);
      
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

  currentBlock_->exitReturn(st->getLocation(), resultVal);
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
  Defn * de = astToDefn(st->getDecl());
  if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
    if (!analyzeValueDefn(var, Task_PrepCodeGeneration)) {
      return false;
    }
    
    DASSERT_OBJ(var->isSingular(), var);
  
    // TODO: Should we insure that Let has an initializer?
    // TODO: Should we check for true constants here?
    if (var->initValue() != NULL) {
      Expr * initVal = inferTypes(var->initValue(), var->getType());
      if (!isErrorResult(initVal)) {
        currentBlock_->append(new InitVarExpr(st->getLocation(), var, initVal));
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
  if (expr->getType()->isEqual(&VoidType::instance)) {
    return expr;
  }

  DASSERT_OBJ(expr->isSingular(), expr);
  return expr;
}

Expr * StmtAnalyzer::astToAssignExpr(const ASTNode * ast, Type * expectedType) {
  if (ast->getNodeType() == ASTNode::Assign) {
    ExprAnalyzer ea(function->module(), activeScope, function);
    return ea.reduceAssign(static_cast<const ASTOper *>(ast), expectedType);
  } else {
    return astToExpr(ast, expectedType);
  }
}

Expr * StmtAnalyzer::astToTestExpr(const ASTNode * test) {
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
    //currentBlock_->append(new InitVarExpr(st->getLocation(), testDefn,
    //    testDefn->initValue()));
    
    // TODO: We need to cast the defn to a boolean as well.
    testExpr = new LValueExpr(test->getLocation(), NULL, testValueDefn);
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

  if (testExpr->getType()->isEqual(&VoidType::instance)) {
    diag.error(test) << "invalid use of void return result: " << test;
    return &Expr::ErrorVal;
  }

  // Compare reference type with null.
  if (testExpr->getType()->isReferenceType()) {
    return new CompareExpr(test->getLocation(),
        llvm::CmpInst::ICMP_EQ, testExpr,
        ConstantNull::get(test->getLocation(), testExpr->getType()));
  }

  // Cast to boolean.
  return BoolType::instance.implicitCast(test->getLocation(), testExpr);
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
          Expr * stateValue = ConstantInteger::get(SourceLocation(), proc.stateVar->getType(),
              localCall->returnState());
          *ei = new AssignmentExpr(SourceLocation(), proc.stateExpr, stateValue);
        }
      }
    }
  }
  
  //function->dumpBlocks();
}

} // namespace tart

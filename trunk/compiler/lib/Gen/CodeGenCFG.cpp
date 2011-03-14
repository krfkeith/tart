/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/Module.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/EnumType.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/CompositeType.h"

#include "tart/Gen/CodeGenerator.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/Intrinsics.h"
#include "llvm/Module.h"

namespace tart {
using namespace llvm;

void CodeGenerator::genLocalStorage(LocalScopeList & lsl) {
  for (LocalScopeList::iterator it = lsl.begin(); it != lsl.end(); ++it) {
    LocalScope * lscope = *it;
    for (Defn * de = lscope->firstMember(); de != NULL; de = de->nextInScope()) {
      if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
        if (var->isSharedRef() || var->hasStorage()) {
          genLocalVar(var, NULL);
        }
      }
    }
  }
}

void CodeGenerator::genLocalRoots(LocalScopeList & lsl) {
  for (LocalScopeList::iterator it = lsl.begin(); it != lsl.end(); ++it) {
    LocalScope * lscope = *it;
    for (Defn * de = lscope->firstMember(); de != NULL; de = de->nextInScope()) {
      if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
        if (var->isSharedRef()) {
          genGCRoot(var->irValue(), var->sharedRefType(), var->name());
        } else if (var->hasStorage() && var->type()->containsReferenceType()) {
          genGCRoot(var->irValue(), var->type(), var->name());
        }
      }
    }
  }
}

void CodeGenerator::genLocalVar(VariableDefn * var, Value * initialVal) {
  // Don't generate the IR if we've already done so
  DASSERT_OBJ(var->storageClass() == Storage_Local, var);
  DASSERT_OBJ(var->irValue() == NULL, var);

  const Type * varType = var->type();
  DASSERT_OBJ(varType != NULL, var);

  // Generate the variable type
  if (var->isSharedRef()) {
    // For a shared variable, create a shared reference on the heap.
    const Type * cellType = var->sharedRefType();
    DASSERT_OBJ(cellType != NULL, var);
    const llvm::Type * irType = cellType->irEmbeddedType();

    Value * cellValue = builder_.CreateCall2(
            getGcAlloc(), gcAllocContext_,
            llvm::ConstantExpr::getIntegerCast(
                llvm::ConstantExpr::getSizeOf(cellType->irType()),
                intPtrType_, false),
            var->name() + StringRef(".shared.alloc"));
    cellValue = builder_.CreatePointerCast(
        cellValue, irType, var->name() + StringRef(".shared"));

    genInitObjVTable(cast<CompositeType>(cellType), cellValue);

    if (initialVal != NULL) {
      builder_.CreateStore(initialVal, builder_.CreateStructGEP(cellValue, 1));
    }

    Value * cellAlloca = builder_.CreateAlloca(irType, NULL, var->name());
    builder_.CreateStore(cellValue, cellAlloca);
    var->setIRValue(cellAlloca);
//    markGCRoot(cellAlloca, NULL, var->name());
  } else {
    // Allocate space for the variable on the stack
    const llvm::Type * irType = varType->irEmbeddedType();
    Value * lValue = builder_.CreateAlloca(irType, 0, var->name());
    var->setIRValue(lValue);
  }
}

Value * CodeGenerator::genSeq(const SeqExpr * in) {
  Value * result = voidValue_;
  size_t savedRootCount = rootStackSize();
  pushRoots(in->scope());

  DIScope saveContext = dbgContext_;
  if (in->scope() != NULL) {
    dbgContext_ = genLexicalBlock(in->location());
  }

  for (SeqExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
    if (atTerminator()) {
      diag.warn(*it) << "Unreachable statement";
    }
    setDebugLocation((*it)->location());
    result = genExpr(*it);
    if (result == NULL) {
      //endLexicalBlock(savedScope);
      return NULL;
    }
  }

  if (!atTerminator()) {
    popRootStack(savedRootCount);
  }

  dbgContext_ = saveContext;
  return result;
}

Value * CodeGenerator::genIf(const IfExpr * in) {
  // Create the set of basic blocks. We don't know yet
  // if we need an else block or endif block.
  BasicBlock * blkThen = createBlock("if.then");
  BasicBlock * blkElse = NULL;
  BasicBlock * blkThenLast = NULL;
  BasicBlock * blkElseLast = NULL;
  BasicBlock * blkDone = NULL;
  size_t savedRootCount = rootStackSize();
  pushRoots(in->scope());

  if (in->elseVal() != NULL) {
    blkElse = createBlock("if.else");
    setDebugLocation(in->test()->location());
    genTestExpr(in->test(), blkThen, blkElse);
  } else {
    blkDone = createBlock("if.end");
    genTestExpr(in->test(), blkThen, blkDone);
  }

  // Generate the contents of the 'then' block.
  moveToEnd(blkThen);
  builder_.SetInsertPoint(blkThen);
  setDebugLocation(in->thenVal()->location());
  Value * thenVal = genExpr(in->thenVal());
  Value * elseVal = NULL;

  // Only generate a branch if we haven't returned or thrown.
  if (!atTerminator()) {
    blkThenLast = builder_.GetInsertBlock();
    blkDone = ensureBlock("endif", blkDone);
    builder_.CreateBr(blkDone);
  }

  // Generate the contents of the 'else' block
  if (blkElse != NULL) {
    moveToEnd(blkElse);
    builder_.SetInsertPoint(blkElse);
    setDebugLocation(in->elseVal()->location());
    elseVal = genExpr(in->elseVal());

    // Only generate a branch if we haven't returned or thrown
    if (!atTerminator()) {
      blkElseLast = builder_.GetInsertBlock();
      blkDone = ensureBlock("endif", blkDone);
      builder_.CreateBr(blkDone);
    }
  }

  // Continue at the 'done' block if there was one.
  if (blkDone != NULL) {
    moveToEnd(blkDone);
    builder_.SetInsertPoint(blkDone);
    popRootStack(savedRootCount);

    // If the if-statement was in expression context, then return the value.
    if (!in->type()->isVoidType()) {
      if (blkThenLast && blkElseLast) {
        // If both branches returned a result, then combine them with a phi-node.
        DASSERT(thenVal != NULL);
        DASSERT(elseVal != NULL);
        PHINode * phi = builder_.CreatePHI(in->type()->irType(), "if");
        phi->addIncoming(thenVal, blkThen);
        phi->addIncoming(elseVal, blkElse);
        return phi;
      } else if (blkThenLast) {
        DASSERT(thenVal != NULL);
        return thenVal;
      } else if (blkElseLast) {
        DASSERT(elseVal != NULL);
        return elseVal;
      } else {
        DFAIL("No value to return from if-statement");
      }
    }
  } else {
    DASSERT(atTerminator());
  }

  return voidValue_;
}

Value * CodeGenerator::genWhile(const WhileExpr * in) {
  // Generate the test expression.
  bool isConstTrue = false;
  bool isConstFalse = false;
  if (in->test()->isConstant() && in->test()->type()->isBooleanType()) {
    const ConstantInteger * ci = cast<ConstantInteger>(in->test());
    if (ci->value()->isNullValue()) {
      isConstFalse = true;
    } else {
      isConstTrue = true;
    }
  }

  if (isConstFalse) {
    // Loop body is never executed
    diag.warn(in->body()) << "Unreachable statement";
    return voidValue_;
  }

  // Create the set of basic blocks.
  BasicBlock * blkTest = NULL;
  BasicBlock * blkBody = createBlock("while.body");
  BasicBlock * blkDone = createBlock("while.end");
  size_t savedRootCount = rootStackSize();
  pushRoots(in->scope());

  if (isConstTrue) {
    builder_.CreateBr(blkBody);
  } else {
    // Branch to the 'test' block.
    blkTest = createBlock("while.test");
    builder_.CreateBr(blkTest);
    builder_.SetInsertPoint(blkTest);
    genTestExpr(in->test(), blkBody, blkDone);
  }

  // Generate the 'body' block.
  BlockExits loopExits(blockExits_);
  loopExits.setBreakBlock(blkDone).setContinueBlock(blkTest ? blkTest : blkBody);
  blockExits_ = &loopExits;
  moveToEnd(blkBody);
  builder_.SetInsertPoint(blkBody);
  genExpr(in->body());
  blockExits_ = loopExits.parent();

  // Only generate a branch if we haven't returned or thrown.
  if (!atTerminator()) {
    builder_.CreateBr(blkTest ? blkTest : blkBody);
  }

  if (isConstTrue && !loopExits.breakUsed()) {
    // Infinite loop
    builder_.ClearInsertionPoint();
    blkDone->removeFromParent();
  } else {
    moveToEnd(blkDone);
    builder_.SetInsertPoint(blkDone);
    popRootStack(savedRootCount);
  }
  return voidValue_;
}

Value * CodeGenerator::genDoWhile(const WhileExpr * in) {
  // Create the set of basic blocks.
  BasicBlock * blkBody = createBlock("dowhile.body");
  BasicBlock * blkDone = createBlock("dowhile.end");
  BasicBlock * blkTest = createBlock("dowhile.test");
  size_t savedRootCount = rootStackSize();
  pushRoots(in->scope());

  // Start by branching to the loop body block.
  builder_.CreateBr(blkBody);

  // Generate the 'body' block.
  BlockExits loopExits(blockExits_);
  loopExits.setBreakBlock(blkDone).setContinueBlock(blkTest);
  blockExits_ = &loopExits;
  builder_.SetInsertPoint(blkBody);
  genExpr(in->body());
  blockExits_ = loopExits.parent();

  // Only generate a branch if we haven't returned or thrown.
  if (!atTerminator()) {
    moveToEnd(blkTest);
    builder_.CreateBr(blkTest);
    builder_.SetInsertPoint(blkTest);
    genTestExpr(in->test(), blkBody, blkDone);
  }

  moveToEnd(blkDone);
  builder_.SetInsertPoint(blkDone);
  popRootStack(savedRootCount);
  return voidValue_;
}

Value * CodeGenerator::genFor(const ForExpr * in) {
  size_t savedRootCount = rootStackSize();
  pushRoots(in->scope());

  if (in->init() != NULL) {
    genExpr(in->init());
  }

  // Create the set of basic blocks.
  BasicBlock * blkTest = in->test() ? createBlock("for.test") : NULL;
  BasicBlock * blkBody = createBlock("for.body");
  BasicBlock * blkIncr = in->incr() ? createBlock("for.incr") : NULL;
  BasicBlock * blkDone = createBlock("for.end");

  // Generate the code for the test.
  if (blkTest) {
    builder_.CreateBr(blkTest);
    builder_.SetInsertPoint(blkTest);
    genTestExpr(in->test(), blkBody, blkDone);
  } else {
    builder_.CreateBr(blkBody);
  }

  // Generate the 'body' block.
  BlockExits loopExits(blockExits_);
  loopExits.setBreakBlock(blkDone);
  loopExits.setContinueBlock(blkIncr ? blkIncr : (blkTest ? blkTest : blkBody));
  blockExits_ = &loopExits;
  moveToEnd(blkBody);
  builder_.SetInsertPoint(blkBody);
  genExpr(in->body());
  blockExits_ = loopExits.parent();

  // Only generate a branch if we haven't returned or thrown.
  if (!atTerminator()) {
    if (in->incr() != NULL) {
      moveToEnd(blkIncr);
      builder_.CreateBr(blkIncr);
      builder_.SetInsertPoint(blkIncr);
      genExpr(in->incr());
    }
    builder_.CreateBr(blkTest ? blkTest : blkBody);
  }

  moveToEnd(blkDone);
  builder_.SetInsertPoint(blkDone);
  popRootStack(savedRootCount);
  return voidValue_;
}

Value * CodeGenerator::genForEach(const ForEachExpr * in) {
  size_t savedRootCount = rootStackSize();
  pushRoots(in->scope());

  Value * iterValue = genExpr(in->iterator());
  if (iterValue == NULL) {
    return NULL;
  }

  // Create the set of basic blocks.
  BasicBlock * blkTest = createBlock("foreach.test");
  BasicBlock * blkBody = createBlock("foreach.body");
  BasicBlock * blkDone = createBlock("foreach.end");

  // Call 'next' and test the result.
  builder_.CreateBr(blkTest);
  builder_.SetInsertPoint(blkTest);
  Value * testValue = genExpr(in->test());
  if (testValue == NULL) {
    return NULL;
  }
  builder_.CreateCondBr(testValue, blkDone, blkBody);

  // Generate the 'body' block.
  BlockExits loopExits(blockExits_);
  loopExits.setBreakBlock(blkDone);
  loopExits.setContinueBlock(blkTest);
  blockExits_ = &loopExits;
  moveToEnd(blkBody);
  builder_.SetInsertPoint(blkBody);

  // Generate the assignments
  for (ExprList::const_iterator it = in->assigns().begin(), itEnd = in->assigns().end();
      it != itEnd; ++it) {
    if (genExpr(*it) == NULL) {
      return NULL;
    }
  }

  // And the actual loop body
  genExpr(in->body());
  blockExits_ = loopExits.parent();

  // Only generate a branch if we haven't returned or thrown.
  if (!atTerminator()) {
    builder_.CreateBr(blkTest);
  }

  moveToEnd(blkDone);
  builder_.SetInsertPoint(blkDone);
  popRootStack(savedRootCount);
  return voidValue_;
}

Value * CodeGenerator::genSwitch(const SwitchExpr * in) {
  const Type * testType = in->value()->type();
  if (testType->isIntType() || isa<EnumType>(testType)) {
    return genIntegerSwitch(in);
  } else if (testType == Builtins::typeString.get()) {
    return genEqSwitch(in);
  } else {
    DFAIL("Bad switch type");
  }
}

Value * CodeGenerator::genIntegerSwitch(const SwitchExpr * in) {
  size_t savedRootCount = rootStackSize();
  Value * switchValue = genExpr(in->value());
  if (switchValue == NULL) {
    return NULL;
  }
  DASSERT(switchValue->getType()->isIntegerTy());

  // Count the number of cases
  size_t numCases = 0;
  for (SwitchExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
    numCases += cast<CaseExpr>(*it)->argCount();
  }

  // Create the switch instruction.
  bool hasElse = in->elseCase() != NULL;
  BasicBlock * blkElse = hasElse ? createBlock("switch.else") : NULL;
  BasicBlock * blkDone = hasElse ? NULL : createBlock("switch.end");

  // Create the PHI node if return type is not void.
  PHINode * phi = NULL;
  if (!in->type()->isVoidType()) {
    phi = PHINode::Create(in->type()->irType());
  }

  // Add cases
  SwitchInst * switchInst = builder_.CreateSwitch(
      switchValue,
      hasElse ? blkElse : blkDone,
      numCases);
  for (SwitchExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
    CaseExpr * ce = cast<CaseExpr>(*it);

    // Generate the jump to the case block.
    BasicBlock * blkCase = createBlock("case");
    for (CaseExpr::const_iterator vi = ce->begin(); vi != ce->end(); ++vi) {
      Constant * caseVal = genConstExpr(*vi);
      switchInst->addCase(cast<ConstantInt>(caseVal), blkCase);
    }

    // Generate the case body.
    builder_.SetInsertPoint(blkCase);
    Value * result = genExpr(ce->body());

    // Jump to the 'done' body.
    if (!atTerminator()) {
      blkDone = ensureBlock("switch.end", blkDone);
      builder_.CreateBr(blkDone);

      // Add to phi
      if (phi != NULL) {
        phi->addIncoming(result, builder_.GetInsertBlock());
      }
    }
  }

  if (hasElse) {
    moveToEnd(blkElse);
    builder_.SetInsertPoint(blkElse);
    Value * result = genExpr(in->elseCase());

    if (!atTerminator()) {
      blkDone = ensureBlock("switch.end", blkDone);
      builder_.CreateBr(blkDone);

      // Add to phi
      if (phi != NULL) {
        phi->addIncoming(result, builder_.GetInsertBlock());
      }
    }
  } else if (phi != NULL) {
    // A switch statement used as an expression must have an 'else' clause.
    // TODO: Check for enum case where all enum values have explicit cases.
    diag.error(in->location()) << "Not all paths return a value";
  }

  Value * result = voidValue_;
  if (blkDone != NULL) {
    moveToEnd(blkDone);
    builder_.SetInsertPoint(blkDone);
    popRootStack(savedRootCount);
    if (phi != NULL) {
      if (phi->getNumIncomingValues() == 0) {
        delete phi;
      } else if (phi->getNumIncomingValues() == 1) {
        result = phi->getIncomingValue(0);
        delete phi;
      } else {
        result = builder_.Insert(phi, "switchval");
      }
    }
  } else {
    builder_.ClearInsertionPoint();
  }

  return result;
}

Value * CodeGenerator::genEqSwitch(const SwitchExpr * in) {
  size_t savedRootCount = rootStackSize();
  Value * switchValue = genExpr(in->value());
  if (switchValue == NULL) {
    return NULL;
  }

  // Create the switch instruction.
  bool hasElse = in->elseCase() != NULL;
  BasicBlock * blkDone = NULL;
  BasicBlock * blkNext = NULL;

  // Create the PHI node if return type is not void.
  PHINode * phi = NULL;
  if (!in->type()->isVoidType()) {
    phi = PHINode::Create(in->type()->irType());
  }

  Function * eqTestFn = genFunctionValue(in->equalityTestFn());

  // Add cases
  for (SwitchExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
    CaseExpr * ce = cast<CaseExpr>(*it);

    BasicBlock * blkCase = createBlock("case"); // Case body
    for (CaseExpr::const_iterator vi = ce->begin(); vi != ce->end(); ++vi) {
      blkNext = createBlock("next");
      Constant * caseVal = genConstExpr(*vi);
      // Generate code for a string comparison.
      ValueList args;
      args.push_back(switchValue);
      args.push_back(caseVal);
      llvm::Twine caseName("case.");
      if (ConstantString * strVal = dyn_cast<ConstantString>(*vi)) {
        caseName.concat(strVal->value());
      }
      Value * testResult = genCallInstr(eqTestFn, args.begin(), args.end(), caseName);
      builder_.CreateCondBr(testResult, blkCase, blkNext);
      builder_.SetInsertPoint(blkNext);
    }

    // Generate the case body.
    moveToEnd(blkCase);
    builder_.SetInsertPoint(blkCase);
    Value * result = genExpr(ce->body());

    // Jump to the 'done' body.
    if (!atTerminator()) {
      blkDone = ensureBlock("switch.end", blkDone);
      builder_.CreateBr(blkDone);

      // Add to phi
      if (phi != NULL) {
        phi->addIncoming(result, builder_.GetInsertBlock());
      }
    }

    moveToEnd(blkNext);
    builder_.SetInsertPoint(blkNext);
  }

  if (hasElse) {
    blkNext = ensureBlock("switch.else", blkNext);
    builder_.SetInsertPoint(blkNext);
    Value * result = genExpr(in->elseCase());

    if (!atTerminator()) {
      blkDone = ensureBlock("switch.end", blkDone);
      builder_.CreateBr(blkDone);

      // Add to phi
      if (phi != NULL) {
        phi->addIncoming(result, builder_.GetInsertBlock());
      }
    }
  } else {
    // A switch statement that returns a value must have an 'else' clause.
    if (phi != NULL) {
      diag.error(in->location()) << "Not all paths return a value";
    }
    blkDone = ensureBlock("switch.end", blkDone);
    builder_.CreateBr(blkDone);
  }

  Value * result = voidValue_;
  if (blkDone != NULL) {
    moveToEnd(blkDone);
    builder_.SetInsertPoint(blkDone);
    popRootStack(savedRootCount);
    if (phi != NULL) {
      if (phi->getNumIncomingValues() == 0) {
        delete phi;
      } else if (phi->getNumIncomingValues() == 1) {
        result = phi->getIncomingValue(0);
        delete phi;
      } else {
        result = builder_.Insert(phi, "switchval");
      }
    }
  } else {
    builder_.ClearInsertionPoint();
  }

  return result;
}

Value * CodeGenerator::genMatch(const MatchExpr * in) {
  size_t savedRootCount = rootStackSize();

  // TODO: For union with a discriminator, we could use a switch instruction.
  const Type * matchType = dealias(in->value()->type());
  TypeShape matchTypeShape = matchType->typeShape();
  bool matchIsLValue = matchTypeShape == Shape_Large_Value;
  if (const UnionType * utype = dyn_cast<UnionType>(matchType)) {
    if (utype->hasRefTypesOnly()) {
    } else {
    }
  }

  // Generate the match value. For union types, this will be an l-value.
  Value * matchValue = genExpr(in->value());
  if (matchValue == NULL) {
    return NULL;
  }

  // Create the switch instruction.
  bool hasElse = in->elseCase() != NULL;
  BasicBlock * blkDone = NULL;
  BasicBlock * blkNext = NULL;

  // Create the PHI node if return type is not void.
  PHINode * phi = NULL;
  if (!in->type()->isVoidType()) {
    phi = PHINode::Create(in->type()->irType());
  }

  // Add cases
  for (MatchExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
    MatchAsExpr * mae = cast<MatchAsExpr>(*it);

    BasicBlock * blkCase = createBlock("match.as"); // Case body
    blkNext = createBlock("next");

    Value * testResult = genExpr(mae->test());
    builder_.CreateCondBr(testResult, blkCase, blkNext);
    builder_.SetInsertPoint(blkNext);

    // Generate the case body.
    moveToEnd(blkCase);
    builder_.SetInsertPoint(blkCase);

    // Generate the body
    genExpr(mae->init());
    Value * result = genExpr(mae->body());

    // Jump to the 'done' body.
    if (!atTerminator()) {
      blkDone = ensureBlock("match.end", blkDone);
      builder_.CreateBr(blkDone);

      // Add to phi
      if (phi != NULL) {
        phi->addIncoming(result, builder_.GetInsertBlock());
      }
    }

    moveToEnd(blkNext);
    builder_.SetInsertPoint(blkNext);
  }

  if (hasElse) {
    blkNext = ensureBlock("match.else", blkNext);
    builder_.SetInsertPoint(blkNext);
    Value * result = genExpr(in->elseCase());

    if (!atTerminator()) {
      blkDone = ensureBlock("match.end", blkDone);
      builder_.CreateBr(blkDone);

      // Add to phi
      if (phi != NULL) {
        phi->addIncoming(result, builder_.GetInsertBlock());
      }
    }
  } else {
    // A switch statement that returns a value must have an 'else' clause.
    if (phi != NULL) {
      diag.error(in->location()) << "Not all paths return a value";
    }
    blkDone = ensureBlock("match.end", blkDone);
    builder_.CreateBr(blkDone);
  }

  Value * result = voidValue_;
  if (blkDone != NULL) {
    moveToEnd(blkDone);
    builder_.SetInsertPoint(blkDone);
    popRootStack(savedRootCount);
    if (phi != NULL) {
      if (phi->getNumIncomingValues() == 0) {
        delete phi;
      } else if (phi->getNumIncomingValues() == 1) {
        result = phi->getIncomingValue(0);
        delete phi;
      } else {
        result = builder_.Insert(phi, "match.result");
      }
    }
  } else {
    builder_.ClearInsertionPoint();
  }

  return result;
}

Value * CodeGenerator::genTry(const TryExpr * in) {
  bool saveIsUnwindBlock = isUnwindBlock_;

  // Set up the block exits.
  BlockExits exits(blockExits_);

  // Create the landing pad block.
  BasicBlock * blkLandingPad = NULL;
  if (in->argCount() > 0 || in->finallyBlock() != NULL) {
    blkLandingPad = createBlock("try.lpad");
    isUnwindBlock_ = true;
    exits.setUnwindBlock(blkLandingPad);
  }

  // Create the 'else' block if any.
  BasicBlock * blkElse = NULL;
  if (in->elseBlock() != NULL) {
    BasicBlock * blkElse = createBlock("try.else");
  }

  // Create a block for the 'finally' statement if any.
  BasicBlock * blkFinally = NULL;
  if (in->finallyBlock() != NULL) {
    blkFinally = createBlock("try.finally");
    exits.setCleanupBlock(blkFinally);
    exits.setReturnVar(builder_.CreateAlloca(builder_.getInt8PtrTy(), NULL, "try.retAddr"));
  }

  // The block after the try/catch statement. This gets created only if branched to.
  BasicBlock * blkDone = NULL;

  // Create the PHI node if return type of the try statement is not void.
  PHINode * phi = NULL;
  if (!in->type()->isVoidType()) {
    phi = PHINode::Create(in->type()->irType());
  }

  // Set up block exits struct to hold cleanups for this scope.
  BlockExits * saveExits = blockExits_;
  blockExits_ = &exits;

  // Generate the try body.
  setDebugLocation(in->body()->location());
  Value * result = genExpr(in->body());

  // The code that generates terminators also handles cleanup, but if there wasn't a terminator,
  // then we need to handle it here.
  if (!atTerminator()) {
    // Go to the else block or the end block.
    BasicBlock * blkNext = blkElse;
    if (blkNext == NULL) {
      blkDone = blkNext = createBlock("try.end");
    }

    // Execute the finally block before jumping to blkNext.
    if (exits.cleanupBlock() != NULL) {
      callCleanup(&exits, blkNext);
    } else {
      builder_.CreateBr(blkNext);
    }

    // Add result to phi
    if (phi != NULL) {
      phi->addIncoming(result, builder_.GetInsertBlock());
    }
  } else if (in->elseBlock()) {
    diag.error(in) << "'else' block is unreachable";
  }

  // Disable the landing pad but keep the cleanups.
  exits.setUnwindBlock(NULL);
  isUnwindBlock_ = saveIsUnwindBlock;

  // If there is a landing pad.
  if (blkLandingPad != NULL) {
    moveToEnd(blkLandingPad);
    builder_.SetInsertPoint(blkLandingPad);

    // Call llvm.eh.exception to get the current exception
    Function * ehException = llvm::Intrinsic::getDeclaration(
        irModule_, llvm::Intrinsic::eh_exception, NULL, 0);
    Value * ehPtr = builder_.CreateCall(ehException, "eh_ptr");
    const StructType * throwableType = cast<StructType>(Builtins::typeThrowable->irType());
    const llvm::Type * unwindExceptionType = throwableType->getContainedType(2);

    // Build the selector list, and detect if there is a 'catch-everything' block.
    // Note that a 'catch-everything' block may catch foreign exceptions, which
    // 'catch Throwable' cannot.
    bool isBacktraceRequested = false;
    const CatchExpr * catchAllExpr = NULL;
    ValueList selectorVals;
    for (TryExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
      const CatchExpr * ce = cast<CatchExpr>(*it);
      VariableDefn * catchVar = ce->var();
      if (catchVar != NULL) {
        if (catchAllExpr != NULL) {
          diag.error(ce) << "Unreachable statement after catch-all";
        }

        const CompositeType * exceptType = cast<CompositeType>(catchVar->type());
        selectorVals.push_back(getTypeInfoBlockPtr(exceptType));
        // If a backtrace was wanted, use the more expensive personality function.
        if (catchVar->hasTrait(Defn::RequestStackTrace)) {
          isBacktraceRequested = true;
        }
      } else {
        catchAllExpr = ce;
      }
    }

    // Put in a cleanup selector at the end if there's no catch-all block.
    if (!catchAllExpr) {
      selectorVals.push_back(getInt32Val(0));
    }

    // Get the llvm.eh.selector intrinsic and the personality function.
    Function * ehSelector = llvm::Intrinsic::getDeclaration(
        irModule_, llvm::Intrinsic::eh_selector, NULL, 0);
    Function * personality = isBacktraceRequested ?
        getExceptionTracePersonality() : getExceptionPersonality();

    // Args to llvm.eh.selector
    ValueList args;
    args.push_back(ehPtr);
    args.push_back(builder_.CreateBitCast(personality, builder_.getInt8Ty()->getPointerTo()));
    args.append(selectorVals.begin(), selectorVals.end());

    // Call llvm.eh.selector to determine the action.
    Value * ehAction = builder_.CreateCall(ehSelector, args.begin(), args.end(), "eh_action");

    // The 'catch everything' block. If there's no actual 'catch everything'
    // statement in the try, then this will be used as the 'resume exception' block.
    BasicBlock * blkCatchAll = NULL;
    // Compute the offset of the unwind info structure from the throwable base.
    llvm::Constant * unwindInfoOffset = llvm::ConstantExpr::getOffsetOf(throwableType, 2);

    // Subtract the offset to get the address of the throwable.
    // Note that we do *not* want to call InBoundsGEP or StructGEP here.
    Value * offsetIndices[1];
    offsetIndices[0] = llvm::ConstantExpr::getNeg(unwindInfoOffset);
    Value * throwValue = builder_.CreateGEP(
        builder_.CreateBitCast(ehPtr, builder_.getInt8PtrTy()),
        &offsetIndices[0], &offsetIndices[1],
        "eh_throwable");
    throwValue = builder_.CreateBitCast(throwValue, Builtins::typeThrowable->irEmbeddedType());

    if (in->argCount() > 0) {
      size_t savedRootCount = rootStackSize();

      // If there's only one selector, then it means that we already know which block
      // is going to handle the exception, and we also know that it's the 'catch everything'
      // block. Otherwise, we need to jump to the correct block.
      if (selectorVals.size() > 1) {
        blkCatchAll = createBlock("try.catchall");
        SwitchInst * si = builder_.CreateSwitch(ehAction, blkCatchAll, selectorVals.size());
        int selectorIndex = 0;
        for (TryExpr::const_iterator it = in->begin(); it != in->end(); ++it) {
          const CatchExpr * ce = cast<CatchExpr>(*it);
          VariableDefn * catchVar = ce->var();
          if (catchVar != NULL) {
            const CompositeType * exceptType = cast<CompositeType>(catchVar->type());
            Twine name = Twine("try.catch.") + exceptType->typeDefn()->qualifiedName();
            BasicBlock * blkCatchBody = createBlock(name);
            si->addCase(getInt32Val(selectorIndex++), blkCatchBody);
            builder_.SetInsertPoint(blkCatchBody);
            setDebugLocation(ce->location());

            // Assign the throwable to the variable
            Value * exceptValue = builder_.CreatePointerCast(
                throwValue, exceptType->irEmbeddedType(), "exception");
            builder_.CreateStore(exceptValue, catchVar->irValue());
          } else {
            // We don't need to add a switch case for the catch-all block because it's the
            // default for the switch statement.
            moveToEnd(blkCatchAll);
            builder_.SetInsertPoint(blkCatchAll);
          }

          // Generate the catch body.
          result = genExpr(ce->body());
          if (!atTerminator()) {
            popRootStack(savedRootCount);
            blkDone = ensureBlock("try.end", blkDone);
            if (exits.cleanupBlock() != NULL) {
              callCleanup(&exits, blkDone);
            } else {
              builder_.CreateBr(blkDone);
            }

            // Add result to phi
            if (phi != NULL) {
              phi->addIncoming(result, builder_.GetInsertBlock());
            }
          }
        }
      }
    }

    // If there was no 'catch-all' block, then there is the possibility that the exception
    // was not caught, in which case we have to do a resume unwind call.
    if (catchAllExpr == NULL) {
      // If the catch-all block exists, but there was no catch-all statement, that means
      // that we generated the block as a fall-through from the switch statement. Otherwise,
      // there never was a switch statement, in which case we should be appending
      // to the landing pad block.
      if (blkCatchAll != NULL) {
        moveToEnd(blkCatchAll);
        builder_.SetInsertPoint(blkCatchAll);
      } else {
        DASSERT(builder_.GetInsertBlock() == blkLandingPad);
      }

      Value * savedThrowable = addTempRoot(Builtins::typeThrowable, throwValue, "throwable");

      // Call our finally block if it exists.
      if (exits.cleanupBlock() != NULL) {
        callCleanup(&exits);
      }

      // Restore block exits here, so that we don't call our finally block again.
      blockExits_ = saveExits;

      // Call _Unwind_Resume
      Function * resumeFunc = getUnwindResume();
      Value * ehUnwindEx = builder_.CreateStructGEP(
          builder_.CreateLoad(savedThrowable), 2, "eh_unwind_ex");
      if (isUnwindBlock_) {
        // If there's another cleanup handler outside of ours, then use invoke.
        BasicBlock * blkUnreachable = createBlock("unreachable");
        builder_.CreateInvoke(resumeFunc, blkUnreachable, getUnwindBlock(), ehUnwindEx, "resume");
        builder_.SetInsertPoint(blkUnreachable);
      } else {
        // Otherwise, just call it and let the exception propagate to outer blocks.
        builder_.CreateCall(resumeFunc, ehUnwindEx, "resume");
      }

      builder_.CreateUnreachable();
    }
  }

  // Restore block exits.
  blockExits_ = saveExits;

  // Generate the 'else' block.
  if (blkElse != NULL) {
    moveToEnd(blkElse);
    builder_.SetInsertPoint(blkElse);
    if (in->elseBlock() != NULL) {
      setDebugLocation(in->elseBlock()->location());
      result = genExpr(in->elseBlock());
    }

    if (!atTerminator()) {
      // Call cleanups before exiting the else block.
      blkDone = ensureBlock("try.end", blkDone);
      if (exits.cleanupBlock() != NULL) {
        callCleanup(&exits, blkDone);
      } else {
        builder_.CreateBr(blkDone);
      }

      // Add result to phi
      if (phi != NULL) {
        phi->addIncoming(result, builder_.GetInsertBlock());
      }
    }
  }

  // Generate the 'finally' block.
  if (blkFinally != NULL) {
    moveToEnd(blkFinally);
    builder_.SetInsertPoint(blkFinally);
    setDebugLocation(in->finallyBlock()->location());
    genExpr(in->finallyBlock());
    if (!atTerminator()) {
      // Load the return address and branch back to it.
      Value * retAddr = builder_.CreateLoad(exits.returnVar());
      IndirectBrInst * brinst = builder_.CreateIndirectBr(retAddr, exits.returnBlocks().size());
      for (BlockExits::BlockSet::const_iterator it = exits.returnBlocks().begin(),
          itEnd = exits.returnBlocks().end(); it != itEnd; ++it) {
        brinst->addDestination(*it);
      }
    } else {
      // Don't allow return, break, or continue from a finally block.
      diag.error(in->finallyBlock()) << "Illegal exit from finally block";
    }
  }

  // Finish up
  result = voidValue_;
  if (blkDone != NULL) {
    moveToEnd(blkDone);
    builder_.SetInsertPoint(blkDone);
    if (phi != NULL) {
      if (phi->getNumIncomingValues() == 0) {
        delete phi;
      } else if (phi->getNumIncomingValues() == 1) {
        result = phi->getIncomingValue(0);
        delete phi;
      } else {
        result = builder_.Insert(phi, "match.result");
      }
    }
  } else {
    builder_.ClearInsertionPoint();
  }

  return result;
}

Value * CodeGenerator::genReturn(const ReturnExpr * in) {
  // Execute all cleanups
  for (BlockExits * be = blockExits_; be != NULL; be = be->parent()) {
    if (be->cleanupBlock() != NULL) {
      callCleanup(be);
    }
  }

  setDebugLocation(in->location());
  if (in->arg() == NULL) {
    // Return nothing
    builder_.CreateRet(NULL);
  } else {
    // Generate the return value.
    const Expr * returnVal = in->arg();
    Value * value = genExpr(returnVal);
    if (value == NULL) {
      return NULL;
    }

    // Handle struct returns and other conventions.
    const Type * returnType = returnVal->canonicalType();
    TypeShape returnShape = returnType->typeShape();
    DASSERT(value != NULL);

    if (returnShape == Shape_Large_Value) {
      value = loadValue(value, returnVal);
    }

    if (returnType->irEmbeddedType() != value->getType()) {
      returnType->irEmbeddedType()->dump(irModule_);
      value->getType()->dump(irModule_);
      DASSERT(false);
    }
    DASSERT_TYPE_EQ(returnVal, returnType->irEmbeddedType(), value->getType());

    if (structRet_ != NULL) {
      builder_.CreateStore(value, structRet_);
      builder_.CreateRet(NULL);
    } else {
      builder_.CreateRet(value);
    }
  }

  return voidValue_;
}

Value * CodeGenerator::genYield(const ReturnExpr * in) {
  DFAIL("Implement");
}

Value * CodeGenerator::genBreak(const BranchExpr * in) {
  for (BlockExits * be = blockExits_; be != NULL; be = be->parent()) {
    if (be->breakBlock() != NULL) {
      setDebugLocation(in->location());
      builder_.CreateBr(be->breakBlock());
      be->setBreakUsed(true);
      return voidValue_;
    }

    if (be->cleanupBlock() != NULL) {
      callCleanup(be);
    }
  }

  diag.error(in->location()) << "'break' statement outside of loop";
  return NULL;
}

Value * CodeGenerator::genContinue(const BranchExpr * in) {
  for (BlockExits * be = blockExits_; be != NULL; be = be->parent()) {
    if (be->continueBlock() != NULL) {
      setDebugLocation(in->location());
      builder_.CreateBr(be->continueBlock());
      return voidValue_;
    }

    if (be->cleanupBlock() != NULL) {
      callCleanup(be);
    }
  }

  diag.error(in->location()) << "'continue' statement outside of loop";
  return NULL;
}

Value * CodeGenerator::genLocalReturn(const BranchExpr * in) {
  for (BlockExits * be = blockExits_; be != NULL; be = be->parent()) {
    if (be->localReturnBlock() != NULL) {
      builder_.CreateBr(be->localReturnBlock());
      return voidValue_;
    }

    if (be->cleanupBlock() != NULL) {
      callCleanup(be);
    }
  }

  diag.error(in->location()) << "'continue' statement outside of loop";
  return NULL;
}

Value * CodeGenerator::genLocalProcedure(const LocalProcedureExpr * in) {
  // Create the set of basic blocks.
  BasicBlock * blkReturn = createBlock("local.ret");
  size_t savedRootCount = rootStackSize();

  // Generate the 'body' block.
  BlockExits loopExits(blockExits_);
  loopExits.setLocalReturnBlock(blkReturn);
  blockExits_ = &loopExits;
  genExpr(in->arg());
  blockExits_ = loopExits.parent();

  // Only generate a branch if we haven't returned or thrown.
  if (!atTerminator()) {
    builder_.CreateBr(blkReturn);
  }

  moveToEnd(blkReturn);
  builder_.SetInsertPoint(blkReturn);
  popRootStack(savedRootCount);
  return voidValue_;
}

bool CodeGenerator::genTestExpr(const Expr * test, BasicBlock * blkTrue, BasicBlock * blkFalse) {
  switch (test->exprType()) {
    case Expr::And: {
      const BinaryExpr * op = static_cast<const BinaryExpr *>(test);
      BasicBlock * blkSecond = BasicBlock::Create(context_, "and", currentFn_, blkTrue);
      genTestExpr(op->first(), blkSecond, blkFalse);
      builder_.SetInsertPoint(blkSecond);
      genTestExpr(op->second(), blkTrue, blkFalse);
      return true;
    }

    case Expr::Or: {
      const BinaryExpr * op = static_cast<const BinaryExpr *>(test);
      BasicBlock * blkSecond = BasicBlock::Create(context_, "or", currentFn_, blkFalse);
      genTestExpr(op->first(), blkTrue, blkSecond);
      builder_.SetInsertPoint(blkSecond);
      genTestExpr(op->second(), blkTrue, blkFalse);
      return true;
    }

    case Expr::Not: {
      // For logical negation, flip the true and false blocks.
      const UnaryExpr * op = static_cast<const UnaryExpr *>(test);
      return genTestExpr(op->arg(), blkFalse, blkTrue);
    }

    default: {
      llvm::Value * testVal = genExpr(test);
      if (testVal == NULL) return false;
      builder_.CreateCondBr(testVal, blkTrue, blkFalse);
      return true;
    }
  }
}

llvm::Value * CodeGenerator::genThrow(const ThrowExpr * in) {
  Expr * exceptVal = in->arg();
  Value * ehPtr = NULL;
  bool isRethrow = (exceptVal == NULL);
  if (isRethrow) {
    // Re-throw the current exception.
    const llvm::Type * throwableType = Builtins::typeThrowable->irType();
    Function * ehException = llvm::Intrinsic::getDeclaration(
        irModule_, llvm::Intrinsic::eh_exception, NULL, 0);
    ehPtr = builder_.CreateBitCast(
        builder_.CreateCall(ehException, "eh_ptr"),
        throwableType->getContainedType(2)->getPointerTo());
  } else {
    // Construct the exception object
    Value * exception = genExpr(exceptVal);
    if (exception == NULL) {
      return NULL;
    }

    const llvm::Type * throwableType = Builtins::typeThrowable->irType();
    irModule_->addTypeName("tart.core.Throwable", throwableType);
    Value * throwable = builder_.CreateBitCast(exception, throwableType->getPointerTo());
    ehPtr = builder_.CreateStructGEP(throwable, 2);
  }

  // Create the unwind instruction.
  Function * unwindFunc = getUnwindRaiseException();
  if (isUnwindBlock_) {
    // If the 'throw' statement is in a try block, then use invoke.
    BasicBlock * blkUnreachable = createBlock("unreachable");
    Value * result = builder_.CreateInvoke(
        unwindFunc, blkUnreachable, getUnwindBlock(), ehPtr, "throw");
    builder_.SetInsertPoint(blkUnreachable);
  } else {
    // Otherwise, just call it - it will get handled further up the call stack.
    builder_.CreateCall(unwindFunc, ehPtr, "throw");
  }

  builder_.CreateUnreachable();
  return voidValue_;
}

void CodeGenerator::callCleanup(BlockExits * be, BasicBlock * blkNext) {
  DASSERT(be->cleanupBlock() != NULL);
  DASSERT(be->returnVar() != NULL);

  // Create a block to return to after the cleanup.
  if (blkNext == NULL) {
    blkNext = createBlock("after.cleanup");
  }

  // Save the return address and jump to the cleanup block.
  Value * returnAddr = BlockAddress::get(blkNext);
  builder_.CreateStore(returnAddr, be->returnVar());
  builder_.CreateBr(be->cleanupBlock());
  be->returnBlocks().insert(blkNext);
  builder_.SetInsertPoint(blkNext);
}

llvm::BasicBlock * CodeGenerator::createBlock(const llvm::Twine & blkName) {
  return BasicBlock::Create(context_, blkName, currentFn_);
}

llvm::BasicBlock * CodeGenerator::getUnwindBlockImpl(BlockExits * be) {
  // If be == NULL, then we didn't set the isUnwindBlock_ flag correctly.
  DASSERT(be != NULL);

  // If there's a catch block, then use that.
  if (be->unwindBlock() != NULL) {
    return be->unwindBlock();
  }

  // Get the unwind block from our parent.
  BasicBlock * parentUnwindBlock = getUnwindBlockImpl(be->parent());
  DASSERT(parentUnwindBlock != NULL);

  if (be->cleanupBlock() != NULL) {
    BasicBlock * blkUnwind = createBlock("unwind");
    builder_.SetInsertPoint(blkUnwind);
    callCleanup(be, parentUnwindBlock);
    return blkUnwind;
  } else {
    return parentUnwindBlock;
  }
}

void CodeGenerator::moveToEnd(llvm::BasicBlock * blk) {
  llvm::BasicBlock * lastBlk = builder_.GetInsertBlock();
  if (lastBlk == NULL && currentFn_ != NULL && !currentFn_->empty()) {
    lastBlk = &currentFn_->back();
  }
  if (lastBlk != NULL) {
    blk->moveAfter(lastBlk);
  }
}

bool CodeGenerator::atTerminator() const {
  return builder_.GetInsertBlock() == NULL || builder_.GetInsertBlock()->getTerminator() != NULL;
}

} // namespace tart

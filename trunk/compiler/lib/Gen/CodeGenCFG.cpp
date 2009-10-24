/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/CompositeType.h"
#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
#include "tart/Objects/Builtins.h"

#include "tart/CFG/Block.h"

#include "llvm/Intrinsics.h"
#include "llvm/Module.h"

namespace tart {
using namespace llvm;

void CodeGenerator::genLocalStorage(BlockList & blocks, LocalScopeList & lsl) {
  builder_.SetInsertPoint(blocks.front()->irBlock());
  for (LocalScopeList::iterator it = lsl.begin(); it != lsl.end(); ++it) {
    LocalScope * lscope = *it;
    for (Defn * de = lscope->firstMember(); de != NULL; de = de->nextInScope()) {
      if (de->defnType() == Defn::Var) {
        genLocalVar(static_cast<VariableDefn *>(de));
      }
    }
  }
}

void CodeGenerator::genLocalVar(VariableDefn * var) {
  // Don't generate the IR if we've already done so
  DASSERT_OBJ(var->storageClass() == Storage_Local, var);
  DASSERT_OBJ(var->irValue() == NULL, var);
  DASSERT_OBJ(var->type().isDefined(), var);

  // Generate the variable type
  TypeRef varType = var->type();
  DASSERT(varType.isDefined());
  const llvm::Type * irType = varType.irEmbeddedType();

  // Allocate space for the variable on the stack
  Value * lValue = builder_.CreateAlloca(irType, 0, var->name());
  var->setIRValue(lValue);
}

void CodeGenerator::genBlocks(BlockList & blocks) {

  // Generate the list of predecessor blocks for each block.
  for (BlockList::iterator b = blocks.begin(); b != blocks.end(); ++b) {
    Block * blk = *b;
    BlockList & succs = blk->succs();
    for (BlockList::iterator s = succs.begin(); s != succs.end(); ++s) {
      (*s)->preds().push_back(blk);
    }
  }

  // Generate the code for each Block.
  for (BlockList::iterator b = blocks.begin(); b != blocks.end(); ++b) {
    Block * blk = *b;

    // Set up the insertion point and catch handler
    builder_.SetInsertPoint(blk->irBlock());
    unwindTarget_ = blk->unwindTarget() ? blk->unwindTarget()->irBlock() : NULL;

    ExprList & exprs = blk->exprs();
    for (ExprList::iterator it = exprs.begin(); it != exprs.end(); ++it) {
      genStopPoint((*it)->location());
      genStmt(*it);
    }

    genStopPoint(blk->termLocation());
    genBlockTerminator(blk);
  }

  // If this is the last block, generate debugging info before the terminator.
  if (debug_ && !dbgFunction_.isNull()) {
    BasicBlock & lastBlock = currentFn_->getBasicBlockList().back();
    builder_.SetInsertPoint(&lastBlock);
    TerminatorInst * term = lastBlock.getTerminator();
    term->removeFromParent();
    dbgFactory_.InsertRegionEnd(dbgFunction_, &lastBlock);
    builder_.Insert(term);
  }

  unwindTarget_ = NULL;
}

void CodeGenerator::genStopPoint(const SourceLocation & loc) {
  if (debug_ &&
      loc.file == module_->moduleSource() &&
      !dbgFunction_.isNull() &&
      loc != dbgLocation_) {
    TokenPosition pos = tokenPosition(loc);
    dbgFactory_.InsertStopPoint(
        dbgCompileUnit_,
        pos.endLine, pos.endCol,
        builder_.GetInsertBlock());
    dbgLocation_ = loc;
  }
}

void CodeGenerator::genStmt(Expr * in) {
  genExpr(in);
}

void CodeGenerator::genBlockTerminator(Block * blk) {
  switch (blk->terminator()) {
    case BlockTerm_Return:
      genReturn(blk->termValue());
      break;

    case BlockTerm_Branch:
      builder_.CreateBr(blk->succIRBlock(0));
      break;

    case BlockTerm_Conditional:
      genTestExpr(
          blk->termValue(),
          blk->succIRBlock(0),
          blk->succIRBlock(1));
      break;

    case BlockTerm_Throw:
    case BlockTerm_ResumeUnwind:
      genThrow(blk);
      break;

    case BlockTerm_LocalReturn:
      genLocalReturn(blk);
      break;

    case BlockTerm_Catch:
      genCatch(blk);
      break;

    case BlockTerm_Switch:
      genSwitch(blk);
      break;

    case BlockTerm_None:
    default:
      diag.fatal() << "Invalid terminator for block '" << blk->label() << "'";
      DFAIL("Invalid terminator");
  }
};

void CodeGenerator::genReturn(Expr * returnVal) {
  if (returnVal == NULL) {
    builder_.CreateRet(NULL);
  } else {
    Value * value = genExpr(returnVal);
    DASSERT(value != NULL);
    if (requiresImplicitDereference(returnVal->type())) {
      //value = builder_.CreateLoad(value);
    }
    //genDoFinally(blk);
    builder_.CreateRet(value);
  }
}

void CodeGenerator::genLocalReturn(Block * blk) {
  BlockList & preds = blk->preds();
  if (preds.size() == 1) {
    builder_.CreateBr(preds.front()->irBlock());
    return;
  }

  DFAIL("Implement local returns");
}

void CodeGenerator::genDoFinally(Block * blk) {
  DFAIL("Implement");
}

bool CodeGenerator::genTestExpr(const Expr * test, BasicBlock * blkTrue, BasicBlock * blkFalse) {
  switch (test->exprType()) {
    case Expr::And: {
      BinaryExpr * op = static_cast<const BinaryExpr *>(test);
      BasicBlock * blkSecond = BasicBlock::Create(context_, "and", currentFn_, blkTrue);
      genTestExpr(op->first(), blkSecond, blkFalse);
      builder_.SetInsertPoint(blkSecond);
      genTestExpr(op->second(), blkTrue, blkFalse);
      return true;
    }

    case Expr::Or: {
      BinaryExpr * op = static_cast<const BinaryExpr *>(test);
      BasicBlock * blkSecond = BasicBlock::Create(context_, "or", currentFn_, blkFalse);
      genTestExpr(op->first(), blkTrue, blkSecond);
      builder_.SetInsertPoint(blkSecond);
      genTestExpr(op->second(), blkTrue, blkFalse);
      return true;
    }

    case Expr::Not: {
      // For logical negation, flip the true and false blocks.
      UnaryExpr * op = static_cast<const UnaryExpr *>(test);
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

void CodeGenerator::genThrow(Block * blk) {
  Expr * exceptVal = NULL;
  Value * throwable = NULL;
  Value * unwindInfo = NULL;

  if (blk->termExprs().size() == 1) {
    exceptVal = blk->termExprs().front();
  }

  if (blk->terminator() == BlockTerm_ResumeUnwind) {
    const llvm::Type * throwableType = Builtins::typeThrowable->irType();
    Function * ehException = llvm::Intrinsic::getDeclaration(
        irModule_, llvm::Intrinsic::eh_exception, NULL, 0);
    unwindInfo = builder_.CreateBitCast(
        builder_.CreateCall(ehException, "eh_ptr"),
        llvm::PointerType::get(throwableType->getContainedType(2), 0));
  } else if (exceptVal != NULL) {
    // Construct the exception object
    Value * exception = genExpr(exceptVal);
    if (exception == NULL) {
      return;
    }

    const llvm::Type * throwableType = Builtins::typeThrowable->irType();
    irModule_->addTypeName("tart.core.Throwable", throwableType);

    throwable = builder_.CreateBitCast(exception, llvm::PointerType::getUnqual(throwableType));
    unwindInfo = builder_.CreateStructGEP(throwable, 2);
  } else {
    diag.warn(blk->termLocation()) << "Unimplemented re-throw of exception.";
    builder_.CreateUnreachable();
    return;
  }

  // Create the argument list to _Unwind_RaiseException.
  Value * unwindArgs[1];
  unwindArgs[0] = unwindInfo;

  // Create the unwind instruction.
  Block * unwindTarget_ = blk->unwindTarget();
  Function * unwindFunc;
  const char * label = "throw";

  if (blk->terminator() == BlockTerm_ResumeUnwind) {
    label = "resume";
    unwindFunc = getUnwindResume();
  } else {
    unwindFunc = getUnwindRaiseException();
  }

  if (unwindTarget_ != NULL) {
    // If the 'throw' statement is in a try block, then invoke _Unwind_RaiseException.
    Function * f = currentFn_;
    BasicBlock * postThrow = BasicBlock::Create(context_, "unreachable", f);
    Value * result = builder_.CreateInvoke(unwindFunc, postThrow,
        unwindTarget_->irBlock(), &unwindArgs[0], &unwindArgs[1], label);
    postThrow->moveAfter(builder_.GetInsertBlock());
    builder_.SetInsertPoint(postThrow);
  } else {
    // Otherwise, just call it - it will get handled further up the call stack.
    builder_.CreateCall(unwindFunc, &unwindArgs[0], &unwindArgs[1], label);
  }

  builder_.CreateUnreachable();
}

void CodeGenerator::genCatch(Block * blk) {
  Function * ehException = llvm::Intrinsic::getDeclaration(
      irModule_, llvm::Intrinsic::eh_exception, NULL, 0);
  Function * ehSelector = llvm::Intrinsic::getDeclaration(
      irModule_, llvm::Intrinsic::eh_selector_i32, NULL, 0);
  Function * personality = getExceptionPersonality();

  // Exception header
  Value * ehPtr = builder_.CreateCall(ehException, "eh_ptr");

  // Args to llvm.eh.selector
  ValueList args;
  args.push_back(ehPtr);
  args.push_back(builder_.CreateBitCast(
      personality, llvm::PointerType::get(llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0)));

  // Add an argument for each catch block, or the finally block if there is one.
  size_t numSelectors = blk->termExprs().size() - 1;
  for (size_t i = 0; i < numSelectors; ++i) {
    // First entry is the throwable, so offset by 1.
    TypeLiteralExpr * catchExpr = cast_or_null<TypeLiteralExpr>(blk->termExprs()[i + 1]);
    if (catchExpr != NULL) {
      // Catch handler
      CompositeType * exceptType = cast<CompositeType>(catchExpr->value());
      args.push_back(getTypeInfoBlockPtr(exceptType));
    } else {
      // Finally handler
      args.push_back(getInt32Val(0));
    }
  }

  // Call llvm.eh.selector to determine the action.
  Value * ehAction = builder_.CreateCall(ehSelector, args.begin(), args.end(), "eh_action");

  // Compute the offset of the unwind info structure from the throwable base.
  llvm::Constant * gepIndices[2];
  gepIndices[0] = getInt32Val(0);
  gepIndices[1] = getInt32Val(2);
  const llvm::Type * throwableType = Builtins::typeThrowable->irType();
  llvm::Constant * unwindInfoOffset = llvm::ConstantExpr::getPtrToInt(
      llvm::ConstantExpr::getGetElementPtr(
          ConstantPointerNull::get(llvm::PointerType::get(throwableType, 0)),
          gepIndices, 2),
      builder_.getInt32Ty());

  // Subtract the offset to get the address of the throwable.
  Value * offsetIndices[2];
  offsetIndices[0] = getInt32Val(0);
  offsetIndices[1] = llvm::ConstantExpr::getNeg(unwindInfoOffset);
  Value * throwValue = builder_.CreateGEP(
      builder_.CreateBitCast(ehPtr, llvm::PointerType::get(
          ArrayType::get(llvm::Type::getInt8Ty(llvm::getGlobalContext()), 0), 0)),
      &offsetIndices[0], &offsetIndices[2],
      "eh_throwable");

  // Get the address of the throwable.
  IRValueExpr * throwExpr = cast<IRValueExpr>(blk->termExprs().front());
  throwExpr->setValue(throwValue);

  // Transfer to the catch block based on ehAction.
  if (numSelectors > 1) {
    SwitchInst * si = builder_.CreateSwitch(ehAction, blk->succIRBlock(0), numSelectors);
    for (size_t i = 0; i < numSelectors; ++i) {
      BasicBlock * catchBody = blk->succIRBlock(i);
      si->addCase(getInt32Val(i), catchBody);
    }
  } else {
    builder_.CreateBr(blk->succIRBlock(0));
  }
}

void CodeGenerator::genSwitch(Block * blk) {
  Expr * testExpr = blk->termExprs()[0];
  Block * defaultBlock = blk->succs()[0];
  Value * switchValue = genExpr(testExpr);
  if (switchValue == NULL) {
    return;
  }

  size_t numCases = blk->succs().size() - 1;
  if (switchValue->getType()->isInteger()) {
    SwitchInst * switchInst = builder_.CreateSwitch(switchValue, defaultBlock->irBlock(), numCases);
    for (size_t i = 0; i < numCases; ++i) {
      Expr * caseValExpr = blk->termExprs()[i + 1];
      Block * caseBody = blk->succs()[i + 1];
      Constant * caseVal = genConstExpr(caseValExpr);
      if (caseVal != NULL) {
        switchInst->addCase(cast<ConstantInt>(caseVal), caseBody->irBlock());
      }
    }
  } else {
    DFAIL("Implement non-integer switch");
  }
}

} // namespace tart

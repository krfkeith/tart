/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Exprs.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/UnionType.h"
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

void CodeGenerator::genLocalStorage(LocalScopeList & lsl) {
  for (LocalScopeList::iterator it = lsl.begin(); it != lsl.end(); ++it) {
    LocalScope * lscope = *it;
    for (Defn * de = lscope->firstMember(); de != NULL; de = de->nextInScope()) {
      if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
        if (var->hasStorage()) {
          genLocalVar(var);
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
        if (var->hasStorage() && var->type()->containsReferenceType()) {
          genGCRoot(var->irValue(), var->type());
        }
      }
    }
  }
}

void CodeGenerator::genLocalVar(VariableDefn * var) {
  // Don't generate the IR if we've already done so
  DASSERT_OBJ(var->storageClass() == Storage_Local, var);
  DASSERT_OBJ(var->irValue() == NULL, var);
  DASSERT_OBJ(var->type() != NULL, var);

  // Generate the variable type
  const Type * varType = var->type();
  DASSERT(varType != NULL);
  const llvm::Type * irType = varType->irEmbeddedType();

  // Allocate space for the variable on the stack
  Value * lValue = builder_.CreateAlloca(irType, 0, var->name());
  var->setIRValue(lValue);
}

void CodeGenerator::initGCRoot(Value * rootValue) {
  AllocaInst * alloca = cast<AllocaInst>(rootValue);
  const llvm::Type * rootType = alloca->getAllocatedType();
  if (const PointerType * ptype = dyn_cast<PointerType>(rootType)) {
    builder_.CreateStore(
        ConstantPointerNull::get(ptype),
        alloca);
  } else {
    builder_.CreateStore(
        ConstantAggregateZero::get(rootType),
        alloca);
  }
}

void CodeGenerator::genGCRoot(Value * allocaValue, const Type * varType) {
  switch (varType->typeClass()) {
    case Type::Class:
      markGCRoot(allocaValue, NULL);
      break;

    case Type::Struct:
    case Type::Tuple:
      if (varType->containsReferenceType()) {
        if (varType->typeShape() != Shape_Small_LValue &&
            varType->typeShape() != Shape_Large_Value) {
          diag.fatal() << "Wrong shape for " << varType << " " << varType->typeShape();
        }
        markGCRoot(allocaValue, getTraceTable(varType));
      }
      break;

    case Type::Union:
      if (varType->containsReferenceType()) {
        const UnionType * utype = static_cast<const UnionType *>(varType);
        if (utype->hasRefTypesOnly()) {
          markGCRoot(allocaValue, NULL);
          break;
        }

        if (varType->typeShape() != Shape_Small_LValue &&
            varType->typeShape() != Shape_Large_Value) {
          diag.fatal() << "Wrong shape for " << varType << " " << varType->typeShape();
        }
        markGCRoot(allocaValue, getTraceTable(varType));
      }
      break;

    case Type::BoundMethod:
      markGCRoot(allocaValue, getTraceTable(varType));
      break;

    default:
      break;
  }
}

void CodeGenerator::genBlocks(BlockList & blocks) {
  // Generate the code for each Block.
  for (BlockList::iterator b = blocks.begin(); b != blocks.end(); ++b) {
    Block * blk = *b;

    // Set up the insertion point and catch handler
    builder_.SetInsertPoint(blk->irBlock());
    unwindTarget_ = blk->unwindTarget() ? blk->unwindTarget()->irBlock() : NULL;

    ExprList & exprs = blk->exprs();
    for (ExprList::iterator it = exprs.begin(); it != exprs.end(); ++it) {
      genStmt(*it);
    }

    genBlockTerminator(blk);
  }

  unwindTarget_ = NULL;
}

void CodeGenerator::setDebugLocation(const SourceLocation & loc) {
  if (debug_ && loc != dbgLocation_) {
    dbgLocation_ = loc;
    if (loc.region == NULL) {
      builder_.SetCurrentDebugLocation(llvm::DebugLoc());
    } else {
      TokenPosition pos = tokenPosition(loc);
      DASSERT(pos.beginLine);
      builder_.SetCurrentDebugLocation(
          DebugLoc::get(pos.beginLine, pos.beginCol, genDIFile(loc.region)));
    }
  }
}

void CodeGenerator::genStmt(Expr * in) {
  setDebugLocation(in->location());
  genExpr(in);
}

void CodeGenerator::genBlockTerminator(Block * blk) {
  setDebugLocation(blk->termLocation());
  switch (blk->terminator()) {
    case BlockTerm_Return:
      genReturn(blk->termValue());
      break;

    case BlockTerm_Branch:
      //clearDeadRoots(blk->deadRoots());
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
    case BlockTerm_TraceCatch:
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
    const Type * returnType = returnVal->canonicalType();
    TypeShape returnShape = returnType->typeShape();
    DASSERT(value != NULL);

#if FC_STRUCTS_INTERNAL
    // TODO: Optimize so that we store directly into sret.
    if (returnShape == Shape_Large_Value) {
      value = loadValue(value, returnVal);
    }
    if (returnType->irEmbeddedType() != value->getType()) {
      returnType->irEmbeddedType()->dump(irModule_);
      value->getType()->dump(irModule_);
      DASSERT(false);
    }
    DASSERT_TYPE_EQ(returnVal, returnType->irEmbeddedType(), value->getType());
#else
    if ((returnShape == Shape_Large_Value || returnShape == Shape_Small_LValue)) {
      value = builder_.CreateLoad(value);
    }
#endif

    //genDoFinally(blk);
    if (structRet_ != NULL) {
      builder_.CreateStore(value, structRet_);
      builder_.CreateRet(NULL);
    } else {
      DASSERT_TYPE_EQ(returnVal, returnType->irEmbeddedType(), value->getType());
      builder_.CreateRet(value);
    }
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
        throwableType->getContainedType(2)->getPointerTo());
  } else if (exceptVal != NULL) {
    // Construct the exception object
    Value * exception = genExpr(exceptVal);
    if (exception == NULL) {
      return;
    }

    const llvm::Type * throwableType = Builtins::typeThrowable->irType();
    irModule_->addTypeName("tart.core.Throwable", throwableType);

    throwable = builder_.CreateBitCast(exception, throwableType->getPointerTo());
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
  bool requestStackTrace = blk->terminator() == BlockTerm_TraceCatch;
  Function * ehException = llvm::Intrinsic::getDeclaration(
      irModule_, llvm::Intrinsic::eh_exception, NULL, 0);
  Function * ehSelector = llvm::Intrinsic::getDeclaration(
      irModule_, llvm::Intrinsic::eh_selector, NULL, 0);
  Function * personality = requestStackTrace ?
      getExceptionTracePersonality() : getExceptionPersonality();

  // Exception header
  Value * ehPtr = builder_.CreateCall(ehException, "eh_ptr");

  // Args to llvm.eh.selector
  ValueList args;
  args.push_back(ehPtr);
  args.push_back(builder_.CreateBitCast(
      personality, builder_.getInt8Ty()->getPointerTo()));

  // Add an argument for each catch block, or the finally block if there is one.
  size_t numSelectors = blk->termExprs().size() - 1;
  for (size_t i = 0; i < numSelectors; ++i) {
    // First entry is the throwable, so offset by 1.
    TypeLiteralExpr * catchExpr = cast_or_null<TypeLiteralExpr>(blk->termExprs()[i + 1]);
    if (catchExpr != NULL) {
      // Catch handler
      const CompositeType * exceptType = cast<CompositeType>(catchExpr->value());
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
          ConstantPointerNull::get(throwableType->getPointerTo()),
          gepIndices, 2),
      builder_.getInt32Ty());

  // Subtract the offset to get the address of the throwable.
  // Note that we do *not* want to call InBoundsGEP here.
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
  DASSERT(switchValue->getType()->isIntegerTy());
  SwitchInst * switchInst = builder_.CreateSwitch(switchValue, defaultBlock->irBlock(), numCases);
  for (size_t i = 0; i < numCases; ++i) {
    Expr * caseValExpr = blk->termExprs()[i + 1];
    Block * caseBody = blk->succs()[i + 1];
    Constant * caseVal = genConstExpr(caseValExpr);
    if (caseVal != NULL) {
      switchInst->addCase(cast<ConstantInt>(caseVal), caseBody->irBlock());
    }
  }
}

} // namespace tart

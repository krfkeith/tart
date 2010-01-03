/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/Intrinsic.h"
#include "llvm/Function.h"

#ifdef NDEBUG
#define DASSERT_TYPE_EQ(expected, actual)
#define DASSERT_TYPE_EQ_MSG(expected, actual, msg)
#else
#define DASSERT_TYPE_EQ(expected, actual) \
      if (expected != actual) {\
        diag.fatal() << "Expected '" << expected << "' == '" << actual << "'"; \
      }

#define DASSERT_TYPE_EQ_MSG(expected, actual, msg) \
      if (expected != actual) {\
        diag.fatal() << "Expected '" << expected << "' == '" << actual << \
            "' " << msg; \
      }

#endif

namespace tart {

using namespace llvm;

Value * CodeGenerator::genCall(const tart::FnCallExpr* in) {
  const FunctionDefn * fn = in->function();
  const FunctionType * fnType = fn->functionType();

  if (fn->isIntrinsic()) {
    return fn->intrinsic()->generate(*this, in);
  }

  ValueList args;

  fnType->irType(); // Need to know the irType for isStructReturn.
  Value * retVal = NULL;
  if (fnType->isStructReturn()) {
    DASSERT(in->exprType() != Expr::CtorCall); // Constructors have no return.
    retVal = builder_.CreateAlloca(fnType->returnType()->irType(), NULL, "sret");
    args.push_back(retVal);
  }

  Value * selfArg = NULL;
  if (in->selfArg() != NULL) {
    if (in->selfArg()->type()->typeClass() == Type::Struct) {
      if (in->exprType() == Expr::CtorCall) {
        selfArg = genExpr(in->selfArg());
      } else {
        selfArg = genLValueAddress(in->selfArg());
      }
    } else {
      selfArg = genExpr(in->selfArg());
    }

    DASSERT_OBJ(selfArg != NULL, in->selfArg());

    // Upcast the self argument type.
    if (fnType->selfParam() != NULL) {
      const Type * selfType = dealias(fnType->selfParam()->type());
      selfArg = genUpCastInstr(selfArg, in->selfArg()->type(), selfType);
    }

    if (fn->storageClass() == Storage_Instance) {
      args.push_back(selfArg);
    }
  }

  const ExprList & inArgs = in->args();
  for (ExprList::const_iterator it = inArgs.begin(); it != inArgs.end(); ++it) {
    const Expr * arg = *it;
    const Type * argType = arg->canonicalType();
    //TypeShape typeShape = argType->typeShape();
    Value * argVal = genExpr(arg);
    if (argVal == NULL) {
      return NULL;
    }

    args.push_back(argVal);
  }

  // Generate the function to call.
  Value * fnVal;
  if (in->exprType() == Expr::VTableCall) {
    DASSERT_OBJ(selfArg != NULL, in);
    const Type * classType = dealias(fnType->selfParam()->type());
    if (classType->typeClass() == Type::Class) {
      fnVal = genVTableLookup(fn, static_cast<const CompositeType *>(classType), selfArg);
    } else if (classType->typeClass() == Type::Interface) {
      fnVal = genITableLookup(fn, static_cast<const CompositeType *>(classType), selfArg);
    } else {
      // Struct or protocol.
      fnVal = genFunctionValue(fn);
    }
  } else {
    fnVal = genFunctionValue(fn);
  }

  Value * result = genCallInstr(fnVal, args.begin(), args.end(), fn->name());
  if (in->exprType() == Expr::CtorCall) {
    // Constructor call returns the 'self' argument.
    /*if (in->selfArg() != NULL && in->selfArg()->type()->typeClass() == Type::Struct) {
      return builder_.CreateLoad(selfArg);
    }*/

    return selfArg;
  } else if (fnType->isStructReturn()) {
    return retVal;
  } else if (fn->returnType()->typeShape() == Shape_Small_LValue) {
    retVal = builder_.CreateAlloca(fnType->returnType()->irType(), NULL, "retval");
    builder_.CreateStore(result, retVal);
    return retVal;
  } else {
    return result;
  }
}

Value * CodeGenerator::genIndirectCall(const tart::IndirectCallExpr* in) {
  const Expr * fn = in->function();
  const Type * fnType = fn->type();

  Value * fnValue;
  ValueList args;

  if (const FunctionType * ft = dyn_cast<FunctionType>(fnType)) {
    fnValue = genExpr(fn);
    if (fnValue != NULL) {
      if (ft->isStatic()) {
        //fnValue = builder_.CreateLoad(fnValue);
      } else {
        //DFAIL("Implement");
      }
    }
  } else if (const BoundMethodType * bmType = dyn_cast<BoundMethodType>(fnType)) {
    Value * fnref = genExpr(fn);
    if (fnref == NULL) {
      return NULL;
    }

    fnValue = builder_.CreateExtractValue(fnref, 0, "method");
    Value * selfArg = builder_.CreateExtractValue(fnref, 1, "self");
    if (selfArg == NULL) {
      return NULL;
    }

    args.push_back(selfArg);
  } else {
    diag.info(in) << in->function() << " - " << in->function()->exprType();
    TFAIL << "Invalid function type: " << in->function() << " - " << in->function()->exprType();
  }

  const ExprList & inArgs = in->args();
  for (ExprList::const_iterator it = inArgs.begin(); it != inArgs.end(); ++it) {
    Value * argVal = genExpr(*it);
    if (argVal == NULL) {
      return NULL;
    }

    args.push_back(argVal);
  }

  return genCallInstr(fnValue, args.begin(), args.end(), "indirect");
}

Value * CodeGenerator::genVTableLookup(const FunctionDefn * method, const CompositeType * classType,
    Value * selfPtr) {
  DASSERT_OBJ(!method->isFinal(), method);
  DASSERT_OBJ(!method->isCtor(), method);
  int methodIndex = method->dispatchIndex();
  if (methodIndex < 0) {
    diag.fatal(method) << "Invalid member index of " << method;
    return NULL;
  }

  // Make sure it's a class.
  DASSERT(classType->typeClass() == Type::Class);
  //DASSERT_TYPE_EQ(classType->irParameterType(), selfPtr->getType());

  // Upcast to type 'object' and load the vtable pointer.
  ValueList indices;
  for (const CompositeType * t = classType; t != NULL && t != Builtins::typeObject; t = t->super()) {
    indices.push_back(getInt32Val(0));
  }
  indices.push_back(getInt32Val(0));
  indices.push_back(getInt32Val(0));

  // Get the TIB
  Value * tib = builder_.CreateLoad(
      builder_.CreateInBoundsGEP(selfPtr, indices.begin(), indices.end()), "tib");
  //DASSERT_TYPE_EQ(llvm::PointerType::get(Builtins::typeTypeInfoBlock.irType(), 0), tib->getType());

  indices.clear();
  indices.push_back(getInt32Val(0));
  indices.push_back(getInt32Val(TIB_METHOD_TABLE));
  indices.push_back(getInt32Val(methodIndex));
  Value * fptr = builder_.CreateLoad(
      builder_.CreateInBoundsGEP(tib, indices.begin(), indices.end()), method->name());
  return builder_.CreateBitCast(fptr, llvm::PointerType::getUnqual(method->type()->irType()));
}

Value * CodeGenerator::genITableLookup(const FunctionDefn * method, const CompositeType * classType,
    Value * objectPtr) {

  // Interface function table entry
  DASSERT(!method->isFinal());
  DASSERT(!method->isCtor());
  int methodIndex = method->dispatchIndex();
  if (methodIndex < 0) {
    diag.fatal(method) << "Invalid member index of " << method;
    return NULL;
  }

  // Make sure it's an interface.
  DASSERT(classType->typeClass() == Type::Interface);

  // Get the interface ID (which is just the type pointer).
  //Constant * itype = getTypeInfoBlockPtr(classType);
  Constant * iname = reflector_.internSymbol(classType->typeDefn()->linkageName());

  // Load the pointer to the TIB.
  objectPtr = builder_.CreatePointerCast(objectPtr, Builtins::typeObject->irEmbeddedType());
  Value * tib = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(objectPtr, 0, 0, "tib_ptr"), "tib");

  // Load the pointer to the dispatcher function.
  Value * dispatcher = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(tib, 0, TIB_IDISPATCH, "idispatch_ptr"), "idispatch");

  // Construct the call to the dispatcher
  ValueList args;
  args.push_back(iname);
  args.push_back(getInt32Val(methodIndex));
  Value * methodPtr = genCallInstr(dispatcher, args.begin(), args.end(), "method_ptr");
  return builder_.CreateBitCast(
      methodPtr, llvm::PointerType::getUnqual(method->type()->irType()), "method");
}

/** Get the address of a value. */
Value * CodeGenerator::genBoundMethod(const BoundMethodExpr * in) {
  const BoundMethodType * type = cast<BoundMethodType>(in->type());
  const FunctionDefn * fn = in->method();
  if (fn->isIntrinsic()) {
    diag.error(in) << "Intrinsic methods cannot be called indirectly.";
    return NULL;
  } else if (fn->isCtor()) {
    diag.error(in) << "Constructors cannot be called indirectly (yet).";
    return NULL;
  }

  Value * selfArg = NULL;
  if (in->selfArg() != NULL) {
    selfArg = genExpr(in->selfArg());

    // Upcast the self argument type.
    if (fn->functionType()->selfParam() != NULL) {
      const Type * selfType = dealias(fn->functionType()->selfParam()->type());
      selfArg = genUpCastInstr(selfArg, in->selfArg()->type(), selfType);
    }
  }

  // Generate the function to call.
  Value * fnVal;
  if (in->exprType() == Expr::VTableCall) {
    DASSERT_OBJ(selfArg != NULL, in);
    const Type * classType = dealias(fn->functionType()->selfParam()->type());
    if (classType->typeClass() == Type::Class) {
      fnVal = genVTableLookup(fn, static_cast<const CompositeType *>(classType), selfArg);
    } else if (classType->typeClass() == Type::Interface) {
      fnVal = genITableLookup(fn, static_cast<const CompositeType *>(classType), selfArg);
    } else {
      // Struct or protocol.
      fnVal = genFunctionValue(fn);
    }
  } else {
    fnVal = genFunctionValue(fn);
  }

  const llvm::Type * fnValType =
      StructType::get(context_, fnVal->getType(), selfArg->getType(), NULL);

  Value * result = builder_.CreateAlloca(fnValType);
  builder_.CreateStore(fnVal, builder_.CreateConstInBoundsGEP2_32(result, 0, 0, "method"));
  builder_.CreateStore(selfArg, builder_.CreateConstInBoundsGEP2_32(result, 0, 1, "self"));
  result = builder_.CreateLoad(
      builder_.CreateBitCast(result, llvm::PointerType::get(type->irType(), 0)));
  return result;
}

Value * CodeGenerator::genNew(const tart::NewExpr* in) {
  if (const CompositeType * ctdef = dyn_cast<CompositeType>(in->type())) {
    const llvm::Type * type = ctdef->irType();
    if (ctdef->typeClass() == Type::Struct) {
      return builder_.CreateAlloca(type, 0, ctdef->typeDefn()->name());
    } else if (ctdef->typeClass() == Type::Class) {
      Function * allocator = getTypeAllocator(ctdef);
      if (allocator != NULL) {
        return builder_.CreateCall(allocator, Twine(ctdef->typeDefn()->name(), StringRef("_new")));
      } else {
        diag.fatal(in) << "Cannot create an instance of type '" <<
        ctdef->typeDefn()->name() << "'";
      }
    }
  }

  DFAIL("IllegalState");
}

Value * CodeGenerator::genCallInstr(Value * func, ValueList::iterator firstArg,
    ValueList::iterator lastArg, const char * name) {
  checkCallingArgs(func, firstArg, lastArg);
  if (unwindTarget_ != NULL) {
    Function * f = currentFn_;
    BasicBlock * normalDest = BasicBlock::Create(context_, "nounwind", f);
    normalDest->moveAfter(builder_.GetInsertBlock());
    Value * result = builder_.CreateInvoke(func, normalDest, unwindTarget_, firstArg, lastArg);
    builder_.SetInsertPoint(normalDest);
    if (!result->getType()->isVoidTy()) {
      result->setName(name);
    }
    return result;
  } else {
    Value * result = builder_.CreateCall(func, firstArg, lastArg);
    if (!result->getType()->isVoidTy()) {
      result->setName(name);
    }
    return result;
  }
}

void CodeGenerator::checkCallingArgs(const llvm::Value * fn,
    ValueList::const_iterator first, ValueList::const_iterator last) {
#if !NDEBUG
  const llvm::FunctionType * fnType = cast<llvm::FunctionType>(fn->getType()->getContainedType(0));
  size_t argCount = last - first;
  DASSERT(fnType->getNumParams() == argCount);
  for (size_t i = 0; i < argCount; ++i) {
    const llvm::Type * paramType = fnType->getContainedType(i + 1);
    const llvm::Type * argType = first[i]->getType();
    if (paramType != argType) {
      diag.error() << "Incorrect type for argument " << i << ": expected '" << *paramType;
      diag.info() << "but was '" << *argType;
      diag.info() << "function value: '" << *fn;
      DFAIL("Called from here");
    }
  }
#endif
}

} // namespace tart

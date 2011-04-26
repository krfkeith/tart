/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/FunctionDefn.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/CompositeType.h"

#include "tart/Gen/CodeGenerator.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/Intrinsic.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/Function.h"

namespace tart {

using namespace llvm;

Value * CodeGenerator::genCall(const tart::FnCallExpr* in) {
  const FunctionDefn * fn = in->function();
  const FunctionType * fnType = fn->functionType();
  bool saveIntermediateStackRoots = true;

  if (fn->isIntrinsic()) {
    return fn->intrinsic()->generate(*this, in);
  }

  size_t savedRootCount = rootStackSize();

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
    Type::TypeClass selfTypeClass = in->selfArg()->type()->typeClass();
    if (selfTypeClass == Type::Struct) {
      if (in->exprType() == Expr::CtorCall) {
        selfArg = genExpr(in->selfArg());
      } else {
        selfArg = genLValueAddress(in->selfArg());
      }
    } else {
      selfArg = genArgExpr(in->selfArg(), saveIntermediateStackRoots);
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

    TypeShape argTypeShape = argType->typeShape();
    Value * argVal = genArgExpr(arg, saveIntermediateStackRoots);
    if (argVal == NULL) {
      return NULL;
    }

    DASSERT_TYPE_EQ(in, argType->irParameterType(), argVal->getType());
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
      DASSERT(classType->typeClass() == Type::Struct);
      // Struct or protocol.
      fnVal = genFunctionValue(fn);
    }
  } else {
    fnVal = genCallableDefn(fn);
  }

  Value * result = genCallInstr(fnVal, args.begin(), args.end(), fn->name());
  if (in->exprType() == Expr::CtorCall) {
    // Constructor call returns the 'self' argument.
    TypeShape selfTypeShape = in->selfArg()->type()->typeShape();
    // A large value type will, at this point, be a pointer.
    if (selfTypeShape == Shape_Small_LValue) {
      selfArg = builder_.CreateLoad(selfArg, "self");
    }

    result = selfArg;
  } else if (fnType->isStructReturn()) {
    result = retVal;
  }

  // Clear out all the temporary roots
  popRootStack(savedRootCount);
  return result;
}

Value * CodeGenerator::genIndirectCall(const tart::IndirectCallExpr* in) {
  const Expr * fn = in->function();
  const Type * fnType = dealias(fn->type());
  bool saveIntermediateStackRoots = true;

  Value * fnValue;
  ValueList args;
  size_t savedRootCount = rootStackSize();

  if (const FunctionType * ft = dyn_cast<FunctionType>(fnType)) {
    fnValue = genArgExpr(fn, saveIntermediateStackRoots);

    if (fnValue != NULL) {
      if (ft->isStatic()) {
        //fnValue = builder_.CreateLoad(fnValue);
      } else {
        //DFAIL("Implement");
      }
    }
  } else {
    diag.info(in) << in->function() << " - " << in->function()->exprType();
    TFAIL << "Invalid function type: " << in->function() << " - " << in->function()->exprType();
  }

  const ExprList & inArgs = in->args();
  for (ExprList::const_iterator it = inArgs.begin(); it != inArgs.end(); ++it) {
    Expr * arg = *it;
    Value * argVal = genArgExpr(arg, saveIntermediateStackRoots);
    if (argVal == NULL) {
      return NULL;
    }

    args.push_back(argVal);
  }

  llvm::Value * result = genCallInstr(fnValue, args.begin(), args.end(), "indirect");

  // Clear out all the temporary roots
  popRootStack(savedRootCount);
  return result;
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
  Twine tibName = Twine("tib.") + classType->typeDefn()->name();
  Twine tibAddrName = Twine("tibaddr.") + classType->typeDefn()->name();
  Value * tib = builder_.CreateLoad(
      builder_.CreateInBoundsGEP(selfPtr, indices.begin(), indices.end(), tibAddrName), tibName);

  indices.clear();
  indices.push_back(getInt32Val(0));
  indices.push_back(getInt32Val(TIB_METHOD_TABLE));
  indices.push_back(getInt32Val(methodIndex));
  Twine methodName = Twine("method.") + method->name();
  Twine methodPtrName = Twine("method.ptr.") + method->name();
  Value * fptr = builder_.CreateLoad(
      builder_.CreateInBoundsGEP(tib, indices.begin(), indices.end()), methodPtrName);
  return builder_.CreateBitCast(fptr, method->type()->irType()->getPointerTo(), methodName);
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
  Twine tibName = Twine("tib.") + classType->typeDefn()->name();
  Twine tibAddrName = Twine("tibaddr.") + classType->typeDefn()->name();
  Value * tib = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(objectPtr, 0, 0, tibAddrName),
      tibName);

  // Load the pointer to the dispatcher function.
  Twine idispName = Twine("idispatch.") + classType->typeDefn()->name();
  Twine idispAddrName = Twine("idispatchaddr.") + classType->typeDefn()->name();
  Value * dispatcher = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(tib, 0, TIB_IDISPATCH, idispAddrName), idispName);

  // Construct the call to the dispatcher
  ValueList args;
  args.push_back(iname);
  args.push_back(getInt32Val(methodIndex));
  Twine methodName = Twine("method.") + method->name();
  Twine methodPtrName = Twine("method.ptr.") + method->name();
  Value * methodPtr = genCallInstr(dispatcher, args.begin(), args.end(), methodPtrName);
  return builder_.CreateBitCast(methodPtr, method->type()->irType()->getPointerTo(), methodName);
}

Value * CodeGenerator::genBoundMethod(const BoundMethodExpr * in) {
  DFAIL("Implement");
#if 0
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
      builder_.CreateBitCast(result, type->irType()->getPointerTo()));
  return result;
#endif
}

Value * CodeGenerator::genNew(const tart::NewExpr* in) {
  if (const CompositeType * ctdef = dyn_cast<CompositeType>(in->type())) {
    const llvm::Type * type = ctdef->irType();
    if (ctdef->typeClass() == Type::Struct) {
      if (ctdef->typeShape() == Shape_ZeroSize) {
        // Don't allocate if it's zero size.
        return ConstantPointerNull::get(type->getPointerTo());
      }
      return builder_.CreateAlloca(type, 0, ctdef->typeDefn()->name());
    } else if (ctdef->typeClass() == Type::Class) {
      DASSERT(gcAllocContext_ != NULL);
      Function * alloc = getGcAlloc();
      Value * newObj = builder_.CreateCall2(
          alloc, gcAllocContext_,
          llvm::ConstantExpr::getIntegerCast(
              llvm::ConstantExpr::getSizeOf(ctdef->irType()),
              intPtrType_, false),
          Twine(ctdef->typeDefn()->name(), StringRef("_new")));
      newObj = builder_.CreatePointerCast(newObj, ctdef->irType()->getPointerTo());
      genInitObjVTable(ctdef, newObj);
      return newObj;
    }
  }

  DFAIL("IllegalState");
}

Value * CodeGenerator::defaultAlloc(const tart::Expr * size) {
  DASSERT(gcAllocContext_ != NULL);
  Value * sizeVal = genExpr(size);
  Function * alloc = getGcAlloc();
  return builder_.CreatePointerCast(
      builder_.CreateCall2(alloc, gcAllocContext_, sizeVal, "newInstance"),
      builder_.getInt8PtrTy());
}

Value * CodeGenerator::genCallInstr(Value * func, ValueList::iterator firstArg,
    ValueList::iterator lastArg, const Twine & name) {
  checkCallingArgs(func, firstArg, lastArg);
  if (isUnwindBlock_) {
    Function * f = currentFn_;
    BasicBlock * normalDest = BasicBlock::Create(context_, "nounwind", f);
    moveToEnd(normalDest);
    Value * result = builder_.CreateInvoke(func, normalDest, getUnwindBlock(), firstArg, lastArg);
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
      diag.error() << "Incorrect type for argument " << i << ": expected '" << *paramType << "'";
      diag.info() << "but was '" << *argType << "'";
      diag.info() << "function value: '" << *fn;
      DFAIL("Called from here");
    }
  }
#endif
}

llvm::Value * CodeGenerator::genArgExpr(const Expr * in, bool saveIntermediateStackRoots) {
  const Type * argType = in->type();
  if (saveIntermediateStackRoots && argType->containsReferenceType()) {
    switch (in->exprType()) {
      // Operations which are known not to generate a stack root.
      case Expr::ConstInt:
      case Expr::ConstFloat:
      case Expr::ConstString:
      case Expr::ConstNull:
      case Expr::ConstObjRef:
      case Expr::ConstEmptyArray:
      case Expr::Truncate:
      case Expr::SignExtend:
      case Expr::ZeroExtend:
      case Expr::IntToFloat:
      case Expr::Compare:
      case Expr::InstanceOf:
      case Expr::RefEq:
      case Expr::Not:
      case Expr::And:
      case Expr::Or:
      case Expr::Complement:
      case Expr::TypeLiteral:
      case Expr::NoOp:
      case Expr::ClearVar:
        return genExpr(in);

      case Expr::LValue: {
        const LValueExpr * lval = static_cast<const LValueExpr *>(in);
        const ValueDefn * value = lval->value();
        // Params are roots, so don't need this.
        if (const ParameterDefn * param = dyn_cast<ParameterDefn>(value)) {
          return genExpr(in);
        }

        switch (value->storageClass()) {
          case Storage_Local:
            return genExpr(in);

          case Storage_Static:
          case Storage_Global:
            if (value->defnType() == Defn::Let) {
              return genExpr(in);
            }
            break;

          case Storage_Instance:
            if (value->defnType() == Defn::Let && lval->base() != NULL) {
              if (const LValueExpr * baseLVal = dyn_cast<LValueExpr>(lval->base())) {
                // Fields of params are taken care of as well.
                if (const ParameterDefn * param = dyn_cast<ParameterDefn>(baseLVal->value())) {
                  return genExpr(in);
                }
              }
            }
            break;

          default:
            break;
        }

        break;
      }

      case Expr::SharedValue: {
        const SharedValueExpr * svExpr = static_cast<const SharedValueExpr *>(in);
        if (svExpr->value() == NULL) {
          svExpr->setValue(genArgExpr(svExpr->arg(), saveIntermediateStackRoots));
        }

        return svExpr->value();
      }

      case Expr::New:
      case Expr::ElementRef:
      case Expr::ArrayLiteral:
      case Expr::FnCall:
      case Expr::CtorCall:
      case Expr::VTableCall:
      case Expr::IndirectCall:
        // These definitely needs to be saved...
        break;
//        return genExpr(in);

      default:
        break;

      case Expr::InitVar:
        // TODO: Handle constructors as a special case?
        //return genInitVar(static_cast<const InitVarExpr *>(in));
        break;

#if 0

      case Expr::BoundMethod:
        //return genBoundMethod(static_cast<const BoundMethodExpr *>(in));
        break;

      case Expr::BinaryOpcode:
        return genBinaryOpcode(static_cast<const BinaryOpcodeExpr *>(in));
#endif

      case Expr::UpCast:
        return genUpCast(static_cast<const CastExpr *>(in), true);

      case Expr::TryCast:
        return genDynamicCast(static_cast<const CastExpr *>(in), true, true);

      case Expr::DynamicCast:
        return genDynamicCast(static_cast<const CastExpr *>(in), false, true);

      case Expr::BitCast:
        return genBitCast(static_cast<const CastExpr *>(in), true);

      case Expr::UnionCtorCast:
        return genUnionCtorCast(static_cast<const CastExpr *>(in), true);
#if 0

      case Expr::UnionMemberCast:
      case Expr::CheckedUnionMemberCast:
        return genUnionMemberCast(static_cast<const CastExpr *>(in));

      case Expr::TupleCtor:
        return genTupleCtor(static_cast<const TupleCtorExpr *>(in));

      case Expr::Assign:
      case Expr::PostAssign:
        return genAssignment(static_cast<const AssignmentExpr *>(in));

      case Expr::MultiAssign:
        return genMultiAssign(static_cast<const MultiAssignExpr *>(in));

      case Expr::PtrDeref:
        return genPtrDeref(static_cast<const UnaryExpr *>(in));

      case Expr::Prog2: {
        const BinaryExpr * binOp = static_cast<const BinaryExpr *>(in);
        genExpr(binOp->first());
        return genExpr(binOp->second());
      }

      case Expr::IRValue: {
        const IRValueExpr * irExpr = static_cast<const IRValueExpr *>(in);
        DASSERT_OBJ(irExpr->value() != NULL, irExpr);
        return irExpr->value();
      }

      case Expr::ClosureEnv:
        return genClosureEnv(static_cast<const ClosureEnvExpr *>(in));
#endif
    }

    Value * argVal = genExpr(in);
    if (currentFn_ == NULL) {
      return argVal;
    }

    if (isa<llvm::Constant>(argVal)) {
      return argVal;
    }

    addTempRoot(in->type(), argVal, "temp.root");
    return argVal;
  } else {
    return genExpr(in);
  }
}

} // namespace tart

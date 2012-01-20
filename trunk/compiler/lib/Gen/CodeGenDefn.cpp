/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/PrimitiveType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/Attributes.h"
#include "llvm/Module.h"
#include "llvm/Function.h"
#include "llvm/Analysis/Verifier.h"

#include "llvm/Support/CommandLine.h"

namespace tart {

extern SystemNamespaceMember<FunctionDefn> gc_allocContext;

extern llvm::cl::opt<bool> SsGC;

using namespace llvm;

bool CodeGenerator::genXDef(Defn * de) {
  if (debug_) {
    dbgFile_ = genDIFile(de);
  }

  switch (de->defnType()) {
    case Defn::Let:
      return genLetDefn(static_cast<VariableDefn *>(de));

    case Defn::Var:
      return genVarValue(static_cast<VariableDefn *>(de));

    case Defn::Function:
      return genFunction(static_cast<FunctionDefn*>(de));

    case Defn::Typedef: {
      return genTypeDefn(static_cast<TypeDefn*>(de));
    }

    case Defn::Macro:
    case Defn::Namespace:
    case Defn::Property:
      return true;

    default:
      diag.fatal(de) << "No generator for " << de;
      return true;
  }
}

Constant * CodeGenerator::genCallableDefn(const FunctionDefn * fdef) {
  Function * fn = irModule_->getFunction(fdef->linkageName());
  if (fn != NULL) {
    return fn;
  }

  const FunctionType * funcType = fdef->functionType();

  if (fdef->isUndefined()) {
    // Return a function that throws an UnsupportedOperationError.
    Function * undefinedMethod = cast<Function>(genFunctionValue(Builtins::funcUndefinedMethod));
    undefinedMethod->setDoesNotReturn(true);
    return llvm::ConstantExpr::getPointerCast(undefinedMethod, funcType->irType()->getPointerTo());
  }

  if (fdef->mergeTo() != NULL) {
    // Return the merged function
    Function * fnVal = genFunctionValue(fdef->mergeTo());
    return llvm::ConstantExpr::getPointerCast(
        fnVal, fdef->functionType()->irType()->getPointerTo());
  }

  return genFunctionValue(fdef);
}

Function * CodeGenerator::genFunctionValue(const FunctionDefn * fdef) {
  DASSERT_OBJ(fdef->passes().isFinished(FunctionDefn::ParameterTypePass), fdef);
  Function * fn = irModule_->getFunction(fdef->linkageName());
  if (fn != NULL) {
    return fn;
  }

  DASSERT_OBJ(!fdef->isAbstract(), fdef);
  DASSERT_OBJ(!fdef->isInterfaceMethod(), fdef);
  DASSERT_OBJ(!fdef->isUndefined(), fdef);
  DASSERT_OBJ(!fdef->isIntrinsic(), fdef);
  DASSERT_OBJ(fdef->isSingular(), fdef);
  DASSERT_OBJ(fdef->passes().isFinished(FunctionDefn::ParameterTypePass), fdef);
  DASSERT_OBJ(fdef->passes().isFinished(FunctionDefn::ReturnTypePass), fdef);
  DASSERT_OBJ(fdef->defnType() != Defn::Macro, fdef);

  const FunctionType * funcType = fdef->functionType();

  // If it's a function from a different module...
  if (fdef->module() != module_) {
    fn = Function::Create(
        cast<llvm::FunctionType>(funcType->irType()),
        Function::ExternalLinkage, fdef->linkageName(),
        irModule_);
    return fn;
  }

  // Generate the function reference
  DASSERT_OBJ(funcType->isSingular(), fdef);

  fn = Function::Create(
      cast<llvm::FunctionType>(funcType->irType()),
      Function::ExternalLinkage, fdef->linkageName(), fdef->module()->irModule());

  if (fdef->flags() & FunctionDefn::NoInline) {
    fn->addFnAttr(llvm::Attribute::NoInline);
  }

  return fn;
}

bool CodeGenerator::genFunction(FunctionDefn * fdef) {
  // Don't generate undefined functions.
  if (fdef->isUndefined() || fdef->isAbstract() || fdef->isInterfaceMethod()) {
    return true;
  }

  DASSERT_OBJ(fdef->isSingular(), fdef);
  DASSERT_OBJ(fdef->type(), fdef);
  DASSERT_OBJ(fdef->type()->isSingular(), fdef);

  // Don't generate intrinsic functions.
  if (fdef->isIntrinsic()) {
    return true;
  }

  // Don't generate a function if it has been merged to another function
  if (fdef->mergeTo() != NULL || fdef->isUndefined()) {
    return true;
  }

  // Create the function
  Function * f = genFunctionValue(fdef);

  if (fdef->hasBody() && f->getBasicBlockList().empty()) {
    FunctionType * ftype = fdef->functionType();

    if (fdef->isSynthetic()) {
      f->setLinkage(GlobalValue::LinkOnceODRLinkage);
    }

    if (gcEnabled_) {
      if (SsGC) {
        f->setGC("shadow-stack");
      } else {
        f->setGC("tart-gc");
      }
    }

    if (debug_) {
      dbgContext_ = genDISubprogram(fdef);
      //dbgContext_ = genLexicalBlock(fdef->location());
      dbgInlineContext_ = DIScope();
      setDebugLocation(fdef->location());
    }

    BasicBlock * prologue = BasicBlock::Create(context_, "prologue", f);

    // Create the LLVM Basic Blocks corresponding to each high level BB.
//    BlockList & blocks = fdef->blocks();
//    for (BlockList::iterator b = blocks.begin(); b != blocks.end(); ++b) {
//      Block * blk = *b;
//      blk->setIRBlock(BasicBlock::Create(context_, blk->label(), f));
//    }

    builder_.SetInsertPoint(prologue);

    // Handle the explicit parameters
    unsigned param_index = 0;
    Function::arg_iterator it = f->arg_begin();

    Value * saveStructRet = structRet_;
    if (ftype->isStructReturn()) {
      it->addAttr(llvm::Attribute::StructRet);
      structRet_ = it;
      ++it;
    }

    // Handle the 'self' parameter
    if (ftype->selfParam() != NULL) {
      ParameterDefn * selfParam = ftype->selfParam();
      const Type * selfParamType = selfParam->type().unqualified();
      DASSERT_OBJ(fdef->storageClass() == Storage_Instance ||
          fdef->storageClass() == Storage_Local, fdef);
      DASSERT_OBJ(it != f->arg_end(), ftype);

      // Check if the self param is a root.
      if (selfParamType->isReferenceType()) {
        selfParam->setFlag(ParameterDefn::LValueParam, true);
        Value * selfAlloca = builder_.CreateAlloca(
            selfParam->type()->irEmbeddedType(), 0, "self.alloca");
        builder_.CreateStore(it, selfAlloca);
        selfParam->setIRValue(selfAlloca);
        markGCRoot(selfAlloca, NULL, "self.alloca");
      } else {
        // Since selfParam is always a pointer, we don't need to mark the object pointed
        // to as a root, since the next call frame up is responsible for tracing it.
        ftype->selfParam()->setIRValue(it);
      }

      it->setName("self");
      ++it;
    }

    // If this function needs to make allocations, cache a copy of the
    // allocation context pointer for this thread, since it can on some
    // platforms be expensive to look up.
    if (fdef->flags() & FunctionDefn::MakesAllocs) {
      Function * gcGetAllocContext = genFunctionValue(gc_allocContext);
      gcAllocContext_ = builder_.CreateCall(gcGetAllocContext, "allocCtx");
    }

    for (; it != f->arg_end(); ++it, ++param_index) {

      // Set the name of the Nth parameter
      ParameterDefn * param = ftype->params()[param_index];
      DASSERT_OBJ(param != NULL, fdef);
      DASSERT_OBJ(param->storageClass() == Storage_Local, param);
      QualifiedType paramType = param->internalType();
      it->setName(param->name());
      Value * paramValue = it;

      // If the parameter is a shared reference, then create the shared ref.
      if (param->isSharedRef()) {
        genLocalVar(param, paramValue);
        genGCRoot(param->irValue(), param->sharedRefType(), param->name());
        continue;
      }

      // If the parameter type contains any reference types, then the parameter needs
      // to be a root.
      bool paramIsRoot = false;
      if (paramType->isReferenceType()) {
        param->setFlag(ParameterDefn::LValueParam, true);
        paramIsRoot = true;
      } else if (paramType->containsReferenceType()) {
        // TODO: Handle roots of various shapes
        //param->setFlag(ParameterDefn::LValueParam, true);
      }

      // See if we need to make a local copy of the param.
      if (param->isLValue()) {
        Value * paramAlloca = builder_.CreateAlloca(paramType->irEmbeddedType(), 0, param->name());
        param->setIRValue(paramAlloca);

        if (paramType->typeShape() == Shape_Large_Value) {
          paramValue = builder_.CreateLoad(paramValue);
        }

        builder_.CreateStore(paramValue, paramAlloca);
        if (paramIsRoot) {
          genGCRoot(paramAlloca, paramType.unqualified(), param->name());
        }
      } else {
        param->setIRValue(paramValue);
      }
    }

    // Generate the body
    Function * saveFn = currentFn_;
    currentFn_ = f;
#if 0
    if (fdef->isGenerator()) {
      assert(false);
    } else {
#endif
      genLocalStorage(fdef->localScopes());
      genDISubprogramStart(fdef);
      genLocalRoots(fdef->localScopes());

      BasicBlock * blkEntry = createBlock("entry");
      builder_.SetInsertPoint(blkEntry);
      genExpr(fdef->body());

      if (!atTerminator()) {
        if (fdef->returnType()->isVoidType()) {
          builder_.CreateRetVoid();
        } else {
          // TODO: Use the location from the last statement of the function.
          diag.error(fdef) << "Missing return statement at end of non-void function.";
        }
      }

      gcAllocContext_ = NULL;
#if 0
    }
#endif

    builder_.SetInsertPoint(prologue);
    builder_.CreateBr(blkEntry);

    currentFn_ = saveFn;
    structRet_ = saveStructRet;

    if (!diag.inRecovery()) {
      if (verifyFunction(*f, PrintMessageAction)) {
        f->dump();
        DFAIL("Function failed to verify");
      }
    }

    //if (debug_ && !dbgContext_.isNull() && !dbgContext_.Verify()) {
    //  dbgContext_.Verify();
    //  DFAIL("BAD DBG");
    //}

    dbgContext_ = DIScope();
    dbgInlineContext_ = DIScope();
    builder_.ClearInsertionPoint();
    builder_.SetCurrentDebugLocation(llvm::DebugLoc());
  }

  return true;
}

Value * CodeGenerator::genLetValue(const VariableDefn * let) {
  // Don't generate the IR if we've already done so
  if (let->irValue() != NULL) {
    return let->irValue();
  }

  // Calculate the type.
  DASSERT(let->type()) << "Undefined type for let " << let;
  llvm::Type * irType = let->type()->irEmbeddedType();
  TypeShape shape = let->type()->typeShape();

  // Generate the value
  Value * value = NULL;
  if (let->initValue() != NULL) {
    // 'hasStorage' is true if:
    // -- it's not a local, and
    // -- the expression is not a constant, or its a constant object or constant array.
    if (let->hasStorage()) {
      if (shape == Shape_Reference) {
        irType = let->type()->irType();
      }

      if (let->module() == module_ || let->isSynthetic()) {
        value = genConstRef(let->initValue(), let->linkageName(), let->isSynthetic());
        if (value == NULL) {
          return NULL;
        }
        value = llvm::ConstantExpr::getPointerCast(cast<Constant>(value), irType->getPointerTo());
        //DASSERT_TYPE_EQ(let->initValue(), irType, value->getType()->getContainedType(0));
      } else {
        let->type()->irTypeComplete();
        value = new GlobalVariable(
            *irModule_, irType, true, GlobalValue::ExternalLinkage, NULL, let->linkageName());
        DASSERT_TYPE_EQ(let->initValue(), irType, value->getType()->getContainedType(0));
      }
    } else {
      value = genExpr(let->initValue());
      DASSERT_TYPE_EQ(let->initValue(), irType, value->getType());
    }

    if (value == NULL) {
      return NULL;
    }
  }

  Value * letValue = NULL;
  if (let->storageClass() == Storage_Local) {
    // If it's a local variable, then use the value directly.
    if (value == NULL) {
      diag.fatal(let) << "Use of value before initialization: " << let;
    }

    letValue = value;
  } else if (llvm::Constant * constantValue = dyn_cast<llvm::Constant>(value)) {
    // See if it's a constant.
    letValue = constantValue;
  } else {
    diag.error(let->location()) << "Non-constant let value " << let;
    DASSERT(let->passes().isFinished(VariableDefn::InitializerPass));
    DFAIL("let value not a constant");
  }

  DIType dbgType;
  if (debug_ && (let->storageClass() == Storage_Global || let->storageClass() == Storage_Static)) {
  //  dbgType = genTypeDebugInfo(letType);
  }

  let->setIRValue(letValue);
  return letValue;
}

bool CodeGenerator::genLetDefn(VariableDefn * let) {
  if (let->storageClass() != Storage_Local) {
    return genLetValue(let) != NULL;
  } else {
    // Don't generate the let value until it is initialized.
    return true;
  }
}

llvm::Value * CodeGenerator::genVarValue(const VariableDefn * var) {
  // If it's not a global, then then the IRValue must have been pre-generated.
  if (var->storageClass() != Storage_Global &&
      var->storageClass() != Storage_Static) {
    // Don't generate the IR if we've already done so.
    if (var->irValue() != NULL) {
      return var->irValue();
    }

    DFAIL("IllegalState");
  }

  return genGlobalVar(var);
}

llvm::Constant * CodeGenerator::genGlobalVar(const VariableDefn * var) {
  // Global variables never set the IRValue field, because that field has a different value
  // depending on what module we are compiling.
  DASSERT(var->defnType() == Defn::Var);
  DASSERT(var->irValue() == NULL);
  DASSERT(var->storageClass() == Storage_Global || var->storageClass() == Storage_Static);

  DASSERT_OBJ(var->passes().isFinished(VariableDefn::InitializerPass), var);

  GlobalVariable * gv = irModule_->getGlobalVariable(var->linkageName());
  if (gv != NULL) {
    return gv;
  }

  const Type * varType = var->type().unqualified();
  DASSERT(varType != NULL);

  // Create the global variable
  GlobalValue::LinkageTypes linkType = Function::ExternalLinkage;
  if (var->isSynthetic()) {
    linkType = Function::LinkOnceAnyLinkage;
  }

  // The reason that this is irType instead of irEmbeddedType is because LLVM always turns
  // the type of a global variable into a pointer anyway.
  llvm::Type * irType = varType->irEmbeddedType();
  gv = new GlobalVariable(*irModule_, irType, false, linkType, NULL, var->linkageName(),
      NULL, var->isThreadLocal());

  // Only supply an initialization expression if the variable was
  // defined in this module - otherwise, it's an external declaration.
  if (var->module() == module_ || var->isSynthetic()) {
    addStaticRoot(gv, varType);
    if (debug_) {
      genDIGlobalVariable(var, gv);
    }

    // If it has an initialization expression
    const Expr * initExpr = var->initValue();
    if (initExpr != NULL) {
      if (initExpr->isConstant()) {
        Constant * initValue = genConstExpr(initExpr);
        if (initValue == NULL) {
          return NULL;
        }

        if (varType->isReferenceType()) {
          initValue = new GlobalVariable(
              *irModule_, initValue->getType(), false, linkType, initValue,
              var->linkageName() + ".init");
          initValue = llvm::ConstantExpr::getPointerCast(initValue, varType->irEmbeddedType());
        }

        gv->setInitializer(initValue);
      } else {
        genModuleInitFunc();

        // Add it to the module init function
        BasicBlock * savePoint = builder_.GetInsertBlock();
        builder_.SetInsertPoint(moduleInitBlock_);

        // Generate the expression.
        Value * initValue = genExpr(initExpr);
        if (initValue != NULL) {
          gv->setInitializer(llvm::Constant::getNullValue(irType));
          builder_.CreateStore(initValue, gv);
        }

        if (savePoint != NULL) {
          builder_.SetInsertPoint(savePoint);
        }
      }
    } else if (!var->isExtern()) {
      // No initializer, so set the value to zerofill.
      gv->setInitializer(llvm::Constant::getNullValue(irType));
    }
  }

  return gv;
}

} // namespace tart

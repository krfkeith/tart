/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/FunctionRegion.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/PrimitiveType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/Attributes.h"
#include "llvm/Module.h"
#include "llvm/Function.h"
#include "llvm/Analysis/Verifier.h"

namespace tart {

extern SystemNamespaceMember<FunctionDefn> gc_allocContext;

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

Function * CodeGenerator::genFunctionValue(const FunctionDefn * fdef) {
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

  // If it's a function from a different module...
  if (fdef->module() != module_) {
    const FunctionType * funcType = fdef->functionType();
    fn = Function::Create(
        cast<llvm::FunctionType>(funcType->irType()),
        Function::ExternalLinkage, fdef->linkageName(),
        irModule_);
    return fn;
  }

  // Generate the function reference
  const FunctionType * funcType = fdef->functionType();
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
  DASSERT_OBJ(fdef->type() != NULL, fdef);
  DASSERT_OBJ(fdef->type()->isSingular(), fdef);

  // Don't generate intrinsic functions.
  if (fdef->isIntrinsic()) {
    return true;
  }

  // Don't generate a function if it has been merged to another function
  if (fdef->mergeTo() != NULL) {
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
      f->setGC("tart-gc");
    }

    if (debug_) {
      dbgContext_ = genDISubprogram(fdef);
      setDebugLocation(fdef->location().forRegion(fdef->region()));
      functionRegion_ = fdef->region();
    }

    BasicBlock * prologue = BasicBlock::Create(context_, "prologue", f);

    // Create the LLVM Basic Blocks corresponding to each high level BB.
    BlockList & blocks = fdef->blocks();
    for (BlockList::iterator b = blocks.begin(); b != blocks.end(); ++b) {
      Block * blk = *b;
      blk->setIRBlock(BasicBlock::Create(context_, blk->label(), f));
    }

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
      DASSERT_OBJ(fdef->storageClass() == Storage_Instance ||
          fdef->storageClass() == Storage_Local, fdef);
      DASSERT_OBJ(it != f->arg_end(), ftype);
      ftype->selfParam()->setIRValue(it);
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
      Value * argVal = it;

      // Set the name of the Nth parameter
      ParameterDefn * param = ftype->params()[param_index];
      DASSERT_OBJ(param != NULL, fdef);
      DASSERT_OBJ(param->storageClass() == Storage_Local, param);
      const Type * paramType = param->internalType();
      it->setName(param->name());
      Value * paramValue = it;

      // See if we need to make a local copy of the param.
      if (param->isSharedRef()) {
        genLocalVar(param, paramValue);
      } else if (param->isLValue()) {
        Value * localValue = builder_.CreateAlloca(paramType->irEmbeddedType(), 0, param->name());
        param->setIRValue(localValue);

        if (paramType->typeShape() == Shape_Large_Value) {
          paramValue = builder_.CreateLoad(paramValue);
        }

        builder_.CreateStore(paramValue, localValue);
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

      builder_.SetInsertPoint(blocks.front()->irBlock());
      genBlocks(fdef->blocks());

      gcAllocContext_ = NULL;
#if 0
    }
#endif

    builder_.SetInsertPoint(prologue);
    builder_.CreateBr(blocks.front()->irBlock());

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

    dbgContext_ = DISubprogram();
    functionRegion_ = NULL;
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
  DASSERT(let->type() != NULL);
  const llvm::Type * irType = let->type()->irEmbeddedType();
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
        value = new GlobalVariable(
            *irModule_, irType, true, GlobalValue::ExternalLinkage, NULL, let->linkageName());
        DASSERT_TYPE_EQ(let->initValue(), irType, value->getType()->getContainedType(0));
      }
    } else {
      value = genExpr(let->initValue());
      DASSERT_TYPE_EQ(let->initValue(), irType, value->getType());
    }

    if (value == NULL) {
      return false;
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

  const Type * varType = var->type();
  DASSERT(varType != NULL);

  // Create the global variable
  GlobalValue::LinkageTypes linkType = Function::ExternalLinkage;
  if (var->isSynthetic()) {
    linkType = Function::LinkOnceAnyLinkage;
  }

  // The reason that this is irType instead of irEmbeddedType is because LLVM always turns
  // the type of a global variable into a pointer anyway.
  const llvm::Type * irType = varType->irEmbeddedType();
  gv = new GlobalVariable(*irModule_, irType, false, linkType, NULL, var->linkageName(),
      NULL, var->isThreadLocal());

  // Only supply an initialization expression if the variable was
  // defined in this module - otherwise, it's an external declaration.
  if (var->module() == module_ || var->isSynthetic()) {
    addStaticRoot(gv, var->type());
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

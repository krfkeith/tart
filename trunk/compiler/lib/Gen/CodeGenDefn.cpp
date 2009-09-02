/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/PrimitiveType.h"

#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/Analysis/Verifier.h>

namespace tart {

using namespace llvm;

bool CodeGenerator::genXDef(Defn * de) {
  switch (de->defnType()) {
    case Defn::Let:
      return genLetDefn(static_cast<VariableDefn *>(de));

    case Defn::Var:
      return genVarValue(static_cast<VariableDefn *>(de));

    case Defn::Function:
      return genFunction(static_cast<FunctionDefn*>(de));

    case Defn::Typedef: {
      TypeDefn * tdef = static_cast<TypeDefn *>(de);
      switch (tdef->typeValue()->typeClass()) {
        case Type::Class:
        case Type::Struct:
        case Type::Interface:
        case Type::Enum:
          return genTypeDefn(static_cast<TypeDefn*>(de));

        case Type::NativePointer:
        case Type::NativeArray:
          return true;

        default:
          diag.fatal(de) << "No generator for " << de;
          return true;
      }
    }

    case Defn::Macro:
    case Defn::Namespace:
      return true;

    default:
      diag.fatal(de) << "No generator for " << de;
      return true;
  }
}

Function * CodeGenerator::genFunctionValue(FunctionDefn * fdef) {
  Function * irFunction = irModule_->getFunction(fdef->linkageName());
  if (irFunction != NULL) {
    return irFunction;
  }

  // If it's a function from a different module...
  if (fdef->module() != module) {
    FunctionType * funcType = fdef->functionType();
    irFunction = Function::Create(
        cast<llvm::FunctionType>(funcType->irType()),
        Function::ExternalLinkage, fdef->linkageName(),
        irModule_);
    return irFunction;
  }

  DASSERT_OBJ(fdef->defnType() != Defn::Macro, fdef);
  DASSERT_OBJ(!fdef->isIntrinsic(), fdef);

#if 0
  // Generate the attributes
  Attributes funcAttrs;
  if (!genAttrs(fdef, funcAttrs)) {
    return NULL;
  }
#endif

  // Generate the function reference
  FunctionType * funcType = fdef->functionType();
  DASSERT_OBJ(funcType->isSingular(), fdef);

  irFunction = Function::Create(
      cast<llvm::FunctionType>(funcType->irType()),
      Function::ExternalLinkage, fdef->linkageName(), fdef->module()->irModule());

  // TODO - Don't store irFunction in the function, as it makes it hard to compile more than
  // one module.
  fdef->setIRFunction(irFunction);
  return irFunction;
}

bool CodeGenerator::genFunction(FunctionDefn * fdef) {
  DASSERT_OBJ(fdef->isSingular(), fdef);
  DASSERT_OBJ(fdef->type() != NULL, fdef);
  DASSERT_OBJ(fdef->type()->isSingular(), fdef);

  // Don't generate intrinsic functions.
  if (fdef->isIntrinsic()) {
    return true;
  }

  // Create the function
  Function * f = genFunctionValue(fdef);

  if (fdef->hasBody() /*&& fdef->module() == module*/) {
    FunctionType * ftype = fdef->functionType();

    if (fdef->isSynthetic()) {
      f->setLinkage(GlobalValue::LinkOnceAnyLinkage);
    }

    if (debug && fdef->module() == module) {
      dbgCompileUnit_ = getCompileUnit(fdef);
      dbgFunction_ = dbgFactory_.CreateSubprogram(
          dbgCompileUnit_, // TODO: Replace for functions within a scope.
          fdef->name(),
          fdef->qualifiedName(),
          fdef->linkageName(),
          dbgCompileUnit_,
          getSourceLineNumber(*fdef),
          DIType(),
          fdef->isSynthetic() /* isLocalToUnit */,
          true /* isDefinition */);
    }

    // Create the LLVM Basic Blocks corresponding to each high level BB.
    BlockList & blocks = fdef->blocks();
    for (BlockList::iterator b = blocks.begin(); b != blocks.end(); ++b) {
      Block * blk = *b;
      blk->setIRBlock(BasicBlock::Create(context_, blk->label(), f));
    }

    //void InsertSubprogramStart(DISubprogram SP, BasicBlock *BB);

    builder_.SetInsertPoint(blocks.front()->irBlock());

    // Handle the explicit parameters
    unsigned param_index = 0;
    Function::arg_iterator it = f->arg_begin();

    // Handle the 'self' parameter
    if (ftype->selfParam() != NULL) {
      DASSERT_OBJ(fdef->storageClass() == Storage_Instance, fdef);
      DASSERT_OBJ(it != f->arg_end(), ftype);
      ftype->selfParam()->setIRValue(it);
      it->setName("self");
      ++it;
    }

    for (; it != f->arg_end(); ++it, ++param_index) {

      // Set the name of the Nth parameter
      ParameterDefn * param = ftype->params()[param_index];
      DASSERT_OBJ(param != NULL, fdef);
      it->setName(param->name());

#if 0
      // See if we need to make a local copy of the param.
      if (param->getFlag(Modified)
        || (!param->getParameterFlag(ParameterDefn::Reference)
          && isAllocValueType(ptypeetType()))) {
        // TODO: For struct parameters, make a copy of whole struct.
        // If parameter was modified, then copy to a local var.
        Value * localValue = builder_.CreateAlloca(it->getType(), 0,
            param->name());
        builder_.CreateStore(it, localValue);
        param->setIRValue(localValue);
      } else {
#endif
        param->setIRValue(it);
#if 0
      }
#endif
    }

    // Generate the body
    FunctionDefn * saveFunction = currentFunction_;
    currentFunction_ = fdef;
#if 0
    if (fdef->isGenerator()) {
      assert(false);
    } else {
#endif
      genLocalStorage();
      genBlocks();
#if 0
    }
#endif

    currentFunction_ = saveFunction;

    if (!diag.inRecovery()) {
      if (llvm::verifyFunction(*f)) {
        f->dump();
      }
    }

    builder_.ClearInsertionPoint();

#if 0
    if (debug) {
      debugBuilder.SetContext(saveDescriptor);
    }
#endif
  }

  return true;
}


//llvm::Function * CodeGenerator::genFunctionValue(FunctionDefn * fdef) {

Value * CodeGenerator::genLetValue(const VariableDefn * let) {
  // Don't generate the IR if we've already done so
  if (let->irValue() != NULL) {
    return let->irValue();
  }

#if 0
  // Generate the attributes
  Attributes letAttrs;
  if (!genAttrs(letDef, letAttrs)) {
    return NULL;
  }
#endif

  // Calculate the type.
  const Type * letType = let->type();
  DASSERT(letType != NULL);
  const llvm::Type * irType = letType->irEmbeddedType();

  // Generate the value
  Value * value = NULL;
  if (let->initValue() != NULL) {
    value = genExpr(let->initValue());
    if (value == NULL) {
      return false;
    }
  }

  Value * letValue = NULL;
  if (let->storageClass() == Storage_Local) {
    // If it's a local variable, then use the value directly.
    letValue = value;
  } else if (llvm::Constant * constantValue = dyn_cast<llvm::Constant>(value)) {
    // See if it's a constant.
    letValue = constantValue;
  } else {
    assert(false && "let value not a constant");
  }

  let->setIRValue(letValue);
  return letValue;
}

bool CodeGenerator::genLetDefn(VariableDefn * let) {
  return genLetValue(let) != NULL;
}

Value * CodeGenerator::genVarValue(const VariableDefn * var) {
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

Value * CodeGenerator::genGlobalVar(const VariableDefn * var) {
  DASSERT(var->storageClass() == Storage_Global || var->storageClass() == Storage_Static);

  GlobalVariable * externalVar = irModule_->getGlobalVariable(var->linkageName());
  if (externalVar != NULL) {
    return externalVar;
  }

  // Global variables never set the IRValue field, because that field has a different value
  // dependig on what module we are compiling.
  DASSERT(var->irValue() == NULL);

#if 0
  // Generate the attributes
  Attributes varAttrs;
  if (!genAttrs(var, varAttrs)) {
    return NULL;
  }
#endif

  const Type * varType = var->type();
  DASSERT(varType != NULL);
  const llvm::Type * irType = varType->irEmbeddedType();

  // Create the global variable
  GlobalValue::LinkageTypes linkType = Function::ExternalLinkage;
  if (var->isSynthetic()) {
    linkType = Function::LinkOnceAnyLinkage;
  }

  bool threadLocal = false;
#if 0
  bool threadLocal = var->findAttribute(Builtins::typeThreadLocalAttribute) != NULL;
  if (threadLocal && var->storageClass() != Storage_Global &&
      var->storageClass() != Storage_Static) {
    diag.fatal(var->location()) <<  "Only global or static variables can be thread-local";
  }
#endif

  GlobalVariable * gv = new GlobalVariable(*irModule_, irType, true, linkType, NULL,
      var->linkageName());

  // Only supply an initialization expression if the variable was
  // defined in this module - otherwise, it's an external declaration.
  if (var->module() == module) {
    // If it has an initialization expression
    const Expr * initExpr = var->initValue();
    if (initExpr != NULL) {
      if (initExpr->isConstant()) {
        Constant * initValue = genConstExpr(initExpr);
        if (initValue == NULL) {
          return false;
        }

        gv->setInitializer(initValue);
      } else {
        DFAIL("Implement");
#if 0
        genModuleInitFunc();

        // Add it to the module init function
        BasicBlock * savePoint = builder_.GetInsertBlock();
        builder_.SetInsertPoint(moduleInitBlock);

        // Generate the expression.
        Value * initValue = genExpr(initExpr);
        if (initValue == NULL) {
          return false;
        }

        // Otherwise use dynamic initialization
        gv->setInitializer(llvm::Constant::getNullValue(irType));
        builder_.CreateStore(initValue, gv);

        if (savePoint != NULL) {
          builder_.SetInsertPoint(savePoint);
        }
#endif
      }
    } else {
      // No initializer, so set the value to zerofill.
      gv->setInitializer(llvm::Constant::getNullValue(irType));
    }
  }

  return gv;
}

} // namespace tart


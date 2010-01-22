/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Gen/StructBuilder.h"
#include "tart/Gen/RuntimeTypeInfo.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/Objects/Builtins.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Analysis/Verifier.h"

namespace tart {

using namespace llvm;

extern SystemClassMember<VariableDefn> functionType_invoke;
extern SystemClassMember<FunctionDefn> functionType_invokeFn;
extern SystemClassMember<VariableDefn> functionType_dcObject;
extern SystemClassMember<FunctionDefn> functionType_checkArgs;

// Members of tart.core.TypeInfoBlock.
SystemClassMember<VariableDefn> tib_name(Builtins::typeTypeInfoBlock, "name");
SystemClassMember<VariableDefn> tib_bases(Builtins::typeTypeInfoBlock, "bases");
SystemClassMember<VariableDefn> tib_idispatch(Builtins::typeTypeInfoBlock, "idispatch");

const llvm::Type * CodeGenerator::genTypeDefn(TypeDefn * tdef) {
  DASSERT_OBJ(tdef->isSingular(), tdef);
  Type * type = tdef->typeValue();
  switch (type->typeClass()) {
    case Type::Primitive:
      return genPrimitiveType(static_cast<PrimitiveType *>(type));

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
      return genCompositeType(static_cast<CompositeType *>(type));

    case Type::Enum:
      return genEnumType(static_cast<EnumType *>(type));

    //case Defn::NArray:
    //case Defn::NativePointer:

    case Type::Alias:
      // No need to generate this.
      return NULL;

    case Type::Protocol:
      // Protocols have no runtime representation.
      return NULL;

    default:
      diag.debug() << type;
      DFAIL("Invalid type defn");
      break;
  }
}

const llvm::Type * CodeGenerator::genPrimitiveType(PrimitiveType * type) {
  DASSERT_OBJ(type->irType() != NULL, type);
  return type->irType();
}

const llvm::Type * CodeGenerator::genCompositeType(const CompositeType * type) {
  if (type->isAttribute() && !type->attributeInfo().isRetained()) {
    return NULL;
  }

  TypeDefn * tdef = type->typeDefn();
  RuntimeTypeInfo * rtype = getRTTypeInfo(type);

  if (irModule_->getTypeName(type->irType()).empty() && type->typeClass() != Type::Interface) {
    irModule_->addTypeName(tdef->linkageName(), type->irType());
  }

  // Don't need to define this twice.
  if (rtype->isExternal() ||
      (rtype->getTypeInfoBlock() != NULL && rtype->getTypeInfoBlock()->hasInitializer())) {
    return type->irType();
  }

  DASSERT_OBJ(type->isSingular(), type);
  DASSERT_OBJ(type->passes().isFinished(CompositeType::BaseTypesPass), type);
  DASSERT_OBJ(type->passes().isFinished(CompositeType::FieldPass), type);
  DASSERT_OBJ(type->passes().isFinished(CompositeType::OverloadingPass), type);
  DASSERT_OBJ(type->irType() != NULL, type);

  createTypeInfoBlock(rtype);
  createTypeAllocator(rtype);
  return type->irType();
}

Constant * CodeGenerator::getTypeInfoBlockPtr(const CompositeType * type) {
  if (type->typeDefn()->isSynthetic() &&
      module_->exportDefs().count(type->typeDefn()) == 0) {
    diag.fatal() << "Attempting to use TIB of synthetic type " << type <<
        " but it has not been imported into the module.";
  }

  return createTypeInfoBlockPtr(getRTTypeInfo(type));
}

Function * CodeGenerator::getTypeAllocator(const CompositeType * type) {
  return createTypeAllocator(getRTTypeInfo(type));
}

Constant * CodeGenerator::createTypeInfoBlockPtr(RuntimeTypeInfo * rtype) {
  if (rtype->getTypeInfoPtr() == NULL) {
    // Create the global variable for the type info block.
    const CompositeType * type = rtype->getType();
    if (type->typeClass() != Type::Class && type->typeClass() != Type::Interface) {
      rtype->setTypeInfoPtr(ConstantPointerNull::get(
          llvm::PointerType::getUnqual(Builtins::typeTypeInfoBlock.irType())));
    } else {
      DASSERT(rtype->getTypeInfoBlock() == NULL);
      rtype->setTypeInfoBlock(
          new GlobalVariable(*irModule_,
              rtype->getTypeInfoBlockType().get(),
              true, GlobalValue::ExternalLinkage, NULL,
              type->typeDefn()->linkageName() + ".type.tib"));
      DASSERT(rtype->getTypeInfoPtr() == NULL);
      rtype->setTypeInfoPtr(
          llvm::ConstantExpr::getBitCast(
              rtype->getTypeInfoBlock(),
              llvm::PointerType::get(Builtins::typeTypeInfoBlock.irType(), 0)));
    }
  }

  return rtype->getTypeInfoPtr();
}

bool CodeGenerator::createTypeInfoBlock(RuntimeTypeInfo * rtype) {
  const CompositeType * type = rtype->getType();

  if (type->typeClass() != Type::Class && type->typeClass() != Type::Interface) {
    return true;
  }

  if (rtype->getTypeInfoPtr() == NULL) {
    createTypeInfoBlockPtr(rtype);
  } else if (rtype->getTypeInfoBlock()->hasInitializer()) {
    return true;
  }

  llvm::PointerType * typePointerType =
      llvm::PointerType::getUnqual(Builtins::typeTypeInfoBlock.irType());

  // Generate the base class list.
  ConstantList baseClassList;
  ClassSet baseClassSet;
  type->ancestorClasses(baseClassSet);

  // Concrete classes first
  for (const CompositeType * s = type->super(); s != NULL; s = s->super()) {
    // If the class lives in this module, then create it.
    genCompositeType(s);
    baseClassList.push_back(getTypeInfoBlockPtr(s));
  }

  // Interfaces next
  for (ClassSet::iterator it = baseClassSet.begin(); it != baseClassSet.end(); ++it) {
    CompositeType * baseType = *it;
    if (baseType->typeClass() == Type::Interface) {
      genCompositeType(baseType);
      baseClassList.push_back(getTypeInfoBlockPtr(baseType));
    }
  }

  // Null pointer at end
  baseClassList.push_back(ConstantPointerNull::get(typePointerType));

  Constant * baseClassArray = ConstantArray::get(
      ArrayType::get(typePointerType, baseClassList.size()),
      baseClassList);
  GlobalVariable * baseClassArrayPtr = new GlobalVariable(*irModule_,
    baseClassArray->getType(), true, GlobalValue::InternalLinkage,
    baseClassArray, type->typeDefn()->linkageName() + ".type.tib.bases");

  // Generate the interface dispatch function
  Function * idispatch = NULL;
  if (type->typeClass() == Type::Class) {
    idispatch = genInterfaceDispatchFunc(type);
    DASSERT(idispatch != NULL);
  }

  // Create the TypeInfoBlock struct
  StructBuilder builder(*this);
  //ConstantList tibMembers;
  //tibMembers.push_back(llvm::ConstantExpr::getPointerCast(
  //    reflector_.getTypePtr(type), Builtins::typeType->irEmbeddedType()));
  builder.addField(reflector_.internSymbol(type->typeDefn()->linkageName()));
  builder.addField(baseClassArrayPtr);
  if (type->typeClass() == Type::Class) {
    builder.addField(idispatch);
    builder.addField(genMethodArray(type->instanceMethods_));
  } else {
    builder.addNullField(tib_idispatch.type());
    builder.addField(genMethodArray(MethodList()));
  }

  // ConstantStruct::get(context_, tibMembers, false);
  Constant * tibStruct = builder.build();

  // Assign the TIB value to the tib global variable.
  GlobalVariable * tibPtr = rtype->getTypeInfoBlock();
  cast<OpaqueType>(rtype->getTypeInfoBlockType().get())->refineAbstractTypeTo(tibStruct->getType());
  tibPtr->setInitializer(tibStruct);
  tibPtr->setLinkage(rtype->getLinkageType());
  return true;
}

Constant * CodeGenerator::genMethodArray(const MethodList & methods) {
  ConstantList methodValues;
  for (MethodList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
    FunctionDefn * method = static_cast<FunctionDefn *>(*it);
    if (!method->isSingular()) {
      diag.fatal(method) << "Non-singular method '" << method->name() << "' in closed type";
    }

    Constant * methodVal;
    if (!method->hasBody()) {
      if (method->isExtern()) {
        DASSERT_OBJ(method->isSingular(), method);
        methodVal = genFunctionValue(method);
      } else {
        // TODO: Replace this with call to throw an exception.
        methodVal = ConstantPointerNull::get(methodPtrType_);
      }
    } else {
      DASSERT_OBJ(method->isSingular(), method);
      if (method->isSynthetic() && module_->exportDefs().count(method) == 0) {
        diag.fatal() << Format_Verbose << "Attempting to refer to synthetic method " <<
            method << " but it has not been imported into the module.";
      }

      methodVal = genFunctionValue(method);
    }

    methodValues.push_back(
        llvm::ConstantExpr::getBitCast(methodVal, methodPtrType_));
  }

  return ConstantArray::get(ArrayType::get(methodPtrType_, methodValues.size()), methodValues);
}

Function * CodeGenerator::genInterfaceDispatchFunc(const CompositeType * type) {

  DASSERT_OBJ(type->typeClass() == Type::Class, type);

  // Create the dispatch function declaration
  std::vector<const llvm::Type *> argTypes;
  argTypes.push_back(llvm::PointerType::get(Builtins::typeString.irType(), 0));
  argTypes.push_back(builder_.getInt32Ty());
  llvm::FunctionType * functype = llvm::FunctionType::get(methodPtrType_, argTypes, false);
  DASSERT(dbgContext_.isNull());
  DASSERT(builder_.getCurrentDebugLocation() == NULL);
  Function * idispatch = Function::Create(
      functype, Function::InternalLinkage,
      type->typeDefn()->linkageName() + ".type.idispatch",
      irModule_);

  // Generate the interface dispatcher body
  const std::string & linkageName = type->typeDefn()->linkageName();
  BasicBlock * savePoint = builder_.GetInsertBlock();

  Function::ArgumentListType::iterator arg = idispatch->getArgumentList().begin();
  Argument * iid = arg++;
  Argument * methodIndex = arg++;
  iid->setName("iid");
  methodIndex->setName("methodIndex");

  ValueList indices;
  indices.push_back(getInt32Val(0));
  indices.push_back(methodIndex);

  BasicBlock * blk = BasicBlock::Create(context_, "first", idispatch);
  for (CompositeType::InterfaceList::const_iterator it =
      type->interfaces_.begin(); it != type->interfaces_.end(); ++it) {

    // Generate the method table for the interface.
    CompositeType * itDecl = it->interfaceType;
    Constant * itableMembers = genMethodArray(it->methods);
    GlobalVariable * itable = new GlobalVariable(*irModule_,
      itableMembers->getType(), true, GlobalValue::InternalLinkage,
      itableMembers,
      itDecl->typeDefn()->linkageName() + "->" + linkageName);
    Constant * iname = reflector_.internSymbol(itDecl->typeDefn()->linkageName());

    // Create the blocks
    BasicBlock * ret = BasicBlock::Create(context_, itDecl->typeDefn()->name(), idispatch);
    BasicBlock * next = BasicBlock::Create(context_, "next", idispatch);

    // Test the interface pointer
    builder_.SetInsertPoint(blk);
    Value * testVal = builder_.CreateICmp(ICmpInst::ICMP_EQ, iid, iname);
    builder_.CreateCondBr(testVal, ret, next);

    // Return the specified method
    builder_.SetInsertPoint(ret);
    Value * method = builder_.CreateLoad(builder_.CreateInBoundsGEP(itable,
        indices.begin(), indices.end()), "method");
    builder_.CreateRet(method);

    blk = next;
  }

  builder_.SetInsertPoint(blk);

  // Throw a typecast exception
  Function * typecastFailure = genFunctionValue(Builtins::funcTypecastError);
  typecastFailure->setDoesNotReturn(true);
  builder_.CreateCall(typecastFailure);
  //builder_.CreateUnreachable();
  builder_.CreateRet(ConstantPointerNull::get(methodPtrType_));

  if (savePoint != NULL) {
    builder_.SetInsertPoint(savePoint);
  }

  return idispatch;
}

Function * CodeGenerator::createTypeAllocator(RuntimeTypeInfo * rtype) {
  const CompositeType * type = rtype->getType();
  if (rtype->getTypeAllocator() == NULL && type->typeClass() == Type::Class) {

    // Declare the allocator function
    llvm::FunctionType * alloctype = llvm::FunctionType::get(
        llvm::PointerType::getUnqual(type->irType()), false);
    Function * allocFunc = Function::Create(
        alloctype, rtype->getLinkageType(),
        type->typeDefn()->linkageName() + ".type.alloc", irModule_);

    if (!rtype->isExternal()) {
      // Save the builder_ state and set to the new allocator function.
      DIScope saveContext = dbgContext_;
      dbgContext_ = DIScope();
      BasicBlock * savePoint = builder_.GetInsertBlock();
      builder_.SetInsertPoint(BasicBlock::Create(context_, "alloc_entry", allocFunc));

      // Allocate an instance of the object
      Value * instance = builder_.CreatePointerCast(
          builder_.CreateCall(
              getGlobalAlloc(),
              llvm::ConstantExpr::getSizeOf(type->irType()),
              "new"),
          llvm::PointerType::get(type->irType(), 0));

      // Generate code to fill in vtable pointer of new object.
      genInitObjVTable(type, instance);
      builder_.CreateRet(instance);

      // Restore the builder_ state.
      dbgContext_ = saveContext;
      if (savePoint != NULL) {
        builder_.SetInsertPoint(savePoint);
      }
    }

    rtype->setTypeAllocator(allocFunc);
  }

  return rtype->getTypeAllocator();
}

void CodeGenerator::genInitObjVTable(const CompositeType * type, Value * instance) {
  ValueList indices;
  indices.push_back(getInt32Val(0));
  const CompositeType * base = type;

  while (base != NULL) {
    // First member of this type.
    indices.push_back(getInt32Val(0));
    base = base->super();
  }

  Value * vtablePtrPtr = builder_.CreateInBoundsGEP(instance, indices.begin(), indices.end());
  Value * typeInfoPtr = getTypeInfoBlockPtr(type);
  builder_.CreateStore(typeInfoPtr, vtablePtrPtr);
}

RuntimeTypeInfo * CodeGenerator::getRTTypeInfo(const CompositeType * type) {
  RTTypeMap::iterator it = compositeTypeMap_.find(type);
  if (it != compositeTypeMap_.end()) {
    return it->second;
  }

  RuntimeTypeInfo * rtype = new RuntimeTypeInfo(type, module_);
  compositeTypeMap_[type] = rtype;
  return rtype;
}

const llvm::Type * CodeGenerator::genEnumType(EnumType * type) {
  // TODO: Implement valueOf
  DefnList enumConstants;
  for (Defn * de = type->memberScope()->firstMember(); de != NULL; de = de->nextInScope()) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
      enumConstants.push_back(var);
    }
  }

  FunctionDefn * toString =
      cast_or_null<FunctionDefn>(type->memberScope()->lookupSingleMember("toString"));

  if (type->isFlags()) {
  } else if (!enumConstants.empty()) {
    VariableDefn * minVal = cast<VariableDefn>(type->memberScope()->lookupSingleMember("minVal"));
    VariableDefn * maxVal = cast<VariableDefn>(type->memberScope()->lookupSingleMember("maxVal"));
    APInt minValInt = cast<ConstantInteger>(minVal->initValue())->intValue();
    APInt maxValInt = cast<ConstantInteger>(maxVal->initValue())->intValue();

    // TODO: What if the spread is greater than 2^63?
    int64_t spread = (maxValInt - minValInt).getSExtValue();
    DASSERT(spread >= 0);
    if ((spread < 16 || spread < int64_t(enumConstants.size()) * 2) && spread < 0x10000) {
      ConstantList enumConstants;
      enumConstants.resize(spread + 1);
      std::fill(enumConstants.begin(), enumConstants.end(), (llvm::Constant *) NULL);
      for (Defn * de = type->memberScope()->firstMember(); de != NULL; de = de->nextInScope()) {
        if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
          if (var != minVal && var != maxVal) {
            ConstantInteger * valInt = cast<ConstantInteger>(var->initValue());
            int64_t offset = (valInt->intValue() - minValInt).getSExtValue();
            DASSERT(offset >= 0);
            DASSERT(offset < int64_t(enumConstants.size()));
            if (enumConstants[offset] == NULL) {
              enumConstants[offset] = reflector_.internSymbol(var->name());
            }
          }
        }
      }

      for (ConstantList::iterator it = enumConstants.begin(); it != enumConstants.end(); ++it) {
        if (*it == NULL) {
          *it = ConstantPointerNull::getNullValue(Builtins::typeString->irEmbeddedType());
        }
      }

      // Create the table of strings.
      Constant * stringArray = llvm::ConstantArray::get(
          llvm::ArrayType::get(Builtins::typeString->irEmbeddedType(), enumConstants.size()),
          &*enumConstants.begin(), enumConstants.size());
      GlobalVariable * stringTable = new GlobalVariable(*irModule_,
          stringArray->getType(), true, GlobalValue::InternalLinkage, stringArray,
          ".names." + type->typeDefn()->linkageName());

      // Now create the toString function.
      Function * toStringFn = cast<Function>(irModule_->getOrInsertFunction(
          toString->linkageName(),
          cast<llvm::FunctionType>(toString->functionType()->irType())));
      DASSERT(toStringFn->arg_size() == 1);
      llvm::Argument * selfArg = toStringFn->arg_begin();
      selfArg->setName("self");
      const IntegerType * selfType = cast<IntegerType>(selfArg->getType());
      unsigned bitWidth = selfType->getBitWidth();
      DASSERT(minValInt.getMinSignedBits() <= bitWidth);

      BasicBlock * blk = BasicBlock::Create(context_, "ts_entry", toStringFn);
      DASSERT(currentFn_ == NULL);
      currentFn_ = toStringFn;
      builder_.SetInsertPoint(blk);

      // Add the enum value to the minVal offset.
      Constant * baseVal = llvm::ConstantExpr::getIntegerCast(
          ConstantInt::get(context_, minValInt), selfType, true);
      Value * index = builder_.CreateSub(toStringFn->arg_begin(), baseVal);

      // Check if it's in range - unsigned less than or equal to 'spread'.
      Value * rangeCheck = builder_.CreateICmpULE(index, ConstantInt::get(selfType, spread));

      // Create a GEP into the string table and load it.
      ValueList args;
      args.push_back(getInt32Val(0));
      args.push_back(index);
      builder_.CreateRet(
          builder_.CreateLoad(
              builder_.CreateInBoundsGEP(stringTable, args.begin(), args.end()), "stringVal"));
      currentFn_ = NULL;

      // Verify the function
      /*if (verifyFunction(*toStringFn, PrintMessageAction)) {
        toStringFn->dump();
        exit(-1);
      }*/
    } else {
      DFAIL("Implement sparse enum tables");
    }
  }

  return type->irType();
}

llvm::Function * CodeGenerator::genInvokeFn(const FunctionType * fnType) {
  const std::string & invokeName = fnType->invokeName();
  llvm::Function * invokeFn = irModule_->getFunction(invokeName);
  if (invokeFn != NULL) {
    return invokeFn;
  }

  const llvm::FunctionType * invokeFnType = cast<llvm::FunctionType>(
      functionType_invoke.type()->irType());
    DASSERT(dbgContext_.isNull());
  invokeFn = Function::Create(invokeFnType, Function::LinkOnceODRLinkage, invokeName, irModule_);
  BasicBlock * blk = BasicBlock::Create(context_, "invoke_entry", invokeFn);
  DASSERT(currentFn_ == NULL);
  currentFn_ = invokeFn;
  builder_.SetInsertPoint(blk);

  DASSERT(invokeFn->arg_size() == 3);
  Function::arg_iterator it = invokeFn->arg_begin();
  Value * fnPtr = it++;
  Value * objPtr = it++;
  Value * argsArray = it++;

  // Check the length of the args array.
  size_t numParams = fnType->params().size();
  llvm::Function * checkArgsFn = genFunctionValue(functionType_checkArgs.get());
  builder_.CreateCall2(checkArgsFn, argsArray, getInt32Val(numParams));

  ValueList args;

  // Handle struct return argument if present.
  Value * sret = NULL;
  fnType->irType(); // Make sure irType is generated, because it sets structReturn property.
  if (fnType->isStructReturn()) {
    sret = builder_.CreateAlloca(fnType->returnType()->irType());
    args.push_back(sret);
  }

  // Handle self argument if present.
  const llvm::FunctionType * callType;
  if (fnType->selfParam() != NULL && !fnType->isStatic()) {
    // Push the 'self' argument.
    callType = fnType->createIRFunctionType(
        Builtins::typeObject, fnType->params(), fnType->returnType());
    args.push_back(builder_.CreatePointerCast(
        objPtr,
        callType->getParamType(sret ? 1 : 0)));
  } else {
    // Use the real function type.
    callType = cast<llvm::FunctionType>(fnType->irType());
  }

  // Typecast all of the arguments.
  for (size_t i = 0; i < numParams; ++i) {
    const Type * paramType = fnType->param(i)->internalType();
    Value * indices[3];
    indices[0] = getInt32Val(0);
    indices[1] = getInt32Val(2);
    indices[2] = getInt32Val(i);
    Value * argAddr = builder_.CreateInBoundsGEP(argsArray, &indices[0], &indices[3]);
    Value * argVal = argAddr;
    ensureLValue(NULL, argAddr->getType());
    argVal = builder_.CreateLoad(argAddr);
    argVal = genCast(argVal, Builtins::typeObject, paramType);
    if (argVal == NULL) {
      currentFn_ = NULL;
      return NULL;
    }

    args.push_back(argVal);
  }

  fnPtr = builder_.CreatePointerCast(fnPtr, llvm::PointerType::get(callType, 0));
  checkCallingArgs(fnPtr, args.begin(), args.end());
  Value * returnVal = builder_.CreateCall(fnPtr, args.begin(), args.end() /*, "invoke"*/);

  if (!fnType->returnType()->isVoidType()) {
    const Type * returnType = fnType->returnType();
    if (sret != NULL) {
      returnVal = sret;
    }

    returnVal = genCast(returnVal, returnType, Builtins::typeObject);
    if (returnVal == NULL) {
      currentFn_ = NULL;
      return NULL;
    }

    builder_.CreateRet(returnVal);
  } else {
    //invokeFnType->dump();
    builder_.CreateRet(ConstantPointerNull::get(
        cast<llvm::PointerType>(invokeFnType->getReturnType())));
  }

  currentFn_ = NULL;
  return invokeFn;
}

llvm::Function * CodeGenerator::genDcObjectFn(const Type * objType) {
  std::string dcObjectName = ".downcast.";
  typeLinkageName(dcObjectName, objType);
  llvm::Function * dcObjectFn = irModule_->getFunction(dcObjectName);
  if (dcObjectFn != NULL) {
    return dcObjectFn;
  }

  DASSERT(currentFn_ == NULL);

  //size_t numParams = fnType->params().size();
  const llvm::FunctionType * dcObjectFnType = getDcObjectFnType();
  DASSERT(dbgContext_.isNull());
  dcObjectFn = Function::Create(
      dcObjectFnType, Function::LinkOnceODRLinkage, dcObjectName, irModule_);
  BasicBlock * blk = BasicBlock::Create(context_, "dc_entry", dcObjectFn);
  currentFn_ = dcObjectFn;
  builder_.SetInsertPoint(blk);

  Function::arg_iterator it = dcObjectFn->arg_begin();
  Value * result = genCast(it, Builtins::typeObject, objType);
  if (result == NULL) {
    currentFn_ = NULL;
    return NULL;
  }

  Value * returnVal = builder_.CreatePointerCast(
      result, cast<llvm::PointerType>(dcObjectFnType->getReturnType()));
  builder_.CreateRet(returnVal);
  currentFn_ = NULL;
  return cast<Function>(llvm::ConstantExpr::getPointerCast(
      dcObjectFn, llvm::PointerType::get(dcObjectFnType_, 0)));
}

llvm::FunctionType * CodeGenerator::getDcObjectFnType() {
  if (dcObjectFnType_ == NULL) {
    const Type * dcObjectType = functionType_dcObject.type();
    const FunctionType * fnType = cast<FunctionType>(dcObjectType);

    // Types of the function parameters.
    std::vector<const llvm::Type *> paramTypes;
    paramTypes.push_back(fnType->params()[0]->type()->irParameterType());

    dcObjectFnType_ = llvm::FunctionType::get(
        fnType->returnType()->irReturnType(), paramTypes, false);
  }

  return dcObjectFnType_;
}

llvm::Function * CodeGenerator::genInterceptFn(const FunctionDefn * fn) {
  std::string interceptName(".intercept.");
  interceptName.append(fn->linkageName());
  llvm::Function * interceptFn = irModule_->getFunction(interceptName);
  if (interceptFn != NULL) {
    return interceptFn;
  }

  const FunctionType * fnType = fn->functionType();
  const llvm::FunctionType * interceptFnType = cast<llvm::FunctionType>(fnType->irType());
  DASSERT(dbgContext_.isNull());
  interceptFn = Function::Create(
      interceptFnType, Function::LinkOnceODRLinkage, interceptName, irModule_);
  BasicBlock * blk = BasicBlock::Create(context_, "invoke_entry", interceptFn);
  DASSERT(currentFn_ == NULL);
  currentFn_ = interceptFn;
  builder_.SetInsertPoint(blk);

  Function::arg_iterator it = interceptFn->arg_begin();
  Value * sret = NULL;
  if (fnType->isStructReturn()) {
    sret = it++;
  }

  Value * selfArg = it++;
  // Need to upcast to object.


  // Function does the following:
  // 1) Creates an array of all params (except the object pointer).
  // 2) Boxes each value and stores it in the array.
  // 3) Gets a pointer to the method object.
  // 4) Gets a pointer to the interceptor instance via the object pointer.
  // 5) Calls the interceptor with object, method and array.
  // 6) Gets the return value, unboxes it, and returns it.

  currentFn_ = NULL;
  return interceptFn;
}

} // namespace tart

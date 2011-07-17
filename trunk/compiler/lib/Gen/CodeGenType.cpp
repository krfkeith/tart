/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Gen/StructBuilder.h"
#include "tart/Gen/RuntimeTypeInfo.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Support/Format.h"

namespace tart {

using namespace llvm;

SystemClassMember<FunctionDefn> functionType_checkArgs(Builtins::typeFunctionType, "checkArgCount");

namespace reflect {
  namespace FunctionType {
    extern SystemClassMember<TypeDefn> CallAdapterFnType;
  }
}

// Members of tart.core.TypeInfoBlock.
SystemClassMember<VariableDefn> tib_type(Builtins::typeTypeInfoBlock, "type");
SystemClassMember<VariableDefn> tib_bases(Builtins::typeTypeInfoBlock, "bases");
SystemClassMember<VariableDefn> tib_traceTable(Builtins::typeTypeInfoBlock, "traceTable");
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
    case Type::Protocol:
      return genCompositeType(static_cast<CompositeType *>(type));

    case Type::Enum:
      return genEnumType(static_cast<EnumType *>(type));

    case Type::Alias:
      // No need to generate this.
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
  if (type->isAttribute() && !type->attributeInfo().isRetained() &&
      type != Builtins::typeAttribute.get()) {
    return NULL;
  }

  TypeDefn * tdef = type->typeDefn();
  RuntimeTypeInfo * rtype = getRTTypeInfo(type);

  if (irModule_->getTypeName(type->irType()).empty() &&
      (type->typeClass() == Type::Class || type->typeClass() == Type::Struct)) {
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
  return type->irType();
}

Constant * CodeGenerator::getTypeInfoBlockPtr(const CompositeType * type) {
  if (type->typeDefn()->isTemplate()) {
    return createTypeInfoBlockPtr(getRTTypeInfo(type));
  } else if (type->typeDefn()->isPartialInstantiation()) {
    return createTypeInfoBlockPtr(getRTTypeInfo(type));
  }

  if (type->typeDefn()->isSynthetic() &&
      module_->exportDefs().count(type->typeDefn()) == 0) {
    diag.fatal() << "Attempting to use TIB of synthetic type " << type <<
        " but it has not been imported into the module.";
  }

  return createTypeInfoBlockPtr(getRTTypeInfo(type));
}

Constant * CodeGenerator::createTypeInfoBlockPtr(RuntimeTypeInfo * rtype) {
  if (rtype->getTypeInfoPtr() == NULL) {
    // Create the global variable for the type info block.
    const CompositeType * type = rtype->getType();
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
            Builtins::typeTypeInfoBlock.irType()->getPointerTo()));
  }

  return rtype->getTypeInfoPtr();
}

bool CodeGenerator::createTypeInfoBlock(RuntimeTypeInfo * rtype) {
  const CompositeType * type = rtype->getType();

  if (rtype->getTypeInfoPtr() == NULL) {
    createTypeInfoBlockPtr(rtype);
  } else if (rtype->getTypeInfoBlock()->hasInitializer()) {
    return true;
  }

  const llvm::PointerType * typePointerType = Builtins::typeTypeInfoBlock.irType()->getPointerTo();

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
      //genCompositeType(baseType);
      baseClassList.push_back(getTypeInfoBlockPtr(baseType));
    }
  }

  // Null pointer at end
  baseClassList.push_back(ConstantPointerNull::get(typePointerType));

  Constant * baseClassArray = ConstantArray::get(
      ArrayType::get(typePointerType, baseClassList.size()),
      baseClassList);
  GlobalVariable * baseClassArrayPtr = new GlobalVariable(*irModule_,
    baseClassArray->getType(), true, GlobalValue::LinkOnceAnyLinkage,
    baseClassArray, type->typeDefn()->linkageName() + ".type.tib.bases");

  // Generate the interface dispatch function
  Function * idispatch = NULL;
  if (type->typeClass() == Type::Class) {
    idispatch = genInterfaceDispatchFunc(type);
    DASSERT(idispatch != NULL);
  }

  // Create the TypeInfoBlock struct
  StructBuilder builder(*this);
  builder.addField(getCompositeTypeObjectPtr(type));

  if (type->typeClass() == Type::Class) {
    llvm::GlobalVariable * traceTable = getTraceTable(type);
    if (traceTable != NULL) {
      builder.addField(traceTable);
    } else {
      builder.addNullField(tib_traceTable.type());
    }
  } else {
    builder.addNullField(tib_traceTable.type());
  }

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

//bool CodeGenerator::createTemplateTypeInfoBlock(const CompositeType * type) {
//  RuntimeTypeInfo * rtype = getRTTypeInfo(type);
//  if (rtype->getTypeInfoPtr() == NULL) {
//    createTypeInfoBlockPtr(rtype);
//  } else if (rtype->getTypeInfoBlock()->hasInitializer()) {
//    return true;
//  }
//
//  // Create the TypeInfoBlock struct
//  StructBuilder builder(*this);
//  if (!type->typeDefn()->isNonreflective() && reflector_.enabled()) {
//    builder.addField(getCompositeTypeObjectPtr(type));
//  } else {
//    builder.addNullField(tib_type.type());
//  }
//  builder.addNullField(tib_traceTable.type());
//  builder.addNullField(tib_bases.type());
//  builder.addNullField(tib_idispatch.type());
//  builder.addField(genMethodArray(MethodList()));
//  Constant * tibStruct = builder.build();
//
//  // Assign the TIB value to the tib global variable.
//  GlobalVariable * tibPtr = rtype->getTypeInfoBlock();
//  cast<OpaqueType>(rtype->getTypeInfoBlockType().get())->refineAbstractTypeTo(tibStruct->getType());
//  tibPtr->setInitializer(tibStruct);
//  tibPtr->setLinkage(rtype->getLinkageType());
//  return true;
//}

Constant * CodeGenerator::genMethodArray(const MethodList & methods) {
  ConstantList methodValues;
  for (MethodList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
    FunctionDefn * method = static_cast<FunctionDefn *>(*it);
    if (method->mergeTo() != NULL) {
      method = method->mergeTo();
    }
    if (!method->isSingular()) {
      diag.fatal(method) << "Non-singular method '" << method->name() << "' in closed type";
    }

    Constant * methodVal;
    if (!method->hasBody()) {
      if (method->isExtern()) {
        DASSERT_OBJ(method->isSingular(), method);
        methodVal = genFunctionValue(method);
      } else if (method->isUndefined()) {
        methodVal = genCallableDefn(method);
      } else if (method->isAbstract()) {
        methodVal = ConstantPointerNull::get(methodPtrType_);
      } else if (method->mdNode() != NULL) {
        methodVal = genFunctionValue(method);
      } else {
        diag.fatal(method) << "Method with no body: " << method;
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
  argTypes.push_back(Builtins::typeString.irType()->getPointerTo());
  argTypes.push_back(builder_.getInt32Ty());
  llvm::FunctionType * functype = llvm::FunctionType::get(methodPtrType_, argTypes, false);
  DASSERT((MDNode *)dbgContext_ == NULL);
  DASSERT(builder_.getCurrentDebugLocation().isUnknown());
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
  TypeDefn * enumDef = type->typeDefn();
  GlobalValue::LinkageTypes linkage = GlobalValue::ExternalLinkage;
  if (enumDef->module() != module_) {
    if (enumDef->isSynthetic()) {
      linkage = GlobalValue::LinkOnceODRLinkage;
    } else {
      return type->irType();
    }
  }

  // TODO: Implement valueOf
  DefnList enumMembers;
  for (Defn * de = type->memberScope()->firstMember(); de != NULL; de = de->nextInScope()) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
      enumMembers.push_back(var);
    }
  }

  FunctionDefn * toString =
      cast_or_null<FunctionDefn>(type->memberScope()->lookupSingleMember("toString"));

  if (type->isFlags()) {
  } else if (!enumMembers.empty()) {
    const Type * baseType = type->baseType();
    VariableDefn * minVal = cast<VariableDefn>(type->memberScope()->lookupSingleMember("minVal"));
    VariableDefn * maxVal = cast<VariableDefn>(type->memberScope()->lookupSingleMember("maxVal"));
    APInt minValInt = cast<ConstantInteger>(minVal->initValue())->intValue();
    APInt maxValInt = cast<ConstantInteger>(maxVal->initValue())->intValue();
    llvm::Constant * nullStrPtr = ConstantPointerNull::getNullValue(
        Builtins::typeString->irEmbeddedType());

    // TODO: What if the spread is greater than 2^63?
#if 1
    int64_t spread = baseType->isUnsignedType() ?
        int64_t((maxValInt - minValInt).getZExtValue()) :
        (maxValInt - minValInt).getSExtValue();
    DASSERT(spread >= 0);
    if ((spread < 16 || spread < int64_t(enumMembers.size()) * 2) && spread < 0x10000) {
      ConstantList enumConstants;
      enumConstants.resize(spread + 1);
      std::fill(enumConstants.begin(), enumConstants.end(), (llvm::Constant *) NULL);
      for (DefnList::const_iterator it = enumMembers.begin(); it != enumMembers.end(); ++it) {
        VariableDefn * var = cast<VariableDefn>(*it);
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

      for (ConstantList::iterator it = enumConstants.begin(); it != enumConstants.end(); ++it) {
        if (*it == NULL) {
          *it = nullStrPtr;
        }
      }

      // Create the table of strings.
      Constant * stringArray = llvm::ConstantArray::get(
          llvm::ArrayType::get(Builtins::typeString->irEmbeddedType(), enumConstants.size()),
          enumConstants);
      GlobalVariable * stringTable = new GlobalVariable(*irModule_,
          stringArray->getType(), true, GlobalValue::InternalLinkage, stringArray,
          ".names." + type->typeDefn()->linkageName());

      // Now create the toString function.
      Function * toStringFn = cast<Function>(irModule_->getOrInsertFunction(
          toString->linkageName(),
          cast<llvm::FunctionType>(toString->functionType()->irType())));
      toStringFn->setLinkage(linkage);
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
      (void)rangeCheck;

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
#endif
      // Create the toString function using a switch statement.
      Function * toStringFn = cast<Function>(irModule_->getOrInsertFunction(
          toString->linkageName(),
          cast<llvm::FunctionType>(toString->functionType()->irType())));
      toStringFn->setLinkage(linkage);
      DASSERT(toStringFn->arg_size() == 1);
      llvm::Argument * selfArg = toStringFn->arg_begin();
      selfArg->setName("self");

      BasicBlock * blk = BasicBlock::Create(context_, "ts_entry", toStringFn);
      BasicBlock * defaultBlk = BasicBlock::Create(context_, "ts_default", toStringFn);

      DASSERT(currentFn_ == NULL);
      currentFn_ = toStringFn;
      builder_.SetInsertPoint(blk);

      SwitchInst * sw = builder_.CreateSwitch(selfArg, defaultBlk, enumMembers.size());

      for (DefnList::const_iterator it = enumMembers.begin(); it != enumMembers.end(); ++it) {
        VariableDefn * var = cast<VariableDefn>(*it);
        if (var != minVal && var != maxVal) {
          ConstantInteger * valInt = cast<ConstantInteger>(var->initValue());
          BasicBlock * blk = BasicBlock::Create(
              context_, StringRef("ts_") + var->name(), toStringFn);
          // TODO: Avoid dups?
          sw->addCase(valInt->value(), blk);

          builder_.SetInsertPoint(blk);
          llvm::Constant * strVal = reflector_.internSymbol(var->name());
          builder_.CreateRet(strVal);
        }
      }

      // If there's no enum constant matching the value, then treat it as integer.
      builder_.SetInsertPoint(defaultBlk);
      DASSERT(!baseType->isUnsizedIntType());
      const PrimitiveType * ptype = cast<PrimitiveType>(baseType);
      const FunctionDefn * pToString = cast<FunctionDefn>(ptype->findSymbol("toString")->front());
      char funcName[32];
      snprintf(funcName, sizeof funcName, "%s_toString", ptype->typeDefn()->name());
      const llvm::FunctionType * funcType = cast<llvm::FunctionType>(pToString->type()->irType());
      llvm::Function * pToStringFn = cast<llvm::Function>(
          irModule_->getOrInsertFunction(funcName, funcType));
      llvm::Value * strVal = builder_.CreateCall(pToStringFn, selfArg);
      builder_.CreateRet(strVal);

      currentFn_ = NULL;

      // Verify the function
      if (verifyFunction(*toStringFn, PrintMessageAction)) {
        toStringFn->dump();
        exit(-1);
      }
#if 1
    }
#endif
  }

  return type->irType();
}

llvm::Value * CodeGenerator::getTypeObjectPtr(const Type * type) {
  if (const CompositeType * ctype = dyn_cast<CompositeType>(type)) {
    llvm::Value * typeObj = getCompositeTypeObjectPtr(ctype);
    return builder_.CreateStructGEP(typeObj, 0);
  } else if (const PrimitiveType * ptype = dyn_cast<PrimitiveType>(type)) {
    llvm::Constant * typeObj = getPrimitiveTypeObjectPtr(ptype);
    return builder_.CreateStructGEP(typeObj, 0);
  }

  // We need a module pointer
  //DASSERT_OBJ(module_->reflectedTypes().count(type), type);
  llvm::Constant * moduleObject = createModuleObjectPtr();
  (void)moduleObject;

  // Need a function call...

  //return cg.genTypeReference(type->value());
  //return cg.createModuleObjectPtr();
  DFAIL("Implement");
}

llvm::Constant * CodeGenerator::getCompositeTypeObjectPtr(const CompositeType * type) {
  return reflector_.getCompositeTypePtr(type);
}

llvm::Constant * CodeGenerator::getPrimitiveTypeObjectPtr(const PrimitiveType * type) {
  std::string typeVarName("tart.reflect.PrimitiveType.");

  switch (type->typeId()) {
    case TypeId_Void: typeVarName += "VOID"; break;
    case TypeId_Null: typeVarName += "NULL"; break;
    case TypeId_Bool: typeVarName += "BOOL"; break;
    case TypeId_Char: typeVarName += "CHAR"; break;
    case TypeId_SInt8: typeVarName += "INT8"; break;
    case TypeId_SInt16: typeVarName += "INT16"; break;
    case TypeId_SInt32: typeVarName += "INT32"; break;
    case TypeId_SInt64: typeVarName += "INT64"; break;
    case TypeId_UInt8: typeVarName += "UINT8"; break;
    case TypeId_UInt16: typeVarName += "UINT16"; break;
    case TypeId_UInt32: typeVarName += "UINT32"; break;
    case TypeId_UInt64: typeVarName += "UINT64"; break;
    case TypeId_Float: typeVarName += "FLOAT"; break;
    case TypeId_Double: typeVarName += "DOUBLE"; break;
    default:
      DFAIL("Illegal state");
      break;
  }

  return irModule_->getOrInsertGlobal(typeVarName, Builtins::typePrimitiveType.get()->irType());
}

const llvm::FunctionType * CodeGenerator::getCallAdapterFnType() {
  if (invokeFnType_ == NULL) {
    const Type * invokeTypeDefn = reflect::FunctionType::CallAdapterFnType.get()->typeValue();
    invokeFnType_ = cast<llvm::FunctionType>(invokeTypeDefn->irType());
  }

  return invokeFnType_;
}

llvm::Function * CodeGenerator::genCallAdapterFn(const FunctionType * fnType) {
  llvm::StringRef invokeName = fnType->invokeName();
  llvm::Function * invokeFn = irModule_->getFunction(invokeName);
  if (invokeFn != NULL) {
    return invokeFn;
  }

  const llvm::FunctionType * invokeFnType = getCallAdapterFnType();
  DASSERT((MDNode *)dbgContext_ == NULL);
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

  fnPtr = builder_.CreatePointerCast(fnPtr, callType->getPointerTo());
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

llvm::Constant * CodeGenerator::genProxyType(const CompositeType * iftype) {
  std::string proxyName(iftype->typeDefn()->linkageName());
  proxyName.append(".type.proxy.tib");
  llvm::GlobalVariable * proxyTib = irModule_->getGlobalVariable(proxyName, false);
  if (proxyTib != NULL) {
    return proxyTib;
  }

  ConstantList baseClassList;
  ClassSet baseClassSet;
  iftype->ancestorClasses(baseClassSet);

  // Interfaces next
  for (ClassSet::iterator it = baseClassSet.begin(); it != baseClassSet.end(); ++it) {
    CompositeType * baseType = *it;
    DASSERT(baseType->typeClass() == Type::Interface);
    genCompositeType(baseType);
    baseClassList.push_back(getTypeInfoBlockPtr(baseType));
  }

  // Null pointer at end
  const llvm::PointerType * typePointerType =
      Builtins::typeTypeInfoBlock.irType()->getPointerTo();
  baseClassList.push_back(ConstantPointerNull::get(typePointerType));

  Constant * baseClassArray = ConstantArray::get(
      ArrayType::get(typePointerType, baseClassList.size()),
      baseClassList);
  GlobalVariable * baseClassArrayPtr = new GlobalVariable(*irModule_,
    baseClassArray->getType(), true, GlobalValue::InternalLinkage,
    baseClassArray, iftype->typeDefn()->linkageName() + ".type.proxy.tib.bases");

  // Generate the interface dispatch function
  Function * idispatch = NULL;
  idispatch = genInterfaceDispatchFunc(iftype);
  DASSERT(idispatch != NULL);

  std::string proxyTypeName("tart.reflect.Proxy[");
  proxyTypeName.append(iftype->typeDefn()->linkageName());
  proxyTypeName.append("]");

  // Create the TypeInfoBlock struct
  StructBuilder builder(*this);
  builder.addField(reflector_.internSymbol(proxyTypeName));
  builder.addField(baseClassArrayPtr);
  builder.addField(idispatch);
  builder.addField(genMethodArray(iftype->instanceMethods_));

  Constant * tibStruct = builder.build();

  proxyTib = new GlobalVariable(*irModule_,
      tibStruct->getType(),
      true, GlobalValue::LinkOnceODRLinkage, NULL,
      proxyName);

  return proxyTib;
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
  DASSERT((MDNode *)dbgContext_ == NULL);
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
  (void)selfArg;
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

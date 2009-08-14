/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Gen/CodeGenerator.h"
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

namespace tart {
  
using namespace llvm;

const llvm::Type * CodeGenerator::genTypeDefn(TypeDefn * tdef) {
  DASSERT_OBJ(tdef->isSingular(), tdef);
  Type * type = tdef->getTypeValue();
  switch (type->typeClass()) {
    case Type::Primitive:
      return genPrimitiveType(static_cast<PrimitiveType *>(type));

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
      return genCompositeType(static_cast<CompositeType *>(type));

    case Type::Enum:
      return genEnumType(static_cast<EnumType *>(type));

    //case Defn::NativeArray:
    //case Defn::NativePointer:

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
  DASSERT_OBJ(type->getIRType() != NULL, type);
  return type->getIRType();
}

const llvm::Type * CodeGenerator::genCompositeType(const CompositeType * type) {
  TypeDefn * tdef = type->typeDefn();
  RuntimeTypeInfo * rtype = getRTTypeInfo(type);

  // Don't need to define this twice.
  if (rtype->isExternal() || (rtype->getTypeObjectPtr() != NULL && 
      rtype->getTypeObjectPtr()->hasInitializer())) {
    return type->getIRType();
  }

  DASSERT_OBJ(type->isSingular(), type);
  DASSERT_OBJ(tdef->isPassFinished(Pass_ResolveBaseTypes), type);
  DASSERT_OBJ(tdef->isPassFinished(Pass_AnalyzeFields), type);
  DASSERT_OBJ(tdef->isPassFinished(Pass_ResolveOverloads), type);
  DASSERT_OBJ(type->getIRType() != NULL, type);

  /*if (type->super() != NULL) {
    const CompositeType * superDecl = type->super();
    if (superDecl != NULL) {
      genCompositeType(superDecl);
    }
  }*/

  irModule_->addTypeName(tdef->getLinkageName(), type->getIRType());
  createTypeInfoBlock(rtype);
  createTypeObject(rtype);
  return type->getIRType();
}

GlobalVariable * CodeGenerator::getTypeObjectPtr(const CompositeType * type) {
  return createTypeObjectPtr(getRTTypeInfo(type));
}

Constant * CodeGenerator::getTypeInfoPtr(const CompositeType * type) {
  return createTypeInfoPtr(getRTTypeInfo(type));
}

Function * CodeGenerator::getTypeAllocator(const CompositeType * type) {
  return createTypeAllocator(getRTTypeInfo(type));
}

GlobalVariable * CodeGenerator::createTypeObjectPtr(RuntimeTypeInfo * rtype) {
  if (rtype->getTypeObjectPtr() == NULL) {
    // Create the global variable for the type object.
    const CompositeType * type = rtype->getType();
    rtype->setTypeObjectPtr(
        new GlobalVariable(
            Builtins::typeType->getIRType(), true,
            GlobalValue::ExternalLinkage, NULL,
            type->typeDefn()->getLinkageName() + ".type", irModule_));
  }

  return rtype->getTypeObjectPtr();
}

Constant * CodeGenerator::createTypeInfoPtr(RuntimeTypeInfo * rtype) {
  if (rtype->getTypeInfoPtr() == NULL) {
    // Create the global variable for the type info block.
    const CompositeType * type = rtype->getType();
    if (type->typeClass() != Type::Class) {
      rtype->setTypeInfoPtr(ConstantPointerNull::get(
          PointerType::getUnqual(Builtins::typeTypeInfoBlock->getIRType())));
    } else {
      rtype->setTypeInfoBlock(
          new GlobalVariable(
              rtype->getTypeInfoBlockType().get(),
              true, rtype->getLinkageType(), NULL,
              type->typeDefn()->getLinkageName() + ".type.tib", irModule_));
      rtype->setTypeInfoPtr(
          llvm::ConstantExpr::ConstantExpr::getBitCast(
              rtype->getTypeInfoBlock(),
              PointerType::get(Builtins::typeTypeInfoBlock->getIRType(), 0)));
    }
  }

  return rtype->getTypeInfoPtr();
}

bool CodeGenerator::createTypeObject(RuntimeTypeInfo * rtype) {
  const CompositeType * type = rtype->getType();
  const std::string & linkName = type->typeDefn()->getLinkageName();

  // Generate the type object.
  ConstantList objMembers;
  ConstantList typeMembers;
  
  objMembers.push_back(getTypeInfoPtr(cast<CompositeType>(Builtins::typeType)));
  typeMembers.push_back(ConstantStruct::get(objMembers));
  typeMembers.push_back(createTypeInfoPtr(rtype));
  if (type->typeClass() == Type::Class && type->super() != NULL) {
    typeMembers.push_back(getTypeObjectPtr(type->super()));
  } else {
    typeMembers.push_back(ConstantPointerNull::get(
        PointerType::getUnqual(Builtins::typeType->getIRType())));
  }

  llvm::Constant * typeStruct = ConstantStruct::get(typeMembers);
  llvm::GlobalVariable * typePtr = createTypeObjectPtr(rtype);
  typePtr->setInitializer(typeStruct);
  return true;
}

bool CodeGenerator::createTypeInfoBlock(RuntimeTypeInfo * rtype) {
  const CompositeType * type = rtype->getType();
  
  if (type->typeClass() != Type::Class) {
    return true;
  }
  
  if (rtype->getTypeInfoPtr() == NULL) {
    createTypeInfoPtr(rtype);
  } else if (rtype->getTypeInfoBlock()->hasInitializer()) {
    return true;
  }

  // Generate the base class list.
  ClassSet baseClassSet;
  type->getAncestorClasses(baseClassSet);
  PointerType * typePointerType = PointerType::getUnqual(Builtins::typeType->getIRType());

  // Concrete classes first
  ConstantList baseClassList;
  for (const CompositeType * s = type->super(); s != NULL; s = s->super()) {
    // If the class lives in this module, then create it.
    genCompositeType(s);
    baseClassList.push_back(getTypeObjectPtr(s));
  }

  // Interfaces next
  for (ClassSet::iterator it = baseClassSet.begin(); it != baseClassSet.end(); ++it) {
    CompositeType * baseType = *it;
    if (baseType->typeClass() == Type::Interface) {
      genCompositeType(baseType);
      baseClassList.push_back(getTypeObjectPtr(baseType));
    }
  }

  // Null pointer at end
  baseClassList.push_back(ConstantPointerNull::get(typePointerType));

  Constant * baseClassArray = ConstantArray::get(
      ArrayType::get(typePointerType, baseClassList.size()),
      baseClassList);
  GlobalVariable * baseClassArrayPtr = new GlobalVariable(
    baseClassArray->getType(), true, GlobalValue::InternalLinkage,
    baseClassArray, type->typeDefn()->getLinkageName() + ".type.tib.bases",
        irModule_);

  // Generate the interface dispatch function
  //Function * idispatch = genInterfaceDispatchFunc(type);
  //DASSERT(idispatch != NULL) {

  // Create the TypeInfoBlock struct
  ConstantList tibMembers;
  tibMembers.push_back(getTypeObjectPtr(type));
  tibMembers.push_back(baseClassArrayPtr);
#if 0
  // TODO: Enable
  tibMembers.push_back(idispatch);
#endif

  tibMembers.push_back(genMethodArray(type->instanceMethods));
  Constant * tibStruct = ConstantStruct::get(tibMembers);

  // Assign the TIB value to the tib global variable.
  GlobalVariable * tibPtr = rtype->getTypeInfoBlock();
  cast<OpaqueType>(rtype->getTypeInfoBlockType().get())->refineAbstractTypeTo(
      tibStruct->getType());
  tibPtr->setInitializer(tibStruct);

  return true;
}

Constant * CodeGenerator::genMethodArray(const MethodList & methods) {
  ConstantList methodValues;
  for (MethodList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
    FunctionDefn * method = static_cast<FunctionDefn *>(*it);
    if (!method->isSingular()) {
      diag.fatal(method) << "Non-singular method '" << method->getName() << "' in closed type";
    }

    Constant * methodVal;
    if (!method->hasBody()) {
      if (method->hasTrait(Defn::Extern)) {
        DASSERT_OBJ(method->isSingular(), method);
        methodVal = genFunctionValue(method);
      } else {
        // TODO: Replace this with call to throw an exception.
        methodVal = ConstantPointerNull::get(methodPtrType);
      }
    } else {
      DASSERT_OBJ(method->isSingular(), method);
      methodVal = genFunctionValue(method);
#if 0
      if (fdef->isSynthetic() && fdef->hasBody() && fdef->module() == module) {
        genFunction(method);
      }
#endif
    }

    methodValues.push_back(
        llvm::ConstantExpr::getBitCast(methodVal, methodPtrType));
  }

  return ConstantArray::get(ArrayType::get(methodPtrType, methodValues.size()), methodValues);
}

Function * CodeGenerator::genInterfaceDispatchFunc(const CompositeType * type) {

  DASSERT_OBJ(type->typeClass() == Type::Class, type);
  
  // Create the dispatch function declaration
  std::vector<const llvm::Type *> argTypes;
  argTypes.push_back(PointerType::get(Builtins::typeType->getIRType(), 0));
  argTypes.push_back(llvm::Type::Int32Ty);
  llvm::FunctionType * functype = llvm::FunctionType::get(methodPtrType, argTypes, false);
  Function * idispatch = Function::Create(
      functype, Function::InternalLinkage,
      type->typeDefn()->getLinkageName() + ".type.idispatch",
      irModule_);

  // Generate the interface dispatcher body
  const std::string & linkageName = type->typeDefn()->getLinkageName();
  BasicBlock * savePoint = builder_.GetInsertBlock();

  Function::ArgumentListType::iterator arg = idispatch->getArgumentList().begin();
  Argument * iid = arg++;
  Argument * methodIndex = arg++;

  ValueList indices;
  indices.push_back(ConstantInt::get(llvm::Type::Int32Ty, 0));
  indices.push_back(methodIndex);

  BasicBlock * blk = BasicBlock::Create("", idispatch);
  for (CompositeType::InterfaceList::const_iterator it =
      type->interfaces.begin(); it != type->interfaces.end(); ++it) {

    // Generate the method table for the interface.
    CompositeType * itDecl = it->interfaceType;
    Constant * itableMembers = genMethodArray(it->methods);
    GlobalVariable * itable = new GlobalVariable(
      itableMembers->getType(), true, GlobalValue::InternalLinkage,
      itableMembers,
      itDecl->typeDefn()->getLinkageName() + "->" + linkageName, irModule_);
    Constant * itype = getTypeObjectPtr(it->interfaceType);

    // Create the blocks
    BasicBlock * ret = BasicBlock::Create(itDecl->typeDefn()->getName(), idispatch);
    BasicBlock * next = BasicBlock::Create("", idispatch);

    // Test the interface pointer
    builder_.SetInsertPoint(blk);
    Value * testVal = builder_.CreateICmp(ICmpInst::ICMP_EQ, iid, itype);
    builder_.CreateCondBr(testVal, ret, next);

    // Return the specified method
    builder_.SetInsertPoint(ret);
    Value * method = builder_.CreateLoad(builder_.CreateGEP(itable,
        indices.begin(), indices.end()), "method");
    builder_.CreateRet(method);

    blk = next;
  }

  builder_.SetInsertPoint(blk);

  // Throw a typecast exception
  // TODO: Enable
#if 0
  builder_.CreateCall(genFunctionValue(Builtins::funcTypecastError));
#endif
  builder_.CreateUnreachable();

  if (savePoint != NULL) {
    builder_.SetInsertPoint(savePoint);
  }
  
  return idispatch;
}

Function * CodeGenerator::createTypeAllocator(RuntimeTypeInfo * rtype) {
  const CompositeType * type = rtype->getType();
  if (rtype->getTypeAllocator() == NULL &&
      type->typeClass() == Type::Class /*&&
      type->hasTrait(Defn::DefaultAlloc)*/) {

    // Declare the allocator function
    std::vector<const llvm::Type *> argTypes;
    llvm::FunctionType * alloctype = llvm::FunctionType::get(PointerType::getUnqual(
        type->getIRType()), argTypes, false);
    Function * allocFunc = Function::Create(
        alloctype, rtype->getLinkageType(),
        type->typeDefn()->getLinkageName() + ".type.alloc", irModule_);

    // Save the builder_ state and set to the new allocator function.
    BasicBlock * savePoint = builder_.GetInsertBlock();
    builder_.SetInsertPoint(BasicBlock::Create("entry", allocFunc));
    
    // Allocate an instance of the object
    Value * instance = builder_.CreateMalloc(type->getIRType());

    // Generate code to fill in vtable pointer of new object.
    genInitObjVTable(type, instance);
    builder_.CreateRet(instance);

    // Restore the builder_ state.
    if (savePoint != NULL) {
      builder_.SetInsertPoint(savePoint);
    }

    rtype->setTypeAllocator(allocFunc);
  }

  return rtype->getTypeAllocator();
}

void CodeGenerator::genInitObjVTable(const CompositeType * type, Value * instance) {
  ValueList indices;
  indices.push_back(ConstantInt::get(llvm::Type::Int32Ty, 0));
  const CompositeType * base = type;

  while (base != NULL) {
    // First member of this type.
    indices.push_back(ConstantInt::get(llvm::Type::Int32Ty, 0));
    base = base->super();
  }

  Value * vtablePtrPtr = builder_.CreateGEP(instance,
      indices.begin(), indices.end());
  Value * typeInfoPtr = getTypeInfoPtr(type);
  builder_.CreateStore(typeInfoPtr, vtablePtrPtr);
}

RuntimeTypeInfo * CodeGenerator::getRTTypeInfo(const CompositeType * type) {
  RTTypeMap::iterator it = compositeTypeMap_.find(type);
  if (it != compositeTypeMap_.end()) {
    return it->second;
  }
  
  RuntimeTypeInfo * rtype = new RuntimeTypeInfo(type, module);
  compositeTypeMap_[type] = rtype;
  return rtype;
}

const llvm::Type * CodeGenerator::genEnumType(EnumType * type) {
  diag.fatal(type->typeDefn()) << "Implement " << type;
  DFAIL("Implement");
}

} // namespace tart

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

namespace tart {

using namespace llvm;

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
  DASSERT_OBJ(type->irType() != NULL, type);
  return type->irType();
}

const llvm::Type * CodeGenerator::genCompositeType(const CompositeType * type) {
  TypeDefn * tdef = type->typeDefn();
  RuntimeTypeInfo * rtype = getRTTypeInfo(type);

  // Don't need to define this twice.
  if (rtype->isExternal() ||
      (rtype->typeDescriptor() != NULL && rtype->typeDescriptor()->hasInitializer())) {
    return type->irType();
  }

  DASSERT_OBJ(type->isSingular(), type);
  DASSERT_OBJ(tdef->isPassFinished(Pass_ResolveBaseTypes), type);
  DASSERT_OBJ(tdef->isPassFinished(Pass_AnalyzeFields), type);
  DASSERT_OBJ(tdef->isPassFinished(Pass_ResolveOverloads), type);
  DASSERT_OBJ(type->irType() != NULL, type);

  irModule_->addTypeName(tdef->linkageName(), type->irType());
  createTypeInfoBlock(rtype);
  createTypeDescriptor(rtype);
  createTypeAllocator(rtype);
  return type->irType();
}

GlobalVariable * CodeGenerator::getTypeDescriptorPtr(const Type * type) {
  if (const CompositeType * ctype = dyn_cast<CompositeType>(type)) {
    return createTypeDescriptorPtr(getRTTypeInfo(ctype));
  } else {
    DFAIL("Implement");
  }
}

Constant * CodeGenerator::getTypeInfoBlockPtr(const CompositeType * type) {
  return createTypeInfoBlockPtr(getRTTypeInfo(type));
}

Function * CodeGenerator::getTypeAllocator(const CompositeType * type) {
  return createTypeAllocator(getRTTypeInfo(type));
}

GlobalVariable * CodeGenerator::createTypeDescriptorPtr(RuntimeTypeInfo * rtype) {
  if (rtype->typeDescriptor() == NULL) {
    // Create the global variable for the type object.
    const CompositeType * type = rtype->getType();
    rtype->setTypeDescriptor(
        new GlobalVariable(*irModule_,
            Builtins::typeTypeDescriptor->irType(), true,
            rtype->getLinkageType(), NULL,
            type->typeDefn()->linkageName() + ".type"));
  }

  return rtype->typeDescriptor();
}

Constant * CodeGenerator::createTypeInfoBlockPtr(RuntimeTypeInfo * rtype) {
  if (rtype->getTypeInfoPtr() == NULL) {
    // Create the global variable for the type info block.
    const CompositeType * type = rtype->getType();
    if (type->typeClass() != Type::Class) {
      rtype->setTypeInfoPtr(ConstantPointerNull::get(
          PointerType::getUnqual(Builtins::typeTypeInfoBlock->irType())));
    } else {
      rtype->setTypeInfoBlock(
          new GlobalVariable(*irModule_,
              rtype->getTypeInfoBlockType().get(),
              true, GlobalValue::ExternalLinkage, NULL,
              type->typeDefn()->linkageName() + ".type.tib"));
      rtype->setTypeInfoPtr(
          llvm::ConstantExpr::getBitCast(
              rtype->getTypeInfoBlock(),
              PointerType::get(Builtins::typeTypeInfoBlock->irType(), 0)));
    }
  }

  return rtype->getTypeInfoPtr();
}

void CodeGenerator::createTypeDescriptor(RuntimeTypeInfo * rtype) {
  llvm::GlobalVariable * typeDescriptorPtr = createTypeDescriptorPtr(rtype);
  const CompositeType * type = rtype->getType();
  const std::string & linkName = type->typeDefn()->linkageName();

  // Generate the type descriptor object.
  StructBuilder sb(*this);
  sb.createObjectHeader(Builtins::typeTypeDescriptor);
  sb.addField(createTypeInfoBlockPtr(rtype));
  sb.addStringField(type->typeDefn()->qualifiedName());
  sb.addIntegerField(Builtins::rfTypeDescriptor.memberTypeKind, 0);
  sb.addTypeReference(type->super());
#if 0
  sb.addArrayField(Builtins::rfTypeDescriptor.memberInterfaces, ConstantList());
  sb.addArrayField(Builtins::rfTypeDescriptor.memberTypeParams, ConstantList());
  sb.addArrayField(Builtins::rfTypeDescriptor.memberAttributes, ConstantList());
  sb.addArrayField(Builtins::rfTypeDescriptor.memberFields, ConstantList());
  sb.addArrayField(Builtins::rfTypeDescriptor.memberProperties, ConstantList());
  sb.addArrayField(Builtins::rfTypeDescriptor.memberConstructors, ConstantList());
  sb.addArrayField(Builtins::rfTypeDescriptor.memberMethods, ConstantList());
#endif

  // Lists

  // Interface list.
  sb.addField(genReflectionDataArray(
      linkName,
      Builtins::rfTypeDescriptor.memberInterfaces,
      ConstantList()));

  // Type parameter list.
  sb.addField(genReflectionDataArray(
      linkName,
      Builtins::rfTypeDescriptor.memberTypeParams,
      ConstantList()));

  // Attribute list.
  sb.addField(genReflectionDataArray(
      linkName,
      Builtins::rfTypeDescriptor.memberAttributes,
      ConstantList()));

  // Field list.
  sb.addField(genReflectionDataArray(
      linkName,
      Builtins::rfTypeDescriptor.memberFields,
      ConstantList()));

  // Property list.
  sb.addField(genReflectionDataArray(
      linkName,
      Builtins::rfTypeDescriptor.memberProperties,
      ConstantList()));

  // Constructor list.
  sb.addField(genReflectionDataArray(
      linkName,
      Builtins::rfTypeDescriptor.memberConstructors,
      ConstantList()));

  // Method list.
  sb.addField(genReflectionDataArray(
      linkName,
      Builtins::rfTypeDescriptor.memberMethods,
      ConstantList()));

  //llvm::Constant * typeDescriptorStruct = ConstantStruct::get(context_, typeDescriptorMembers);
  llvm::Constant * typeDescriptorStruct = sb.build();

  typeDescriptorPtr->setInitializer(typeDescriptorStruct);
}

bool CodeGenerator::createTypeInfoBlock(RuntimeTypeInfo * rtype) {
  const CompositeType * type = rtype->getType();

  if (type->typeClass() != Type::Class) {
    return true;
  }

  if (rtype->getTypeInfoPtr() == NULL) {
    createTypeInfoBlockPtr(rtype);
  } else if (rtype->getTypeInfoBlock()->hasInitializer()) {
    return true;
  }

  // Generate the base class list.
  ClassSet baseClassSet;
  type->ancestorClasses(baseClassSet);
  PointerType * typePointerType = PointerType::getUnqual(Builtins::typeTypeInfoBlock->irType());

  // Concrete classes first
  ConstantList baseClassList;
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
  Function * idispatch = genInterfaceDispatchFunc(type);
  DASSERT(idispatch != NULL);

  // Create the TypeInfoBlock struct
  ConstantList tibMembers;
  tibMembers.push_back(getTypeDescriptorPtr(type));
  tibMembers.push_back(baseClassArrayPtr);
  tibMembers.push_back(idispatch);
  tibMembers.push_back(genMethodArray(type->instanceMethods_));
  Constant * tibStruct = ConstantStruct::get(context_, tibMembers);

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
  argTypes.push_back(PointerType::get(Builtins::typeTypeInfoBlock->irType(), 0));
  argTypes.push_back(builder_.getInt32Ty());
  llvm::FunctionType * functype = llvm::FunctionType::get(methodPtrType, argTypes, false);
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

  BasicBlock * blk = BasicBlock::Create(context_, "", idispatch);
  for (CompositeType::InterfaceList::const_iterator it =
      type->interfaces_.begin(); it != type->interfaces_.end(); ++it) {

    // Generate the method table for the interface.
    CompositeType * itDecl = it->interfaceType;
    Constant * itableMembers = genMethodArray(it->methods);
    GlobalVariable * itable = new GlobalVariable(*irModule_,
      itableMembers->getType(), true, GlobalValue::InternalLinkage,
      itableMembers,
      itDecl->typeDefn()->linkageName() + "->" + linkageName);
    Constant * itype = getTypeInfoBlockPtr(it->interfaceType);

    // Create the blocks
    BasicBlock * ret = BasicBlock::Create(context_, itDecl->typeDefn()->name(), idispatch);
    BasicBlock * next = BasicBlock::Create(context_, "", idispatch);

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
  Function * typecastFailure = genFunctionValue(Builtins::funcTypecastError);
  typecastFailure->setDoesNotReturn(true);
  builder_.CreateCall(typecastFailure);
  //builder_.CreateUnreachable();
  builder_.CreateRet(ConstantPointerNull::get(methodPtrType));

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
        type->irType()), argTypes, false);
    Function * allocFunc = Function::Create(
        alloctype, rtype->getLinkageType(),
        type->typeDefn()->linkageName() + ".type.alloc", irModule_);

    if (!rtype->isExternal()) {
      // Save the builder_ state and set to the new allocator function.
      BasicBlock * savePoint = builder_.GetInsertBlock();
      builder_.SetInsertPoint(BasicBlock::Create(context_, "entry", allocFunc));

      // Allocate an instance of the object
      Value * instance = builder_.CreateMalloc(type->irType());

      // Generate code to fill in vtable pointer of new object.
      genInitObjVTable(type, instance);
      builder_.CreateRet(instance);

      // Restore the builder_ state.
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

  Value * vtablePtrPtr = builder_.CreateGEP(instance, indices.begin(), indices.end());
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
  // TODO: Implement valueOf, asString
  return type->irType();
  //diag.fatal(type->typeDefn()) << "Implement " << type;
  //DFAIL("Implement");
}

} // namespace tart

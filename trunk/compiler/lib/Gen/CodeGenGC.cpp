/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Gen/StructBuilder.h"

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

#include "tart/Objects/SystemDefs.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Intrinsics.h"

#define TRACE_ROOTS DMSG(0)

namespace tart {

using namespace llvm;

// Members of tart.core.TypeInfoBlock.

extern SystemClassMember<VariableDefn> tib_traceTable;

// Members of tart.gc.TraceAction.
SystemClassMember<FunctionDefn> traceAction_tracePointer(Builtins::typeTraceAction, "tracePointer");
SystemClassMember<FunctionDefn> traceAction_traceDescriptors(
    Builtins::typeTraceAction, "traceDescriptors");

void CodeGenerator::genGCRoot(Value * allocaValue, const Type * varType, StringRef rootName) {
  switch (varType->typeClass()) {
    case Type::Class:
    case Type::Interface:
      markGCRoot(allocaValue, NULL, rootName);
      break;

    case Type::Struct:
    case Type::Tuple:
      if (varType->containsReferenceType()) {
        if (varType->typeShape() != Shape_Small_LValue &&
            varType->typeShape() != Shape_Large_Value) {
          diag.fatal() << "Wrong shape for " << varType << " " << varType->typeShape();
        }
        markGCRoot(allocaValue, getTraceTable(varType), rootName);
      }
      break;

    case Type::Union:
      if (varType->containsReferenceType()) {
        const UnionType * utype = static_cast<const UnionType *>(varType);
        if (utype->hasRefTypesOnly()) {
          markGCRoot(allocaValue, NULL, rootName);
          break;
        }

        if (varType->typeShape() != Shape_Small_LValue &&
            varType->typeShape() != Shape_Large_Value) {
          diag.fatal() << "Wrong shape for " << varType << " " << varType->typeShape();
        }
        markGCRoot(allocaValue, getTraceTable(varType), rootName);
      }
      break;

    default:
      break;
  }
}

void CodeGenerator::markGCRoot(Value * value, llvm::Constant * metadata, StringRef rootName) {
  if (gcEnabled_) {
    Function * gcroot = llvm::Intrinsic::getDeclaration(irModule_, llvm::Intrinsic::gcroot);

    if (rootName.empty()) {
      value = builder_.CreatePointerCast(value, builder_.getInt8PtrTy()->getPointerTo());
    } else {
      value = builder_.CreatePointerCast(value, builder_.getInt8PtrTy()->getPointerTo(),
          rootName + ".rptr");
    }
    if (metadata == NULL) {
      DASSERT(isa<llvm::PointerType>(value->getType()->getContainedType(0)));
      metadata = llvm::ConstantPointerNull::get(builder_.getInt8PtrTy());
    } else {
      metadata = llvm::ConstantExpr::getPointerCast(metadata, builder_.getInt8PtrTy());
    }
    builder_.CreateCall2(gcroot, value, metadata);
  }
}

void CodeGenerator::initGCRoot(Value * rootValue) {
  AllocaInst * alloca = cast<AllocaInst>(rootValue);
  llvm::Type * rootType = alloca->getAllocatedType();
  if (PointerType * ptype = dyn_cast<PointerType>(rootType)) {
    builder_.CreateStore(ConstantPointerNull::get(ptype), alloca);
  } else {
    builder_.CreateStore(ConstantAggregateZero::get(rootType), alloca);
  }
}

void CodeGenerator::pushGCRoot(Value * allocaValue, const Type * varType) {
  switch (varType->typeClass()) {
    case Type::Class:
    case Type::Interface:
      rootStack_.push_back(allocaValue);
      break;

    case Type::Struct:
    case Type::Tuple:
    case Type::Union:
      if (varType->containsReferenceType()) {
        rootStack_.push_back(allocaValue);
      }
      break;

    default:
      break;
  }
}

void CodeGenerator::pushRoots(LocalScope * scope) {
  if (gcEnabled_ && scope != NULL) {
    for (Defn * de = scope->firstMember(); de != NULL; de = de->nextInScope()) {
      if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
        if (var->isSharedRef()) {
          pushGCRoot(var->irValue(), var->sharedRefType());
        } else if (var->hasStorage() && var->type()->containsReferenceType()) {
          pushGCRoot(var->irValue(), var->type().unqualified());
        }
      }
    }
  }
}

void CodeGenerator::popRootStack(size_t level) {
  while (rootStack_.size() > level) {
    initGCRoot(rootStack_.back());
    rootStack_.pop_back();
  }
}

Value * CodeGenerator::addTempRoot(const Type * type, Value * value, const Twine & name) {
  // Save the current insertion point
  IRBuilderBase::InsertPoint savePt = builder_.saveIP();

  // Add an alloca to the prologue block.
  builder_.SetInsertPoint(&currentFn_->getBasicBlockList().front());
  llvm::Value * tempRoot = builder_.CreateAlloca(value->getType(), NULL, name);
  if (gcEnabled_) {
    genGCRoot(tempRoot, type, tempRoot->getName());
  }

  // Restore the IP
  builder_.restoreIP(savePt);

  // Store the root value in the alloca
  builder_.CreateStore(value, tempRoot, false);
  if (gcEnabled_) {
    rootStack_.push_back(tempRoot);
  }
  return tempRoot;
}

llvm::GlobalVariable * CodeGenerator::getTraceTable(const Type * type) {
  TraceTableMap::const_iterator it = traceTableMap_.find(type);
  if (it != traceTableMap_.end()) {
    return it->second;
  }

  return (traceTableMap_[type] = createTraceTable(type));
}

llvm::GlobalVariable * CodeGenerator::createTraceTable(const Type * type) {
  // See if any of the instance members contain reference types. This does
  // not include the superclass.
  if (const CompositeType * ctype = dyn_cast<CompositeType>(type)) {
    bool hasReferenceMembers = !ctype->traceMethods().empty();
    if (!hasReferenceMembers) {
      // Search for any reference members.
      for (DefnList::const_iterator it = ctype->instanceFields().begin();
          it != ctype->instanceFields().end(); ++it) {
        if (*it == NULL) {
          continue;
        }
        VariableDefn * var = cast<VariableDefn>(*it);
        if (var->type()->containsReferenceType()) {
          hasReferenceMembers = true;
          break;
        }
      }
    }

    // If the type contains no reference members at all, then use the supertype's
    // trace table.
    if (!hasReferenceMembers) {
      if (ctype->super() != NULL) {
        return getTraceTable(ctype->super());
      } else {
        return NULL;
      }
    }

    ctype->createIRTypeFields();
  } else if (!type->containsReferenceType()) {
    return NULL;
  }

  llvm::Type * traceTableType = tib_traceTable->type()->irType();
  llvm::Type * traceDescriptorType = traceTableType->getContainedType(0);

  ConstantList traceTable;
  ConstantList fieldOffsets;
  ConstantList indices;
  indices.push_back(getInt32Val(0));

  // Fill the trace table.
  llvm::Constant * basePtr = llvm::ConstantPointerNull::get(type->irType()->getPointerTo());
  createTraceTableEntries(type, basePtr, traceTable, fieldOffsets, indices);

  // If there are field offsets, create a trace table entry for the offsets.
  llvm::PointerType * fieldOffsetArrayType = intPtrType_->getPointerTo();
  if (!fieldOffsets.empty()) {
    llvm::SmallString<64> fieldOffsetsName(".fieldoffsets.");
    typeLinkageName(fieldOffsetsName, type);
    llvm::Constant * fieldOffsetsTable = ConstantArray::get(
        ArrayType::get(intPtrType_, fieldOffsets.size()),
        fieldOffsets);
    GlobalVariable * fieldOffsetsVar = new GlobalVariable(*irModule_,
        fieldOffsetsTable->getType(), true, GlobalValue::LinkOnceODRLinkage,
        fieldOffsetsTable, Twine(fieldOffsetsName));
    StructBuilder sbEntry(*this);
    sbEntry.addField(getInt16Val(0));
    sbEntry.addField(getInt16Val(fieldOffsets.size()));
    sbEntry.addField(getInt32Val(0));
    sbEntry.addField(llvm::ConstantExpr::getPointerCast(fieldOffsetsVar, fieldOffsetArrayType));
    traceTable.insert(traceTable.begin(), sbEntry.build(Builtins::typeTraceDescriptor));
  }

  if (traceTable.empty()) {
    return NULL;
  }

  // Mark the last entry in the table. This needs to be done by replacing, rather than by
  // patching the entry.
  llvm::ConstantStruct * finalEntry = cast<ConstantStruct>(traceTable.back());
  StructBuilder sbFinalEntry(*this);
  sbFinalEntry.addField(getInt16Val(1));
  sbFinalEntry.addField(finalEntry->getOperand(1));
  sbFinalEntry.addField(finalEntry->getOperand(2));
  sbFinalEntry.addField(finalEntry->getOperand(3));
  traceTable[traceTable.size() - 1] = sbFinalEntry.build(Builtins::typeTraceDescriptor);

  llvm::Constant * traceTableValue = ConstantArray::get(
      ArrayType::get(traceDescriptorType, traceTable.size()), traceTable);

  llvm::SmallString<64> varName(".tracetable.");
  typeLinkageName(varName, type);
  return new GlobalVariable(*irModule_,
      traceTableValue->getType(),
      true, GlobalValue::LinkOnceODRLinkage, traceTableValue,
      Twine(varName));
}

void CodeGenerator::createTraceTableEntries(const Type * type, llvm::Constant * basePtr,
    ConstantList & traceTable, ConstantList & fieldOffsets, ConstantList & indices) {

  switch (type->typeClass()) {
    case Type::Struct:
    case Type::Class:
      createCompositeTraceTableEntries(static_cast<const CompositeType *> (type), basePtr,
          traceTable, fieldOffsets, indices);
      break;

    case Type::Tuple: {
      const TupleType * ttype = static_cast<const TupleType *>(type);
      size_t indicesSize = indices.size();
      int memberIndex = 0;
      for (TupleType::const_iterator it = ttype->begin(); it != ttype->end(); ++it, ++memberIndex) {
        const Type * memberType = it->type();
        if (memberType->isReferenceType()) {
          indices.push_back(getInt32Val(memberIndex));
          llvm::Constant * fieldOffset = llvm::ConstantExpr::getInBoundsGetElementPtr(basePtr,
              indices);
          fieldOffset = llvm::ConstantExpr::getPtrToInt(fieldOffset, intPtrType_);
          fieldOffsets.push_back(fieldOffset);
          indices.resize(indicesSize);
        } else if (memberType->containsReferenceType()) {
          indices.push_back(getInt32Val(memberIndex));
          createTraceTableEntries(memberType, basePtr, traceTable, fieldOffsets, indices);
          indices.resize(indicesSize);
        }
      }
      break;
    }

    case Type::Union: {
      const UnionType * ut = static_cast<const UnionType *>(type);
      if (ut->hasRefTypesOnly()) {
        // If it only contains reference types, then it's just a pointer.
        llvm::Constant * fieldOffset =
            llvm::ConstantExpr::getInBoundsGetElementPtr(basePtr, indices);
        fieldOffset = llvm::ConstantExpr::getPtrToInt(fieldOffset, intPtrType_);
        fieldOffsets.push_back(fieldOffset);
        break;
      } else if (ut->containsReferenceType()) {
        llvm::Function * traceMethod = getUnionTraceMethod(ut);
        DASSERT(traceMethod != NULL);
        llvm::Constant * fieldOffset =
            llvm::ConstantExpr::getInBoundsGetElementPtr(basePtr, indices);
        fieldOffset = llvm::ConstantExpr::getPtrToInt(fieldOffset, builder_.getInt32Ty());
        llvm::PointerType * fieldOffsetArrayType = intPtrType_->getPointerTo();

        StructBuilder sbEntry(*this);
        sbEntry.addField(getInt16Val(0));
        sbEntry.addField(getInt16Val(0));
        sbEntry.addField(fieldOffset);
        sbEntry.addField(llvm::ConstantExpr::getPointerCast(traceMethod, fieldOffsetArrayType));
        traceTable.push_back(sbEntry.build(Builtins::typeTraceDescriptor));
      }
      break;
    }

    case Type::Interface:
    case Type::Protocol:
    default:
      DFAIL("Invalid trace type");
  }
}

void CodeGenerator::createCompositeTraceTableEntries(const CompositeType * type,
    llvm::Constant * basePtr, ConstantList & traceTable, ConstantList & fieldOffsets,
    ConstantList & indices) {

  size_t indicesSize = indices.size();
  if (type->super() != NULL) {
    indices.push_back(getInt32Val(0));
    createCompositeTraceTableEntries(type->super(), basePtr, traceTable, fieldOffsets, indices);
    indices.resize(indicesSize);
  }

  for (DefnList::const_iterator it = type->instanceFields().begin();
      it != type->instanceFields().end(); ++it) {
    if (*it == NULL) {
      continue;
    }
    VariableDefn * var = cast<VariableDefn>(*it);
    switch (var->type()->typeClass()) {
      case Type::Class: {
        //basePtr->getType()->dump(irModule_);
        indices.push_back(getInt32Val(var->memberIndex()));
        llvm::Constant * fieldOffset =
            llvm::ConstantExpr::getInBoundsGetElementPtr(basePtr, indices);
        fieldOffset = llvm::ConstantExpr::getPtrToInt(fieldOffset, intPtrType_);
        fieldOffsets.push_back(fieldOffset);
        indices.resize(indicesSize);
        break;
      }

      case Type::Struct:
      case Type::Tuple:
      case Type::Union:
        indices.push_back(getInt32Val(var->memberIndex()));
        createTraceTableEntries(
            var->type().unqualified(), basePtr, traceTable, fieldOffsets, indices);
        indices.resize(indicesSize);
        break;

      case Type::FlexibleArray:
        //DFAIL("Implement");
        break;

      default:
        break;
    }
  }

  llvm::PointerType * fieldOffsetArrayType = intPtrType_->getPointerTo();
  for (MethodList::const_iterator it = type->traceMethods().begin();
      it != type->traceMethods().end(); ++it) {
    Function * traceMethod = genFunctionValue(*it);
    DASSERT(traceMethod != NULL);

    StructBuilder sbEntry(*this);
    sbEntry.addField(getInt16Val(0));
    sbEntry.addField(getInt16Val(0));
    sbEntry.addField(getInt32Val(0));
    sbEntry.addField(llvm::ConstantExpr::getPointerCast(traceMethod, fieldOffsetArrayType));
    traceTable.push_back(sbEntry.build(Builtins::typeTraceDescriptor));
  }
}

llvm::Function * CodeGenerator::getUnionTraceMethod(const UnionType * utype) {
  TraceMethodMap::const_iterator it = traceMethodMap_.find(utype);
  if (it != traceMethodMap_.end()) {
    return it->second;
  }

  llvm::SmallString<64> fName(".utrace.");
  typeLinkageName(fName, utype);

  std::vector<llvm::Type *> argTypes;
  argTypes.push_back(builder_.getInt8PtrTy());
  argTypes.push_back(Builtins::typeTraceAction.get()->irParameterType());
  llvm::FunctionType * fnType = llvm::FunctionType::get(builder_.getVoidTy(), argTypes, false);

  llvm::Function * fn = Function::Create(
      fnType, Function::LinkOnceODRLinkage, Twine(fName), irModule_);
  BasicBlock * blk = BasicBlock::Create(context_, "entry", fn);
  BasicBlock * blkExit = BasicBlock::Create(context_, "exit", fn);
  BasicBlock * blkPtrTrace = NULL;

  BasicBlock * savePoint = builder_.GetInsertBlock();
  builder_.SetInsertPoint(blk);
  Function::arg_iterator argIter = fn->arg_begin();
  Value * unionPtr = argIter++;
  Value * actionPtr = argIter++;

  unionPtr = builder_.CreatePointerCast(unionPtr, utype->irType()->getPointerTo());
  Value * unionValuePtr = builder_.CreatePointerCast(
      builder_.CreateConstInBoundsGEP2_32(unionPtr, 0, 1),
      builder_.getInt8PtrTy()->getPointerTo());
  Value * discVal = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(unionPtr, 0, 0), false, "disc");
  IntegerType * discType = cast<IntegerType>(discVal->getType());
  SwitchInst * si = builder_.CreateSwitch(discVal, blkExit, utype->members().size());
  int32_t typeIndex = 0;
  for (TupleType::const_iterator it = utype->members().begin(); it != utype->members().end();
      ++it, ++typeIndex) {
    const Type * memberType = it->type();
    if (memberType->isReferenceType()) {
      if (blkPtrTrace == NULL) {
        blkPtrTrace = BasicBlock::Create(context_, "ptr_trace", fn);
        si->addCase(ConstantInt::get(discType, typeIndex, true), blkPtrTrace);

        builder_.SetInsertPoint(blkPtrTrace);

        // Generate virtual call to TraceAction.tracePointer(action, baseAddr);
        llvm::Value * fnValue = genVTableLookup(traceAction_tracePointer.get(),
            Builtins::typeTraceAction, actionPtr);
        ValueList args;
        args.push_back(actionPtr);
        args.push_back(builder_.CreatePointerCast(unionValuePtr,
            Builtins::typeObject->irEmbeddedType()->getPointerTo()));
        genCallInstr(fnValue, args, "trace");
        builder_.CreateBr(blkExit);
      } else {
        si->addCase(ConstantInt::get(discType, typeIndex, true), blkPtrTrace);
      }
    } else if (memberType->containsReferenceType()) {
      BasicBlock * blkValueTrace = BasicBlock::Create(context_, "value_trace", fn);
      si->addCase(ConstantInt::get(discType, typeIndex, true), blkValueTrace);
      builder_.SetInsertPoint(blkValueTrace);
      llvm::Constant * traceTable = getTraceTable(memberType);
      llvm::Value * fnValue = genFunctionValue(traceAction_traceDescriptors.get());
      ValueList args;
      args.push_back(actionPtr);
      args.push_back(builder_.CreatePointerCast(unionValuePtr, builder_.getInt8PtrTy()));
      args.push_back(builder_.CreateConstInBoundsGEP2_32(traceTable, 0, 0));
      genCallInstr(fnValue, args, "trace");
      builder_.CreateBr(blkExit);
    }
  }

  builder_.SetInsertPoint(blkExit);
  builder_.CreateRetVoid();

  if (savePoint != NULL) {
    builder_.SetInsertPoint(savePoint);
  }

  traceMethodMap_[utype] = fn;
  return fn;
}

void CodeGenerator::addStaticRoot(llvm::GlobalVariable * gv, const Type * type) {
  // Don't add the root if it's already been added.
  if (staticRoots_.count(gv) > 0) {
    return;
  }

  DASSERT(gv->getName().size() > 0);

  // Check if this is an instance or a pointer.
  llvm::Type * varType = gv->getType();
  llvm::Type * objType = varType->getContainedType(0);
  bool isPointer = isa<llvm::PointerType>(objType);

  // A constant variable can only point to other constants, so there's no need for it
  // to be a root.
  if (gv->isConstant() && !isPointer) {
    TRACE_ROOTS << "Ignoring constant root '" << gv->getName() << "': " << type;
    return;
  }

  // Cases to handle:
  // 1) A static object reference.
  // 2) A static object instance.
  // 3) A static struct.
  llvm::GlobalVariable * traceTable = NULL;
  if (const CompositeType * ctype = dyn_cast<CompositeType>(type)) {
    if (ctype->typeClass() == Type::Class || ctype->typeClass() == Type::Interface) {
      if (isPointer) {
        // It's an object reference. For convenience, just use the same trace table
        // as a tuple containing an object reference.
        traceTable = getTraceTable(TupleType::get(QualifiedType(Builtins::typeObject.get())));
        DASSERT(traceTable != NULL);
        TRACE_ROOTS << "Adding static object reference root " << gv->getName();
      } else {
        // A static object instance
        traceTable = getTraceTable(type);
        if (traceTable) {
          TRACE_ROOTS << "Adding static object instance root " << gv->getName();
        } else {
          TRACE_ROOTS << "Ignoring static object instance root with no traceable members " <<
              gv->getName();
        }
      }
    } else if (ctype->typeClass() == Type::Struct) {
      // A static struct
      traceTable = getTraceTable(type);
      if (traceTable) {
        TRACE_ROOTS << "Adding static struct root " << gv->getName();
      } else {
        TRACE_ROOTS << "Ignoring struct root with no traceable members " << gv->getName();
      }
    }
  } else {
    // Some other type that may have traceable fields.
    traceTable = getTraceTable(type);
    if (traceTable) {
      TRACE_ROOTS << "Adding non-composite root " << gv->getName();
    } else {
      TRACE_ROOTS << "Ignoring non-traceable non-composite root " << gv->getName();
    }
  }

  // traceTable will be NULL if there are no reference fields within the data type.
  if (traceTable != NULL) {
    staticRoots_[gv] = traceTable;
  }
}

void CodeGenerator::emitStaticRoots() {
  if (!staticRoots_.empty()) {

    // GEP indices
    Constant * indices[2];
    indices[0] = indices[1] = getInt32Val(0);

    PointerType * bytePtrType = builder_.getInt8PtrTy();
    ConstantList rootList;
    Twine rootSym("roots.", module()->linkageName());
    SmallString<128> rootSymStr;
    NamedMDNode * node = irModule_->getOrInsertNamedMetadata(rootSym.toStringRef(rootSymStr));
    for (StaticRootMap::const_iterator it = staticRoots_.begin(); it != staticRoots_.end(); ++it) {
      Constant * traceTable = llvm::ConstantExpr::getInBoundsGetElementPtr(it->second, indices);
      // We need to do this to prevent the dead global pass from deleting the trace tables.
      llvm::Constant * entry = ConstantStruct::get(
          cast<StructType>(Builtins::typeStaticRoot->irTypeComplete()),
          llvm::ConstantPointerNull::get(bytePtrType),
          traceTable,
          NULL);
      rootList.push_back(entry);
      Value * mdOperands[2];
      mdOperands[0] = it->first;
      mdOperands[1] = traceTable;
      node->addOperand(MDNode::get(context_, mdOperands));
    }

    // Create the appending linkage list of static roots.
    genAppendingArray(rootList, "GC_static_roots_array");
  }
}

GlobalVariable * CodeGenerator::genAppendingArray(
    ArrayRef<llvm::Constant*> elements, StringRef name) {
  ArrayType * arrayType = ArrayType::get(elements.front()->getType(), elements.size());
  Constant * arrayData = ConstantArray::get(arrayType, elements);
  return new GlobalVariable(
      *irModule_, arrayData->getType(), true, GlobalValue::AppendingLinkage, arrayData, name);
}

} // namespace tart

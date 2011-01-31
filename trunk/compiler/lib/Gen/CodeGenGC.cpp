/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Gen/StructBuilder.h"
//#include "tart/Gen/RuntimeTypeInfo.h"
//
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
//
#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
//
//#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
//#include "llvm/DerivedTypes.h"
//#include "llvm/Analysis/Verifier.h"
//#include "llvm/Support/Format.h"

#define DEBUG_STATIC_ROOTS 0

namespace tart {

using namespace llvm;

//SystemClassMember<FunctionDefn> functionType_checkArgs(Builtins::typeFunctionType, "checkArgCount");
//
//namespace reflect {
//  namespace FunctionType {
//    extern SystemClassMember<TypeDefn> CallAdapterFnType;
//  }
//}
//
//// Members of tart.core.TypeInfoBlock.
//
extern SystemClassMember<VariableDefn> tib_traceTable;

// Members of tart.gc.TraceAction.
SystemClassMember<FunctionDefn> traceAction_tracePointer(Builtins::typeTraceAction, "tracePointer");
SystemClassMember<FunctionDefn> traceAction_traceDescriptors(
    Builtins::typeTraceAction, "traceDescriptors");

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
    bool hasReferenceMembers = false;
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

    // If the type contains no reference members at all, then use the supertype's
    // trace table.
    if (!hasReferenceMembers) {
      if (ctype->super() != NULL) {
        return getTraceTable(ctype->super());
      } else {
        return NULL;
      }
    }
  } else if (!type->containsReferenceType()) {
    return NULL;
  }

  const llvm::Type * traceTableType = tib_traceTable->type()->irType();
  const llvm::Type * traceDescriptorType = traceTableType->getContainedType(0);

  ConstantList traceTable;
  ConstantList fieldOffsets;
  ConstantList indices;
  indices.push_back(getInt32Val(0));

  // Fill the trace table.
  llvm::Constant * basePtr = llvm::ConstantPointerNull::get(type->irType()->getPointerTo());
  createTraceTableEntries(type, basePtr, traceTable, fieldOffsets, indices);

  // If there are field offsets, create a trace table entry for the offsets.
  const llvm::PointerType * fieldOffsetArrayType = intPtrType_->getPointerTo();
  if (!fieldOffsets.empty()) {
    std::string fieldOffsetsName(".fieldoffsets.");
    typeLinkageName(fieldOffsetsName, type);
    llvm::Constant * fieldOffsetsTable = ConstantArray::get(
        ArrayType::get(intPtrType_, fieldOffsets.size()),
        fieldOffsets);
    GlobalVariable * fieldOffsetsVar = new GlobalVariable(*irModule_,
        fieldOffsetsTable->getType(), true, GlobalValue::LinkOnceODRLinkage,
        fieldOffsetsTable, fieldOffsetsName);
    llvm::Constant * tableEntryFields[4];
    tableEntryFields[0] = getInt16Val(0);
    tableEntryFields[1] = getInt16Val(fieldOffsets.size());
    tableEntryFields[2] = getInt32Val(0);
    tableEntryFields[3] = llvm::ConstantExpr::getPointerCast(
        fieldOffsetsVar, fieldOffsetArrayType);
    llvm::Constant * tableEntry = ConstantStruct::get(context_, tableEntryFields, 4, false);
    traceTable.insert(traceTable.begin(), tableEntry);
  }

  if (traceTable.empty()) {
    return NULL;
  }

  // Mark the last entry in the table. This needs to be done by replacing, rather than by
  // patching the entry.
  llvm::ConstantStruct * finalEntry = cast<ConstantStruct>(traceTable.back());
  llvm::Constant * finalEntryFields[4];
  finalEntryFields[0] = getInt16Val(1);
  finalEntryFields[1] = finalEntry->getOperand(1);
  finalEntryFields[2] = finalEntry->getOperand(2);
  finalEntryFields[3] = finalEntry->getOperand(3);
  traceTable[traceTable.size() - 1] = ConstantStruct::get(context_, finalEntryFields, 4, false);

  llvm::Constant * traceTableValue = ConstantArray::get(
      ArrayType::get(traceDescriptorType, traceTable.size()), traceTable);

  std::string varName(".tracetable.");
  typeLinkageName(varName, type);
  return new GlobalVariable(*irModule_,
      traceTableValue->getType(),
      true, GlobalValue::LinkOnceODRLinkage, traceTableValue,
      varName);
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
        const Type * memberType = *it;
        if (memberType->isReferenceType()) {
          indices.push_back(getInt32Val(memberIndex));
          llvm::Constant * fieldOffset = llvm::ConstantExpr::getInBoundsGetElementPtr(basePtr,
              &indices[0], indices.size());
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
            llvm::ConstantExpr::getInBoundsGetElementPtr(basePtr, &indices[0], indices.size());
        fieldOffset = llvm::ConstantExpr::getPtrToInt(fieldOffset, intPtrType_);
        fieldOffsets.push_back(fieldOffset);
        break;
      } else if (ut->containsReferenceType()) {
        llvm::Function * traceMethod = getUnionTraceMethod(ut);
        DASSERT(traceMethod != NULL);
        llvm::Constant * fieldOffset =
            llvm::ConstantExpr::getInBoundsGetElementPtr(basePtr, &indices[0], indices.size());
        fieldOffset = llvm::ConstantExpr::getPtrToInt(fieldOffset, builder_.getInt32Ty());
        const llvm::PointerType * fieldOffsetArrayType = intPtrType_->getPointerTo();

        llvm::Constant * descriptorFields[4];
        descriptorFields[0] = getInt16Val(0);
        descriptorFields[1] = getInt16Val(0);
        descriptorFields[2] = fieldOffset;
        descriptorFields[3] = llvm::ConstantExpr::getPointerCast(traceMethod, fieldOffsetArrayType);

        llvm::Constant * desc = ConstantStruct::get(context_, descriptorFields, 4, false);
        traceTable.push_back(desc);
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
            llvm::ConstantExpr::getInBoundsGetElementPtr(basePtr, &indices[0], indices.size());
        fieldOffset = llvm::ConstantExpr::getPtrToInt(fieldOffset, intPtrType_);
        fieldOffsets.push_back(fieldOffset);
        indices.resize(indicesSize);
        break;
      }

      case Type::Struct:
      case Type::Tuple:
      case Type::Union:
        indices.push_back(getInt32Val(var->memberIndex()));
        createTraceTableEntries(var->type(), basePtr, traceTable, fieldOffsets, indices);
        indices.resize(indicesSize);
        break;

      case Type::FlexibleArray:
        //DFAIL("Implement");
        break;

      default:
        break;
    }
  }
}

llvm::Function * CodeGenerator::getUnionTraceMethod(const UnionType * utype) {
  TraceMethodMap::const_iterator it = traceMethodMap_.find(utype);
  if (it != traceMethodMap_.end()) {
    return it->second;
  }

  std::string fName(".utrace.");
  typeLinkageName(fName, utype);

  std::vector<const llvm::Type *> argTypes;
  argTypes.push_back(builder_.getInt8PtrTy());
  argTypes.push_back(Builtins::typeTraceAction.get()->irParameterType());
  llvm::FunctionType * fnType = llvm::FunctionType::get(builder_.getVoidTy(), argTypes, false);

  llvm::Function * fn = Function::Create(fnType, Function::LinkOnceODRLinkage, fName, irModule_);
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
  const IntegerType * discType = cast<IntegerType>(discVal->getType());
  SwitchInst * si = builder_.CreateSwitch(discVal, blkExit, utype->members().size());
  int32_t typeIndex = 0;
  for (TupleType::const_iterator it = utype->members().begin(); it != utype->members().end();
      ++it, ++typeIndex) {
    const Type * memberType = *it;
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
        genCallInstr(fnValue, args.begin(), args.end(), "trace");
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
      genCallInstr(fnValue, args.begin(), args.end(), "trace");
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

void CodeGenerator::addStaticRoot(const llvm::GlobalVariable * gv, const Type * type) {
  // Don't add the root if it's already been added.
  if (staticRoots_.count(gv) > 0) {
    return;
  }

  DASSERT(gv->getName().size() > 0);

  // Check if this is an instance or a pointer.
  const llvm::Type * varType = gv->getType();
  const llvm::Type * objType = varType->getContainedType(0);
  bool isPointer = isa<llvm::PointerType>(objType);

  // A constant variable can only point to other constants, so there's no need for it
  // to be a root.
  if (gv->isConstant() && !isPointer) {
    #if DEBUG_STATIC_ROOTS
      //diag.debug() << "Ignoring constant root '" << gv->getName() << "': " << type;
    #endif
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
        traceTable = getTraceTable(TupleType::get(Builtins::typeObject.get()));
        DASSERT(traceTable != NULL);
        #if DEBUG_STATIC_ROOTS
          //diag.debug() << "Adding static object reference root " << gv->getName();
        #endif
      } else {
        // A static object instance
        traceTable = getTraceTable(type);
        #if DEBUG_STATIC_ROOTS
          /*if (traceTable) {
            diag.debug() << "Adding static object instance root " << gv->getName();
          } else {
            diag.debug() << "Ignoring static object instance root with no traceable members " <<
                gv->getName();
          }*/
        #endif
      }
    } else if (ctype->typeClass() == Type::Struct) {
      // A static struct
      traceTable = getTraceTable(type);
      #if DEBUG_STATIC_ROOTS
        /*if (traceTable) {
          diag.debug() << "Adding static struct root " << gv->getName();
        } else {
          diag.debug() << "Ignoring struct root with no traceable members " << gv->getName();
        }*/
      #endif
    }
  } else {
    // Some other type that may have traceable fields.
    traceTable = getTraceTable(type);
    #if DEBUG_STATIC_ROOTS
      /*if (traceTable) {
        diag.debug() << "Adding non-composite root " << gv->getName();
      } else {
        diag.debug() << "Ignoring non-traceable non-composite root " << gv->getName();
      }*/
    #endif
  }

  // traceTable will be NULL if there are no reference fields within the data type.
  if (traceTable != NULL) {
    staticRoots_[gv] = traceTable;
  }
}

void CodeGenerator::emitStaticRoots() {
  if (!staticRoots_.empty()) {
    ConstantList entries;

    for (StaticRootMap::const_iterator it = staticRoots_.begin(); it != staticRoots_.end(); ++it) {
      Constant * fieldName = ConstantArray::get(context_, it->first->getName());
      GlobalVariable * fieldNameVar = new GlobalVariable(
new GlobalVariable(*irModule_, stackRootArray->getType(), true, GlobalValue::AppendingLinkage,
          stackRootArray, ".static_roots");

      Constant * fields[2];
      fields[0] = ConstantArray::get(context_, it->first->getName());
      fields[1] = it->second;

      llvm::Constant * entry = llvm::ConstantStruct::get(context_, fields, 2, false);
      entries.push_back(entry);
    }

    ArrayType * arrayType = ArrayType::get(entries[0]->getType(), entries.size());
    Constant * stackRootArray = ConstantArray::get(arrayType, entries);

    new GlobalVariable(*irModule_, stackRootArray->getType(), true, GlobalValue::AppendingLinkage,
        stackRootArray, ".static_roots");
  }
}

} // namespace tart

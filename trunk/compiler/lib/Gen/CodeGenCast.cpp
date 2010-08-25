/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/CFG.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Module.h"
#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"
//
#include "llvm/Type.h"
#include "llvm/Value.h"
#include "llvm/Function.h"

namespace tart {

using namespace llvm;

Value * CodeGenerator::genCast(Value * in, const Type * fromType, const Type * toType) {
  // If types are the same, no need for a cast.
  if (fromType->isEqual(toType)) {
    return in;
  }

  const FunctionDefn * converter = NULL;
  TypePair conversionKey(fromType, toType);
  ConverterMap::iterator it = module_->converters().find(conversionKey);
  if (it != module_->converters().end()) {
    converter = it->second;
  }

  if (converter != NULL) {
    ValueList args;
    Value * fnVal = genFunctionValue(converter);
    if (converter->functionType()->isStructReturn()) {
      Value * sret = builder_.CreateAlloca(toType->irType(), 0, "sret");
      args.push_back(sret);
      args.push_back(in);
      genCallInstr(fnVal, args.begin(), args.end(), "convert");
      return sret;
    } else {
      args.push_back(in);
      return genCallInstr(fnVal, args.begin(), args.end(), "convert");
    }
  }

  if (const CompositeType * cfrom = dyn_cast<CompositeType>(fromType)) {
    if (const CompositeType * cto = dyn_cast<CompositeType>(toType)) {
      return genCompositeCast(in, cfrom, cto, true);
    } else if (const PrimitiveType * pto = dyn_cast<PrimitiveType>(toType)) {
      diag.debug() << "Need unbox cast from " << fromType << " to " << toType;
      DFAIL("Implement");
    } else if (const EnumType * eto = dyn_cast<EnumType>(toType)) {
      return genCast(in, fromType, eto->baseType());
    }
  } else if (const PrimitiveType * pfrom = dyn_cast<PrimitiveType>(fromType)) {
    if (const PrimitiveType * pto = dyn_cast<PrimitiveType>(toType)) {
    } else if (toType == Builtins::typeObject) {
      const TemplateSignature * tsig = Builtins::objectCoerceFn()->templateSignature();
      const FunctionDefn * coerceFn = dyn_cast_or_null<FunctionDefn>(
          tsig->findSpecialization(TupleType::get(fromType)));
      if (coerceFn == NULL) {
        diag.error() << "Missing function Object.coerce[" << fromType << "]";
        DFAIL("Missing Object.coerce fn");
      }

      ValueList args;
      Value * fnVal = genFunctionValue(coerceFn);
      args.push_back(in);
      return genCallInstr(fnVal, args.begin(), args.end(), "coerce");
    } else if (const CompositeType * cto = dyn_cast<CompositeType>(toType)) {
      // TODO: This would be *much* easier to handle in the analysis phase.
      // But that means doing the invoke function in the analysis phase as well.
      //return tart.core.ValueRef[type].create(in).
    }
  } else if (const EnumType * efrom = dyn_cast<EnumType>(fromType)) {
    return genCast(in, efrom->baseType(), toType);
  }

  diag.debug() << "Unsupported cast from " << fromType << " to " << toType;
  DFAIL("Implement");
}

Value * CodeGenerator::genNumericCast(const CastExpr * in) {
  Value * value = genExpr(in->arg());
  TypeId fromTypeId = TypeId_Void;
  if (const PrimitiveType * ptype = dyn_cast<PrimitiveType>(in->arg()->type())) {
    fromTypeId = ptype->typeId();
  }

  if (value != NULL) {
    llvm::Instruction::CastOps castType;
    switch (in->exprType()) {
      case Expr::Truncate:
        if (isFloatingTypeId(fromTypeId)) {
          castType = llvm::Instruction::FPTrunc;
        } else {
          castType = llvm::Instruction::Trunc;
        }
        break;

      case Expr::SignExtend:
        if (isFloatingTypeId(fromTypeId)) {
          castType = llvm::Instruction::FPExt;
        } else {
          castType = llvm::Instruction::SExt;
        }
        break;

      case Expr::ZeroExtend:
        castType = llvm::Instruction::ZExt;
        break;

      case Expr::IntToFloat:
        if (isUnsignedIntegerTypeId(fromTypeId)) {
          castType = llvm::Instruction::UIToFP;
        } else {
          castType = llvm::Instruction::SIToFP;
        }
        break;

      default:
        DFAIL("IllegalState");
    }

    return builder_.CreateCast(castType, value, in->type()->irType());
  }

  return NULL;
}

Value * CodeGenerator::genUpCast(const CastExpr * in) {
  Value * value = genExpr(in->arg());
  const Type * fromType = dealias(in->arg()->type());
  const Type * toType = dealias(in->type());

  if (value != NULL && fromType != NULL && toType != NULL) {
    return genUpCastInstr(value, fromType, toType);
  }

  return NULL;
}

Value * CodeGenerator::genDynamicCast(const CastExpr * in, bool throwOnFailure) {
  Value * value = genExpr(in->arg());
  const CompositeType * fromCls = cast<CompositeType>(in->arg()->type());
  const CompositeType * toCls = cast<CompositeType>(in->type());
  return genCompositeCast(value, fromCls, toCls, throwOnFailure);
}

Value * CodeGenerator::genBitCast(const CastExpr * in) {
  Value * value = genExpr(in->arg());
  const Type * toType = in->type();

  if (value != NULL && toType != NULL) {
    //if (toType->typeClass() == Type::Function)
    return builder_.CreateBitCast(value, toType->irEmbeddedType(), "bitcast");
  }

  DFAIL("Bad bitcast");
  return NULL;
}

Value * CodeGenerator::genCompositeCast(Value * in,
    const CompositeType * fromCls, const CompositeType * toCls, bool throwOnFailure) {
  if (toCls->isReferenceType() && fromCls->isReferenceType()) {
    if (fromCls->isSubclassOf(toCls)) {
      // Upcast, no need for type test.
      return genUpCastInstr(in, fromCls, toCls);
    }

    // Composite to composite.
    Value * typeTest = genCompositeTypeTest(in, fromCls, toCls);
    if (throwOnFailure) {
      throwCondTypecastError(typeTest);
      return builder_.CreatePointerCast(in, toCls->irEmbeddedType(), "typecast");
    } else {
      DFAIL("Implement null on failure");
    }
  }

  diag.debug() << "Unsupported cast from " << fromCls << " to " << toCls;
  DFAIL("Implement");
}

Value * CodeGenerator::genUnionCtorCast(const CastExpr * in) {
  const Type * fromType = in->arg()->type();
  const Type * toType = in->type();
  Value * value = NULL;

  if (!fromType->isVoidType()) {
    value = genExpr(in->arg());
    if (value == NULL) {
      return NULL;
    }
  }

  if (toType != NULL) {
    const UnionType * utype = cast<UnionType>(toType);
    if (!utype->hasRefTypesOnly()) {
      int index = utype->getTypeIndex(fromType);
      if (index < 0) {
        diag.error() << "Can't convert " << fromType << " to " << utype;
      }
      DASSERT(index >= 0);
      Value * indexVal = ConstantInt::get(utype->irType()->getContainedType(0), index);

      Value * uvalue = builder_.CreateAlloca(utype->irType(), 0, "union_lval");
      builder_.CreateStore(indexVal,
          builder_.CreateConstInBoundsGEP2_32(uvalue, 0, 0, "union_index_ptr"));
      if (value != NULL) {
        TypeShape fromShape = fromType->typeShape();
#if FC_STRUCTS_INTERNAL
        DASSERT_TYPE_EQ(in, fromType->irParameterType(), value->getType());
        if (fromShape == Shape_Large_Value) {
          value = builder_.CreateLoad(value, "deref");
        }
        DASSERT_TYPE_EQ(in, fromType->irEmbeddedType(), value->getType());
#else
        if (fromShape == Shape_Small_LValue || fromShape == Shape_Large_Value) {
          value = builder_.CreateLoad(value);
        }
#endif

        const llvm::Type * fieldType = fromType->irEmbeddedType();
        builder_.CreateStore(value,
            builder_.CreateBitCast(
                builder_.CreateConstInBoundsGEP2_32(uvalue, 0, 1, "union_val_ptr"),
                llvm::PointerType::get(fieldType, 0)));
      }

      if (utype->typeShape() == Shape_Large_Value) {
        return uvalue;
      }

      return builder_.CreateLoad(uvalue);

#if 0
      // TODO: An alternate method of constructing the value that doesn't involve an alloca.
      // This won't work until union types are supported in LLVM.
      Value * uvalue = UndefValue::get(utype->irType());
      uvalue = builder_.CreateInsertValue(uvalue, indexVal, 0);
      uvalue = builder_.CreateInsertValue(uvalue, value, 1);
      return uvalue;
#endif
    } else {
      // The type returned from irType() is a pointer type.
      //Value * uvalue = builder_.CreateBitCast(utype->irType());
      return builder_.CreateBitCast(value, utype->irType());
    }
  }

  return NULL;
}

Value * CodeGenerator::genUnionMemberCast(const CastExpr * in) {
  // Retrieve a value from a union. Presumes that the type-test has already been done.
  bool checked = in->exprType() == Expr::CheckedUnionMemberCast;
  const Type * fromType = in->arg()->type();
  const Type * toType = in->type();
  if (fromType != NULL) {
    const UnionType * utype = cast<UnionType>(fromType);
    if (!utype->hasRefTypesOnly()) {
      Value * value;
      // Our current process for handling unions requires that the union be an LValue,
      // so that we can bitcast the pointer to the data.
      if (in->exprType() == Expr::LValue || in->exprType() == Expr::ElementRef ||
          utype->typeShape() == Shape_Large_Value) {
        value = genLValueAddress(in->arg());
        if (value == NULL) {
          return NULL;
        }
      } else {
        // Create a temp var.
        value = genExpr(in->arg());
        if (value == NULL) {
          return NULL;
        }

        Value * var = builder_.CreateAlloca(value->getType(), 0, "union_lval");
        builder_.CreateStore(value, var);
        value = var;
      }

      if (checked) {
        Value * test = genUnionTypeTest(value, utype, toType, true);
        throwCondTypecastError(test);
      }

#if 0
      const llvm::Type * fieldType = toType->irEmbeddedType();
      const llvm::Type * unionTypeForMember = llvm::PointerType::get(llvm::StructType::get(
          context_, utype->getDiscriminatorType(), fieldType, NULL), 0);

      return builder_.CreateLoad(
          builder_.CreateConstInBoundsGEP2_32(
              builder_.CreateBitCast(value, unionTypeForMember), 0, 1));
#else
      const llvm::Type * fieldType = toType->irEmbeddedType();

      if (toType->typeShape() == Shape_Large_Value) {
        #if LLVM_UNION_SUPPORT
          DASSERT(value->getType()->getTypeID() == llvm::Type::PointerTyID);
          DASSERT(value->getType()->getContainedType(0)->getTypeID() == llvm::Type::StructTyID);
          const llvm::Type * rawUnionType =
              value->getType()->getContainedType(0)->getContainedType(1);
          if (rawUnionType->getTypeID() == llvm::Type::UnionTyID) {
            DFAIL("Implement");
          } else {
            DASSERT(rawUnionType == fieldType);
            return builder_.CreateConstInBoundsGEP2_32(value, 0, 1, "union_val_ptr");
          }
        #endif

        return builder_.CreateBitCast(
            builder_.CreateConstInBoundsGEP2_32(value, 0, 1, "union_val_ptr"),
            llvm::PointerType::get(fieldType, 0));
      }

      #if LLVM_UNION_SUPPORT
        DASSERT(value->getType()->getTypeID() == llvm::Type::PointerTyID);
        DASSERT(value->getType()->getContainedType(0)->getTypeID() == llvm::Type::StructTyID);
        const llvm::Type * rawUnionType =
            value->getType()->getContainedType(0)->getContainedType(1);
        if (rawUnionType->getTypeID() == llvm::Type::UnionTyID) {
          int index = utype->getNonVoidTypeIndex(toType);
          if (index == -1) {
            // There's no such type in the union, above check should have failed.
            // Return a dummy value.
            return builder_.CreateLoad(
                builder_.CreateBitCast(
                    builder_.CreateConstInBoundsGEP2_32(value, 0, 1, "union_val_ptr"),
                    llvm::PointerType::get(fieldType, 0)), "union_val");
          }

          Value * indices[3];
          indices[0] = getInt32Val(0);
          indices[1] = getInt32Val(1);
          indices[2] = getInt32Val(index);
          return builder_.CreateLoad(
              builder_.CreateInBoundsGEP(value, &indices[0], &indices[3], "union_val_ptr"),
              "union_val");
        } else {
          DASSERT(rawUnionType == fieldType);
          return builder_.CreateLoad(
              builder_.CreateConstInBoundsGEP2_32(value, 0, 1, "union_val_ptr"),
              "union_val");
        }
      #endif

      return builder_.CreateLoad(
          builder_.CreateBitCast(
              builder_.CreateConstInBoundsGEP2_32(value, 0, 1, "union_val_ptr"),
              llvm::PointerType::get(fieldType, 0)), "union_val");
#endif
    } else {
      // The union contains only pointer types, so we know that its representation is simply
      // a single pointer, so a bit cast will work.
      Value * refTypeVal = genExpr(in->arg());
      refTypeVal = builder_.CreatePointerCast(refTypeVal, toType->irEmbeddedType());

      if (checked) {
        if (utype->hasNullType()) {
          if (toType->isNullType()) {
            Value * test = builder_.CreateICmpEQ(
                refTypeVal,
                ConstantPointerNull::get(cast<PointerType>(toType->irEmbeddedType())),
                "null_cmp");
            throwCondTypecastError(test);
          } else {
            Value * test = builder_.CreateICmpNE(
                refTypeVal,
                ConstantPointerNull::get(cast<PointerType>(toType->irEmbeddedType())),
                "null_cmp");
            throwCondTypecastError(test);
          }
        }

        if (const CompositeType * cto = dyn_cast<CompositeType>(toType)) {
          if (!utype->isSupertypeOfAllMembers(cto)) {
            Value * test = genCompositeTypeTest(refTypeVal, Builtins::typeObject.get(), cto);
            throwCondTypecastError(test);
          }
        } else {
          DFAIL("Illegal state");
        }
      }

      return refTypeVal;
    }
  }

  return NULL;
}

Value * CodeGenerator::genUpCastInstr(Value * val, const Type * from, const Type * to) {

  if (from == to) {
    return val;
  }

  DASSERT_OBJ(isa<CompositeType>(to), to);
  DASSERT_OBJ(isa<CompositeType>(from), from);

  const CompositeType * toType = dyn_cast<CompositeType>(to);
  const CompositeType * fromType = dyn_cast<CompositeType>(from);

  if (!fromType->isSubclassOf(toType)) {
    diag.fatal() << "'" << fromType << "' does not inherit from '" <<
    toType << "'";
    return val;
  }

  DASSERT(val->getType()->getTypeID() == llvm::Type::PointerTyID);

  // If it's an interface, then we'll need to simply bit-cast it.
  if (toType->typeClass() == Type::Interface) {
    return builder_.CreateBitCast(val, llvm::PointerType::get(toType->irType(), 0), "intf_ptr");
  }

  // List of GetElementPtr indices
  ValueList indices;

  // Once index to dereference the pointer.
  indices.push_back(getInt32Val(0));

  // One index for each supertype
  while (fromType != toType) {
    DASSERT_OBJ(fromType->super() != NULL, fromType);
    fromType = fromType->super();
    indices.push_back(getInt32Val(0));
  }

  return builder_.CreateInBoundsGEP(val, indices.begin(), indices.end(), "upcast");
}

Value * CodeGenerator::genCompositeTypeTest(Value * val, const CompositeType * fromType,
    const CompositeType * toType) {
  DASSERT(fromType != NULL);
  DASSERT(toType != NULL);

  // Make sure it's a class.
  DASSERT(toType->typeClass() == Type::Class || toType->typeClass() == Type::Interface);
  Constant * toTypeObj = getTypeInfoBlockPtr(toType);

  // Bitcast to object type
  Value * valueAsObjType = builder_.CreateBitCast(val,
      llvm::PointerType::getUnqual(Builtins::typeObject->irType()));

  // Upcast to type 'object' and load the TIB pointer.
  ValueList indices;
  indices.push_back(getInt32Val(0));
  indices.push_back(getInt32Val(0));
  Value * tib = builder_.CreateLoad(
      builder_.CreateInBoundsGEP(valueAsObjType, indices.begin(), indices.end()),
      "tib");

  ValueList args;
  args.push_back(tib);
  args.push_back(toTypeObj);
  Function * upcastTest = genFunctionValue(Builtins::funcHasBase);
  checkCallingArgs(upcastTest, args.begin(), args.end());
  Value * result = builder_.CreateCall(upcastTest, args.begin(), args.end());
  return result;
}

Value * CodeGenerator::genUnionTypeTest(llvm::Value * in, const UnionType * unionType,
    const Type * toType, bool valIsLVal) {
  DASSERT(unionType != NULL);
  DASSERT(toType != NULL);

  if (!unionType->hasRefTypesOnly()) {
    // The index of the actual type.
    Value * actualTypeIndex;
    if (valIsLVal || unionType->typeShape() == Shape_Large_Value) {
      // Load the type index field.
      actualTypeIndex = builder_.CreateLoad(
          builder_.CreateConstInBoundsGEP2_32(in, 0, 0, "union_index_ptr"), "union_index");
    } else {
      // Extract the type index field.
      actualTypeIndex = builder_.CreateExtractValue(in, 0, "union_index");
    }

    int testIndex = unionType->getTypeIndex(toType);
    if (testIndex < 0) {
      return ConstantInt::getFalse(context_);
    }

    Constant * testIndexValue = ConstantInt::get(actualTypeIndex->getType(), testIndex);
    Value * testResult = builder_.CreateICmpEQ(actualTypeIndex, testIndexValue, "union_isa");

#if 0
    // This section of code was based on a hybrid formula where all reference types
    // shared the same testIndex.
    if (testIndex == 0 && unionType->numRefTypes() > 1) {
      BasicBlock * blkIsRefType = BasicBlock::Create(context_, "is_ref_type", currentFn_);
      BasicBlock * blkEndTest = BasicBlock::Create(context_, "utest_end", currentFn_);

      // If it isn't a reference type, branch to the end (fail).
      BasicBlock * blkInitial = builder_.GetInsertBlock();
      builder_.CreateCondBr(testResult, blkIsRefType, blkEndTest);

      // If it is a reference type, then test if it's the right kind of reference type.
      builder_.SetInsertPoint(blkIsRefType);
      const CompositeType * cto = cast<CompositeType>(toType);
      if (valIsLVal) {
        in = builder_.CreateLoad(in);
      }

      Value * refTypeVal = builder_.CreateBitCast(in, toType->irEmbeddedType());
      Value * subclassTest = genCompositeTypeTest(refTypeVal, Builtins::typeObject.get(), cto);
      blkIsRefType = builder_.GetInsertBlock();
      builder_.CreateBr(blkEndTest);

      // Combine the two branches into one boolean test result.
      builder_.SetInsertPoint(blkEndTest);
      PHINode * phi = builder_.CreatePHI(builder_.getInt1Ty());
      phi->addIncoming(ConstantInt::getFalse(context_), blkInitial);
      phi->addIncoming(subclassTest, blkIsRefType);
      testResult = phi;
    }
#endif

    return testResult;
  } else {
    // It's only reference types.
    if (valIsLVal) {
      in = builder_.CreateLoad(in);
    }

    Value * refTypeVal = builder_.CreateBitCast(in, toType->irEmbeddedType());
    if (unionType->hasNullType()) {
      if (unionType->isSingleOptionalType()) {
        if (toType->isNullType()) {
          return builder_.CreateICmpEQ(
              refTypeVal,
              ConstantPointerNull::get(cast<PointerType>(toType->irEmbeddedType())),
              "null_cmp");
        } else {
          return builder_.CreateICmpNE(
              refTypeVal,
              ConstantPointerNull::get(cast<PointerType>(toType->irEmbeddedType())),
              "null_cmp");
        }
      } else {
        DFAIL("Implement Null + multiple types union");
      }
    }

    const CompositeType * cto = cast<CompositeType>(toType);
    return genCompositeTypeTest(refTypeVal, Builtins::typeObject.get(), cto);
  }
}

void CodeGenerator::throwCondTypecastError(Value * typeTestResult) {
  BasicBlock * blkCastFail = BasicBlock::Create(context_, "typecast_fail", currentFn_);
  BasicBlock * blkCastSucc = BasicBlock::Create(context_, "typecast_succ", currentFn_);
  builder_.CreateCondBr(typeTestResult, blkCastSucc, blkCastFail);
  builder_.SetInsertPoint(blkCastFail);
  throwTypecastError();
  builder_.SetInsertPoint(blkCastSucc);
}

void CodeGenerator::throwTypecastError() {
  Function * typecastFailure = genFunctionValue(Builtins::funcTypecastError);
  typecastFailure->setDoesNotReturn(true);
  if (unwindTarget_ != NULL) {
    Function * f = currentFn_;
    ValueList emptyArgs;
    BasicBlock * normalDest = BasicBlock::Create(context_, "nounwind", f);
    normalDest->moveAfter(builder_.GetInsertBlock());
    builder_.CreateInvoke(typecastFailure, normalDest, unwindTarget_,
        emptyArgs.begin(), emptyArgs.end(), "");
    builder_.SetInsertPoint(normalDest);
    builder_.CreateUnreachable();
  } else {
    builder_.CreateCall(typecastFailure);
    builder_.CreateUnreachable();
  }
}

} // namespace tart

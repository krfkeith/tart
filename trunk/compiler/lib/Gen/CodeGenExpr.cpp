/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Expr.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Constant.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Module.h"
#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/Intrinsic.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Module.h"

namespace tart {

FormatStream & operator<<(FormatStream & out, const llvm::Type * type) {
  out << type->getDescription();
  return out;
}

FormatStream & operator<<(FormatStream & out, const llvm::Value * value) {
  // Use a temporary string stream to get the printed form of the value.
  std::string s;
  llvm::raw_string_ostream ss(s);
  value->print(ss);
  out << ss.str();
  return out;
}

FormatStream & operator<<(FormatStream & out, const ValueList & values) {
  for (ValueList::const_iterator it = values.begin(); it != values.end(); ++it) {
    if (it != values.begin()) {
      out << ", ";
    }

    out << *it;
  }

  return out;
}

using namespace llvm;

namespace {
/** Return the type that would be generated from a GEP instruction. */
const llvm::Type * getGEPType(const llvm::Type * type, ValueList::const_iterator first,
    ValueList::const_iterator last) {
  for (ValueList::const_iterator it = first; it != last; ++it) {
    const ConstantInt * index = cast<ConstantInt> (*it);
    type = type->getContainedType(index->getSExtValue());
  }

  return type;
}

#ifdef NDEBUG
#define DASSERT_TYPE_EQ(expected, actual)
#define DASSERT_TYPE_EQ_MSG(expected, actual, msg)
#else
#define DASSERT_TYPE_EQ(expected, actual) \
      if (expected != actual) {\
        diag.fatal() << "Expected '" << expected << "' == '" << actual << "'"; \
      }

#define DASSERT_TYPE_EQ_MSG(expected, actual, msg) \
      if (expected != actual) {\
        diag.fatal() << "Expected '" << expected << "' == '" << actual << \
            "' " << msg; \
      }

#endif
}

Value * CodeGenerator::genExpr(const Expr * in) {
  switch (in->exprType()) {
    case Expr::ConstInt:
      return static_cast<const ConstantInteger *> (in)->value();

    case Expr::ConstFloat: {
      const ConstantFloat * cfloat = static_cast<const ConstantFloat *> (in);
      return cfloat->value();
    }

    case Expr::ConstString:
      return genStringLiteral(static_cast<const ConstantString *> (in)->value());

    case Expr::ConstNull: {
      //DASSERT_OBJ(in->type()->isReferenceType(), in->type());
      return ConstantPointerNull::get(cast<llvm::PointerType>(in->type()->irParameterType()));
    }

    case Expr::ConstObjRef:
      return genConstantObjectPtr(static_cast<const ConstantObjectRef *>(in), "");

    case Expr::LValue: {
      return genLoadLValue(static_cast<const LValueExpr *>(in));
    }

    case Expr::BoundMethod: {
      return genBoundMethod(static_cast<const BoundMethodExpr *>(in));
    }

    case Expr::ElementRef: {
      Value * addr = genElementAddr(static_cast<const UnaryExpr *>(in));
      return addr != NULL ? builder_.CreateLoad(addr) : NULL;
    }

    case Expr::InitVar:
      return genInitVar(static_cast<const InitVarExpr *>(in));

    case Expr::BinaryOpcode:
      return genBinaryOpcode(static_cast<const BinaryOpcodeExpr *>(in));

    case Expr::Truncate:
    case Expr::SignExtend:
    case Expr::ZeroExtend:
    case Expr::IntToFloat:
      return genNumericCast(static_cast<const CastExpr *>(in));

    case Expr::UpCast:
      return genUpCast(static_cast<const CastExpr *>(in));

    case Expr::BitCast:
      return genBitCast(static_cast<const CastExpr *>(in));

    case Expr::UnionCtorCast:
      return genUnionCtorCast(static_cast<const CastExpr *>(in));

    case Expr::UnionMemberCast:
      return genUnionMemberCast(static_cast<const CastExpr *>(in));

    case Expr::Assign:
    case Expr::PostAssign:
      return genAssignment(static_cast<const AssignmentExpr *>(in));

    case Expr::Compare:
      return genCompare(static_cast<const CompareExpr *>(in));

    case Expr::InstanceOf:
      return genInstanceOf(static_cast<const InstanceOfExpr *>(in));

    case Expr::RefEq:
      return genRefEq(static_cast<const BinaryExpr *>(in), false);

    case Expr::PtrDeref:
      return genPtrDeref(static_cast<const UnaryExpr *>(in));

    case Expr::Not:
      return genNot(static_cast<const UnaryExpr *>(in));

    case Expr::And:
    case Expr::Or:
      return genLogicalOper(static_cast<const BinaryExpr *>(in));

    case Expr::FnCall:
    case Expr::CtorCall:
    case Expr::VTableCall:
      return genCall(static_cast<const FnCallExpr *>(in));

    case Expr::IndirectCall:
      return genIndirectCall(static_cast<const IndirectCallExpr *>(in));

    case Expr::New:
      return genNew(static_cast<const NewExpr *>(in));

    case Expr::Prog2: {
      const BinaryExpr * binOp = static_cast<const BinaryExpr *>(in);
      genExpr(binOp->first());
      return genExpr(binOp->second());
    }

    case Expr::IRValue: {
      const IRValueExpr * irExpr = static_cast<const IRValueExpr *>(in);
      DASSERT_OBJ(irExpr->value() != NULL, irExpr);
      return irExpr->value();
    }

    case Expr::ArrayLiteral: {
      const ArrayLiteralExpr * arrayLiteral = static_cast<const ArrayLiteralExpr *>(in);
      return genArrayLiteral(arrayLiteral);
    }

    case Expr::NoOp:
      return NULL;

    default:
      diag.debug(in) << "No generator for " <<
      exprTypeName(in->exprType()) << " [" << in << "]";
      DFAIL("Implement");
  }
}

llvm::Constant * CodeGenerator::genConstExpr(const Expr * in) {
  switch (in->exprType()) {
    case Expr::ConstInt:
      return static_cast<const ConstantInteger *>(in)->value();

    case Expr::ConstObjRef:
      return genConstantObject(static_cast<const ConstantObjectRef *>(in));

    case Expr::ConstNArray:
      return genConstantArray(static_cast<const ConstantNativeArray *>(in));

    default:
      diag.fatal(in) << "Not a constant: " <<
      exprTypeName(in->exprType()) << " [" << in << "]";
      DFAIL("Implement");
  }
}

llvm::GlobalVariable * CodeGenerator::genConstRef(const Expr * in, StringRef name) {
  switch (in->exprType()) {
    case Expr::ConstObjRef:
      return genConstantObjectPtr(static_cast<const ConstantObjectRef *>(in), name);

    //case Expr::ConstNArray:
      //return genConstantArrayPtr(static_cast<const ConstantNativeArray *>(in));

    default:
      diag.fatal(in) << "Not a constant reference: " <<
      exprTypeName(in->exprType()) << " [" << in << "]";
  }
}

Value * CodeGenerator::genInitVar(const InitVarExpr * in) {
  Value * initValue = genExpr(in->initExpr());
  if (initValue == NULL) {
    return NULL;
  }

  VariableDefn * var = in->getVar();
  if (var->defnType() == Defn::Let) {
    var->setIRValue(initValue);
  } else {
    builder_.CreateStore(initValue, var->irValue());
  }

  return initValue;
}

Value * CodeGenerator::genAssignment(const AssignmentExpr * in) {
  Value * rvalue = genExpr(in->fromExpr());
  Value * lvalue = genLValueAddress(in->toExpr());

  if (rvalue != NULL && lvalue != NULL) {
    if (in->exprType() == Expr::PostAssign) {
      Value * result = builder_.CreateLoad(lvalue);
      builder_.CreateStore(rvalue, lvalue);
      return result;
    } else {
      return builder_.CreateStore(rvalue, lvalue);
    }
  }

  return NULL;
}

Value * CodeGenerator::genBinaryOpcode(const BinaryOpcodeExpr * in) {
  Value * lOperand = genExpr(in->first());
  Value * rOperand = genExpr(in->second());
  return builder_.CreateBinOp(in->opCode(), lOperand, rOperand);
}

llvm::Value * CodeGenerator::genCompare(const tart::CompareExpr* in) {
  Value * first = genExpr(in->first());
  Value * second = genExpr(in->second());
  CmpInst::Predicate pred = in->getPredicate();
  if (pred >= CmpInst::FIRST_ICMP_PREDICATE &&
      pred <= CmpInst::LAST_ICMP_PREDICATE) {
    return builder_.CreateICmp(pred, first, second);
  } else if (pred <= CmpInst::LAST_FCMP_PREDICATE) {
    return builder_.CreateFCmp(pred, first, second);
  } else {
    DFAIL("Invalid predicate");
  }
}

Value * CodeGenerator::genInstanceOf(const tart::InstanceOfExpr* in) {
  DASSERT_OBJ(in->value()->type() != NULL, in);
  Value * val = genExpr(in->value());
  if (val == NULL) {
    return NULL;
  }

  if (const UnionType * utype = dyn_cast<UnionType>(in->value()->type())) {
    return genUnionTypeTest(val, utype, in->toType());
  }

  const CompositeType * fromType = cast<CompositeType>(in->value()->type());
  const CompositeType * toType = cast<CompositeType>(in->toType());
  return genCompositeTypeTest(val, fromType, toType);
}

Value * CodeGenerator::genRefEq(const BinaryExpr * in, bool invert) {
  DASSERT_OBJ(in->first()->type()->isEqual(in->second()->type()), in);
  Value * first = genExpr(in->first());
  Value * second = genExpr(in->second());
  if (first != NULL && second != NULL) {
    if (invert) {
      return builder_.CreateICmpNE(first, second);
    } else {
      return builder_.CreateICmpEQ(first, second);
    }
  }

  return NULL;
}

Value * CodeGenerator::genPtrDeref(const UnaryExpr * in) {
  Value * ptrVal = genExpr(in->arg());
  if (ptrVal != NULL) {
    DASSERT(ptrVal->getType()->getTypeID() == llvm::Type::PointerTyID);
    DASSERT_TYPE_EQ_MSG(
        in->type()->irType(),
        ptrVal->getType()->getContainedType(0), "for expression " << in);
    return builder_.CreateLoad(ptrVal);
  }

  return NULL;
}

Value * CodeGenerator::genNot(const UnaryExpr * in) {
  switch (in->arg()->exprType()) {
    case Expr::RefEq:
    return genRefEq(static_cast<const BinaryExpr *>(in->arg()), true);

    default: {
      Value * result = genExpr(in->arg());
      return result ? builder_.CreateNot(result) : NULL;
    }
  }
}

Value * CodeGenerator::genLogicalOper(const BinaryExpr * in) {
  BasicBlock * blkTrue = BasicBlock::Create(context_, "true_branch", currentFn_);
  BasicBlock * blkFalse = BasicBlock::Create(context_, "false_branch", currentFn_);
  BasicBlock * blkNext = BasicBlock::Create(context_, "combine", currentFn_);

  blkTrue->moveAfter(builder_.GetInsertBlock());
  blkFalse->moveAfter(blkTrue);
  blkNext->moveAfter(blkFalse);

  if (!genTestExpr(in, blkTrue, blkFalse)) {
    return NULL;
  }

  builder_.SetInsertPoint(blkTrue);
  builder_.CreateBr(blkNext);

  builder_.SetInsertPoint(blkFalse);
  builder_.CreateBr(blkNext);

  builder_.SetInsertPoint(blkNext);
  PHINode * phi = builder_.CreatePHI(builder_.getInt1Ty());
  phi->addIncoming(ConstantInt::getTrue(context_), blkTrue);
  phi->addIncoming(ConstantInt::getFalse(context_), blkFalse);
  return phi;
}

Value * CodeGenerator::genLoadLValue(const LValueExpr * lval) {
  const ValueDefn * var = lval->value();

  // It's a member or element expression
  if (lval->base() != NULL) {
    Value * addr = genMemberFieldAddr(lval);
    return addr != NULL ? builder_.CreateLoad(addr, var->name()) : NULL;
  }

  // It's a global, static, or parameter
  if (var->defnType() == Defn::Let) {
    const VariableDefn * let = static_cast<const VariableDefn *>(var);
    Value * letValue = genLetValue(let);
    if (let->hasStorage()) {
      letValue = builder_.CreateLoad(letValue, var->name());
    }

    return letValue;
  } else if (var->defnType() == Defn::Var) {
    Value * varValue = genVarValue(static_cast<const VariableDefn *>(var));
    return builder_.CreateLoad(varValue, var->name());
  } else if (var->defnType() == Defn::Parameter) {
    const ParameterDefn * param = static_cast<const ParameterDefn *>(var);
    if (param->irValue() == NULL) {
      diag.fatal(param) << "Invalid parameter IR value for parameter '" << param << "'";
    }
    DASSERT_OBJ(param->irValue() != NULL, param);
    if (param->isLValue()) {
      return builder_.CreateLoad(param->irValue(), param->name());
    }
    return param->irValue();
  } else {
    DFAIL("IllegalState");
  }
}

Value * CodeGenerator::genLValueAddress(const Expr * in) {
  switch (in->exprType()) {
    case Expr::LValue: {
      const LValueExpr * lval = static_cast<const LValueExpr *>(in);

      // It's a reference to a class member.
      if (lval->base() != NULL) {
        return genMemberFieldAddr(lval);
      }

      // It's a global, static, or parameter
      const ValueDefn * var = lval->value();
      if (var->defnType() == Defn::Var) {
        return genVarValue(static_cast<const VariableDefn *>(var));
      } else if (var->defnType() == Defn::Parameter) {
        const ParameterDefn * param = static_cast<const ParameterDefn *>(var);
        if (param->type().typeClass() == Type::Struct) {
          return param->irValue();
        }

        DASSERT_OBJ(param->isLValue(), param);
        return param->irValue();
      } else {
        diag.fatal(lval) << Format_Type << "Can't take address of non-lvalue " << lval;
        DFAIL("IllegalState");
      }
    }

    case Expr::ElementRef: {
      return genElementAddr(static_cast<const UnaryExpr *>(in));
      break;
    }

    default:
      diag.fatal(in) << "Not an LValue " << exprTypeName(in->exprType()) << " [" << in << "]";
      DFAIL("Implement");
  }
}

Value * CodeGenerator::genMemberFieldAddr(const LValueExpr * lval) {
  const Defn * de = lval->value();
  DASSERT(lval->base() != NULL);
  ValueList indices;
  std::stringstream labelStream;
  FormatStream fs(labelStream);
  Value * baseVal = genGEPIndices(lval, indices, fs);
  if (baseVal == NULL) {
    return NULL;
  }

  return builder_.CreateInBoundsGEP(
      baseVal, indices.begin(), indices.end(), labelStream.str().c_str());
}

Value * CodeGenerator::genElementAddr(const UnaryExpr * in) {
  ValueList indices;
  std::stringstream labelStream;
  FormatStream fs(labelStream);
  Value * baseVal = genGEPIndices(in, indices, fs);
  if (baseVal == NULL) {
    return NULL;
  }

  return builder_.CreateInBoundsGEP(baseVal, indices.begin(), indices.end(),
      labelStream.str().c_str());
}

Value * CodeGenerator::genGEPIndices(const Expr * expr, ValueList & indices,
    FormatStream & labelStream) {

  switch (expr->exprType()) {
    case Expr::LValue: {
      const LValueExpr * lval = static_cast<const LValueExpr *>(expr);
      Value * baseAddr = genBaseExpr(lval->base(), indices, labelStream);
      const VariableDefn * field = cast<VariableDefn>(lval->value());

      // TODO: Do the search later. (to handle fields in base classes.)
      //Type * referringType = obj->getCanonicalType();
      //assert(referringType == definingType);
      DASSERT(field->memberIndex() >= 0);

      indices.push_back(getInt32Val(field->memberIndex()));
      labelStream << "." << field->name();

      // Assert that the type is what we expected: A pointer to the field type.
      //DASSERT_TYPE_EQ_MSG(
      //    PointerType::getUntypeeld->getType()->irType()),
      //    getGEPType(baseAddr->getType(), indices.begin(), indices.end()),
      //    "for field: " << field);

      return baseAddr;
    }

    case Expr::ElementRef: {
      const BinaryExpr * indexOp = static_cast<const BinaryExpr *>(expr);
      const Expr * arrayExpr = indexOp->first();
      const Expr * indexExpr = indexOp->second();
      Value * arrayVal;

      if (arrayExpr->type()->typeClass() == Type::NAddress) {
        // Handle auto-deref of Address type.
        arrayVal = genExpr(arrayExpr);
        labelStream << arrayExpr;
      } else {
        arrayVal = genBaseExpr(arrayExpr, indices, labelStream);
      }

      // TODO: Make sure the dimensions are in the correct order here.
      // I think they might be backwards.
      labelStream << "[" << indexExpr << "]";
      Value * indexVal = genExpr(indexExpr);
      if (indexVal == NULL) {
        return NULL;
      }

      indices.push_back(indexVal);
      return arrayVal;
    }

    default:
    DFAIL("Bad GEP call");
    break;
  }

  return NULL;
}

Value * CodeGenerator::genBaseExpr(const Expr * in, ValueList & indices,
    FormatStream & labelStream) {

  // If the base is a pointer
  bool needsDeref = false;

  // True if the base address itself has a base.
  bool hasBase = false;

  /*  Determine if the expression is actually a pointer that needs to be
   dereferenced. This happens under the following circumstances:

   1) The expression is an explicit pointer dereference.
   2) The expression is a variable or parameter containing a reference type.
   3) The expression is a parameter to a value type, but has the reference
   flag set (which should only be true for the self parameter.)
   */

  const Expr * base = in;
  if (const LValueExpr * lval = dyn_cast<LValueExpr>(base)) {
    const ValueDefn * field = lval->value();
    const Type * fieldType = field->type().dealias();
    if (const ParameterDefn * param = dyn_cast<ParameterDefn>(field)) {
      fieldType = param->internalType().dealias();
      if (param->getFlag(ParameterDefn::Reference)) {
        needsDeref = true;
      }
    }

    if (fieldType->isReferenceType()) {
      needsDeref = true;
    }

    if (lval->base() != NULL) {
      hasBase = true;
    }
  } else if (base->exprType() == Expr::PtrDeref) {
    base = static_cast<const UnaryExpr *>(base)->arg();
    needsDeref = true;
  } else if (base->exprType() == Expr::ElementRef) {
    hasBase = true;
  } else if (base->type()->isReferenceType()) {
    needsDeref = true;
  }

  Value * baseAddr;
  if (hasBase && !needsDeref) {
    // If it's a field within a larger object, then we can simply take a
    // relative address from the base.
    baseAddr = genGEPIndices(base, indices, labelStream);
  } else {
    // Otherwise generate a pointer value.
    labelStream << base;
    baseAddr = genExpr(base);
    if (needsDeref) {
      // baseAddr is of pointer type, we need to add an extra 0 to convert it
      // to the type of thing being pointed to.
      indices.push_back(getInt32Val(0));
    }
  }

  // Uncomment to enable some debugging stuff
#if 0
  diag.debug() << "Base address '" << base << "' has type '" << baseAddr->type() << "'";
  if (!indices.empty()) {
    diag.debug() << "Base address '" << base << "' with indices {" <<
    indices << "} dereferences as '" <<
    getGEPType(baseAddr->type(), indices.begin(), indices.end()) << "'";
  }
#endif

  // Assert that the type is what we expected.
  DASSERT_OBJ(in->type() != NULL, in);
  if (!indices.empty()) {
    DASSERT_TYPE_EQ(
        in->type()->irType(),
        getGEPType(baseAddr->getType(), indices.begin(), indices.end()));
  }

  return baseAddr;
}

Value * CodeGenerator::genCast(Value * in, const Type * fromType, const Type * toType) {
  // If types are the same, no need for a cast.
  if (fromType->isEqual(toType)) {
    return in;
  }

  TypeRefPair conversionKey(fromType, toType);
  ConverterMap::iterator it = module_->converters().find(conversionKey);
  if (it != module_->converters().end()) {
    const FunctionDefn * converter = it->second;
    diag.debug(converter) << Format_Type << converter;
    DFAIL("Implement");
  }

  if (const CompositeType * cfrom = dyn_cast<CompositeType>(fromType)) {
    if (const CompositeType * cto = dyn_cast<CompositeType>(toType)) {
      if (cto->isReferenceType() && cfrom->isReferenceType()) {
        if (cfrom->isSubclassOf(cto)) {
          // Upcast, no need for type test.
          return genUpCastInstr(in, cfrom, cto);
        } else if (cto->isSubclassOf(cfrom)) {
        }

        // Composite to composite.
        Value * typeTest = genCompositeTypeTest(in, cfrom, cto);
        BasicBlock * blkCastFail = BasicBlock::Create(context_, "typecast_fail", currentFn_);
        BasicBlock * blkCastSucc = BasicBlock::Create(context_, "typecast_succ", currentFn_);
        builder_.CreateCondBr(typeTest, blkCastSucc, blkCastFail);
        builder_.SetInsertPoint(blkCastFail);
        Function * typecastFailure = genFunctionValue(Builtins::funcTypecastError);
        typecastFailure->setDoesNotReturn(true);
        builder_.CreateCall(typecastFailure);
        builder_.CreateUnreachable();
        builder_.SetInsertPoint(blkCastSucc);
        return builder_.CreatePointerCast(in, cto->irEmbeddedType(), "typecast");
      }
    } else if (const PrimitiveType * pto = dyn_cast<PrimitiveType>(toType)) {
    } else if (const EnumType * eto = dyn_cast<EnumType>(toType)) {
      return genCast(in, fromType, eto->baseType());
    }
  } else if (const PrimitiveType * pfrom = dyn_cast<PrimitiveType>(fromType)) {
    if (const PrimitiveType * pto = dyn_cast<PrimitiveType>(toType)) {
    } else if (toType == Builtins::typeObject) {
      const TemplateSignature * tsig = Builtins::objectCoerceFn()->templateSignature();
      const FunctionDefn * coerceFn = dyn_cast_or_null<FunctionDefn>(
          tsig->findSpecialization(TupleType::get(TypeRef(fromType))));
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
        if (isFloatingType(fromTypeId)) {
          castType = llvm::Instruction::FPTrunc;
        } else {
          castType = llvm::Instruction::Trunc;
        }
        break;

      case Expr::SignExtend:
        if (isFloatingType(fromTypeId)) {
          castType = llvm::Instruction::FPExt;
        } else {
          castType = llvm::Instruction::SExt;
        }
        break;

      case Expr::ZeroExtend:
        castType = llvm::Instruction::ZExt;
        break;

      case Expr::IntToFloat:
        if (isUnsignedIntegerType(fromTypeId)) {
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
  const Type * fromType = in->arg()->type();
  const Type * toType = in->type();

  if (value != NULL && fromType != NULL && toType != NULL) {
    return genUpCastInstr(value, fromType, toType);
  }

  return NULL;
}

Value * CodeGenerator::genBitCast(const CastExpr * in) {
  Value * value = genExpr(in->arg());
  const Type * toType = in->type();

  if (value != NULL && toType != NULL) {
    return builder_.CreateBitCast(value, toType->irEmbeddedType(), "bitcast");
  }

  return NULL;
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

    if (utype->numValueTypes() > 0 || utype->hasVoidType()) {
      int index = utype->getTypeIndex(fromType);
      Value * indexVal = ConstantInt::get(utype->irType()->getContainedType(0), index);

      Value * uvalue = builder_.CreateAlloca(utype->irType());
      builder_.CreateStore(indexVal, builder_.CreateConstGEP2_32(uvalue, 0, 0));
      if (value != NULL) {
        const llvm::Type * fieldType = fromType->irEmbeddedType();
        builder_.CreateStore(value,
            builder_.CreateBitCast(
                builder_.CreateConstGEP2_32(uvalue, 0, 1),
                llvm::PointerType::get(fieldType, 0)));
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
    } else if (fromType->isVoidType()) {
      // If there are no value types, but there is a void type, then represent void
      // as a null pointer.
      return llvm::ConstantPointerNull::getNullValue(utype->irType());
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
  const Type * fromType = in->arg()->type();
  const Type * toType = in->type();
  Value * value = genLValueAddress(in->arg());

  if (value == NULL) {
    return NULL;
  }

  if (fromType != NULL) {
    const UnionType * utype = cast<UnionType>(fromType);
    if (utype->numValueTypes() > 0 || utype->hasVoidType()) {
      int index = utype->getTypeIndex(toType);
      const llvm::Type * fieldType = toType->irEmbeddedType();
      return builder_.CreateLoad(
          builder_.CreateBitCast(
              builder_.CreateConstGEP2_32(value, 0, 1),
              llvm::PointerType::get(fieldType, 0)));
    //} else if (toType->isVoidType()) {
    //  // If the union's representation if a pointer then a 'void' type gets turned into null.
    //  return ConstantPointerNull::getNullValue(PointerType::get(toType->irType(), 0));
    } else {
      // The union contains only pointer types, so we know that its representation is simply
      // a single pointer, so a bit cast will work.
      return builder_.CreateBitCast(value, llvm::PointerType::get(toType->irType(), 0));
    }
  }

  return NULL;
}

Value * CodeGenerator::genCall(const tart::FnCallExpr* in) {
  const FunctionDefn * fn = in->function();

  if (fn->isIntrinsic()) {
    return fn->intrinsic()->generate(*this, in);
  }

  ValueList args;

  Value * selfArg = NULL;
  if (in->selfArg() != NULL) {
    selfArg = genExpr(in->selfArg());

    // Upcast the self argument type.
    if (fn->functionType()->selfParam() != NULL) {
      Type * selfType = fn->functionType()->selfParam()->type().dealias();
      selfArg = genUpCastInstr(selfArg, in->selfArg()->type(), selfType);
    }

    if (fn->storageClass() == Storage_Instance) {
      args.push_back(selfArg);
    }
  }

  const ExprList & inArgs = in->args();
  for (ExprList::const_iterator it = inArgs.begin(); it != inArgs.end(); ++it) {
    Value * argVal = genExpr(*it);
    if (argVal == NULL) {
      return NULL;
    }

    args.push_back(argVal);
  }

  // Generate the function to call.
  Value * fnVal;
  if (in->exprType() == Expr::VTableCall) {
    DASSERT_OBJ(selfArg != NULL, in);
    const Type * classType = fn->functionType()->selfParam()->type().dealias();
    if (classType->typeClass() == Type::Class) {
      fnVal = genVTableLookup(fn, static_cast<const CompositeType *>(classType), selfArg);
    } else if (classType->typeClass() == Type::Interface) {
      fnVal = genITableLookup(fn, static_cast<const CompositeType *>(classType), selfArg);
    } else {
      // Struct or protocol.
      fnVal = genFunctionValue(fn);
    }
  } else {
    fnVal = genFunctionValue(fn);
  }

  Value * result = genCallInstr(fnVal, args.begin(), args.end(), fn->name());
  if (in->exprType() == Expr::CtorCall) {
    // Constructor call returns the 'self' argument.
    return selfArg;
  } else {
    return result;
  }
}

Value * CodeGenerator::genIndirectCall(const tart::IndirectCallExpr* in) {
  const Expr * fn = in->function();
  const Type * fnType = fn->type();

  Value * fnValue;
  ValueList args;

  if (const FunctionType * ft = dyn_cast<FunctionType>(fnType)) {
    fnValue = genExpr(fn);
    if (fnValue != NULL) {
      if (ft->isStatic()) {
        //fnValue = builder_.CreateLoad(fnValue);
      } else {
        DFAIL("Implement");
      }
    }
  } else if (const BoundMethodType * bmType = dyn_cast<BoundMethodType>(fnType)) {
    Value * fnref = genExpr(fn);
    if (fnref == NULL) {
      return NULL;
    }

    fnValue = builder_.CreateExtractValue(fnref, 0, "method");
    Value * selfArg = builder_.CreateExtractValue(fnref, 1, "self");
    if (selfArg == NULL) {
      return NULL;
    }

    args.push_back(selfArg);
  } else {
    diag.info(in) << in->function() << " - " << in->function()->exprType();
    DFAIL("Invalid function type");
  }

#if 0
  Value * selfArg = NULL;
  if (in->selfArg() != NULL) {
    selfArg = genExpr(in->selfArg());
    if (fn->storageClass() == Storage_Instance) {
      args.push_back(selfArg);
    }
  }
#endif

  const ExprList & inArgs = in->args();
  for (ExprList::const_iterator it = inArgs.begin(); it != inArgs.end(); ++it) {
    Value * argVal = genExpr(*it);
    if (argVal == NULL) {
      return NULL;
    }

    args.push_back(argVal);
  }

  // TODO: VCalls and ICalls.

  // Generate the function to call.
    //fnVal = genFunctionValue(fn);

  return genCallInstr(fnValue, args.begin(), args.end(), "indirect");
}

Value * CodeGenerator::genVTableLookup(const FunctionDefn * method, const CompositeType * classType,
    Value * selfPtr) {
  DASSERT_OBJ(!method->isFinal(), method);
  DASSERT_OBJ(!method->isCtor(), method);
  int methodIndex = method->dispatchIndex();
  if (methodIndex < 0) {
    diag.fatal(method) << "Invalid member index of " << method;
    return NULL;
  }

  // Make sure it's a class.
  DASSERT(classType->typeClass() == Type::Class);
  DASSERT_TYPE_EQ(classType->irParameterType(), selfPtr->getType());

  // Upcast to type 'object' and load the vtable pointer.
  ValueList indices;
  for (const CompositeType * t = classType; t != NULL && t != Builtins::typeObject; t = t->super()) {
    indices.push_back(getInt32Val(0));
  }
  indices.push_back(getInt32Val(0));
  indices.push_back(getInt32Val(0));

  // Get the TIB
  Value * tib = builder_.CreateLoad(
      builder_.CreateInBoundsGEP(selfPtr, indices.begin(), indices.end()), "tib");
  DASSERT_TYPE_EQ(llvm::PointerType::get(Builtins::typeTypeInfoBlock->irType(), 0), tib->getType());

  indices.clear();
  indices.push_back(getInt32Val(0));
  indices.push_back(getInt32Val(TIB_METHOD_TABLE));
  indices.push_back(getInt32Val(methodIndex));
  Value * fptr = builder_.CreateLoad(
      builder_.CreateInBoundsGEP(tib, indices.begin(), indices.end()), method->name());
  return builder_.CreateBitCast(fptr, llvm::PointerType::getUnqual(method->type().irType()));
}

Value * CodeGenerator::genITableLookup(const FunctionDefn * method, const CompositeType * classType,
    Value * objectPtr) {

  // Interface function table entry
  DASSERT(!method->isFinal());
  DASSERT(!method->isCtor());
  int methodIndex = method->dispatchIndex();
  if (methodIndex < 0) {
    diag.fatal(method) << "Invalid member index of " << method;
    return NULL;
  }

  // Make sure it's an interface.
  DASSERT(classType->typeClass() == Type::Interface);

  // Get the interface ID (which is just the type pointer).
  Constant * itype = getTypeInfoBlockPtr(classType);

  // Load the pointer to the TIB.
  Value * tib = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(objectPtr, 0, 0, "tib_ptr"), "tib");

  // Load the pointer to the dispatcher function.
  Value * dispatcher = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(tib, 0, TIB_IDISPATCH, "idispatch_ptr"), "idispatch");

  // Construct the call to the dispatcher
  ValueList args;
  args.push_back(itype);
  args.push_back(getInt32Val(methodIndex));
  Value * methodPtr = genCallInstr(dispatcher, args.begin(), args.end(), "method_ptr");
  return builder_.CreateBitCast(
      methodPtr, llvm::PointerType::getUnqual(method->type().irType()), "method");
}

/** Get the address of a value. */
Value * CodeGenerator::genBoundMethod(const BoundMethodExpr * in) {
  const BoundMethodType * type = cast<BoundMethodType>(in->type());
  const FunctionDefn * fn = in->method();
  if (fn->isIntrinsic()) {
    diag.error(in) << "Intrinsic methods cannot be called indirectly.";
    return NULL;
  } else if (fn->isCtor()) {
    diag.error(in) << "Constructors cannot be called indirectly (yet).";
    return NULL;
  }

  Value * selfArg = NULL;
  if (in->selfArg() != NULL) {
    selfArg = genExpr(in->selfArg());

    // Upcast the self argument type.
    if (fn->functionType()->selfParam() != NULL) {
      Type * selfType = fn->functionType()->selfParam()->type().dealias();
      selfArg = genUpCastInstr(selfArg, in->selfArg()->type(), selfType);
    }
  }

  // Generate the function to call.
  Value * fnVal;
  if (in->exprType() == Expr::VTableCall) {
    DASSERT_OBJ(selfArg != NULL, in);
    const Type * classType = fn->functionType()->selfParam()->type().dealias();
    if (classType->typeClass() == Type::Class) {
      fnVal = genVTableLookup(fn, static_cast<const CompositeType *>(classType), selfArg);
    } else if (classType->typeClass() == Type::Interface) {
      fnVal = genITableLookup(fn, static_cast<const CompositeType *>(classType), selfArg);
    } else {
      // Struct or protocol.
      fnVal = genFunctionValue(fn);
    }
  } else {
    fnVal = genFunctionValue(fn);
  }


  const llvm::Type * fnValType =
      StructType::get(context_, fnVal->getType(), selfArg->getType(), NULL);

  Value * result = builder_.CreateAlloca(fnValType);
  builder_.CreateStore(fnVal, builder_.CreateConstInBoundsGEP2_32(result, 0, 0, "method"));
  builder_.CreateStore(selfArg, builder_.CreateConstInBoundsGEP2_32(result, 0, 1, "self"));
  result = builder_.CreateLoad(
      builder_.CreateBitCast(result, llvm::PointerType::get(type->irType(), 0)));
  return result;
}

Value * CodeGenerator::genNew(const tart::NewExpr* in) {
  if (const CompositeType * ctdef = dyn_cast<CompositeType>(in->type())) {
    const llvm::Type * type = ctdef->irType();
    if (ctdef->typeClass() == Type::Struct) {
      return builder_.CreateAlloca(type, 0, ctdef->typeDefn()->name());
    } else if (ctdef->typeClass() == Type::Class) {
      Function * allocator = getTypeAllocator(ctdef);
      if (allocator != NULL) {
        return builder_.CreateCall(allocator, Twine(ctdef->typeDefn()->name(), StringRef("_new")));
      } else {
        diag.fatal(in) << "Cannot create an instance of type '" <<
        ctdef->typeDefn()->name() << "'";
      }
    }
  }

  DFAIL("IllegalState");
}

Value * CodeGenerator::genCallInstr(Value * func, ValueList::iterator firstArg,
    ValueList::iterator lastArg, const char * name) {
  if (unwindTarget_ != NULL) {
    Function * f = currentFn_;
    BasicBlock * normalDest = BasicBlock::Create(context_, "nounwind", f);
    normalDest->moveAfter(builder_.GetInsertBlock());
    Value * result = builder_.CreateInvoke(func, normalDest, unwindTarget_, firstArg, lastArg, name);
    builder_.SetInsertPoint(normalDest);
    return result;
  } else {
    return builder_.CreateCall(func, firstArg, lastArg, name);
  }
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

llvm::Constant * CodeGenerator::genStringLiteral(const llvm::StringRef & strval,
    const llvm::StringRef & symName) {
  StringLiteralMap::iterator it = stringLiteralMap_.find(strval);
  if (it != stringLiteralMap_.end()) {
    return it->second;
  }

  const CompositeType * strType = dyn_cast<CompositeType>(Builtins::typeString);
  const llvm::Type * irType = strType->irType();

  Constant * strVal = ConstantArray::get(context_, strval, false);
  llvm::Type * charDataType = ArrayType::get(builder_.getInt8Ty(), 0);

  // Self-referential member values
  UndefValue * strDataStart = UndefValue::get(llvm::PointerType::getUnqual(charDataType));
  UndefValue * strSource = UndefValue::get(llvm::PointerType::getUnqual(irType));

  // Object type members
  std::vector<Constant *> objMembers;
  objMembers.push_back(getTypeInfoBlockPtr(strType));

  // String type members
  std::vector<Constant *> members;
  members.push_back(ConstantStruct::get(context_, objMembers, false));
  members.push_back(getInt32Val(strval.size()));
  members.push_back(strSource);
  members.push_back(strDataStart);
  members.push_back(ConstantArray::get(context_, strval, false));

  // If the name is blank, then the string is internal only.
  // If the name is non-blank, then it's assumed that this name is a globally unique
  // identifier of the string.
  Twine name;
  GlobalValue::LinkageTypes linkage = GlobalValue::LinkOnceODRLinkage;
  if (symName.empty()) {
    name = "string";
    linkage = GlobalValue::InternalLinkage;
  } else {
    name = "string." + symName;
  }

  Constant * strStruct = ConstantStruct::get(context_, members, false);
  Constant * strConstant = llvm::ConstantExpr::getPointerCast(
      new GlobalVariable(*irModule_,
          strStruct->getType(), true, linkage, strStruct, name),
      llvm::PointerType::getUnqual(irType));

  Constant * indices[2];
  indices[0] = getInt32Val(0);
  indices[1] = getInt32Val(4);

  strDataStart->replaceAllUsesWith(llvm::ConstantExpr::getGetElementPtr(strConstant, indices, 2));
  strSource->replaceAllUsesWith(strConstant);

  stringLiteralMap_[strval] = strConstant;
  return strConstant;
}

Value * CodeGenerator::genArrayLiteral(const ArrayLiteralExpr * in) {
  const CompositeType * arrayType = cast<CompositeType>(in->type());
  TypeRef elementType = arrayType->typeDefn()->templateInstance()->typeArg(0);
  size_t arrayLength = in->args().size();

  const llvm::Type * etype = elementType.irEmbeddedType();

  // Arguments to the array-creation function
  ValueList args;
  args.push_back(getInt32Val(arrayLength));
  Function * allocFunc = findMethod(arrayType, "alloc");
  Value * result = genCallInstr(allocFunc, args.begin(), args.end(), "ArrayLiteral");

  // Evaluate the array elements.
  ValueList arrayVals;
  arrayVals.resize(arrayLength);
  for (size_t i = 0; i < arrayLength; ++i) {
    Value * el = genExpr(in->args()[i]);
    if (el == NULL) {
      return NULL;
    }

    arrayVals[i] = el;
  }

  // Store the array elements into their slots.
  if (arrayLength > 0) {
    Value * arrayData = builder_.CreateStructGEP(result, 2, "data");
    for (size_t i = 0; i < arrayLength; ++i) {
      Value * arraySlot = builder_.CreateStructGEP(arrayData, i);
      builder_.CreateStore(arrayVals[i], arraySlot);
    }
  }

  // TODO: Optimize array creation when most of the elements are constants.

  return result;
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
  Value * result = builder_.CreateCall(upcastTest, args.begin(), args.end());
  return result;
}

Value * CodeGenerator::genUnionTypeTest(llvm::Value * val, const UnionType * fromType,
    const Type * toType) {
  DASSERT(fromType != NULL);
  DASSERT(toType != NULL);

  if (fromType->numValueTypes() > 0 || fromType->hasVoidType()) {
    // The index of the actual type.
    Value * actualTypeIndex = builder_.CreateExtractValue(val, 0);

    int testIndex = fromType->getTypeIndex(toType);
    Constant * testIndexValue = ConstantInt::get(actualTypeIndex->getType(), testIndex);

    if (testIndex == 0 && fromType->numRefTypes() > 1) {
      DFAIL("Add special handling for reference types.");
    }

    return builder_.CreateICmpEQ(actualTypeIndex, testIndexValue, "isa");
  } /*else if (fromType->hasVoidType()) {
    if (toType->isVoidType()) {
      // If we're testing vs. void, then just compare to a null pointer.
      return builder_.CreateICmp(CmpInst::ICMP_EQ, val,
          ConstantPointerNull::getNullValue(val->getType()));
    }

    // Otherwise, we have to do both a null test and a type test.
    BasicBlock * start = builder_.GetInsertBlock();
    BasicBlock * blkIsNotNull = BasicBlock::Create(context_, "is_not_null", currentFn_);
    BasicBlock * blkIsNull = BasicBlock::Create(context_, "is_null", currentFn_);
    blkIsNotNull->moveAfter(builder_.GetInsertBlock());
    blkIsNull->moveAfter(blkIsNotNull);

    // Do the test for null, and branch to the end if it is null.
    Value * isNullTest = builder_.CreateICmp(
        CmpInst::ICMP_NE, val, ConstantPointerNull::getNullValue(val->getType()));
    builder_.CreateCondBr(isNullTest, blkIsNotNull, blkIsNull);
    builder_.SetInsertPoint(blkIsNotNull);

    Value * testResult;
    if (fromType->numRefTypes() > 1) {
      // More than one reference type means we have to do a type test.
      int testIndex = fromType->getTypeIndex(toType);
      CompositeType * elementType = cast<CompositeType>(fromType->typeParam(testIndex).type());
      testResult = genCompositeTypeTest(val, elementType, cast<CompositeType>(toType));
    } else {
      // Only one reference type means that the null pointer check is enough.
      testResult = ConstantInt::getTrue(context_);
    }

    builder_.CreateBr(blkIsNull);

    builder_.SetInsertPoint(blkIsNull);
    PHINode * phi = builder_.CreatePHI(builder_.getInt1Ty());
    phi->addIncoming(testResult, blkIsNotNull);
    phi->addIncoming(ConstantInt::getFalse(context_), start);
    return phi;

  } */else {
    DFAIL("Implement");
  }
}

llvm::Constant * CodeGenerator::genSizeOf(Type * type, bool memberSize) {
  ValueList indices;
  indices.push_back(getInt32Val(1));

  const llvm::Type * irType = type->irType();
  if (memberSize && type->isReferenceType()) {
    irType = llvm::PointerType::get(irType, 0);
  }

  return llvm::ConstantExpr::getPtrToInt(
      llvm::ConstantExpr::getGetElementPtr(
          ConstantPointerNull::get(llvm::PointerType::get(irType, 0)),
          &indices[0], 1),
      builder_.getInt32Ty());
}

Value * CodeGenerator::genVarSizeAlloc(const SourceLocation & loc,
    const Type * objType, const Expr * sizeExpr) {

  if (!objType->isReferenceType()) {
    diag.fatal(loc) << "__valloc can only be used with reference types.";
    return NULL;
  }

  const llvm::Type * resultType = objType->irType();
  resultType = llvm::PointerType::get(resultType, 0);

  Value * sizeValue;
  switch (sizeExpr->exprType()) {
    case Expr::LValue:
    case Expr::ElementRef:
      sizeValue = genLValueAddress(sizeExpr);
      break;

    default:
      sizeValue = genExpr(sizeExpr);
      break;
  }

  if (isa<llvm::PointerType>(sizeValue->getType())) {
    if (Constant * c = dyn_cast<Constant>(sizeValue)) {
      sizeValue = llvm::ConstantExpr::getPtrToInt(c, builder_.getInt64Ty());
    } else {
      sizeValue = builder_.CreatePtrToInt(sizeValue, builder_.getInt64Ty());
    }
  }

  std::stringstream labelStream;
  FormatStream fs(labelStream);
  fs << objType;
  Value * alloc = builder_.CreateCall(getGlobalAlloc(), sizeValue, labelStream.str().c_str());
  Value * instance = builder_.CreateBitCast(alloc, resultType);

  if (const CompositeType * classType = dyn_cast<CompositeType>(objType)) {
    genInitObjVTable(classType, instance);
  }

  return instance;
}

GlobalVariable * CodeGenerator::genConstantObjectPtr(const ConstantObjectRef * obj,
    llvm::StringRef name) {
  Constant * constObject = genConstantObject(obj);
  if (name != "") {
    GlobalVariable * gv = irModule_->getGlobalVariable(name, true);
    if (gv != NULL) {
      return gv;
    }
  }

  return new GlobalVariable(
      *irModule_, constObject->getType(), true, GlobalValue::ExternalLinkage, constObject, name);
}

Constant * CodeGenerator::genConstantObject(const ConstantObjectRef * obj) {
  ConstantObjectMap::iterator it = constantObjectMap_.find(obj);
  if (it != constantObjectMap_.end()) {
    return it->second;
  }

  const CompositeType * type = cast<CompositeType>(obj->type());
  llvm::Constant * structVal = genConstantObjectStruct(obj, type);

  constantObjectMap_[obj] = structVal;
  return structVal;
}

Constant * CodeGenerator::genConstantObjectStruct(
    const ConstantObjectRef * obj, const CompositeType * type) {
  ConstantList fieldValues;
  if (type == Builtins::typeObject) {
    // Generate the TIB pointer.
    llvm::Constant * tibPtr = getTypeInfoBlockPtr(cast<CompositeType>(obj->type()));
    if (tibPtr == NULL) {
      return NULL;
    }

    fieldValues.push_back(tibPtr);
  } else {
    // Generate the superclass fields.
    if (type->super() != NULL) {
      llvm::Constant * superFields = genConstantObjectStruct(obj, type->super());
      if (superFields == NULL) {
        return NULL;
      }

      fieldValues.push_back(superFields);
    }

    // Now generate the values for each member.
    for (DefnList::const_iterator it = type->instanceFields().begin();
        it != type->instanceFields().end(); ++it) {
      if (VariableDefn * var = cast_or_null<VariableDefn>(*it)) {
        Expr * value = obj->getMemberValue(var);
        if (value == NULL) {
          diag.error(obj) << "Member value '" << var << "' has not been initialized.";
          return NULL;
        }

        Constant * irValue = genConstExpr(value);
        if (irValue == NULL) {
          return NULL;
        }

        fieldValues.push_back(irValue);
      }
    }
  }

  return ConstantStruct::get(context_, fieldValues, false);
}

llvm::Constant * CodeGenerator::genConstantArray(const ConstantNativeArray * array) {
  ConstantList elementValues;
  for (ExprList::const_iterator it = array->elements().begin(); it != array->elements().end(); ++it) {
    Constant * value = genConstExpr(*it);
    if (value == NULL) {
      return NULL;
    }

    elementValues.push_back(value);
  }

  return ConstantArray::get(cast<ArrayType>(array->type()->irType()), elementValues);
}

} // namespace tart

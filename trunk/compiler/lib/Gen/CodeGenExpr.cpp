/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Constant.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Closure.h"
#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"

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
    if (const llvm::PointerType * atype = dyn_cast<llvm::PointerType>(type)) {
      type = type->getContainedType(0);
    } else if (const ArrayType * atype = dyn_cast<ArrayType>(type)) {
      type = type->getContainedType(0);
    } else {
      const ConstantInt * index = cast<ConstantInt> (*it);
      type = type->getContainedType(index->getSExtValue());
    }
  }

  return type;
}

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

    case Expr::ConstNull:
      return ConstantPointerNull::get(cast<llvm::PointerType>(in->type()->irParameterType()));

    case Expr::ConstObjRef:
      return genConstantObjectPtr(static_cast<const ConstantObjectRef *>(in), "", false);

    case Expr::LValue:
      return genLoadLValue(static_cast<const LValueExpr *>(in));

    case Expr::BoundMethod:
      return genBoundMethod(static_cast<const BoundMethodExpr *>(in));

    case Expr::ElementRef:
      return genLoadElement(static_cast<const BinaryExpr *>(in));

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

    case Expr::TryCast:
      return genDynamicCast(static_cast<const CastExpr *>(in), true);

    case Expr::DynamicCast:
      return genDynamicCast(static_cast<const CastExpr *>(in), false);

    case Expr::BitCast:
      return genBitCast(static_cast<const CastExpr *>(in));

    case Expr::UnionCtorCast:
      return genUnionCtorCast(static_cast<const CastExpr *>(in));

    case Expr::UnionMemberCast:
    case Expr::CheckedUnionMemberCast:
      return genUnionMemberCast(static_cast<const CastExpr *>(in));

    case Expr::TupleCtor:
      return genTupleCtor(static_cast<const TupleCtorExpr *>(in));

    case Expr::Assign:
    case Expr::PostAssign:
      return genAssignment(static_cast<const AssignmentExpr *>(in));

    case Expr::MultiAssign:
      return genMultiAssign(static_cast<const MultiAssignExpr *>(in));

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

    case Expr::SharedValue: {
      const SharedValueExpr * svExpr = static_cast<const SharedValueExpr *>(in);
      if (svExpr->value() == NULL) {
        svExpr->setValue(genExpr(svExpr->arg()));
      }

      return svExpr->value();
    }

    case Expr::ArrayLiteral:
      return genArrayLiteral(static_cast<const ArrayLiteralExpr *>(in));

    case Expr::ClosureEnv:
      return genClosureEnv(static_cast<const ClosureEnvExpr *>(in));

    case Expr::TypeLiteral:
      return reflector_.emitTypeReference(static_cast<const TypeLiteralExpr *>(in)->value());

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
    case Expr::ConstNull:
      return ConstantPointerNull::getNullValue(PointerType::get(in->type()->irType(), 0));

    case Expr::ConstInt:
      return static_cast<const ConstantInteger *>(in)->value();

    case Expr::ConstObjRef:
      return genConstantObject(static_cast<const ConstantObjectRef *>(in));

    case Expr::ConstNArray:
      return genConstantArray(static_cast<const ConstantNativeArray *>(in));

    case Expr::ConstString:
      return genStringLiteral(static_cast<const ConstantString *> (in)->value());

    case Expr::UnionCtorCast:
      return genConstantUnion(static_cast<const CastExpr *>(in));

    case Expr::LValue: {
      const LValueExpr * lval = static_cast<const LValueExpr *>(in);
      if (lval->base() == NULL) {
        if (const VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {
          if (var->defnType() == Defn::Let) {
            return cast<Constant>(genLetValue(var));
          }
        }
      }

      diag.fatal(in) << "Not a constant: " <<
      exprTypeName(in->exprType()) << " [" << in << "]";
      DFAIL("Implement");
    }

    default:
      diag.fatal(in) << "Not a constant: " <<
      exprTypeName(in->exprType()) << " [" << in << "]";
      DFAIL("Implement");
  }
}

llvm::GlobalVariable * CodeGenerator::genConstRef(const Expr * in, StringRef name, bool synthetic) {
  switch (in->exprType()) {
    case Expr::ConstObjRef:
      return genConstantObjectPtr(static_cast<const ConstantObjectRef *>(in), name, synthetic);

    //case Expr::ConstNArray:
      //return genConstantArrayPtr(static_cast<const ConstantNativeArray *>(in));

    default:
      diag.fatal(in) << "Not a constant reference: " <<
      exprTypeName(in->exprType()) << " [" << in << "]";
      return NULL;
  }
}

Value * CodeGenerator::genInitVar(const InitVarExpr * in) {
  VariableDefn * var = in->getVar();
  TypeShape typeShape = var->canonicalType()->typeShape();
  const Type * initExprType = in->initExpr()->canonicalType();
  Value * initValue = genExpr(in->initExpr());
  if (initValue == NULL) {
    return NULL;
  }

  if (var->defnType() == Defn::Let) {
    // Bind the initValue to the variable. Large value types will be allocas.
    DASSERT_OBJ(var->initValue() == NULL, var);
    DASSERT_OBJ(initValue != NULL, var);
    DASSERT_TYPE_EQ(in->initExpr(), var->type()->irParameterType(), initValue->getType());
    var->setIRValue(initValue);
  } else {
    // Store the initValue to the variable.
#if FC_STRUCTS_INTERNAL
    DASSERT_TYPE_EQ(in->initExpr(), var->type()->irParameterType(), initValue->getType());
    if (typeShape == Shape_Large_Value) {
      initValue = loadValue(initValue, in->initExpr(), "deref");
    }
    DASSERT_TYPE_EQ(in->initExpr(), var->type()->irEmbeddedType(), initValue->getType());
#endif

    DASSERT_TYPE_EQ_MSG(in->initExpr(),
        var->irValue()->getType()->getContainedType(0),
        initValue->getType(), "genInitVar:Var");
    builder_.CreateStore(initValue, var->irValue());
  }

  return initValue;
}

Value * CodeGenerator::genAssignment(const AssignmentExpr * in) {
  Value * lvalue = genLValueAddress(in->toExpr());
  Value * rvalue = genExpr(in->fromExpr());
  return doAssignment(in, lvalue, rvalue);
}

Value * CodeGenerator::doAssignment(const AssignmentExpr * in, Value * lvalue, Value * rvalue) {
  if (rvalue != NULL && lvalue != NULL) {
    // TODO: We could also do this via memcpy.
    TypeShape typeShape = in->fromExpr()->canonicalType()->typeShape();
#if FC_STRUCTS_INTERNAL
    if (typeShape == Shape_Large_Value) {
      rvalue = loadValue(rvalue, in->fromExpr(), "deref");
    }
#endif

    DASSERT_TYPE_EQ_MSG(in,
        lvalue->getType()->getContainedType(0),
        rvalue->getType(), "doAssignment");

    if (in->exprType() == Expr::PostAssign) {
      Value * result = builder_.CreateLoad(lvalue);
      builder_.CreateStore(rvalue, lvalue);
      return result;
    } else {
      if (rvalue->getType() != lvalue->getType()->getContainedType(0)) {
        diag.error(in) << "Invalid assignment:";
        rvalue->getType()->dump(irModule_);
        lvalue->getType()->dump(irModule_);
        exit(-1);
      }

      builder_.CreateStore(rvalue, lvalue);
      return rvalue;
    }
  }

  return NULL;
}

Value * CodeGenerator::genMultiAssign(const MultiAssignExpr * in) {
  ValueList fromVals;

  // Evaluate all of the source args before setting a destination arg.
  size_t numArgs = in->argCount();
  for (ExprList::const_iterator it = in->args().begin(); it != in->args().end(); ++it) {
    const AssignmentExpr * assign = cast<AssignmentExpr>(*it);
    Value * fromVal = genExpr(assign->fromExpr());

    fromVals.push_back(fromVal);
  }

  // Now store them.
  for (size_t i = 0; i < fromVals.size(); ++i) {
    const AssignmentExpr * assign = cast<AssignmentExpr>(in->arg(i));
    Value * toVal = genLValueAddress(assign->toExpr());
    if (toVal == NULL) {
      return NULL;
    }

    doAssignment(assign, toVal, fromVals[i]);
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
  CmpInst::Predicate pred = in->predicate();
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
    return genUnionTypeTest(val, utype, in->toType(), false);
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
    DASSERT_TYPE_EQ_MSG(in,
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
  TypeShape typeShape = var->canonicalType()->typeShape();

  // It's a member or element expression
  if (lval->base() != NULL) {
    return genLoadMemberField(lval);
  }

  // It's a global, static, or parameter
  if (var->defnType() == Defn::Let) {
    const VariableDefn * let = static_cast<const VariableDefn *>(var);
    Value * letValue = genLetValue(let);

    // If this is a let-value that has actual storage
#if FC_STRUCTS_INTERNAL
    if (let->hasStorage() && typeShape != Shape_Large_Value && typeShape != Shape_Reference) {
      letValue = loadValue(letValue, lval, var->name());
    }
#endif

    DASSERT_TYPE_EQ(lval, let->type()->irParameterType(), letValue->getType());
    return letValue;
  } else if (var->defnType() == Defn::Var) {
    Value * varValue = genVarValue(static_cast<const VariableDefn *>(var));
#if FC_STRUCTS_INTERNAL
    if (typeShape == Shape_Large_Value) {
      return varValue;
    }
#endif

    varValue = loadValue(varValue, lval, var->name());
    DASSERT_TYPE_EQ(lval, var->type()->irEmbeddedType(), varValue->getType());
    return varValue;
  } else if (var->defnType() == Defn::Parameter) {
    const ParameterDefn * param = static_cast<const ParameterDefn *>(var);
    if (param->irValue() == NULL) {
      diag.fatal(param) << "Invalid parameter IR value for parameter '" << param << "'";
    }

    DASSERT_OBJ(param->irValue() != NULL, param);
    if (param->isLValue() && typeShape != Shape_Large_Value) {
      return loadValue(param->irValue(), lval, param->name());
    }

    return param->irValue();
  } else if (var->defnType() == Defn::MacroArg) {
    const VariableDefn * arg = static_cast<const VariableDefn *>(var);
    return genExpr(arg->initValue());
  } else {
    DFAIL("IllegalState");
  }
}

Value * CodeGenerator::genLValueAddress(const Expr * in) {
  if (in->exprType() == Expr::PtrDeref) {
    const UnaryExpr * ue = static_cast<const UnaryExpr *>(in);
    return genExpr(ue->arg());
  }

  if (in->type()->typeShape() == Shape_Large_Value) {
    return genExpr(in);
  }

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
        return param->irValue();
      } else if (var->defnType() == Defn::MacroArg) {
        return genLValueAddress(static_cast<const VariableDefn *>(var)->initValue());
      } else {
        TypeShape shape = lval->type()->typeShape();
        if (shape == Shape_Small_LValue || shape == Shape_Large_Value || shape == Shape_Reference) {
          return genLetValue(static_cast<const VariableDefn *>(var));
        }
        diag.fatal(lval) << Format_Type << "Can't take address of non-lvalue " << lval;
        DFAIL("IllegalState");
      }
    }

    case Expr::ElementRef: {
      return genElementAddr(static_cast<const BinaryExpr *>(in));
      break;
    }

    default:
      diag.fatal(in) << "Not an LValue " << exprTypeName(in->exprType()) << " [" << in << "]";
      DFAIL("Implement");
  }
}

Value * CodeGenerator::genLoadMemberField(const LValueExpr * lval) {
  TypeShape baseShape = lval->base()->type()->typeShape();

  if (baseShape == Shape_Small_RValue || baseShape == Shape_Small_LValue) {
    if (!hasAddress(lval->base())) {
      // If the base expression is an SSA value then we have to use extract value
      // instead of GEP.
      const VariableDefn * var = cast<VariableDefn>(lval->value());
      Value * baseValue = genExpr(lval->base());
      return builder_.CreateExtractValue(baseValue, var->memberIndex(), "fieldValue");
    }
  }

  Value * addr = genMemberFieldAddr(lval);
  if (addr == NULL) {
    return NULL;
  }

  if (lval->value()->type()->typeShape() == Shape_Large_Value) {
    return addr;
  }

  return loadValue(addr, lval, lval->value()->name());
}

Value * CodeGenerator::genMemberFieldAddr(const LValueExpr * lval) {
  DASSERT(lval->base() != NULL);
  ValueList indices;
  StrFormatStream fs;
  Value * baseVal = genGEPIndices(lval, indices, fs);
  if (baseVal == NULL) {
    return NULL;
  }

  ensureLValue(lval, baseVal->getType());
  return builder_.CreateInBoundsGEP(baseVal, indices.begin(), indices.end(), fs.str());
}

Value * CodeGenerator::genLoadElement(const BinaryExpr * in) {
  TypeShape baseShape = in->first()->type()->typeShape();
  if (baseShape == Shape_Small_RValue || baseShape == Shape_Small_LValue) {
    const Expr * tupleExpr = in->first();
    const Expr * indexExpr = in->second();
    Value * tupleVal = genExpr(tupleExpr);
    Value * indexVal = genExpr(indexExpr);
    if (tupleVal == NULL && indexVal == NULL) {
      return NULL;
    }

    uint32_t index = cast<ConstantInt>(indexVal)->getValue().getZExtValue();
    return builder_.CreateExtractValue(tupleVal, index, "");
  } else {
    TypeShape typeShape = in->type()->typeShape();
    Value * addr = genElementAddr(static_cast<const BinaryExpr *>(in));
    if (/*typeShape == Shape_Small_LValue || */typeShape == Shape_Large_Value) {
      return addr;
    }

    return addr != NULL ? builder_.CreateLoad(addr) : NULL;
  }
}

Value * CodeGenerator::genElementAddr(const BinaryExpr * in) {
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

Value * CodeGenerator::genGEPIndices(const Expr * expr, ValueList & indices, FormatStream & label) {
  switch (expr->exprType()) {
    case Expr::LValue: {
      // In this case, lvalue refers to a member of the base expression.
      const LValueExpr * lval = static_cast<const LValueExpr *>(expr);
      Value * baseAddr = genBaseAddress(lval->base(), indices, label);
      const VariableDefn * field = cast<VariableDefn>(lval->value());

      DASSERT(isa<PointerType>(baseAddr->getType()));
      DASSERT_TYPE_EQ(lval->base(),
          lval->base()->type()->irType(),
          getGEPType(baseAddr->getType(), indices.begin(), indices.end()));

      // Handle upcasting of the base to match the field.
      const CompositeType * fieldClass = field->definingClass();
      DASSERT(fieldClass != NULL);
      const CompositeType * baseClass = cast<CompositeType>(lval->base()->type());
      DASSERT(baseClass->isSubclassOf(fieldClass));
      while (baseClass != fieldClass) {
        baseClass = baseClass->super();
        indices.push_back(getInt32Val(0));
      }

      DASSERT(field->memberIndex() >= 0);
      indices.push_back(getInt32Val(field->memberIndex()));
      label << "." << field->name();

      // Assert that the type is what we expected: A pointer to the field type.
      DASSERT_TYPE_EQ(expr,
          expr->type()->irEmbeddedType(),
          getGEPType(baseAddr->getType(), indices.begin(), indices.end()));

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
        label << arrayExpr;
      } else {
        arrayVal = genBaseAddress(arrayExpr, indices, label);
      }

      // TODO: Make sure the dimensions are in the correct order here.
      // I think they might be backwards.
      label << "[" << indexExpr << "]";
      Value * indexVal = genExpr(indexExpr);
      if (indexVal == NULL) {
        return NULL;
      }

      indices.push_back(indexVal);

      // Assert that the type is what we expected: A pointer to the field or element type.
      DASSERT_TYPE_EQ(expr,
          expr->type()->irEmbeddedType(),
          getGEPType(arrayVal->getType(), indices.begin(), indices.end()));

      return arrayVal;
    }

    default:
      DFAIL("Bad GEP call");
      break;
  }

  return NULL;
}

Value * CodeGenerator::genBaseAddress(const Expr * in, ValueList & indices,
    FormatStream & labelStream) {

  // If the base is a pointer
  bool needsDeref = false;
  bool needsAddress = false;

  // True if the base address itself has a base.
  bool baseHasBase = false;

  /*  Determine if the expression is actually a pointer that needs to be
      dereferenced. This happens under the following circumstances:

      1) The expression is an explicit pointer dereference.
      2) The expression is a variable or parameter containing a reference type.
      3) The expression is a variable of an aggregate value type.
      4) The expression is a parameter to a value type, but has the reference
         flag set (which should only be true for the 'self' parameter.)
   */

  const Expr * base = in;
  if (const LValueExpr * lval = dyn_cast<LValueExpr>(base)) {
    const ValueDefn * field = lval->value();
    const Type * fieldType = dealias(field->type());
    if (const ParameterDefn * param = dyn_cast<ParameterDefn>(field)) {
      fieldType = dealias(param->internalType());
      if (param->getFlag(ParameterDefn::Reference)) {
        needsDeref = true;
      }
    }

    TypeShape typeShape = fieldType->typeShape();
    switch (typeShape) {
      case Shape_Primitive:
      case Shape_Small_RValue:
        break;

      case Shape_Small_LValue:
        needsAddress = true;
        needsDeref = true;
        break;

      case Shape_Large_Value:
      case Shape_Reference:
        needsDeref = true;
        break;

      default:
        diag.fatal(in) << "Invalid type shape";
    }

    if (lval->base() != NULL) {
      baseHasBase = true;
    } else {
    }
  } else if (base->exprType() == Expr::PtrDeref) {
    base = static_cast<const UnaryExpr *>(base)->arg();
    needsDeref = true;
  } else if (base->exprType() == Expr::ElementRef) {
    baseHasBase = true;
  } else {
    TypeShape typeShape = base->type()->typeShape();
    switch (typeShape) {
      case Shape_Primitive:
      case Shape_Small_RValue:
      case Shape_Small_LValue:
        break;

      case Shape_Large_Value:
      case Shape_Reference:
        needsDeref = true;
        break;

      default:
        diag.fatal(in) << "Invalid type shape";
    }
  }

  Value * baseAddr;
  if (baseHasBase && !needsDeref) {
    // If it's a field within a larger object, then we can simply take a
    // relative address from the base.
    baseAddr = genGEPIndices(base, indices, labelStream);
  } else {
    // Otherwise generate a pointer value.
    labelStream << base;
    if (needsAddress) {
      baseAddr = genLValueAddress(base);
    } else {
      baseAddr = genExpr(base);
    }
    if (needsDeref) {
      // baseAddr is of pointer type, we need to add an extra 0 to convert it
      // to the type of thing being pointed to.
      indices.push_back(getInt32Val(0));
    }
  }

  // Assert that the type is what we expected.
  DASSERT_OBJ(in->type() != NULL, in);
  if (!indices.empty()) {
    DASSERT_TYPE_EQ(in,
        in->type()->irType(),
        getGEPType(baseAddr->getType(), indices.begin(), indices.end()));
  }

  DASSERT(isa<PointerType>(baseAddr->getType()));
  return baseAddr;
}

Value * CodeGenerator::genTupleCtor(const TupleCtorExpr * in) {
  const TupleType * tt = cast<TupleType>(in->canonicalType());
  if (in->canonicalType()->typeShape() == Shape_Small_RValue) {
    // Small tuple values are stored in SSA vars.
    Value * tupleValue = llvm::UndefValue::get(tt->irType());
    size_t index = 0;
    for (ExprList::const_iterator it = in->args().begin(); it != in->args().end(); ++it, ++index) {
      Value * fieldValue = genExpr(*it);
      tupleValue = builder_.CreateInsertValue(tupleValue, fieldValue, index);
    }

    return tupleValue;
  } else {
    // Large tuple values stored in local allocas.
    Value * tupleValue = builder_.CreateAlloca(tt->irType(), 0, "tuple");
    size_t index = 0;
    for (ExprList::const_iterator it = in->args().begin(); it != in->args().end(); ++it, ++index) {
      Value * fieldPtr = builder_.CreateConstInBoundsGEP2_32(tupleValue, 0, index);
      Value * fieldValue = genExpr(*it);
      builder_.CreateStore(fieldValue, fieldPtr, false);
    }

    return tupleValue;
  }
}

llvm::Constant * CodeGenerator::genStringLiteral(const llvm::StringRef & strval,
    const llvm::StringRef & symName) {
  StringLiteralMap::iterator it = stringLiteralMap_.find(strval);
  if (it != stringLiteralMap_.end()) {
    return it->second;
  }

  const CompositeType * strType = Builtins::typeString.get();
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
    name = ".string";
    linkage = GlobalValue::InternalLinkage;
  } else {
    name = ".string." + symName;
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
  const Type * elementType = arrayType->typeDefn()->templateInstance()->typeArg(0);
  size_t arrayLength = in->args().size();

  //diag.debug() << "Generating array literal of type " << elementType << ", length " << arrayLength;

  const llvm::Type * etype = elementType->irEmbeddedType();

  // Arguments to the array-creation function
  ValueList args;
  args.push_back(getIntVal(arrayLength));
  Function * allocFunc = findMethod(arrayType, "alloc");
  Value * result = genCallInstr(allocFunc, args.begin(), args.end(), "ArrayLiteral");

  // Evaluate the array elements.
  ValueList arrayVals;
  arrayVals.resize(arrayLength);
  for (size_t i = 0; i < arrayLength; ++i) {
    Expr * arg = in->args()[i];
    Value * el = genExpr(arg);
    if (el == NULL) {
      return NULL;
    }

    if (arg->type()->typeShape() == Shape_Large_Value) {
      el = loadValue(el, arg, "deref");
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

Value * CodeGenerator::genClosureEnv(const ClosureEnvExpr * in) {
  return llvm::ConstantPointerNull::get(llvm::PointerType::get(in->type()->irType(), 0));
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
    llvm::StringRef name, bool synthetic) {
  if (name != "") {
    GlobalVariable * gv = irModule_->getGlobalVariable(name, true);
    if (gv != NULL) {
      return gv;
    }
  }

  Constant * constObject = genConstantObject(obj);
  DASSERT(constObject != NULL);
  return new GlobalVariable(
      *irModule_, constObject->getType(), true,
      synthetic ? GlobalValue::LinkOnceODRLinkage : GlobalValue::ExternalLinkage,
      constObject, name);
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

llvm::Constant * CodeGenerator::genConstantUnion(const CastExpr * in) {
  const Type * fromType = in->arg()->type();
  const Type * toType = in->type();
  Constant * value = NULL;

  if (!fromType->isVoidType()) {
    value = genConstExpr(in->arg());
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
      DFAIL("Implement");
#if 0
      Value * uvalue = builder_.CreateAlloca(utype->irType());
      builder_.CreateStore(indexVal, builder_.CreateConstInBoundsGEP2_32(uvalue, 0, 0));
      if (value != NULL) {
        const llvm::Type * fieldType = fromType->irEmbeddedType();
        builder_.CreateStore(value,
            builder_.CreateBitCast(
                builder_.CreateConstInBoundsGEP2_32(uvalue, 0, 1),
                llvm::PointerType::get(fieldType, 0)));
      }

      return builder_.CreateLoad(uvalue);
#endif

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
      return llvm::ConstantExpr::getBitCast(value, utype->irType());
    }
  }

  return NULL;
}

} // namespace tart

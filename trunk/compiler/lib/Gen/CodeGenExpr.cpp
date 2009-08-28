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
#include "tart/CFG/Template.h"
#include "tart/CFG/UnionType.h"
#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/Intrinsic.h"

namespace tart {

FormatStream & operator<<(FormatStream & out, const llvm::Type * type) {
  out << type->getDescription();
  return out;
}

FormatStream & operator<<(FormatStream & out, const llvm::Value * value) {
  // Use a temporary string stream to get the printed form of the value.
  std::stringstream ss;
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
      DASSERT_OBJ(in->getType()->isReferenceType(), in->getType());
        return ConstantPointerNull::get(PointerType::getUnqual(in->getType()->irType()));
    }

    case Expr::LValue: {
      return genLoadLValue(static_cast<const LValueExpr *>(in));
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

    case Expr::FnCall:
    case Expr::CtorCall:
    case Expr::VTableCall:
      return genCall(static_cast<const FnCallExpr *>(in));

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

    default:
    diag.fatal(in) << "Not a constant: " <<
    exprTypeName(in->exprType()) << " [" << in << "]";
    DFAIL("Implement");
  }
}

Value * CodeGenerator::genInitVar(InitVarExpr * in) {
  Value * initValue = genExpr(in->getInitExpr());
  if (initValue == NULL) {
    return NULL;
  }

  VariableDefn * var = in->getVar();
  if (var->defnType() == Defn::Let) {
    var->setIRValue(initValue);
  } else {
    builder_.CreateStore(initValue, var->getIRValue());
  }

  return initValue;
}

Value * CodeGenerator::genAssignment(AssignmentExpr * in) {
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

Value * CodeGenerator::genBinaryOpcode(BinaryOpcodeExpr * in) {
  Value * lOperand = genExpr(in->first());
  Value * rOperand = genExpr(in->second());
  return builder_.CreateBinOp(in->opCode(), lOperand, rOperand);
}

llvm::Value * CodeGenerator::genCompare(CompareExpr * in) {
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

Value * CodeGenerator::genInstanceOf(InstanceOfExpr * in) {
  DASSERT_OBJ(in->value()->getType() != NULL, in);
  Value * val = genExpr(in->value());
  if (val == NULL) {
    return NULL;
  }

  if (UnionType * utype = dyn_cast<UnionType>(in->value()->getType())) {
    return genUnionTypeTest(val, utype, in->toType());
  }

  CompositeType * fromType = cast<CompositeType>(in->value()->getType());
  CompositeType * toType = cast<CompositeType>(in->toType());
  DASSERT_OBJ(fromType != NULL, in);
  DASSERT_OBJ(toType != NULL, in);
  return genCompositeTypeTest(val, fromType, toType);
}

Value * CodeGenerator::genRefEq(const BinaryExpr * in, bool invert) {
  DASSERT_OBJ(in->first()->getType()->isEqual(in->second()->getType()), in);
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
        in->getType()->irType(),
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

Value * CodeGenerator::genLoadLValue(const LValueExpr * lval) {
  const ValueDefn * var = lval->value();

  // It's a member or element expression
  if (lval->base() != NULL) {
    Value * addr = genMemberFieldAddr(lval);
    return addr != NULL ? builder_.CreateLoad(addr, var->name()) : NULL;
  }

  // It's a global, static, or parameter
  if (var->defnType() == Defn::Let) {
    return genLetValue(static_cast<const VariableDefn *>(var));
  } else if (var->defnType() == Defn::Var) {
    Value * varValue = genVarValue(static_cast<const VariableDefn *>(var));
    return builder_.CreateLoad(varValue, var->name());
  } else if (var->defnType() == Defn::Parameter) {
    const ParameterDefn * param = static_cast<const ParameterDefn *>(var);
    if (param->getIRValue() == NULL) {
      diag.fatal(param) << "Invalid parameter IR value for " << param << " in function " <<
      currentFunction_;
    }
    DASSERT_OBJ(param->getIRValue() != NULL, param);
    return param->getIRValue();
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
        DFAIL("Implement");
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

  return builder_.CreateGEP(baseVal, indices.begin(), indices.end(),
      labelStream.str().c_str());
}

Value * CodeGenerator::genElementAddr(const UnaryExpr * in) {
  ValueList indices;
  std::stringstream labelStream;
  FormatStream fs(labelStream);
  Value * baseVal = genGEPIndices(in, indices, fs);
  if (baseVal == NULL) {
    return NULL;
  }

  return builder_.CreateGEP(baseVal, indices.begin(), indices.end(),
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
      //    PointerType::getUnqual(field->getType()->irType()),
      //    getGEPType(baseAddr->getType(), indices.begin(), indices.end()),
      //    "for field: " << field);

      return baseAddr;
    }

    case Expr::ElementRef: {
      const BinaryExpr * indexOp = static_cast<const BinaryExpr *>(expr);
      const Expr * arrayExpr = indexOp->first();
      const Expr * indexExpr = indexOp->second();
      Value * arrayVal = genBaseExpr(arrayExpr, indices, labelStream);

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

  // If the base is a reference
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
    const Type * fieldType = dealias(field->getType());
    if (const ParameterDefn * param = dyn_cast<ParameterDefn>(field)) {
      fieldType = dealias(param->internalType());
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
  } else if (base->getType()->isReferenceType()) {
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
  diag.debug() << "Base address '" << base << "' has type '" << baseAddr->getType() << "'";
  if (!indices.empty()) {
    diag.debug() << "Base address '" << base << "' with indices {" <<
    indices << "} dereferences as '" <<
    getGEPType(baseAddr->getType(), indices.begin(), indices.end()) << "'";
  }
#endif

  // Assert that the type is what we expected.
  DASSERT_OBJ(in->getType() != NULL, in);
  if (!indices.empty()) {
    DASSERT_TYPE_EQ(
        in->getType()->irType(),
        getGEPType(baseAddr->getType(), indices.begin(), indices.end()));
  }

  return baseAddr;
}

Value * CodeGenerator::genNumericCast(CastExpr * in) {
  Value * value = genExpr(in->arg());
  if (value != NULL) {
    llvm::Instruction::CastOps castType;
    switch (in->exprType()) {
      case Expr::Truncate:
      castType = llvm::Instruction::Trunc;
      break;

      case Expr::SignExtend:
      castType = llvm::Instruction::SExt;
      break;

      case Expr::ZeroExtend:
      castType = llvm::Instruction::ZExt;
      break;

      default:
      DFAIL("IllegalState");
    }

    return builder_.CreateCast(castType, value, in->getType()->irType());
  }

  return NULL;
}

Value * CodeGenerator::genUpCast(CastExpr * in) {
  Value * value = genExpr(in->arg());
  Type * fromType = in->arg()->getType();
  Type * toType = in->getType();

  if (value != NULL && fromType != NULL && toType != NULL) {
    return genUpCastInstr(value, fromType, toType);
  }

  return NULL;
}

Value * CodeGenerator::genBitCast(CastExpr * in) {
  Value * value = genExpr(in->arg());
  Type * toType = in->getType();

  if (value != NULL && toType != NULL) {
    return builder_.CreateBitCast(
        value, PointerType::get(toType->irType(), 0), "bitcast");
  }

  return NULL;
}

Value * CodeGenerator::genUnionCtorCast(CastExpr * in) {
  Type * fromType = in->arg()->type();
  Type * toType = in->type();
  Value * value = NULL;

  if (!fromType->isVoidType()) {
    value = genExpr(in->arg());
    if (value == NULL) {
      return NULL;
    }
  }

  if (toType != NULL) {
    UnionType * utype = cast<UnionType>(toType);

    if (utype->numValueTypes() > 0) {
      int index = utype->getTypeIndex(fromType);
      Value * indexVal = ConstantInt::get(utype->irType()->getContainedType(0), index);

      Value * uvalue = builder_.CreateAlloca(utype->irType());
      builder_.CreateStore(indexVal, builder_.CreateConstGEP2_32(uvalue, 0, 0));
      if (value != NULL) {
        const llvm::Type * fieldType = fromType->irType();
        if (fromType->isReferenceType()) {
          fieldType = llvm::PointerType::get(fieldType, 0);
        }
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
    } else {
      // The type returned from irType() is a pointer type.
      //Value * uvalue = builder_.CreateBitCast(utype->irType());
      return builder_.CreateBitCast(value, utype->irType());
    }
  }

  return NULL;
}

Value * CodeGenerator::genUnionMemberCast(CastExpr * in) {
  // Retrieve a value from a union. Presumes that the type-test has already been done.
  Type * fromType = in->arg()->getType();
  Type * toType = in->getType();
  Value * value = genLValueAddress(in->arg());

  if (value == NULL) {
    return NULL;
  }

  if (fromType != NULL) {
    UnionType * utype = cast<UnionType>(fromType);
    if (utype->numValueTypes() > 0) {
      int index = utype->getTypeIndex(toType);
      const llvm::Type * fieldType = toType->irType();
      if (toType->isReferenceType()) {
        fieldType = llvm::PointerType::get(fieldType, 0);
      }

      return builder_.CreateLoad(
          builder_.CreateBitCast(
              builder_.CreateConstGEP2_32(value, 0, 1),
              PointerType::get(fieldType, 0)));
    } else {
      // The union contains only pointer types, so we know that its representation is simply
      // a single pointer, so a bit cast will work.
      return builder_.CreateBitCast(value, PointerType::get(toType->irType(), 0));
    }
  }

  return NULL;
}

Value * CodeGenerator::genCall(FnCallExpr * in) {
  FunctionDefn * fn = in->function();

  if (fn->isIntrinsic()) {
    return fn->intrinsic()->generate(*this, in);
  }

  ValueList args;

  Value * selfArg = NULL;
  if (in->selfArg() != NULL) {
    selfArg = genExpr(in->selfArg());
    if (fn->storageClass() == Storage_Instance) {
      args.push_back(selfArg);
    }
  }

  ExprList & inArgs = in->args();
  for (ExprList::iterator it = inArgs.begin(); it != inArgs.end(); ++it) {
    args.push_back(genExpr(*it));
  }

  // TODO: VCalls and ICalls.

  // Generate the function to call.
  Value * fnVal;
  if (in->exprType() == Expr::VTableCall) {
    DASSERT_OBJ(selfArg != NULL, in);
    const Type * classType = dealias(fn->functionType()->selfParam()->getType());
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

  indices.clear();
  indices.push_back(getInt32Val(0));
  indices.push_back(getInt32Val(3));
  indices.push_back(getInt32Val(methodIndex));
  Value * fptr = builder_.CreateLoad(
      builder_.CreateGEP(tib, indices.begin(), indices.end()), method->name());
  return builder_.CreateBitCast(fptr, PointerType::getUnqual(method->type()->irType()));
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
  GlobalVariable * itype = getTypeObjectPtr(classType);

  // Load the pointer to the TIB.
  Value * tib = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(objectPtr, 0, 0, "tib_ptr"), "tib");

  // Load the pointer to the dispatcher function.
  Value * dispatcher = builder_.CreateLoad(
      builder_.CreateConstInBoundsGEP2_32(tib, 0, 2, "idispatch_ptr"), "idispatch");

  // Construct the call to the dispatcher
  ValueList args;
  args.push_back(itype);
  args.push_back(getInt32Val(methodIndex));
  Value * methodPtr = genCallInstr(dispatcher, args.begin(), args.end(), "method_ptr");
  return builder_.CreateBitCast(
      methodPtr, PointerType::getUnqual(method->type()->irType()), "method");
}

Value * CodeGenerator::genNew(NewExpr * in) {
  if (CompositeType * ctdef = dyn_cast<CompositeType>(in->getType())) {
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
    Function * f = currentFunction_->irFunction();
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
    return builder_.CreateBitCast(val, PointerType::get(toType->irType(), 0), "intf_ptr");
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

  return builder_.CreateGEP(val, indices.begin(), indices.end(), "upcast");
}

llvm::Value * CodeGenerator::genStringLiteral(const std::string & strval) {
  StringLiteralMap::iterator it = stringLiteralMap_.find(strval);
  if (it != stringLiteralMap_.end()) {
    return it->second;
  }

  const CompositeType * strType = dyn_cast<CompositeType>(Builtins::typeString);
  const llvm::Type * irType = strType->irType();

  Constant * strVal = ConstantArray::get(context_, strval, false);
  llvm::Type * charDataType = ArrayType::get(builder_.getInt8Ty(), 0);

  // Self-referential member values
  UndefValue * strDataStart = UndefValue::get(PointerType::getUnqual(charDataType));
  UndefValue * strSource = UndefValue::get(PointerType::getUnqual(irType));

  // Object type members
  std::vector<Constant *> objMembers;
  objMembers.push_back(getTypeInfoPtr(strType));

  // String type members
  std::vector<Constant *> members;
  members.push_back(ConstantStruct::get(context_, objMembers));
  members.push_back(getInt32Val(strval.size()));
  members.push_back(strSource);
  members.push_back(strDataStart);
  members.push_back(ConstantArray::get(context_, strval, false));

  Constant * strStruct = ConstantStruct::get(context_, members);
  Constant * strConstant = llvm::ConstantExpr::getPointerCast(
      new GlobalVariable(*irModule_,
          strStruct->getType(), true,
          GlobalValue::InternalLinkage,
          strStruct, "string"),
      PointerType::getUnqual(irType));

  Constant * indices[2];
  indices[0] = getInt32Val(0);
  indices[1] = getInt32Val(4);

  strDataStart->replaceAllUsesWith(llvm::ConstantExpr::getGetElementPtr(strConstant, indices, 2));
  strSource->replaceAllUsesWith(strConstant);

  stringLiteralMap_[strval] = strConstant;
  return strConstant;
}

Value * CodeGenerator::genArrayLiteral(const ArrayLiteralExpr * in) {
  const CompositeType * arrayType = cast<CompositeType>(in->getType());
  const Type * elementType = arrayType->typeDefn()->templateInstance()->paramValues()[0];
  size_t arrayLength = in->args().size();

  const llvm::Type * etype = elementType->irType();
  if (elementType->isReferenceType()) {
    etype = PointerType::getUnqual(etype);
  }

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

Value * CodeGenerator::genCompositeTypeTest(Value * val, CompositeType * fromType,
    CompositeType * toType) {
  DASSERT(fromType != NULL);
  DASSERT(toType != NULL);

  // Make sure it's a class.
  DASSERT(toType->typeClass() == Type::Class || toType->typeClass() == Type::Interface);
  Constant * toTypeObj = getTypeObjectPtr(toType);

  // Bitcast to object type
  Value * valueAsObjType = builder_.CreateBitCast(val,
      PointerType::getUnqual(Builtins::typeObject->irType()));

  // Upcase to type 'object' and load the TIB pointer.
  ValueList indices;
  indices.push_back(getInt32Val(0));
  indices.push_back(getInt32Val(0));
  Value * tib = builder_.CreateLoad(
      builder_.CreateGEP(valueAsObjType, indices.begin(), indices.end()),
      "tib");

  ValueList args;
  args.push_back(tib);
  args.push_back(toTypeObj);
  Function * upcastTest = genFunctionValue(Builtins::funcHasBase);
  Value * result = builder_.CreateCall(upcastTest, args.begin(), args.end());
  return result;
}

Value * CodeGenerator::genUnionTypeTest(llvm::Value * val, UnionType * fromType, Type * toType) {
  DASSERT(fromType != NULL);
  DASSERT(toType != NULL);

  // The index of the actual type.
  Value * actualTypeIndex = builder_.CreateExtractValue(val, 0);

  int testIndex = fromType->getTypeIndex(toType);
  Constant * testIndexValue = ConstantInt::get(actualTypeIndex->getType(), testIndex);

  if (index == 0 && fromType->numRefTypes() != 0) {
    DFAIL("Add special handling for reference types.");
  }

  return builder_.CreateICmpEQ(actualTypeIndex, testIndexValue, "isa");
}

llvm::Constant * CodeGenerator::genSizeOf(Type * type, bool memberSize) {
  ValueList indices;
  indices.push_back(getInt32Val(1));

  const llvm::Type * irType = type->irType();
  if (memberSize && type->isReferenceType()) {
    irType = PointerType::get(irType, 0);
  }

  return llvm::ConstantExpr::getPtrToInt(
      llvm::ConstantExpr::getGetElementPtr(
          ConstantPointerNull::get(PointerType::get(irType, 0)),
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
  resultType = PointerType::get(resultType, 0);

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

  if (isa<PointerType>(sizeValue->getType())) {
    if (Constant * c = dyn_cast<Constant>(sizeValue)) {
      sizeValue = llvm::ConstantExpr::getPtrToInt(c, builder_.getInt32Ty());
    } else {
      sizeValue = builder_.CreatePtrToInt(sizeValue, builder_.getInt32Ty());
    }
  }

  std::stringstream labelStream;
  FormatStream fs(labelStream);
  fs << objType;
  Value * alloc = builder_.CreateMalloc(builder_.getInt8Ty(), sizeValue, labelStream.str().c_str());
  Value * instance = builder_.CreateBitCast(alloc, resultType);

  if (const CompositeType * classType = dyn_cast<CompositeType>(objType)) {
    genInitObjVTable(classType, instance);
  }

  return instance;
}

} // namespace tart

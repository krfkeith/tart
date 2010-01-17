/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/UnitType.h"
#include "tart/CFG/Constant.h"
#include "tart/CFG/Block.h"
#include "tart/Sema/EvalPass.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// EvalPass

Expr * EvalPass::eval(Expr * in, bool allowPartial) {
  allowPartial = false;
  return EvalPass(allowPartial).evalExpr(in);
}

Expr * EvalPass::evalExpr(Expr * in) {
  if (in == NULL) {
    return NULL;
  }

  switch (in->exprType()) {
    case Expr::Invalid:
      return &Expr::ErrorVal;

    case Expr::TypeCount:
      DFAIL("Invalid");

    case Expr::NoOp:
      return in;

    case Expr::ConstInt:
    case Expr::ConstFloat:
    case Expr::ConstString:
    case Expr::ConstNull:
    case Expr::ConstObjRef:
    case Expr::TypeLiteral:
      return in;

#if 0
    case Expr::Instance:
      return evalInstance(static_cast<ConstantObjectRef *>(in));
#endif

    case Expr::LValue:
      return evalLValue(static_cast<LValueExpr *>(in));

#if 0
    case Expr::BoundMethod:
      return evalBoundMethod(static_cast<BoundMethodExpr *>(in));

    case Expr::ScopeName:
      return evalScopeName(static_cast<ScopeNameExpr *>(in));

    case Expr::ElementRef:
      return evalElementRef(static_cast<BinaryExpr *>(in));
#endif

    case Expr::Assign:
      return evalAssign(static_cast<AssignmentExpr *>(in));

#if 0
    case Expr::PostAssign:
      return evalPostAssign(static_cast<AssignmentExpr *>(in));

    case Expr::MultiAssign:
      ??

    case Expr::Call:
    case Expr::ExactCall:
    //case Expr::ICall:
    case Expr::Construct:
      return evalCall(static_cast<CallExpr *>(in));
#endif

    case Expr::FnCall:
    case Expr::CtorCall:
      return evalFnCall(static_cast<FnCallExpr *>(in));

    case Expr::New:
      return evalNew(static_cast<NewExpr *>(in));

#if 0
    case Expr::Instantiate:
      return evalInstantiate(static_cast<InstantiateExpr *>(in));

    case Expr::ImplicitCast:
    case Expr::ExplicitCast:
    case Expr::UpCast:
    case Expr::TryCast:
    case Expr::DynamicCast:
    case Expr::Truncate:
    case Expr::SignExtend:
    case Expr::ZeroExtend:
    case Expr::IntToFloat:
    case Expr::BitCast:
    case Expr::UnionMemberCast:
      return evalCast(static_cast<CastExpr *>(in));
#endif
    case Expr::UnionCtorCast:
      return evalUnionCtorCast(static_cast<CastExpr *>(in));

    case Expr::BinaryOpcode:
      return evalBinaryOpcode(static_cast<BinaryOpcodeExpr *>(in));

#if 0
    case Expr::Compare:
      return evalCompare(static_cast<CompareExpr *>(in));

    case Expr::InstanceOf:
      return evalInstanceOf(static_cast<InstanceOfExpr *>(in));

    case Expr::RefEq:
      return evalRefEq(static_cast<BinaryExpr *>(in));

    case Expr::PtrDeref:
      return evalPtrDeref(static_cast<UnaryExpr *>(in));

    case Expr::Not:
      return evalNot(static_cast<UnaryExpr *>(in));

    case Expr::InitVar:
      return evalInitVar(static_cast<InitVarExpr *>(in));

    case Expr::Prog2:
      return evalProg2(static_cast<BinaryExpr *>(in));
#endif

    case Expr::ArrayLiteral:
      return evalArrayLiteral(static_cast<ArrayLiteralExpr *>(in));

#if 0
    case Expr::IRValue:
      return in;

    case Expr::PatternVar:
      DFAIL("PatternVar");

    case Expr::PtrCall:
      DFAIL("PtrCall");
#endif

    default:
      break;
  }

  diag.error(in) << "Expr type not handled: " << exprTypeName(in->exprType()) << " : " << in;
  DFAIL("Fall through");
}

ConstantExpr * EvalPass::evalConstantExpr(Expr * in) {
  Expr * e = evalExpr(in);
  if (e != NULL) {
    if (ConstantExpr * ce = dyn_cast<ConstantExpr>(e)) {
      return ce;
    } else if (!allowPartial_) {
      diag.error(in) << "Not a constant: " << in;
    }
  }

  return NULL;
}

bool EvalPass::evalBlocks(BlockList & blocks) {
  Block * block = blocks.front();

  for (;;) {
    for (ExprList::iterator it = block->exprs().begin(); it != block->exprs().end(); ++it) {
      evalExpr(*it);
    }

    switch (block->terminator()) {
      case BlockTerm_None:
        DFAIL("Invalid block term");
        break;

      case BlockTerm_Branch:
        block = block->succs()[0];
        break;

      case BlockTerm_Return: {
        if (block->termValue() != NULL) {
          callFrame_->setReturnVal(evalExpr(block->termValue()));
        } else {
          callFrame_->setReturnVal(NULL);
        }

        return true;
      }

      case BlockTerm_Conditional:
      case BlockTerm_Switch:
      case BlockTerm_Throw:
      case BlockTerm_ResumeUnwind:
      case BlockTerm_LocalReturn:
      case BlockTerm_Catch:
      case BlockTerm_TraceCatch:
        DFAIL("block term not handled");
        break;
    }
  }
}

Expr * EvalPass::evalFnCall(FnCallExpr * in) {
  FunctionDefn * func = in->function();
  CallFrame frame;
  frame.setFunction(func);
  frame.setSelfArg(evalExpr(in->selfArg()));
  for (ExprList::iterator it = in->args().begin(); it != in->args().end(); ++it) {
    Expr * arg = evalExpr(*it);
    if (arg == NULL) {
      return NULL;
    }

    frame.args().push_back(arg);
  }

  Expr * evalResult = func->eval(in->location(), frame.selfArg(), frame.args());
  if (evalResult != NULL) {
    return evalResult;
  }

  if (!func->hasBody()) {
    if (!allowPartial_) {
      diag.error(in) << "Cannot evaluate function " << func << " at compile time";
    }

    return NULL;
  }

  if (!AnalyzerBase::analyzeFunction(func, Task_PrepEvaluation)) {
    return NULL;
  }

  DASSERT_OBJ(!func->blocks().empty(), func);

  CallFrame * prevFrame = setCallFrame(&frame);
  evalBlocks(func->blocks());
  setCallFrame(prevFrame);

  if (in->exprType() == Expr::CtorCall) {
    return frame.selfArg();
  }

  return frame.returnVal();
}

Expr * EvalPass::evalLValue(LValueExpr * in) {
  if (ParameterDefn * param = dyn_cast<ParameterDefn>(in->value())) {
    FunctionType * ftype = callFrame_->function()->functionType();
    if (param == ftype->selfParam()) {
      return callFrame_->selfArg();
    }

    for (size_t i = 0; i < ftype->params().size(); ++i) {
      if (ftype->params()[i] == param) {
        return callFrame_->args()[i];
      }
    }

    DFAIL("Couldn't locate param");
  } else  if (VariableDefn * var = dyn_cast<VariableDefn>(in->value())) {
    if (var->defnType() == Defn::Let) {
      if (var->storageClass() == Storage_Global || var->storageClass() == Storage_Static) {
        return evalExpr(var->initValue());
      }
    }

    switch (var->storageClass()) {
      case Storage_Global:
        DFAIL("IMPLEMENT Storage_Global");
        break;

      case Storage_Instance:
        DFAIL("IMPLEMENT Storage_Instance");
        break;

      case Storage_Class:
        DFAIL("IMPLEMENT Storage_Class");
        break;

      case Storage_Static:
        diag.debug(var) << var;
        DFAIL("IMPLEMENT Storage_Static");
        break;

      case Storage_Local:
        DFAIL("IMPLEMENT Storage_Local");
        break;

      case Storage_Param:
        DFAIL("Invalid storage class");
        break;

      case Storage_Closure:
        DFAIL("IMPLEMENT Storage_Closure");
        break;
    }
  }

  DFAIL("Not an LValue");
}

Expr * EvalPass::evalNew(NewExpr * in) {
  const CompositeType * type = cast<CompositeType>(in->type());
  if (!AnalyzerBase::analyzeTypeDefn(type->typeDefn(), Task_PrepEvaluation)) {
    return NULL;
  }

  return new ConstantObjectRef(in->location(), type);
}

Expr * EvalPass::evalAssign(AssignmentExpr * in) {
  Expr * from = evalExpr(in->fromExpr());
  if (from == NULL) {
    return NULL;
  }

  store(from, in->toExpr());
  return in->toExpr();
}

void EvalPass::store(Expr * value, Expr * dest) {
  if (dest->exprType() == Expr::LValue) {
    LValueExpr * lvalue = static_cast<LValueExpr *>(dest);
    if (VariableDefn * var = dyn_cast<VariableDefn>(lvalue->value())) {
      if (var->defnType() == Defn::Let) {
        if (var->storageClass() == Storage_Global || var->storageClass() == Storage_Static) {
          DFAIL("Invalid assignment to constant");
          return;
        }
      }

      switch (var->storageClass()) {
        case Storage_Global:
          DFAIL("IMPLEMENT Storage_Global");
          break;

        case Storage_Instance: {
          DASSERT(lvalue->base() != NULL);
          Expr * base = evalExpr(lvalue->base());
          if (base == NULL) {
            return;
          }

          if (ConstantObjectRef * inst = dyn_cast<ConstantObjectRef>(base)) {
            inst->setMemberValue(var, value);
          } else {
            diag.fatal(base) << "Base not handled " << base;
          }
          break;
        }

        case Storage_Class:
          DFAIL("IMPLEMENT Storage_Class");
          break;

        case Storage_Static:
          diag.debug(var) << var;
          DFAIL("IMPLEMENT Storage_Static");
          break;

        case Storage_Local:
          DFAIL("IMPLEMENT Storage_Local");
          break;

        case Storage_Param:
          DFAIL("Invalid storage class");
          break;

        case Storage_Closure:
          DFAIL("IMPLEMENT Storage_Closure");
          break;
      }
    } else if (ParameterDefn * param = dyn_cast<ParameterDefn>(lvalue->value())) {
      DFAIL("Implement");
    }
  } else {
    DFAIL("Implement");
  }
}

Expr * EvalPass::evalArrayLiteral(ArrayLiteralExpr * in) {
  const CompositeType * arrayType = cast<CompositeType>(in->type());
  if (!AnalyzerBase::analyzeType(arrayType, Task_PrepEvaluation)) {
    return NULL;
  }

  const Type * elementType = arrayType->typeParam(0);
  TypeList naTypeArgs;
  naTypeArgs.push_back(const_cast<Type *>(elementType));
  naTypeArgs.push_back(UnitType::get(
      ConstantInteger::get(in->location(), &UInt64Type::instance, in->args().size())));
  ConstantNativeArray * arrayData =
      new ConstantNativeArray(
          in->location(),
          NativeArrayType::get(TupleType::get(naTypeArgs)));
  for (ExprList::iterator it = in->args().begin(); it != in->args().end(); ++it) {
    Expr * element = elementType->implicitCast((*it)->location(), evalExpr(*it));
    if (element == NULL) {
      return NULL;
    }

    arrayData->elements().push_back(element);

    //*it = element;
  }

  // Constant array objects are special because of their variable size.
  ConstantObjectRef * arrayObj = new ConstantObjectRef(in->location(), arrayType);
  arrayObj->setMemberValue("_length",
      ConstantInteger::getUInt(arrayData->elements().size())->at(in->location()));
  arrayObj->setMemberValue("_data", arrayData);
  return arrayObj;
}

Expr * EvalPass::evalUnionCtorCast(CastExpr *in) {
  const Type * fromType = in->arg()->type();
  const Type * toType = in->type();
  Expr * value = NULL;

  if (!fromType->isVoidType()) {
    value = evalExpr(in->arg());
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

      DFAIL("Implement");
#if 0
      Value * indexVal = ConstantInt::get(utype->irType()->getContainedType(0), index);
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
    } else {
      in->setArg(value);
      return in;
    }
  }

  return NULL;
}

Expr * EvalPass::evalBinaryOpcode(BinaryOpcodeExpr *in) {
  llvm::Constant * c0 = asConstNumber(evalConstantExpr(in->first()));
  llvm::Constant * c1 = asConstNumber(evalConstantExpr(in->second()));
  if (c0 == NULL || c1 == NULL) {
    return NULL;
  }

  llvm::Constant * cresult = llvm::ConstantExpr::get(in->opCode(), c0, c1);

  switch (in->opCode()) {
    case llvm::Instruction::Add:
      break;

    default:
      break;
  }

  DFAIL("Implement");
}

llvm::Constant * EvalPass::asConstNumber(ConstantExpr * e) {
  if (e == NULL) {
    return NULL;
  } else if (ConstantInteger * ci = dyn_cast<ConstantInteger>(e)) {
    return ci->value();
  } else if (ConstantFloat * cf = dyn_cast<ConstantFloat>(e)) {
    return cf->value();
  } else if (allowPartial_) {
    return NULL;
  } else {
    diag.error(e) << "Not a number " << e;
    return NULL;
  }
}

} // namespace tart

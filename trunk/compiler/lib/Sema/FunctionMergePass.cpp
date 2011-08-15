/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/FunctionMergePass.h"
#include "tart/Sema/AnalyzerBase.h"

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Expr/Closure.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/UnionType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Intrinsic.h"

namespace tart {

/// -------------------------------------------------------------------
/// FunctionMergePass

bool FunctionMergePass::visit(FunctionDefn * from, FunctionDefn * to) {
  int indentLevel = diag.getIndentLevel();

  if (showMessages_) {
    diag.debug() << Format_Verbose << "Merging " << from;
    diag.indent();
  }

//  BlockList & fromBlocks = from->blocks();
//  BlockList & toBlocks = to->blocks();

//  int index = 0;
//  for (BlockList::iterator bi = fromBlocks.begin(); bi != fromBlocks.end(); ++bi) {
//    (*bi)->setIndex(++index);
//  }
//
//  index = 0;
//  for (BlockList::iterator bi = toBlocks.begin(); bi != toBlocks.end(); ++bi) {
//    (*bi)->setIndex(++index);
//  }

//  size_t blkCount = fromBlocks.size();
//  if (toBlocks.size() != blkCount) {
//    diag.debug() << "Merge failure: Difference in block count: " <<
//        fromBlocks.size() << " vs " << toBlocks.size();
//    diag.setIndentLevel(indentLevel);
//    return false;
//  }

//  for (size_t i = 0; i < blkCount; ++i) {
//    if (!visitBlock(fromBlocks[i], toBlocks[i])) {
//      diag.setIndentLevel(indentLevel);
//      return false;
//    }
//  }
  if (!visitExpr(from->body(), to->body())) {
    return false;
  }

  diag.setIndentLevel(indentLevel);
  return true;
}

bool FunctionMergePass::visitExpr(Expr * from, Expr * to) {
  if (from == NULL && to == NULL) {
    return true;
  }

  if (from == NULL || to == NULL) {
    diag.debug() << "Merge failure: null expression";
    return false;
  }

  if (from->exprType() != to->exprType()) {
    reportDifference("Difference in exprType", from, to);
    return false;
  }

  switch (from->exprType()) {
//    case Expr::Invalid:
//      return &Expr::ErrorVal;
//
//    case Expr::TypeCount:
//      DFAIL("Invalid");
//
    case Expr::NoOp:
      return true;

    case Expr::ConstInt:
    case Expr::ConstFloat:
    case Expr::ConstString:
      return static_cast<ConstantExpr *>(from)->isEqual(static_cast<ConstantExpr *>(to));

//    case Expr::TypeLiteral:
//      return visitTypeLiteral(static_cast<TypeLiteralExpr *>(from), static_cast<TypeLiteralExpr *>(to));
//
    case Expr::ConstNull:
      return true;

    case Expr::ConstObjRef:
      //      return visitConstantObjectRef(static_cast<ConstantObjectRef *>(from), static_cast<ConstantObjectRef *>(to));
      return false;

    case Expr::ConstEmptyArray:
      return from->type()->isEqual(to->type());

//    case Expr::ConstNArray:
//      return visitConstantNativeArray(static_cast<ConstantNativeArray *>(from), static_cast<ConstantNativeArray *>(to));

    case Expr::LValue:
      return visitLValue(static_cast<LValueExpr *>(from), static_cast<LValueExpr *>(to));

//    case Expr::BoundMethod:
//      return visitBoundMethod(static_cast<BoundMethodExpr *>(from), static_cast<BoundMethodExpr *>(to));
//
//    case Expr::ScopeName:
//      return visitScopeName(static_cast<ScopeNameExpr *>(from), static_cast<ScopeNameExpr *>(to));
//
    case Expr::ElementRef:
      return visitElementRef(static_cast<BinaryExpr *>(from), static_cast<BinaryExpr *>(to));

    case Expr::Assign:
    case Expr::PostAssign:
      return visitAssign(static_cast<AssignmentExpr *>(from), static_cast<AssignmentExpr *>(to));

//    case Expr::MultiAssign:
//      return visitMultiAssign(static_cast<MultiAssignExpr *>(from), static_cast<MultiAssignExpr *>(to));
//
//    case Expr::Call:
//    case Expr::ExactCall:
//    //case Expr::ICall:
//    case Expr::Construct:
//      return visitCall(static_cast<CallExpr *>(from), static_cast<CallExpr *>(to));
//
    case Expr::FnCall:
    case Expr::CtorCall:
    case Expr::VTableCall:
      return visitFnCall(static_cast<FnCallExpr *>(from), static_cast<FnCallExpr *>(to));

//    case Expr::IndirectCall:
//      return visitIndirectCall(static_cast<IndirectCallExpr *>(from), static_cast<IndirectCallExpr *>(to));
//
    case Expr::New:
      return visitNew(static_cast<NewExpr *>(from), static_cast<NewExpr *>(to));

//    case Expr::ImplicitCast:
//    case Expr::ExplicitCast:
    case Expr::UpCast:
//    case Expr::TryCast:
//    case Expr::DynamicCast:
//    case Expr::UnboxCast:
//    case Expr::Truncate:
    case Expr::SignExtend:
//    case Expr::ZeroExtend:
//    case Expr::IntToFloat:
//    case Expr::BitCast:
//    case Expr::UnionMemberCast:
//    case Expr::CheckedUnionMemberCast:
      return visitCast(static_cast<CastExpr *>(from), static_cast<CastExpr *>(to));

    case Expr::UnionCtorCast:
      return visitUnionCtorCast(static_cast<CastExpr *>(from), static_cast<CastExpr *>(to));

    case Expr::BinaryOpcode:
      return visitBinaryOpcode(static_cast<BinaryOpcodeExpr *>(from), static_cast<BinaryOpcodeExpr *>(to));

    case Expr::Compare:
      return visitCompare(static_cast<CompareExpr *>(from), static_cast<CompareExpr *>(to));

//    case Expr::InstanceOf:
//      return visitInstanceOf(static_cast<InstanceOfExpr *>(from), static_cast<InstanceOfExpr *>(to));
//
//    case Expr::RefEq:
//      return visitRefEq(static_cast<BinaryExpr *>(from), static_cast<BinaryExpr *>(to));
//
//    case Expr::PtrDeref:
//      return visitPtrDeref(static_cast<UnaryExpr *>(from), static_cast<UnaryExpr *>(to));
//
    case Expr::Not:
      return visitNot(static_cast<UnaryExpr *>(from), static_cast<UnaryExpr *>(to));

    case Expr::And:
    case Expr::Or:
      return visitLogicalOper(static_cast<BinaryExpr *>(from), static_cast<BinaryExpr *>(to));

    case Expr::Complement:
      return visitComplement(static_cast<UnaryExpr *>(from), static_cast<UnaryExpr *>(to));

    case Expr::InitVar:
      return visitInitVar(static_cast<InitVarExpr *>(from), static_cast<InitVarExpr *>(to));
//
//    case Expr::ClearVar:
//      return visitClearVar(static_cast<ClearVarExpr *>(from), static_cast<ClearVarExpr *>(to));
//
    case Expr::Prog2:
      return visitProg2(static_cast<BinaryExpr *>(from), static_cast<BinaryExpr *>(to));

    case Expr::ArrayLiteral:
      return visitArrayLiteral(
          static_cast<ArrayLiteralExpr *>(from),
          static_cast<ArrayLiteralExpr *>(to));

//    case Expr::TupleCtor:
//      return visitTupleCtor(static_cast<TupleCtorExpr *>(from), static_cast<TupleCtorExpr *>(to));
//
//    case Expr::ClosureEnv:
//      return visitClosureScope(static_cast<ClosureEnvExpr *>(from), static_cast<ClosureEnvExpr *>(to));
//
//    case Expr::IRValue:
//      return in;
//
//    case Expr::SharedValue:
//      return visitSharedValue(static_cast<SharedValueExpr *>(from), static_cast<SharedValueExpr *>(to));
//
//    case Expr::PatternVar:
//      DFAIL("PatternVar");

    case Expr::Seq:
      return visitSeq(static_cast<SeqExpr *>(from), static_cast<SeqExpr *>(to));

    case Expr::If:
      return visitIf(static_cast<IfExpr *>(from), static_cast<IfExpr *>(to));

    case Expr::For:
      return visitFor(static_cast<ForExpr *>(from), static_cast<ForExpr *>(to));

    case Expr::Throw:
      return visitThrow(static_cast<ThrowExpr *>(from), static_cast<ThrowExpr *>(to));

    case Expr::Return:
      return visitReturn(static_cast<ReturnExpr *>(from), static_cast<ReturnExpr *>(to));

    case Expr::LocalProcedure:
      return visitLocalProcedure(
          static_cast<LocalProcedureExpr *>(from), static_cast<LocalProcedureExpr *>(to));

    case Expr::LocalReturn:
      return visitLocalReturn(static_cast<ReturnExpr *>(from), static_cast<ReturnExpr *>(to));

    case Expr::Break:
    case Expr::Continue:
      return true;

    default:
      break;
  }

  diag.error(from) << "Expr type not handled: " << exprTypeName(from->exprType());
  DFAIL("Fall through");
}

#if 0
bool FunctionMergePass::visitConstantObjectRef(ConstantObjectRef * from, ConstantObjectRef * to) {
  ExprList & members = in->members();
  for (ExprList::iterator it = members.begin(); it != members.end(); ++it) {
    *it = visitExpr(*it);
  }

  return in;
}

bool FunctionMergePass::visitConstantNativeArray(ConstantNativeArray * from, ConstantNativeArray * to) {
  ExprList & elements = in->elements();
  for (ExprList::iterator it = elements.begin(); it != elements.end(); ++it) {
    *it = visitExpr(*it);
  }

  return in;
}
#endif

bool FunctionMergePass::visitLValue(LValueExpr * from, LValueExpr * to) {
  if (from->base() != NULL) {
    if (!visitExpr(from->base(), to->base())) {
      reportDifference(from, to);
      return false;
    }
  }

  if (from->value() != to->value()) {
    ValueDefn * fromVal = from->value();
    ValueDefn * toVal = to->value();
    if (fromVal->ast() == toVal->ast()) {
      if (fromVal->defnType() == Defn::Let ||
          fromVal->defnType() == Defn::Var ||
          fromVal->defnType() == Defn::Parameter) {
        //diag.debug() << Format_Verbose << "var " << from->value() << " == " << to->value();
        return true;
      } else if (fromVal->defnType() == Defn::MacroArg) {
        VariableDefn * fromVar = static_cast<VariableDefn *>(fromVal);
        VariableDefn * toVar = static_cast<VariableDefn *>(toVal);
        //diag.info() << Format_Verbose << "macro arg " << from->value() << " == " << to->value();
        DASSERT(fromVar->initValue() != NULL);
        return visitExpr(fromVar->initValue(), toVar->initValue());
      }
    }

    reportDifference("Difference in l-value", from->value(), to->value());
//    diag.info() << Format_Verbose << "Cannot merge l-value " << from->value() <<
//        " with " << to->value();
    return false;
  }

  return true;
}

#if 0
bool FunctionMergePass::visitBoundMethod(BoundMethodExpr * from, BoundMethodExpr * to) {
  in->setSelfArg(visitExpr(in->selfArg()));
  return in;
}

bool FunctionMergePass::visitScopeName(ScopeNameExpr * from, ScopeNameExpr * to) {
  return in;
}
#endif

bool FunctionMergePass::visitElementRef(BinaryExpr * from, BinaryExpr * to) {
  return visitExpr(to->first(), from->first()) && visitExpr(to->second(), from->second());
}

bool FunctionMergePass::visitAssign(AssignmentExpr * from, AssignmentExpr * to) {
  return visitExpr(from->fromExpr(), to->fromExpr()) && visitExpr(from->toExpr(), to->toExpr());
}

#if 0
bool FunctionMergePass::visitMultiAssign(MultiAssignExpr * from, MultiAssignExpr * to) {
  visitExprArgs(in);
  return in;
}

bool FunctionMergePass::visitCall(CallExpr * from, CallExpr * to) {
  //in->setFunction(visitExpr(in->function()));
  visitExprArgs(in);
  return in;
}
#endif

bool FunctionMergePass::visitFnCall(FnCallExpr * from, FnCallExpr * to) {
  if (!visitFunctionRef(from->function(), to->function())) {
    reportDifference("Difference in function reference", from->function(), to->function());
    return false;
  }

  if (!visitExpr(from->selfArg(), to->selfArg())) {
    reportDifference(from, to);
    return false;
  }

  return visitExprArgs(from, to);
}

#if 0
bool FunctionMergePass::visitIndirectCall(CallExpr * from, CallExpr * to) {
  visitExpr(in->function());
  //in->setSelfArg(visitExpr(in->selfArg()));
  visitExprArgs(in);
  return in;
}
#endif

bool FunctionMergePass::visitNew(NewExpr * from, NewExpr * to) {
  if (from->type() != to->type()) {
    reportDifference("Difference in type of new expr", from, to);
    return false;
  }

  return true;
}

bool FunctionMergePass::visitCast(CastExpr * from, CastExpr * to) {
  if (!areTypesCompatible(from->type(), to->type())) {
    reportDifference("Difference in type of cast expr", from, to);
    return false;
  }

  return visitExpr(from->arg(), to->arg());
}

bool FunctionMergePass::visitUnionCtorCast(CastExpr * from, CastExpr * to) {
  if (!areTypesCompatible(from->type(), to->type())) {
    reportDifference("Difference in type of union cast expr", from, to);
    return false;
  }

  return visitExpr(from->arg(), to->arg());
}

bool FunctionMergePass::visitBinaryOpcode(BinaryOpcodeExpr * from, BinaryOpcodeExpr * to) {
  return visitExpr(from->first(), to->first()) && visitExpr(from->second(), to->second());
}

bool FunctionMergePass::visitCompare(CompareExpr * from, CompareExpr * to) {
  return visitExpr(from->first(), to->first()) && visitExpr(from->second(), to->second());
}

#if 0
bool FunctionMergePass::visitInstanceOf(InstanceOfExpr * from, InstanceOfExpr * to) {
  in->setValue(visitExpr(in->value()));
  return in;
}

bool FunctionMergePass::visitRefEq(BinaryExpr * from, BinaryExpr * to) {
  in->setFirst(visitExpr(in->first()));
  in->setSecond(visitExpr(in->second()));
  return in;
}

bool FunctionMergePass::visitPtrDeref(UnaryExpr * from, UnaryExpr * to) {
  in->setArg(visitExpr(in->arg()));
  return in;
}
#endif

bool FunctionMergePass::visitLogicalOper(BinaryExpr * from, BinaryExpr * to) {
  return visitExpr(from->first(), to->first()) && visitExpr(from->second(), to->second());
}

bool FunctionMergePass::visitNot(UnaryExpr * from, UnaryExpr * to) {
  return visitExpr(from->arg(), to->arg());
}

bool FunctionMergePass::visitComplement(UnaryExpr * from, UnaryExpr * to) {
  return visitExpr(from->arg(), to->arg());
}

bool FunctionMergePass::visitInitVar(InitVarExpr * from, InitVarExpr * to) {
  VariableDefn * fromVar = from->var();
  VariableDefn * toVar = to->var();
  StringRef fromName(fromVar->name());
  if (!fromName.equals(toVar->name())) {
    reportDifference("Difference in variable name", fromVar, toVar);
    return false;
  }

  if (fromVar->type()->typeClass() != toVar->type()->typeClass()) {
    reportDifference("Difference in variable type", fromVar, toVar);
    return false;
  }

  // For now, we only support reference types.
  if (fromVar->type() != toVar->type()) {
    if (!fromVar->type()->isReferenceType()) {
      reportDifference("Non-reference type", fromVar, toVar);
      return false;
    }
  }

  return visitExpr(from->initExpr(), to->initExpr());
}

bool FunctionMergePass::visitProg2(BinaryExpr * from, BinaryExpr * to) {
  return visitExpr(from->first(), to->first()) &&
         visitExpr(from->second(), to->second());
}

bool FunctionMergePass::visitArrayLiteral(ArrayLiteralExpr * from, ArrayLiteralExpr * to) {
  if (!from->type()->isEqual(to->type())) {
    return false;
  }

  if (from->argCount() != to->argCount()) {
    return false;
  }

  for (size_t i = 0, end = from->argCount(); i < end; ++i) {
    if (!visitExpr(from->arg(i), to->arg(i))) {
      return false;
    }
  }

  return true;
}

bool FunctionMergePass::visitSeq(SeqExpr * from, SeqExpr * to) {
  if (from->argCount() != to->argCount()) {
    return false;
  }

  for (size_t i = 0; i < from->argCount(); ++i) {
    if (!visitExpr(from->arg(i), to->arg(i))) {
      return false;
    }
  }

  return true;
}

bool FunctionMergePass::visitIf(IfExpr * from, IfExpr * to) {
  return visitExpr(from->test(), to->test()) &&
      visitExpr(from->thenVal(), to->thenVal()) &&
      visitExpr(from->elseVal(), to->elseVal());
}

bool FunctionMergePass::visitFor(ForExpr * from, ForExpr * to) {
  return visitExpr(from->init(), to->init()) &&
      visitExpr(from->test(), to->test()) &&
      visitExpr(from->incr(), to->incr()) &&
      visitExpr(from->body(), to->body());
}

bool FunctionMergePass::visitReturn(ReturnExpr * from, ReturnExpr * to) {
  return visitExpr(from->arg(), to->arg());
}

bool FunctionMergePass::visitThrow(ThrowExpr * from, ThrowExpr * to) {
  return visitExpr(from->arg(), to->arg());
}

bool FunctionMergePass::visitLocalProcedure(LocalProcedureExpr * from, LocalProcedureExpr * to) {
  return visitExpr(from->arg(), to->arg());
}

bool FunctionMergePass::visitLocalReturn(ReturnExpr * from, ReturnExpr * to) {
  return visitExpr(from->arg(), to->arg());
}

#if 0
bool FunctionMergePass::visitClearVar(ClearVarExpr * from, ClearVarExpr * to) {
  return in;
}

bool FunctionMergePass::visitProg2(BinaryExpr * from, BinaryExpr * to) {
  in->setFirst(visitExpr(in->first()));
  in->setSecond(visitExpr(in->second()));
  return in;
}

bool FunctionMergePass::visitArrayLiteral(ArrayLiteralExpr * from, ArrayLiteralExpr * to) {
  visitExprArgs(in);
  return in;
}

bool FunctionMergePass::visitTupleCtor(TupleCtorExpr * from, TupleCtorExpr * to) {
  visitExprArgs(in);
  return in;
}

bool FunctionMergePass::visitSharedValue(SharedValueExpr * from, SharedValueExpr * to) {
  in->setArg(visitExpr(in->arg()));
  return in;
}
#endif

bool FunctionMergePass::visitExprArgs(ArglistExpr * from, ArglistExpr * to) {
  ExprList & fromArgs = from->args();
  ExprList & toArgs = to->args();
  if (fromArgs.size() != toArgs.size()) {
    reportDifference("Difference in variable expr arg count", from, to);
    return false;
  }

  for (size_t i = 0; i < fromArgs.size(); ++i) {
    if (!visitExpr(fromArgs[i], toArgs[i])) {
      reportDifference(from, to);
      return false;
    }
  }

  return true;
}

#if 0
bool FunctionMergePass::visitClosureScope(ClosureEnvExpr * from, ClosureEnvExpr * to) {
  return in;
}
#endif

bool FunctionMergePass::visitFunctionRef(FunctionDefn * from, FunctionDefn * to) {
  if (from == to) {
    return true;
  }

  if (from->isIntrinsic() && from->intrinsic() == to->intrinsic()) {
    if (from->intrinsic()->canCoalesce()) {
      return true;
    }
  }

  if (from->hasTrait(Defn::Mergeable)) {
    AnalyzerBase::analyzeFunction(from, Task_PrepCodeGeneration);
    if (from->mergeTo() == to) {
      return true;
    }
  }

  return false;
}

bool FunctionMergePass::areTypesCompatible(const Type * from, const Type * to) {
  if (from->typeClass() != to->typeClass()) {
    if ((from->typeClass() == Type::Class || from->typeClass() == Type::Interface) &&
        (to->typeClass() == Type::Class || to->typeClass() == Type::Interface)) {
      return true;
    }

    return false;
  }

  switch (from->typeClass()) {
    case Type::Primitive: {
      const PrimitiveType * pFrom = static_cast<const PrimitiveType *>(from);
      const PrimitiveType * pTo = static_cast<const PrimitiveType *>(to);
      if (pFrom->typeId() == pTo->typeId()) {
        return true;
      }

      return false;
    }

    case Type::Class:
    case Type::Interface:
      // Any reference type is compatible with any other.
      return true;

    case Type::Tuple: {
      const TupleType * ttFrom = static_cast<const TupleType *>(from);
      const TupleType * ttTo = static_cast<const TupleType *>(to);
      if (ttFrom->members().size() != ttTo->members().size()) {
        return false;
      }

      size_t memberSize = ttFrom->members().size();
      for (size_t i = 0; i < memberSize; ++i) {
        if (!areTypesCompatible(ttFrom->members()[i], ttTo->members()[i])) {
          return false;
        }
      }

      return true;
    }


    case Type::Union: {
      const UnionType * fromType = cast<UnionType>(from);
      const UnionType * toType = cast<UnionType>(to);
      return areTypesCompatible(fromType->typeArgs(), toType->typeArgs());
    }

    default:
      diag.debug() << "Type not handled: " << from;
      return false;
  }
}

void FunctionMergePass::reportDifference(const char * msg, Formattable * from, Formattable * to) {
  if (showMessages_) {
    diag.debug() << msg << ":";
    reportDifference(from, to);
  }
}

void FunctionMergePass::reportDifference(Formattable * from, Formattable * to) {
  if (showMessages_) {
    diag.indent();
    diag.debug() << Format_Verbose << from;
    diag.debug() << Format_Verbose << to;
  }
}

} // namespace tart

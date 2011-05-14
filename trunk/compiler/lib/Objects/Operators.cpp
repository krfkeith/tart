/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Type/StaticType.h"
#include "tart/Expr/Constant.h"
#include "tart/Defn/Module.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Objects/Builtins.h"
#include "tart/Common/Diagnostics.h"

#include "tart/Defn/Template.h"
#include "tart/Type/FunctionType.h"

#include "llvm/Instructions.h"

namespace tart {

namespace {
using namespace llvm;

inline bool areBothConstInts(const Expr * a0, const Expr * a1) {
  return a0->exprType() == Expr::ConstInt &&
      a1->exprType() == Expr::ConstInt;
}

inline bool areBothConstFloats(const Expr * a0, const Expr * a1) {
  return a0->exprType() == Expr::ConstFloat &&
      a1->exprType() == Expr::ConstFloat;
}

/** A class representing a binary operator on integer types.
    The operator takes two arguments and returns a single value,
    all of which are the same type.
*/
template<class T, Instruction::BinaryOps opCode>
class IntegerBinOpFunction : public FunctionDefn {
public:
  IntegerBinOpFunction(const char * name)
      : FunctionDefn(NULL, name, &StaticFnType2<T, T, T>::value) {}

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 2);
    Expr * arg0 = args[0];
    Expr * arg1 = args[1];

    DASSERT_OBJ(arg0->type()->isIntType(), arg0);
    DASSERT_OBJ(arg1->type()->isIntType(), arg1);

    if (areBothConstInts(arg0, arg1)) {
      ConstantInteger * c0 = static_cast<ConstantInteger *>(arg0);
      ConstantInteger * c1 = static_cast<ConstantInteger *>(arg1);
      DASSERT(c0->type() == c1->type());
      return new ConstantInteger(
            c0->location() | c1->location(),
            c0->type(),
            cast<ConstantInt>(
                llvm::ConstantExpr::get(opCode, c0->value(), c1->value())));
    } else {
      return new BinaryOpcodeExpr(opCode, loc, &T::instance, arg0, arg1);
    }
  }
};

/** The builtin add operator. */
template<class T>
class OperatorAddDecl {
public:
  static IntegerBinOpFunction<T, Instruction::Add> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::Add> OperatorAddDecl<T>::value("infixAdd");

/** The builtin subtract operator. */
template<class T>
class OperatorSubDecl {
public:
  static IntegerBinOpFunction<T, Instruction::Sub> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::Sub> OperatorSubDecl<T>::value("infixSubtract");

/** The builtin multiply operator. */
template<class T>
class OperatorMulDecl {
public:
  static IntegerBinOpFunction<T, Instruction::Mul> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::Mul> OperatorMulDecl<T>::value("infixMultiply");

/** The builtin divide operator (signed). */
template<class T>
class OperatorSDivDecl {
public:
  static IntegerBinOpFunction<T, Instruction::SDiv> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::SDiv> OperatorSDivDecl<T>::value("infixDivide");

/** The builtin divide operator (unsigned). */
template<class T>
class OperatorUDivDecl {
public:
  static IntegerBinOpFunction<T, Instruction::UDiv> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::UDiv> OperatorUDivDecl<T>::value("infixDivide");

/** The builtin modulus operator (signed). */
template<class T>
class OperatorSModDecl {
public:
  static IntegerBinOpFunction<T, Instruction::SRem> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::SRem> OperatorSModDecl<T>::value("infixModulus");

/** The builtin modulus operator (unsigned). */
template<class T>
class OperatorUModDecl {
public:
  static IntegerBinOpFunction<T, Instruction::URem> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::URem> OperatorUModDecl<T>::value("infixModulus");

/** The builtin bitwise 'or' operator. */
template<class T>
class BitwiseOrDecl {
public:
  static IntegerBinOpFunction<T, Instruction::Or> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::Or> BitwiseOrDecl<T>::value("infixBitOr");

/** The builtin bitwise 'and' operator. */
template<class T>
class BitwiseAndDecl {
public:
  static IntegerBinOpFunction<T, Instruction::And> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::And> BitwiseAndDecl<T>::value("infixBitAnd");

/** The builtin bitwise 'xor' operator. */
template<class T>
class BitwiseXorDecl {
public:
  static IntegerBinOpFunction<T, Instruction::Xor> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::Xor> BitwiseXorDecl<T>::value("infixBitXor");

/** The builtin shift left operator. */
template<class T>
class OperatorLShiftDecl {
public:
  static IntegerBinOpFunction<T, Instruction::Shl> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::Shl> OperatorLShiftDecl<T>::value("infixLShift");

/** The builtin arithmetic shift right operator. */
template<class T>
class OperatorARShiftDecl {
public:
  static IntegerBinOpFunction<T, Instruction::AShr> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::AShr> OperatorARShiftDecl<T>::value("infixRShift");

/** The builtin logical shift right operator. */
template<class T>
class OperatorRShiftDecl {
public:
  static IntegerBinOpFunction<T, Instruction::LShr> value;
};

template<class T>
IntegerBinOpFunction<T, Instruction::LShr> OperatorRShiftDecl<T>::value("infixRShift");

/** A class representing a binary operator on floating point types.
    The operator takes two arguments and returns a single value,
    all of which are the same type.
*/
template<class T, Instruction::BinaryOps opCode>
class FloatBinOpFunction : public FunctionDefn {
public:
  FloatBinOpFunction(const char * name)
      : FunctionDefn(NULL, name, &StaticFnType2<T, T, T>::value) {}

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 2);
    Expr * arg0 = args[0];
    Expr * arg1 = args[1];

    DASSERT_OBJ(arg0->type()->isFPType(), arg0);
    DASSERT_OBJ(arg1->type()->isFPType(), arg1);

    if (areBothConstFloats(arg0, arg1)) {
      ConstantFloat * c0 = static_cast<ConstantFloat *>(arg0);
      ConstantFloat * c1 = static_cast<ConstantFloat *>(arg1);
      DASSERT(c0->type() == c1->type());
      return new ConstantFloat(
            c0->location() | c1->location(),
            c0->type(),
            cast<ConstantFP>(
                llvm::ConstantExpr::get(opCode, c0->value(), c1->value())));
    } else {
      return new BinaryOpcodeExpr(opCode, loc, &T::instance, arg0, arg1);
    }
  }
};

/** The builtin add operator (float). */
template<class T>
class OperatorFAddDecl {
public:
  static FloatBinOpFunction<T, Instruction::FAdd> value;
};

template<class T>
FloatBinOpFunction<T, Instruction::FAdd> OperatorFAddDecl<T>::value("infixAdd");

/** The builtin subtract operator (float). */
template<class T>
class OperatorFSubDecl {
public:
  static FloatBinOpFunction<T, Instruction::FSub> value;
};

template<class T>
FloatBinOpFunction<T, Instruction::FSub> OperatorFSubDecl<T>::value("infixSubtract");

/** The builtin multiply operator (float). */
template<class T>
class OperatorFMulDecl {
public:
  static FloatBinOpFunction<T, Instruction::FMul> value;
};

template<class T>
FloatBinOpFunction<T, Instruction::FMul> OperatorFMulDecl<T>::value("infixMultiply");

/** The builtin divide operator (float). */
template<class T>
class OperatorFDivDecl {
public:
  static FloatBinOpFunction<T, Instruction::FDiv> value;
};

template<class T>
FloatBinOpFunction<T, Instruction::FDiv> OperatorFDivDecl<T>::value("infixDivide");

/** The builtin modulus operator (float). */
template<class T>
class OperatorFModDecl {
public:
  static FloatBinOpFunction<T, Instruction::FRem> value;
};

template<class T>
FloatBinOpFunction<T, Instruction::FRem> OperatorFModDecl<T>::value("infixModulus");

/** Comparison op */
template<class T, CmpInst::Predicate pred>
class ComparisonOp : public FunctionDefn {
public:
  ComparisonOp(const char * name)
      : FunctionDefn(NULL, name, &StaticFnType2<BoolType, T, T>::value) {}

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 2);
    Expr * arg0 = args[0];
    Expr * arg1 = args[1];

    if (areBothConstInts(arg0, arg1)) {
      const ConstantInteger * c0 = static_cast<const ConstantInteger *>(arg0);
      const ConstantInteger * c1 = static_cast<const ConstantInteger *>(arg1);
      DASSERT(c0->type() == c1->type());
      return new ConstantInteger(
            c0->location() | c1->location(),
            &BoolType::instance,
            cast<ConstantInt>(
              llvm::ConstantExpr::getCompare(pred, c0->value(), c1->value())));
    } else if (areBothConstFloats(arg0, arg1)) {
      const ConstantFloat * c0 = static_cast<const ConstantFloat *>(arg0);
      const ConstantFloat * c1 = static_cast<const ConstantFloat *>(arg1);
      DASSERT(c0->type() == c1->type());
      return new ConstantInteger(
            c0->location() | c1->location(),
            &BoolType::instance,
            cast<ConstantInt>(
              llvm::ConstantExpr::getCompare(pred, c0->value(), c1->value())));
    } else {
      return new CompareExpr(loc, pred, arg0, arg1);
    }
  }

  static ComparisonOp value;
};

/** Integer comparison. */
template<class T, CmpInst::Predicate pred>
class OperatorCmp {
public:
  static ComparisonOp<T, pred> equal;
  static ComparisonOp<T, pred> unequal;
  static ComparisonOp<T, pred> less;
  static ComparisonOp<T, pred> greater;
  static ComparisonOp<T, pred> lessOrEqual;
  static ComparisonOp<T, pred> greaterOrEqual;
};

template<class T, CmpInst::Predicate pred>
ComparisonOp<T, pred> OperatorCmp<T, pred>::equal("infixEqual");

template<class T, CmpInst::Predicate pred>
ComparisonOp<T, pred> OperatorCmp<T, pred>::unequal("infixNotEqual");

template<class T, CmpInst::Predicate pred>
ComparisonOp<T, pred> OperatorCmp<T, pred>::less("infixLT");

template<class T, CmpInst::Predicate pred>
ComparisonOp<T, pred> OperatorCmp<T, pred>::greater("infixGT");

template<class T, CmpInst::Predicate pred>
ComparisonOp<T, pred> OperatorCmp<T, pred>::lessOrEqual("infixLE");

template<class T, CmpInst::Predicate pred>
ComparisonOp<T, pred> OperatorCmp<T, pred>::greaterOrEqual("infixGE");

/** The negation unary operator. In LLVM, negation is implemented by
    subtractions from zero.
*/
template<class T>
class NegateOp : public FunctionDefn {
public:
  NegateOp()
      : FunctionDefn(NULL, "unaryNegate", &StaticFnType1<T, T>::value) {}

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 1);
    Expr * arg = args[0];

    if (arg->exprType() == Expr::ConstInt) {
      const ConstantInteger * cn = static_cast<const ConstantInteger *>(arg);
      return new ConstantInteger(
            cn->location(),
            cn->type(),
            cast<ConstantInt>(llvm::ConstantExpr::getNeg(cn->value())));
    } else if (arg->exprType() == Expr::ConstFloat) {
      const ConstantFloat * cn = static_cast<const ConstantFloat *>(arg);
      return new ConstantFloat(
            cn->location(),
            cn->type(),
            cast<ConstantFP>(llvm::ConstantExpr::getFNeg(cn->value())));
    } else {
      const llvm::Type * argType = arg->type()->irType();
      if (arg->type()->isIntType()) {
        const llvm::IntegerType * intType = cast<llvm::IntegerType>(argType);
        ConstantInt * zero = ConstantInt::get(intType, 0, true);
        Expr * constantZero = new ConstantInteger(
            arg->location(),
            arg->type(),
            zero);
        return new BinaryOpcodeExpr(
            Instruction::Sub, loc, arg->type(),
            constantZero, arg);
      } else if (arg->type()->isFPType()) {
        Expr * constantZero = new ConstantFloat(
            arg->location(),
            arg->type(),
            cast<ConstantFP>(ConstantFP::getZeroValueForNegation(argType)));
        return new BinaryOpcodeExpr(
            Instruction::FSub, loc, arg->type(),
            constantZero, arg);
      } else {
        DFAIL("Invalid type");
      }
    }
  }

  static NegateOp value;
};

template<class T>
NegateOp<T> NegateOp<T>::value;

/** The negation operator for unsized integers. */
template<>
class NegateOp<UnsizedIntType> : public FunctionDefn {
public:
  NegateOp()
      : FunctionDefn(NULL, "unaryNegate",
            &StaticFnType1<UnsizedIntType, UnsizedIntType>::value) {}

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 1);
    Expr * arg = args[0];
    assert(arg->exprType() == Expr::ConstInt);
    ConstantInteger * cn = static_cast<ConstantInteger *>(arg);
    Constant * value = cn->value();

    const APInt & intVal = cast<ConstantInt>(value)->getValue();
    if (intVal.isAllOnesValue()) {
      // If the value is all ones, then in order to negate it losslessly, we need
      // to expand the bit width.
      int bitsRequired = intVal.getBitWidth();
      if (bitsRequired < 8) bitsRequired = 8;
      else if (bitsRequired < 16) bitsRequired = 16;
      else if (bitsRequired < 32) bitsRequired = 32;
      else if (bitsRequired < 64) bitsRequired = 64;
      else if (bitsRequired < 128) bitsRequired = 128;
      else {
        diag.fatal(loc) << "Number too large";
      }

      value = llvm::ConstantExpr::getSExt(value, IntegerType::get(llvm::getGlobalContext(), bitsRequired));
    }

    return new ConstantInteger(
          cn->location(),
          cn->type(),
          cast<ConstantInt>(llvm::ConstantExpr::getNeg(value)));
  }

  static NegateOp value;
};

NegateOp<UnsizedIntType> NegateOp<UnsizedIntType>::value;

/** The successor operator. Calculates the successor of the current value. */
template<class T>
class SuccessorOp : public FunctionDefn {
public:
  SuccessorOp() : FunctionDefn(NULL, "successorOf", &StaticFnType1<T, T>::value) {}

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 1);
    Expr * arg = args[0];

    const llvm::IntegerType * intType = cast<llvm::IntegerType>(T::instance.irType());
    ConstantInt * one = ConstantInt::get(intType, 1, true);
    if (arg->exprType() == Expr::ConstInt) {
      const ConstantInteger * cn = static_cast<const ConstantInteger *>(arg);
      return new ConstantInteger(
            cn->location(),
            cn->type(),
            cast<ConstantInt>(llvm::ConstantExpr::getAdd(cn->value(), one)));
    } else {
      Expr * constantOne = new ConstantInteger(
          arg->location(),
          arg->type(),
          one);
      return new BinaryOpcodeExpr(
          Instruction::Add, loc, arg->type(),
          arg, constantOne);
    }
  }

  static SuccessorOp value;
};

template<class T>
SuccessorOp<T> SuccessorOp<T>::value;

/** The predececessor operator. Calculates the predececessor of the current value. */
template<class T>
class PredeccessorOp : public FunctionDefn {
public:
  PredeccessorOp() : FunctionDefn(NULL, "predeccessorOf", &StaticFnType1<T, T>::value) {}

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 1);
    Expr * arg = args[0];

    const llvm::IntegerType * intType = cast<llvm::IntegerType>(T::instance.irType());
    ConstantInt * one = ConstantInt::get(intType, 1, true);
    if (arg->exprType() == Expr::ConstInt) {
      const ConstantInteger * cn = static_cast<const ConstantInteger *>(arg);
      return new ConstantInteger(
            cn->location(),
            cn->type(),
            cast<ConstantInt>(llvm::ConstantExpr::getSub(cn->value(), one)));
    } else {
      Expr * constantOne = new ConstantInteger(
            arg->location(),
            arg->type(),
            one);
      return new BinaryOpcodeExpr(
          Instruction::Sub, loc, arg->type(),
          arg, constantOne);
    }
  }

  static PredeccessorOp value;
};

template<class T>
PredeccessorOp<T> PredeccessorOp<T>::value;

static SourceString infixAddrCmpSrc(
    " @tart.annex.Intrinsic def infixEqual[%T](:__Address[T], :__Address[T]) -> bool;"
    " @tart.annex.Intrinsic def infixLT[%T](:__Address[T], :__Address[T]) -> bool;"
    " @tart.annex.Intrinsic def infixLE[%T](:__Address[T], :__Address[T]) -> bool;"
    " @tart.annex.Intrinsic def infixGT[%T](:__Address[T], :__Address[T]) -> bool;"
    " @tart.annex.Intrinsic def infixGE[%T](:__Address[T], :__Address[T]) -> bool;"
    );

// TODO: Implement this
//    " @tart.annex.Intrinsic"
//    " def infixEqual[%T](:T, :T) -> bool;"

static SourceString infixNotEqualSrc(
    " def infixNotEqual[%T](p0:T, p1:T) -> bool { return not (p0 == p1); }");

static SourceString infixLogicalSrc(
    " @tart.annex.Intrinsic def infixLogicalAnd(:bool, :bool) -> bool;"
    " @tart.annex.Intrinsic def infixLogicalOr(:bool, :bool) -> bool;"
    );

static SourceString infixAddSrc(
    " @tart.annex.Intrinsic def infixAdd[%T](:__Address[T],  :int32) -> __Address[T];"
    " @tart.annex.Intrinsic def infixAdd[%T](:__Address[T],  :int64) -> __Address[T];"
    " @tart.annex.Intrinsic def infixAdd[%T](:__Address[T], :uint32) -> __Address[T];"
    " @tart.annex.Intrinsic def infixAdd[%T](:__Address[T], :uint64) -> __Address[T];"
    );

} // namespace

void Builtins::initOperators() {
  using namespace llvm;

  module.addMember(&OperatorAddDecl<Int8Type>::value);
  module.addMember(&OperatorAddDecl<Int16Type>::value);
  module.addMember(&OperatorAddDecl<Int32Type>::value);
  module.addMember(&OperatorAddDecl<Int64Type>::value);
  module.addMember(&OperatorAddDecl<UInt8Type>::value);
  module.addMember(&OperatorAddDecl<UInt16Type>::value);
  module.addMember(&OperatorAddDecl<UInt32Type>::value);
  module.addMember(&OperatorAddDecl<UInt64Type>::value);
  module.addMember(&OperatorFAddDecl<FloatType>::value);
  module.addMember(&OperatorFAddDecl<DoubleType>::value);
  module.addMember(&OperatorAddDecl<UnsizedIntType>::value);

  module.addMember(&OperatorSubDecl<Int8Type>::value);
  module.addMember(&OperatorSubDecl<Int16Type>::value);
  module.addMember(&OperatorSubDecl<Int32Type>::value);
  module.addMember(&OperatorSubDecl<Int64Type>::value);
  module.addMember(&OperatorSubDecl<UInt8Type>::value);
  module.addMember(&OperatorSubDecl<UInt16Type>::value);
  module.addMember(&OperatorSubDecl<UInt32Type>::value);
  module.addMember(&OperatorSubDecl<UInt64Type>::value);
  module.addMember(&OperatorFSubDecl<FloatType>::value);
  module.addMember(&OperatorFSubDecl<DoubleType>::value);
  module.addMember(&OperatorSubDecl<UnsizedIntType>::value);

  module.addMember(&OperatorMulDecl<Int8Type>::value);
  module.addMember(&OperatorMulDecl<Int16Type>::value);
  module.addMember(&OperatorMulDecl<Int32Type>::value);
  module.addMember(&OperatorMulDecl<Int64Type>::value);
  module.addMember(&OperatorMulDecl<UInt8Type>::value);
  module.addMember(&OperatorMulDecl<UInt16Type>::value);
  module.addMember(&OperatorMulDecl<UInt32Type>::value);
  module.addMember(&OperatorMulDecl<UInt64Type>::value);
  module.addMember(&OperatorFMulDecl<FloatType>::value);
  module.addMember(&OperatorFMulDecl<DoubleType>::value);
  module.addMember(&OperatorMulDecl<UnsizedIntType>::value);

  module.addMember(&OperatorSDivDecl<Int8Type>::value);
  module.addMember(&OperatorSDivDecl<Int16Type>::value);
  module.addMember(&OperatorSDivDecl<Int32Type>::value);
  module.addMember(&OperatorSDivDecl<Int64Type>::value);
  module.addMember(&OperatorUDivDecl<UInt8Type>::value);
  module.addMember(&OperatorUDivDecl<UInt16Type>::value);
  module.addMember(&OperatorUDivDecl<UInt32Type>::value);
  module.addMember(&OperatorUDivDecl<UInt64Type>::value);
  module.addMember(&OperatorFDivDecl<FloatType>::value);
  module.addMember(&OperatorFDivDecl<DoubleType>::value);
  module.addMember(&OperatorSDivDecl<UnsizedIntType>::value);

  module.addMember(&OperatorSModDecl<Int8Type>::value);
  module.addMember(&OperatorSModDecl<Int16Type>::value);
  module.addMember(&OperatorSModDecl<Int32Type>::value);
  module.addMember(&OperatorSModDecl<Int64Type>::value);
  module.addMember(&OperatorUModDecl<UInt8Type>::value);
  module.addMember(&OperatorUModDecl<UInt16Type>::value);
  module.addMember(&OperatorUModDecl<UInt32Type>::value);
  module.addMember(&OperatorUModDecl<UInt64Type>::value);
  module.addMember(&OperatorFModDecl<FloatType>::value);
  module.addMember(&OperatorFModDecl<DoubleType>::value);
  module.addMember(&OperatorSModDecl<UnsizedIntType>::value);

  module.addMember(&OperatorCmp<BoolType,  CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<CharType,  CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<Int8Type, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<Int16Type, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<Int32Type, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<Int64Type, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<UInt8Type, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<UInt16Type, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<UInt32Type, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<UInt64Type, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<FloatType, CmpInst::FCMP_OEQ>::equal);
  module.addMember(&OperatorCmp<DoubleType, CmpInst::FCMP_OEQ>::equal);
  module.addMember(&OperatorCmp<UnsizedIntType, CmpInst::ICMP_EQ>::equal);

  module.addMember(&OperatorCmp<BoolType,  CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<CharType,  CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<Int8Type, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<Int16Type, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<Int32Type, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<Int64Type, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<UInt8Type, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<UInt16Type, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<UInt32Type, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<UInt64Type, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<FloatType, CmpInst::FCMP_ONE>::unequal);
  module.addMember(&OperatorCmp<DoubleType, CmpInst::FCMP_ONE>::unequal);
  module.addMember(&OperatorCmp<UnsizedIntType, CmpInst::ICMP_NE>::unequal);

  module.addMember(&OperatorCmp<CharType,  CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<Int8Type, CmpInst::ICMP_SLT>::less);
  module.addMember(&OperatorCmp<Int16Type, CmpInst::ICMP_SLT>::less);
  module.addMember(&OperatorCmp<Int32Type, CmpInst::ICMP_SLT>::less);
  module.addMember(&OperatorCmp<Int64Type, CmpInst::ICMP_SLT>::less);
  module.addMember(&OperatorCmp<UInt8Type, CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<UInt16Type, CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<UInt32Type, CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<UInt64Type, CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<FloatType, CmpInst::FCMP_OLT>::less);
  module.addMember(&OperatorCmp<DoubleType, CmpInst::FCMP_OLT>::less);
  module.addMember(&OperatorCmp<UnsizedIntType, CmpInst::ICMP_SLT>::less);

  module.addMember(&OperatorCmp<CharType,  CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<Int8Type, CmpInst::ICMP_SGT>::greater);
  module.addMember(&OperatorCmp<Int16Type, CmpInst::ICMP_SGT>::greater);
  module.addMember(&OperatorCmp<Int32Type, CmpInst::ICMP_SGT>::greater);
  module.addMember(&OperatorCmp<Int64Type, CmpInst::ICMP_SGT>::greater);
  module.addMember(&OperatorCmp<UInt8Type, CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<UInt16Type, CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<UInt32Type, CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<UInt64Type, CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<FloatType, CmpInst::FCMP_OGT>::greater);
  module.addMember(&OperatorCmp<DoubleType, CmpInst::FCMP_OGT>::greater);
  module.addMember(&OperatorCmp<UnsizedIntType, CmpInst::ICMP_SGT>::greater);

  module.addMember(&OperatorCmp<CharType,  CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<Int8Type, CmpInst::ICMP_SLE>::lessOrEqual);
  module.addMember(&OperatorCmp<Int16Type, CmpInst::ICMP_SLE>::lessOrEqual);
  module.addMember(&OperatorCmp<Int32Type, CmpInst::ICMP_SLE>::lessOrEqual);
  module.addMember(&OperatorCmp<Int64Type, CmpInst::ICMP_SLE>::lessOrEqual);
  module.addMember(&OperatorCmp<UInt8Type, CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<UInt16Type, CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<UInt32Type, CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<UInt64Type, CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<FloatType, CmpInst::FCMP_OLE>::lessOrEqual);
  module.addMember(&OperatorCmp<DoubleType, CmpInst::FCMP_OLE>::lessOrEqual);
  module.addMember(&OperatorCmp<UnsizedIntType, CmpInst::ICMP_SLE>::lessOrEqual);

  module.addMember(&OperatorCmp<CharType,  CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<Int8Type, CmpInst::ICMP_SGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<Int16Type, CmpInst::ICMP_SGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<Int32Type, CmpInst::ICMP_SGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<Int64Type, CmpInst::ICMP_SGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<UInt8Type, CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<UInt16Type, CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<UInt32Type, CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<UInt64Type, CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<FloatType, CmpInst::FCMP_OGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<DoubleType, CmpInst::FCMP_OGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<UnsizedIntType, CmpInst::ICMP_SGE>::greaterOrEqual);

  // TODO: Emit a warning about negating unsigneds...
  module.addMember(&NegateOp<Int8Type>::value);
  module.addMember(&NegateOp<Int16Type>::value);
  module.addMember(&NegateOp<Int32Type>::value);
  module.addMember(&NegateOp<Int64Type>::value);
  module.addMember(&NegateOp<UInt8Type>::value);
  module.addMember(&NegateOp<UInt16Type>::value);
  module.addMember(&NegateOp<UInt32Type>::value);
  module.addMember(&NegateOp<UInt64Type>::value);
  module.addMember(&NegateOp<FloatType>::value);
  module.addMember(&NegateOp<DoubleType>::value);
  module.addMember(&NegateOp<UnsizedIntType>::value);

  module.addMember(&SuccessorOp<CharType>::value);
  module.addMember(&SuccessorOp<Int8Type>::value);
  module.addMember(&SuccessorOp<Int16Type>::value);
  module.addMember(&SuccessorOp<Int32Type>::value);
  module.addMember(&SuccessorOp<Int64Type>::value);
  module.addMember(&SuccessorOp<UInt8Type>::value);
  module.addMember(&SuccessorOp<UInt16Type>::value);
  module.addMember(&SuccessorOp<UInt32Type>::value);
  module.addMember(&SuccessorOp<UInt64Type>::value);
  module.addMember(&SuccessorOp<UnsizedIntType>::value);

  module.addMember(&PredeccessorOp<CharType>::value);
  module.addMember(&PredeccessorOp<Int8Type>::value);
  module.addMember(&PredeccessorOp<Int16Type>::value);
  module.addMember(&PredeccessorOp<Int32Type>::value);
  module.addMember(&PredeccessorOp<Int64Type>::value);
  module.addMember(&PredeccessorOp<UInt8Type>::value);
  module.addMember(&PredeccessorOp<UInt16Type>::value);
  module.addMember(&PredeccessorOp<UInt32Type>::value);
  module.addMember(&PredeccessorOp<UInt64Type>::value);
  module.addMember(&PredeccessorOp<UnsizedIntType>::value);

  module.addMember(&BitwiseAndDecl<CharType>::value);
  module.addMember(&BitwiseAndDecl<Int8Type>::value);
  module.addMember(&BitwiseAndDecl<Int16Type>::value);
  module.addMember(&BitwiseAndDecl<Int32Type>::value);
  module.addMember(&BitwiseAndDecl<Int64Type>::value);
  module.addMember(&BitwiseAndDecl<UInt8Type>::value);
  module.addMember(&BitwiseAndDecl<UInt16Type>::value);
  module.addMember(&BitwiseAndDecl<UInt32Type>::value);
  module.addMember(&BitwiseAndDecl<UInt64Type>::value);
  module.addMember(&BitwiseAndDecl<UnsizedIntType>::value);

  module.addMember(&BitwiseOrDecl<CharType>::value);
  module.addMember(&BitwiseOrDecl<Int8Type>::value);
  module.addMember(&BitwiseOrDecl<Int16Type>::value);
  module.addMember(&BitwiseOrDecl<Int32Type>::value);
  module.addMember(&BitwiseOrDecl<Int64Type>::value);
  module.addMember(&BitwiseOrDecl<UInt8Type>::value);
  module.addMember(&BitwiseOrDecl<UInt16Type>::value);
  module.addMember(&BitwiseOrDecl<UInt32Type>::value);
  module.addMember(&BitwiseOrDecl<UInt64Type>::value);
  module.addMember(&BitwiseOrDecl<UnsizedIntType>::value);

  module.addMember(&BitwiseXorDecl<CharType>::value);
  module.addMember(&BitwiseXorDecl<Int8Type>::value);
  module.addMember(&BitwiseXorDecl<Int16Type>::value);
  module.addMember(&BitwiseXorDecl<Int32Type>::value);
  module.addMember(&BitwiseXorDecl<Int64Type>::value);
  module.addMember(&BitwiseXorDecl<UInt8Type>::value);
  module.addMember(&BitwiseXorDecl<UInt16Type>::value);
  module.addMember(&BitwiseXorDecl<UInt32Type>::value);
  module.addMember(&BitwiseXorDecl<UInt64Type>::value);
  module.addMember(&BitwiseXorDecl<UnsizedIntType>::value);

  module.addMember(&OperatorLShiftDecl<CharType>::value);
  module.addMember(&OperatorLShiftDecl<Int8Type>::value);
  module.addMember(&OperatorLShiftDecl<Int16Type>::value);
  module.addMember(&OperatorLShiftDecl<Int32Type>::value);
  module.addMember(&OperatorLShiftDecl<Int64Type>::value);
  module.addMember(&OperatorLShiftDecl<UInt8Type>::value);
  module.addMember(&OperatorLShiftDecl<UInt16Type>::value);
  module.addMember(&OperatorLShiftDecl<UInt32Type>::value);
  module.addMember(&OperatorLShiftDecl<UInt64Type>::value);
  module.addMember(&OperatorLShiftDecl<UnsizedIntType>::value);

  module.addMember(&OperatorARShiftDecl<CharType>::value);
  module.addMember(&OperatorARShiftDecl<Int8Type>::value);
  module.addMember(&OperatorARShiftDecl<Int16Type>::value);
  module.addMember(&OperatorARShiftDecl<Int32Type>::value);
  module.addMember(&OperatorARShiftDecl<Int64Type>::value);
  module.addMember(&OperatorRShiftDecl<UInt8Type>::value);
  module.addMember(&OperatorRShiftDecl<UInt16Type>::value);
  module.addMember(&OperatorRShiftDecl<UInt32Type>::value);
  module.addMember(&OperatorRShiftDecl<UInt64Type>::value);
  module.addMember(&OperatorARShiftDecl<UnsizedIntType>::value);

  compileBuiltins(infixAddrCmpSrc);
  compileBuiltins(infixNotEqualSrc);
  compileBuiltins(infixLogicalSrc);
  compileBuiltins(infixAddSrc);
}

}

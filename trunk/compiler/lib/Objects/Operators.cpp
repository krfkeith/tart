/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/StaticType.h"
#include "tart/CFG/Constant.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Objects/Builtins.h"
#include "tart/Common/Diagnostics.h"
#include <llvm/Instructions.h>

#include "tart/CFG/Template.h"
#include "tart/CFG/FunctionType.h"

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

/** A class representing a binary operator on primitive types.
    The operator takes two arguments and returns a single value,
    all of which are the same type.
*/
template<int typ, Instruction::BinaryOps opCode>
class BinaryOpFunction : public FunctionDefn {
public:
  BinaryOpFunction(const char * name)
      : FunctionDefn(NULL, name, &StaticFnType2<typ, typ, typ>::value) {}

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    assert(args.size() == 2);
    //const Expr * arg0 = derefDeclaredConstant(args[0]);
    //const Expr * arg1 = derefDeclaredConstant(args[1]);
    Expr * arg0 = args[0];
    Expr * arg1 = args[1];

    if (areBothConstInts(arg0, arg1)) {
      ConstantInteger * c0 = static_cast<ConstantInteger *>(arg0);
      ConstantInteger * c1 = static_cast<ConstantInteger *>(arg1);
      DASSERT(c0->getType() == c1->getType());
      return new ConstantInteger(
            c0->getLocation() | c1->getLocation(),
            c0->getType(),
            cast<ConstantInt>(
                llvm::ConstantExpr::get(opCode, c0->value(), c1->value())));
    } else if (areBothConstFloats(arg0, arg1)) {
      ConstantFloat * c0 = static_cast<ConstantFloat *>(arg0);
      ConstantFloat * c1 = static_cast<ConstantFloat *>(arg1);
      DASSERT(c0->getType() == c1->getType());
      return new ConstantFloat(
            c0->getLocation() | c1->getLocation(),
            c0->getType(),
            cast<ConstantFP>(
                llvm::ConstantExpr::get(opCode, c0->value(), c1->value())));
    } else {
      return new BinaryOpcodeExpr(opCode, loc, &StaticType<typ>::value,
          arg0, arg1);
    }
  }
};

/** The builtin add operator. */
template<int type>
class OperatorAddDecl {
public:
  static BinaryOpFunction<type, Instruction::Add> value;
};

template<int type>
BinaryOpFunction<type, Instruction::Add> OperatorAddDecl<type>::value("infixAdd");

/** The builtin subtract operator. */
template<int type>
class OperatorSubDecl {
public:
  static BinaryOpFunction<type, Instruction::Sub> value;
};

template<int type>
BinaryOpFunction<type, Instruction::Sub> OperatorSubDecl<type>::value("infixSubtract");

/** The builtin multiply operator. */
template<int type>
class OperatorMulDecl {
public:
  static BinaryOpFunction<type, Instruction::Mul> value;
};

template<int type>
BinaryOpFunction<type, Instruction::Mul> OperatorMulDecl<type>::value("infixMultiply");

/** The builtin divide operator (signed). */
template<int type>
class OperatorSDivDecl {
public:
  static BinaryOpFunction<type, Instruction::SDiv> value;
};

template<int type>
BinaryOpFunction<type, Instruction::SDiv> OperatorSDivDecl<type>::value("infixDivide");

/** The builtin divide operator (unsigned). */
template<int type>
class OperatorUDivDecl {
public:
  static BinaryOpFunction<type, Instruction::UDiv> value;
};

template<int type>
BinaryOpFunction<type, Instruction::UDiv> OperatorUDivDecl<type>::value("infixDivide");

/** The builtin divide operator (float). */
template<int type>
class OperatorFDivDecl {
public:
  static BinaryOpFunction<type, Instruction::FDiv> value;
};

template<int type>
BinaryOpFunction<type, Instruction::FDiv> OperatorFDivDecl<type>::value("infixDivide");

/** The builtin modulus operator (signed). */
template<int type>
class OperatorSModDecl {
public:
  static BinaryOpFunction<type, Instruction::SRem> value;
};

template<int type>
BinaryOpFunction<type, Instruction::SRem> OperatorSModDecl<type>::value("infixModulus");

/** The builtin modulus operator (unsigned). */
template<int type>
class OperatorUModDecl {
public:
  static BinaryOpFunction<type, Instruction::URem> value;
};

template<int type>
BinaryOpFunction<type, Instruction::URem> OperatorUModDecl<type>::value("infixModulus");

/** The builtin modulus operator (float). */
template<int type>
class OperatorFModDecl {
public:
  static BinaryOpFunction<type, Instruction::FRem> value;
};

template<int type>
BinaryOpFunction<type, Instruction::FRem> OperatorFModDecl<type>::value("infixModulus");

/** The builtin bitwise 'or' operator. */
template<int type>
class BitwiseOrDecl {
public:
  static BinaryOpFunction<type, Instruction::Or> value;
};

template<int type>
BinaryOpFunction<type, Instruction::Or> BitwiseOrDecl<type>::value("infixBitOr");

/** The builtin bitwise 'and' operator. */
template<int type>
class BitwiseAndDecl {
public:
  static BinaryOpFunction<type, Instruction::And> value;
};

template<int type>
BinaryOpFunction<type, Instruction::And> BitwiseAndDecl<type>::value("infixBitAnd");

/** The builtin bitwise 'xor' operator. */
template<int type>
class BitwiseXorDecl {
public:
  static BinaryOpFunction<type, Instruction::Xor> value;
};

template<int type>
BinaryOpFunction<type, Instruction::Xor> BitwiseXorDecl<type>::value("infixBitXor");

/** Comparison op */
template<int typ, CmpInst::Predicate pred>
class ComparisonOp : public FunctionDefn {
public:
  ComparisonOp(const char * name)
      : FunctionDefn(NULL, name, &StaticFnType2<TypeId_Bool, typ, typ>::value) {}

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    assert(args.size() == 2);
    //const Expr * arg0 = derefDeclaredConstant(args[0]);
    //const Expr * arg1 = derefDeclaredConstant(args[1]);
    Expr * arg0 = args[0];
    Expr * arg1 = args[1];

    if (areBothConstInts(arg0, arg1)) {
      ConstantInteger * c0 = static_cast<const ConstantInteger *>(arg0);
      ConstantInteger * c1 = static_cast<const ConstantInteger *>(arg1);
      DASSERT(c0->getType() == c1->getType());
      return new ConstantInteger(
            c0->getLocation() | c1->getLocation(),
            &BoolType::instance,
            cast<ConstantInt>(
              llvm::ConstantExpr::getCompare(pred, c0->value(), c1->value())));
    } else if (areBothConstFloats(arg0, arg1)) {
      ConstantFloat * c0 = static_cast<const ConstantFloat *>(arg0);
      ConstantFloat * c1 = static_cast<const ConstantFloat *>(arg1);
      DASSERT(c0->getType() == c1->getType());
      return new ConstantInteger(
            c0->getLocation() | c1->getLocation(),
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
template<int type, CmpInst::Predicate pred>
class OperatorCmp {
public:
  static ComparisonOp<type, pred> equal;
  static ComparisonOp<type, pred> unequal;
  static ComparisonOp<type, pred> less;
  static ComparisonOp<type, pred> greater;
  static ComparisonOp<type, pred> lessOrEqual;
  static ComparisonOp<type, pred> greaterOrEqual;
};

template<int type, CmpInst::Predicate pred>
ComparisonOp<type, pred> OperatorCmp<type, pred>::equal("infixEQ");

template<int type, CmpInst::Predicate pred>
ComparisonOp<type, pred> OperatorCmp<type, pred>::unequal("infixNE");

template<int type, CmpInst::Predicate pred>
ComparisonOp<type, pred> OperatorCmp<type, pred>::less("infixLT");

template<int type, CmpInst::Predicate pred>
ComparisonOp<type, pred> OperatorCmp<type, pred>::greater("infixGT");

template<int type, CmpInst::Predicate pred>
ComparisonOp<type, pred> OperatorCmp<type, pred>::lessOrEqual("infixLE");

template<int type, CmpInst::Predicate pred>
ComparisonOp<type, pred> OperatorCmp<type, pred>::greaterOrEqual("infixGE");

/** The negation unary operator. In LLVM, negation is implemented by
    subtractions from zero.
*/
template<int typ>
class NegateOp : public FunctionDefn {
public:
  NegateOp()
      : FunctionDefn(NULL, "unaryNegate", &StaticFnType1<typ, typ>::value) {}

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    assert(args.size() == 1);
    //const Expr * arg = derefDeclaredConstant(args[0]);
    Expr * arg = args[0];

    if (arg->exprType() == Expr::ConstInt) {
      ConstantInteger * cn = static_cast<const ConstantInteger *>(arg);
      return new ConstantInteger(
            cn->getLocation(),
            cn->getType(),
            cast<ConstantInt>(llvm::ConstantExpr::getNeg(cn->value())));
    } else if (arg->exprType() == Expr::ConstFloat) {
      ConstantFloat * cn = static_cast<const ConstantFloat *>(arg);
      return new ConstantFloat(
            cn->getLocation(),
            cn->getType(),
            cast<ConstantFP>(llvm::ConstantExpr::getNeg(cn->value())));
    } else {
      DFAIL("Implement");
      /*Expr * constantZero = ConstNumber::get(
            arg->getLocation(),
            cast<const PrimitiveType>(arg->getType()),
            Constant::getNullValue(arg->getType()->getIRType()));
      Expr * result = new BinaryExpr(Instruction::Sub, constantZero, arg);
      result->setType(arg->getType());
      return result;*/
    }
  }

  static NegateOp value;
};

template<int typ>
NegateOp<typ> NegateOp<typ>::value;

/** The negation operator for unsized integers. */
template<>
class NegateOp<TypeId_UnsizedInt> : public FunctionDefn {
public:
  NegateOp()
      : FunctionDefn(NULL, "unaryNegate",
            &StaticFnType1<TypeId_UnsizedInt, TypeId_UnsizedInt>::value)
  {}

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    assert(args.size() == 1);
    //const Expr * arg = derefDeclaredConstant(args[0]);
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
          cn->getLocation(),
          cn->getType(),
          cast<ConstantInt>(llvm::ConstantExpr::getNeg(value)));
  }

  static NegateOp value;
};

NegateOp<TypeId_UnsizedInt> NegateOp<TypeId_UnsizedInt>::value;

/** The successor operator. Calculates the successor of the current value. */
template<int typ>
class SuccessorOp : public FunctionDefn {
public:
  SuccessorOp()
      : FunctionDefn(NULL, "successorOf", &StaticFnType1<typ, typ>::value) {}

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    assert(args.size() == 1);
    //const Expr * arg = derefDeclaredConstant(args[0]);
    Expr * arg = args[0];

    const llvm::IntegerType * intType = cast<llvm::IntegerType>(tart::StaticType<typ>::value.getIRType());
    ConstantInt * one = ConstantInt::get(intType, 1, true);
    if (arg->exprType() == Expr::ConstInt) {
      ConstantInteger * cn = static_cast<const ConstantInteger *>(arg);
      return new ConstantInteger(
            cn->getLocation(),
            cn->getType(),
            cast<ConstantInt>(llvm::ConstantExpr::getAdd(cn->value(), one)));
    } else {
      Expr * constantOne = new ConstantInteger(
          arg->getLocation(),
          arg->getType(),
          one);
      return new BinaryOpcodeExpr(
          Instruction::Add, loc, arg->getType(),
          arg, constantOne);
    }
  }

  static SuccessorOp value;
};

template<int typ>
SuccessorOp<typ> SuccessorOp<typ>::value;

/** The predececessor operator. Calculates the predececessor of the current value. */
template<int typ>
class PredeccessorOp : public FunctionDefn {
public:
  PredeccessorOp()
      : FunctionDefn(NULL, "predeccesorOf", &StaticFnType1<typ, typ>::value) {}

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    assert(args.size() == 1);
    //const Expr * arg = derefDeclaredConstant(args[0]);
    Expr * arg = args[0];

    const llvm::IntegerType * intType = cast<llvm::IntegerType>(tart::StaticType<typ>::value.getIRType());
    ConstantInt * one = ConstantInt::get(intType, 1, true);
    if (arg->exprType() == Expr::ConstInt) {
      ConstantInteger * cn = static_cast<const ConstantInteger *>(arg);
      return new ConstantInteger(
            cn->getLocation(),
            cn->getType(),
            cast<ConstantInt>(llvm::ConstantExpr::getSub(cn->value(), one)));
    } else {
      Expr * constantOne = new ConstantInteger(
            arg->getLocation(),
            arg->getType(),
            one);
      return new BinaryOpcodeExpr(
          Instruction::Sub, loc, arg->getType(),
          arg, constantOne);
    }
  }

  static PredeccessorOp value;
};

template<int typ>
PredeccessorOp<typ> PredeccessorOp<typ>::value;

static SourceString infixEqSrc(
    " @Intrinsic"
    " def infixEQ[%T](:NativePointer<[T]>, :NativePointer<[T]>) -> bool;"
    " @Intrinsic"
    " def infixNE[%T](:NativePointer<[T]>, :NativePointer<[T]>) -> bool;");

static SourceString infixNeSrc(
    " def infixNE[%T](p0:T, p1:T) -> bool { return not (p0 == p1); }");

} // namespace

void Builtins::initOperators() {
  using namespace llvm;

  module.addMember(&OperatorAddDecl<TypeId_SInt8>::value);
  module.addMember(&OperatorAddDecl<TypeId_SInt16>::value);
  module.addMember(&OperatorAddDecl<TypeId_SInt32>::value);
  module.addMember(&OperatorAddDecl<TypeId_SInt64>::value);
  module.addMember(&OperatorAddDecl<TypeId_UInt8>::value);
  module.addMember(&OperatorAddDecl<TypeId_UInt16>::value);
  module.addMember(&OperatorAddDecl<TypeId_UInt32>::value);
  module.addMember(&OperatorAddDecl<TypeId_UInt64>::value);
  module.addMember(&OperatorAddDecl<TypeId_Float>::value);
  module.addMember(&OperatorAddDecl<TypeId_Double>::value);
  module.addMember(&OperatorAddDecl<TypeId_UnsizedInt>::value);

  module.addMember(&OperatorSubDecl<TypeId_SInt8>::value);
  module.addMember(&OperatorSubDecl<TypeId_SInt16>::value);
  module.addMember(&OperatorSubDecl<TypeId_SInt32>::value);
  module.addMember(&OperatorSubDecl<TypeId_SInt64>::value);
  module.addMember(&OperatorSubDecl<TypeId_UInt8>::value);
  module.addMember(&OperatorSubDecl<TypeId_UInt16>::value);
  module.addMember(&OperatorSubDecl<TypeId_UInt32>::value);
  module.addMember(&OperatorSubDecl<TypeId_UInt64>::value);
  module.addMember(&OperatorSubDecl<TypeId_Float>::value);
  module.addMember(&OperatorSubDecl<TypeId_Double>::value);
  module.addMember(&OperatorSubDecl<TypeId_UnsizedInt>::value);

  module.addMember(&OperatorMulDecl<TypeId_SInt8>::value);
  module.addMember(&OperatorMulDecl<TypeId_SInt16>::value);
  module.addMember(&OperatorMulDecl<TypeId_SInt32>::value);
  module.addMember(&OperatorMulDecl<TypeId_SInt64>::value);
  module.addMember(&OperatorMulDecl<TypeId_UInt8>::value);
  module.addMember(&OperatorMulDecl<TypeId_UInt16>::value);
  module.addMember(&OperatorMulDecl<TypeId_UInt32>::value);
  module.addMember(&OperatorMulDecl<TypeId_UInt64>::value);
  module.addMember(&OperatorMulDecl<TypeId_Float>::value);
  module.addMember(&OperatorMulDecl<TypeId_Double>::value);
  module.addMember(&OperatorMulDecl<TypeId_UnsizedInt>::value);

  module.addMember(&OperatorSDivDecl<TypeId_SInt8>::value);
  module.addMember(&OperatorSDivDecl<TypeId_SInt16>::value);
  module.addMember(&OperatorSDivDecl<TypeId_SInt32>::value);
  module.addMember(&OperatorSDivDecl<TypeId_SInt64>::value);
  module.addMember(&OperatorUDivDecl<TypeId_UInt8>::value);
  module.addMember(&OperatorUDivDecl<TypeId_UInt16>::value);
  module.addMember(&OperatorUDivDecl<TypeId_UInt32>::value);
  module.addMember(&OperatorUDivDecl<TypeId_UInt64>::value);
  module.addMember(&OperatorFDivDecl<TypeId_Float>::value);
  module.addMember(&OperatorFDivDecl<TypeId_Double>::value);
  module.addMember(&OperatorSDivDecl<TypeId_UnsizedInt>::value);

  module.addMember(&OperatorSModDecl<TypeId_SInt8>::value);
  module.addMember(&OperatorSModDecl<TypeId_SInt16>::value);
  module.addMember(&OperatorSModDecl<TypeId_SInt32>::value);
  module.addMember(&OperatorSModDecl<TypeId_SInt64>::value);
  module.addMember(&OperatorUModDecl<TypeId_UInt8>::value);
  module.addMember(&OperatorUModDecl<TypeId_UInt16>::value);
  module.addMember(&OperatorUModDecl<TypeId_UInt32>::value);
  module.addMember(&OperatorUModDecl<TypeId_UInt64>::value);
  module.addMember(&OperatorFModDecl<TypeId_Float>::value);
  module.addMember(&OperatorFModDecl<TypeId_Double>::value);
  module.addMember(&OperatorSModDecl<TypeId_UnsizedInt>::value);

  // TODO: Add bool and pointer types
  module.addMember(&OperatorCmp<TypeId_Bool,  CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_Char,  CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_SInt8, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_SInt16, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_SInt32, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_SInt64, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_UInt8, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_UInt16, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_UInt32, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_UInt64, CmpInst::ICMP_EQ>::equal);
  module.addMember(&OperatorCmp<TypeId_Float, CmpInst::FCMP_OEQ>::equal);
  module.addMember(&OperatorCmp<TypeId_Double, CmpInst::FCMP_OEQ>::equal);
  module.addMember(&OperatorCmp<TypeId_UnsizedInt, CmpInst::ICMP_EQ>::equal);

  module.addMember(&OperatorCmp<TypeId_Bool,  CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_Char,  CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_SInt8, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_SInt16, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_SInt32, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_SInt64, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_UInt8, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_UInt16, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_UInt32, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_UInt64, CmpInst::ICMP_NE>::unequal);
  module.addMember(&OperatorCmp<TypeId_Float, CmpInst::FCMP_ONE>::unequal);
  module.addMember(&OperatorCmp<TypeId_Double, CmpInst::FCMP_ONE>::unequal);
  module.addMember(&OperatorCmp<TypeId_UnsizedInt, CmpInst::ICMP_NE>::unequal);

  module.addMember(&OperatorCmp<TypeId_Char,  CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<TypeId_SInt8, CmpInst::ICMP_SLT>::less);
  module.addMember(&OperatorCmp<TypeId_SInt16, CmpInst::ICMP_SLT>::less);
  module.addMember(&OperatorCmp<TypeId_SInt32, CmpInst::ICMP_SLT>::less);
  module.addMember(&OperatorCmp<TypeId_SInt64, CmpInst::ICMP_SLT>::less);
  module.addMember(&OperatorCmp<TypeId_UInt8, CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<TypeId_UInt16, CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<TypeId_UInt32, CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<TypeId_UInt64, CmpInst::ICMP_ULT>::less);
  module.addMember(&OperatorCmp<TypeId_Float, CmpInst::FCMP_OLT>::less);
  module.addMember(&OperatorCmp<TypeId_Double, CmpInst::FCMP_OLT>::less);
  module.addMember(&OperatorCmp<TypeId_UnsizedInt, CmpInst::ICMP_SLT>::less);

  module.addMember(&OperatorCmp<TypeId_Char,  CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<TypeId_SInt8, CmpInst::ICMP_SGT>::greater);
  module.addMember(&OperatorCmp<TypeId_SInt16, CmpInst::ICMP_SGT>::greater);
  module.addMember(&OperatorCmp<TypeId_SInt32, CmpInst::ICMP_SGT>::greater);
  module.addMember(&OperatorCmp<TypeId_SInt64, CmpInst::ICMP_SGT>::greater);
  module.addMember(&OperatorCmp<TypeId_UInt8, CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<TypeId_UInt16, CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<TypeId_UInt32, CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<TypeId_UInt64, CmpInst::ICMP_UGT>::greater);
  module.addMember(&OperatorCmp<TypeId_Float, CmpInst::FCMP_OGT>::greater);
  module.addMember(&OperatorCmp<TypeId_Double, CmpInst::FCMP_OGT>::greater);
  module.addMember(&OperatorCmp<TypeId_UnsizedInt, CmpInst::ICMP_SGT>::greater);

  module.addMember(&OperatorCmp<TypeId_Char,  CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_SInt8, CmpInst::ICMP_SLE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_SInt16, CmpInst::ICMP_SLE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_SInt32, CmpInst::ICMP_SLE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_SInt64, CmpInst::ICMP_SLE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_UInt8, CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_UInt16, CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_UInt32, CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_UInt64, CmpInst::ICMP_ULE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_Float, CmpInst::FCMP_OLE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_Double, CmpInst::FCMP_OLE>::lessOrEqual);
  module.addMember(&OperatorCmp<TypeId_UnsizedInt, CmpInst::ICMP_SLE>::lessOrEqual);

  module.addMember(&OperatorCmp<TypeId_Char,  CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_SInt8, CmpInst::ICMP_SGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_SInt16, CmpInst::ICMP_SGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_SInt32, CmpInst::ICMP_SGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_SInt64, CmpInst::ICMP_SGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_UInt8, CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_UInt16, CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_UInt32, CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_UInt64, CmpInst::ICMP_UGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_Float, CmpInst::FCMP_OGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_Double, CmpInst::FCMP_OGE>::greaterOrEqual);
  module.addMember(&OperatorCmp<TypeId_UnsizedInt, CmpInst::ICMP_SGE>::greaterOrEqual);

  // TODO: Emit a warning about negating unsigneds...
  module.addMember(&NegateOp<TypeId_SInt8>::value);
  module.addMember(&NegateOp<TypeId_SInt16>::value);
  module.addMember(&NegateOp<TypeId_SInt32>::value);
  module.addMember(&NegateOp<TypeId_SInt64>::value);
  module.addMember(&NegateOp<TypeId_UInt8>::value);
  module.addMember(&NegateOp<TypeId_UInt16>::value);
  module.addMember(&NegateOp<TypeId_UInt32>::value);
  module.addMember(&NegateOp<TypeId_UInt64>::value);
  module.addMember(&NegateOp<TypeId_Float>::value);
  module.addMember(&NegateOp<TypeId_Double>::value);
  module.addMember(&NegateOp<TypeId_UnsizedInt>::value);

  module.addMember(&SuccessorOp<TypeId_Char>::value);
  module.addMember(&SuccessorOp<TypeId_SInt8>::value);
  module.addMember(&SuccessorOp<TypeId_SInt16>::value);
  module.addMember(&SuccessorOp<TypeId_SInt32>::value);
  module.addMember(&SuccessorOp<TypeId_SInt64>::value);
  module.addMember(&SuccessorOp<TypeId_UInt8>::value);
  module.addMember(&SuccessorOp<TypeId_UInt16>::value);
  module.addMember(&SuccessorOp<TypeId_UInt32>::value);
  module.addMember(&SuccessorOp<TypeId_UInt64>::value);
  module.addMember(&SuccessorOp<TypeId_UnsizedInt>::value);

  module.addMember(&PredeccessorOp<TypeId_Char>::value);
  module.addMember(&PredeccessorOp<TypeId_SInt8>::value);
  module.addMember(&PredeccessorOp<TypeId_SInt16>::value);
  module.addMember(&PredeccessorOp<TypeId_SInt32>::value);
  module.addMember(&PredeccessorOp<TypeId_SInt64>::value);
  module.addMember(&PredeccessorOp<TypeId_UInt8>::value);
  module.addMember(&PredeccessorOp<TypeId_UInt16>::value);
  module.addMember(&PredeccessorOp<TypeId_UInt32>::value);
  module.addMember(&PredeccessorOp<TypeId_UInt64>::value);
  module.addMember(&PredeccessorOp<TypeId_UnsizedInt>::value);

  module.addMember(&BitwiseAndDecl<TypeId_SInt8>::value);
  module.addMember(&BitwiseAndDecl<TypeId_SInt16>::value);
  module.addMember(&BitwiseAndDecl<TypeId_SInt32>::value);
  module.addMember(&BitwiseAndDecl<TypeId_SInt64>::value);
  module.addMember(&BitwiseAndDecl<TypeId_UInt8>::value);
  module.addMember(&BitwiseAndDecl<TypeId_UInt16>::value);
  module.addMember(&BitwiseAndDecl<TypeId_UInt32>::value);
  module.addMember(&BitwiseAndDecl<TypeId_UInt64>::value);
  module.addMember(&BitwiseAndDecl<TypeId_UnsizedInt>::value);

  module.addMember(&BitwiseOrDecl<TypeId_SInt8>::value);
  module.addMember(&BitwiseOrDecl<TypeId_SInt16>::value);
  module.addMember(&BitwiseOrDecl<TypeId_SInt32>::value);
  module.addMember(&BitwiseOrDecl<TypeId_SInt64>::value);
  module.addMember(&BitwiseOrDecl<TypeId_UInt8>::value);
  module.addMember(&BitwiseOrDecl<TypeId_UInt16>::value);
  module.addMember(&BitwiseOrDecl<TypeId_UInt32>::value);
  module.addMember(&BitwiseOrDecl<TypeId_UInt64>::value);
  module.addMember(&BitwiseOrDecl<TypeId_UnsizedInt>::value);

  module.addMember(&BitwiseXorDecl<TypeId_SInt8>::value);
  module.addMember(&BitwiseXorDecl<TypeId_SInt16>::value);
  module.addMember(&BitwiseXorDecl<TypeId_SInt32>::value);
  module.addMember(&BitwiseXorDecl<TypeId_SInt64>::value);
  module.addMember(&BitwiseXorDecl<TypeId_UInt8>::value);
  module.addMember(&BitwiseXorDecl<TypeId_UInt16>::value);
  module.addMember(&BitwiseXorDecl<TypeId_UInt32>::value);
  module.addMember(&BitwiseXorDecl<TypeId_UInt64>::value);
  module.addMember(&BitwiseXorDecl<TypeId_UnsizedInt>::value);

  compileBuiltins(infixEqSrc);
  compileBuiltins(infixNeSrc);
}

}

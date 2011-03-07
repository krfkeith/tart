/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_EVALPASS_H
#define TART_SEMA_EVALPASS_H

#ifndef TART_EXPR_CONSTANT_H
#include "tart/Expr/Constant.h"
#endif

#include "llvm/ADT/DenseMap.h"

namespace tart {

class LValueExpr;
class FnCallExpr;
class NewExpr;
class AssignmentExpr;
class CastExpr;
class ArrayLiteralExpr;
class BinaryOpcodeExpr;
class CompareExpr;
class SeqExpr;
class ReturnExpr;

/// -------------------------------------------------------------------
/// Pass which evalutes a compile-time expression.
class EvalPass {
public:

  /** Evaluate the given expression.
      'allowPartial' - means its ok if the evaluation cannot be done at compile-time.
    */
  static Expr * eval(Module * module, Expr * in, bool allowPartial = false);

private:
  enum RunState {
    RUNNING,
    BREAK,
    CONTINUE,
    RETURN,
    THROW,
  };

  typedef llvm::DenseMap<VariableDefn *, Expr *> VariableMap;

  // Contains the parameters and local variables of a call frame.
  class CallFrame {
  public:
    CallFrame(CallFrame * prev)
      : prev_(prev), function_(NULL), selfArg_(NULL), returnVal_(NULL), runState_(RUNNING) {}

    // Return the caller's call frame.
    CallFrame * prev() const { return prev_; }

    // The function being called.
    FunctionDefn * function() const { return function_; }
    void setFunction(FunctionDefn * value) { function_ = value; }

    // Argument list (not including 'self' argument.)
    ExprList & args() { return args_; }

    // The 'self' argument.
    Expr * selfArg() const { return selfArg_; }
    void setSelfArg(Expr * value) { selfArg_ = value; }

    // The return value of the function.
    Expr * returnVal() const { return returnVal_; }
    void setReturnVal(Expr * value) { returnVal_ = value; }

    Expr * getLocal(VariableDefn * var);
    void setLocal(VariableDefn * var, Expr * value);

    const SourceLocation & callLocation() const { return callLocation_; }
    void setCallLocation(const SourceLocation & loc) { callLocation_ = loc; }

    RunState runState() const { return runState_; }
    void setRunState(RunState rs) { runState_ = rs; }

  private:
    CallFrame * prev_;
    FunctionDefn * function_;
    ExprList args_;
    Expr * selfArg_;
    Expr * returnVal_;
    VariableMap locals_;
    SourceLocation callLocation_;
    RunState runState_;
  };

  enum BooleanResult {
    BOOLEAN_FALSE = 0,
    BOOLEAN_TRUE = 1,
    BOOLEAN_ERROR = -1,
  };

  Module * module_;
  bool allowPartial_;
  CallFrame * callFrame_;

  EvalPass(Module * module, bool allowPartial)
    : module_(module)
    , allowPartial_(allowPartial)
    , callFrame_(NULL)
  {}

  Expr * evalExpr(Expr * in);
  ConstantExpr * evalConstantExpr(Expr * in);
  Expr * evalLValue(LValueExpr * in);
  Expr * evalFnCall(FnCallExpr * in);
  Expr * evalNew(NewExpr * in);
  Expr * evalAssign(AssignmentExpr * in);
  Expr * evalNot(UnaryExpr * in);
  Expr * evalComplement(UnaryExpr * in);
  Expr * evalArrayLiteral(ArrayLiteralExpr * in);
  Expr * evalUnionCtorCast(CastExpr *in);
  Expr * evalBinaryOpcode(BinaryOpcodeExpr *in);
  Expr * evalCompare(CompareExpr *in);
  Expr * evalSeq(SeqExpr * in);
  Expr * evalReturn(ReturnExpr * in);

  llvm::Constant * asConstNumber(ConstantExpr * e);
  BooleanResult asConstBoolean(Expr * in);

  void store(Expr * value, Expr * dest);

  /** Set the current call frame, and return the previous frame. */
  CallFrame * setCallFrame(CallFrame * newFrame) {
    CallFrame * result = callFrame_;
    callFrame_ = newFrame;
    return result;
  }

  void showCallStack();
};

} // namespace tart

#endif

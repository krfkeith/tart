/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_EVALPASS_H
#define TART_SEMA_EVALPASS_H

#ifndef TART_CFG_CONSTANT_H
#include "tart/CFG/Constant.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Pass which evalutes a compile-time expression.
class EvalPass {
public:

  /** Evaluate the given expression.
      'allowPartial' - means its ok if the evaluation cannot be done at compile-time.
    */
  static Expr * eval(Expr * in, bool allowPartial = false);

private:

  // Contains the parameters and local variables of a call frame.
  class CallFrame {
  public:
    CallFrame() : function_(NULL), selfArg_(NULL), returnVal_(NULL) {}

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

  private:
    FunctionDefn * function_;
    ExprList args_;
    Expr * selfArg_;
    Expr * returnVal_;
  };

  bool allowPartial_;
  CallFrame * callFrame_;

  EvalPass(bool allowPartial)
    : allowPartial_(allowPartial)
    , callFrame_(NULL)
  {}

  bool evalBlocks(BlockList & blocks);

  Expr * evalExpr(Expr * in);
  Expr * evalLValue(LValueExpr * in);
  Expr * evalFnCall(FnCallExpr * in);
  Expr * evalNew(NewExpr * in);
  Expr * evalAssign(AssignmentExpr * in);
  Expr * evalArrayLiteral(ArrayLiteralExpr * in);

  void store(Expr * value, Expr * dest);

  /** Set the current call frame, and return the previous frame. */
  CallFrame * setCallFrame(CallFrame * newFrame) {
    CallFrame * result = callFrame_;
    callFrame_ = newFrame;
    return result;
  }
};

} // namespace tart

#endif

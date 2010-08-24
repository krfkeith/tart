/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_EXPR_H
#define TART_CFG_EXPR_H

#ifndef TART_COMMON_GC_H
#include "tart/Common/GC.h"
#endif

#ifndef TART_COMMON_SOURCELOCATION_H
#include "tart/Common/SourceLocation.h"
#endif

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#include <llvm/Instructions.h>
#include <llvm/ADT/SetVector.h>

namespace tart {

class Scope;
class ErrorExpr;
class VariableDefn;
class CompositeType;
class TupleType;
class LocalScope;

typedef llvm::SmallSetVector<SpCandidate *, 8> SpCandidateSet;
typedef llvm::SmallVector<SpCandidate *, 8> SpCandidateList;

/// -------------------------------------------------------------------
/// A Control Flow Graph value or expression
class Expr : public GC, public Formattable, public Locatable {
public:
  enum ExprType {
    #define EXPR_TYPE(x) x,
    #include "ExprType.def"
    #undef EXPR_TYPE
    TypeCount,
  };

private:
  const ExprType    exprType_;
  SourceLocation    loc_;
  const Type      * type_;

  static const ExprList emptyList;

public:
  Expr(ExprType k, const SourceLocation & l, const Type * type)
    : exprType_(k)
    , loc_(l)
    , type_(type)
  {}

  virtual ~Expr() {}

  /** The type of expression node. */
  ExprType exprType() const { return exprType_; }

  /** The type of this expression. */
  const Type * type() const { return type_; }
  void setType(const Type * type) { type_ = type; }

  /** The type of this expression with aliases removed. */
  const Type * canonicalType() const;

  /** Return true if this expression is a constant. */
  virtual bool isConstant() const { return false; }

  /** Return true if this expression has no side effects. */
  virtual bool isSideEffectFree() const = 0;

  /** Return true if this expression has been fully resolved. */
  virtual bool isSingular() const = 0;

  /** Return true if this can be assigned to. */
  virtual bool isLValue() const { return false; }

  /** Where in the source file this expression comes from. */
  const SourceLocation & location() const { return loc_; }

  /** Cast operator so we can use an expression node as a location. */
  operator const SourceLocation & () const { return loc_; }

  /** Set the location of this expression. */
  Expr * at(const SourceLocation & loc) { loc_ = loc; return this; }

  /** Produce a textual representation of this value. */
  virtual void format(FormatStream & out) const;

  /** Trace through all references in this expression. */
  void trace() const;

  /** LLVM dynamic casting primitive. */
  static inline bool classof(const Expr *) { return true; }

  /** A placeholder node used to signal an error in the computation. */
  static ErrorExpr ErrorVal;
};

/// -------------------------------------------------------------------
/// Return result indicating a fatal compilation error. Used when no
/// valid result can be returned.
class ErrorExpr : public Expr {
public:
  /** Constructor. */
  ErrorExpr();

  // Overrides

  bool isSideEffectFree() const { return true; }
  bool isSingular() const { return true; }
};

/// -------------------------------------------------------------------
/// An operation with a single argument
class UnaryExpr : public Expr {
public:
  /** Constructor. */
  UnaryExpr(ExprType k, const SourceLocation & loc, const Type * type, Expr * a)
    : Expr(k, loc, type)
    , arg_(a)
  {}

  /** The argument expression. */
  Expr * arg() const { return arg_; }
  void setArg(Expr * ex) { arg_ = ex; }

  // Overrides

  bool isSideEffectFree() const;
  bool isConstant() const;
  bool isSingular() const;
  void format(FormatStream & out) const;
  void trace() const;

protected:
  Expr * arg_;
};

/// -------------------------------------------------------------------
/// An operation with two arguments
class BinaryExpr : public Expr {
private:
  Expr * first_;
  Expr * second_;

public:
  /** Constructor. */
  BinaryExpr(ExprType k, const SourceLocation & loc, const Type * type)
    : Expr(k, loc, type)
    , first_(NULL)
    , second_(NULL)
  {}

  /** Constructor. */
  BinaryExpr(ExprType k, const SourceLocation & loc, const Type * type,
      Expr * f, Expr * s)
    : Expr(k, loc, type)
    , first_(f)
    , second_(s)
  {}

  /** The first argument. */
  Expr * first() const { return first_; }
  void setFirst(Expr * ex) { first_ = ex; }

  /** The second argument. */
  Expr * second() const { return second_; }
  void setSecond(Expr * ex) { second_ = ex; }

  // Overrides

  bool isSideEffectFree() const;
  bool isConstant() const;
  bool isSingular() const;
  bool isLValue() const;
  void format(FormatStream & out) const;
  void trace() const;
};

/// -------------------------------------------------------------------
/// An operation with a variable number of arguments
class ArglistExpr : public Expr {
public:
  /** The argument list. */
  ExprList & args() { return args_; }
  const ExprList & args() const { return args_; }
  const Expr * arg(size_t index) const { return args_[index]; }
  Expr * arg(size_t index) { return args_[index]; }
  size_t argCount() const { return args_.size(); }
  void appendArg(Expr * arg);

  // Overrides

  bool isSingular() const;
  void trace() const;

protected:
  ExprList args_;

  bool areArgsConstant() const;
  bool areArgsSideEffectFree() const;

  ArglistExpr(ExprType k, const SourceLocation & loc, const Type * type)
    : Expr(k, loc, type)
  {}
};

/// -------------------------------------------------------------------
/// A reference to a variable or field.
class LValueExpr : public Expr {
private:
  Expr * base_;
  ValueDefn * value_;

public:
  static LValueExpr * get(const SourceLocation & loc, Expr * baseVal, ValueDefn * val) {
    return new LValueExpr(loc, baseVal, val);
  }

  static LValueExpr * get(ValueDefn * val);

  /** Constructor. */
  LValueExpr(const SourceLocation & loc, Expr * baseVal, ValueDefn * val);

  /** Return the reference to the base (the 'self' param) */
  Expr * base() const { return base_; }
  void setBase(Expr * b) { base_ = b; }

  /** Return the reference to the definition */
  const ValueDefn * value() const { return value_; }
  ValueDefn * value() { return value_; }

  // If the input expression is an LValue which is bound to a compile-time constant,
  // return the constant, otherwise return the input expression.
  static Expr * constValue(Expr * lv);

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const { return true; }
  bool isSingular() const;
  bool isLValue() const;

  static inline bool classof(const LValueExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == LValue;
  }
};

/// -------------------------------------------------------------------
/// A reference to a scope name
class ScopeNameExpr : public Expr {
private:
  Defn * value_;

public:
  ScopeNameExpr(const SourceLocation & loc, Defn * value)
    : Expr(ScopeName, loc, NULL)
    , value_(value)
  {}

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const { return true; }
  bool isSingular() const;
  const Defn * value() const { return value_; }
  Defn * value() { return value_; }

  static inline bool classof(const ScopeNameExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ScopeName;
  }
};

/// -------------------------------------------------------------------
/// An assignment expression
class AssignmentExpr : public Expr {
private:
  Expr * fromExpr_;
  Expr * toExpr_;

public:
  AssignmentExpr(const SourceLocation & loc, Expr * to, Expr * from);
  AssignmentExpr(ExprType k, const SourceLocation & loc, Expr * to, Expr * from);

  Expr * fromExpr() const { return fromExpr_; }
  void setFromExpr(Expr * ex) { fromExpr_ = ex; }

  Expr * toExpr() const { return toExpr_; }
  void setToExpr(Expr * ex) { toExpr_ = ex; }

  // Overrides

  bool isSideEffectFree() const { return false; }
  bool isSingular() const {
    return fromExpr_->isSingular() && toExpr_->isSingular();
  }
  void format(FormatStream & out) const;

  static inline bool classof(const AssignmentExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Assign || ex->exprType() == PostAssign;
  }
};

/// -------------------------------------------------------------------
/// An multiple assignment expression
class MultiAssignExpr : public ArglistExpr {
public:
  MultiAssignExpr(const SourceLocation & loc, const Type * type);

  // Overrides

  bool isSideEffectFree() const { return false; }

  static inline bool classof(const MultiAssignExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == MultiAssign;
  }
};

/// -------------------------------------------------------------------
/// An initialization of a local variable
class InitVarExpr : public Expr {
private:
  VariableDefn * var;
  Expr * initExpr_;

public:
  InitVarExpr(const SourceLocation & loc, VariableDefn * var, Expr * expr);

  Expr * initExpr() const { return initExpr_; }
  void setInitExpr(Expr * e) { initExpr_ = e; }
  VariableDefn * getVar() const { return var; }

  // Overrides

  bool isSideEffectFree() const { return false; }
  bool isSingular() const;
  void format(FormatStream & out) const;

  static inline bool classof(const InitVarExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == InitVar;
  }
};

/// -------------------------------------------------------------------
/// A bound method: Method reference + object to call.
class BoundMethodExpr : public Expr {
private:
  Expr * selfArg_;
  FunctionDefn * method_;

public:
  /** Constructor. */
  BoundMethodExpr(const SourceLocation & loc, Expr * selfArg, FunctionDefn * method,
      const Type * type);

  /** Return the reference to the 'self' param */
  Expr * selfArg() const { return selfArg_; }
  void setSelfArg(Expr * b) { selfArg_ = b; }

  /** Return the reference to the method */
  const FunctionDefn * method() const { return method_; }
  FunctionDefn * method() { return method_; }

  // Overrides

  void format(FormatStream & out) const;
  void trace() const;
  bool isSideEffectFree() const { return true; }
  bool isSingular() const;

  static inline bool classof(const BoundMethodExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == BoundMethod;
  }
};

/// -------------------------------------------------------------------
/// A general function call
class CallExpr : public ArglistExpr {
private:
  Expr * function_;
  Candidates candidates_;
  const Type * expectedReturnType_;

public:
  CallExpr(ExprType k, const SourceLocation & loc, Expr * f)
    : ArglistExpr(k, loc, NULL)
    , function_(f)
    , expectedReturnType_(NULL)
  {}

  /** The function expression being called. */
  Expr * function() { return function_; }
  void setFunction(Expr * ex) { function_ = ex; }

  /** The list of overload candidates. */
  Candidates & candidates() { return candidates_; }

  /** The function expression being called. */
  const Type * expectedReturnType() const { return expectedReturnType_; }
  void setExpectedReturnType(const Type * t) { expectedReturnType_ = t; }

  /** If all of the overload candidates have the same type for the Nth
      parameter slot, then return that type, otherwise return NULL. */
  const Type * singularParamType(int arg);

  /** If all of the overload candidates have the same return type, then
      return that type, otherwise return NULL. */
  const Type * singularResultType();

  /** Return either the single non-culled candidate, or NULL. */
  CallCandidate * singularCandidate();

  /** Return true if there is at least one non-culled candidate. */
  bool hasAnyCandidates() const;

  // Overridden methods

  void format(FormatStream & out) const;
  bool isSideEffectFree() const { return false; }
  bool isSingular() const;
  void trace() const;

  static inline bool classof(const CallExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Call || ex->exprType() == Construct;
  }
};

/// -------------------------------------------------------------------
/// A call to a template
class SpecializeExpr : public Expr {
private:
  SpCandidateList candidates_;
  TupleType * args_;

public:
  SpecializeExpr(SLC & loc, const SpCandidateSet & candidates, TupleType * args)
    : Expr(Specialize, loc, NULL)
    , candidates_(candidates.begin(), candidates.end())
    , args_(args)
  {}

  /** The list of overload candidates. */
  const SpCandidateList & candidates() const { return candidates_; }
  SpCandidateList & candidates() { return candidates_; }

  TupleType * args() const { return args_; }

  /** Return true if there is at least one non-culled candidate. */
  bool hasAnyCandidates() const;

  // Overridden methods

  void format(FormatStream & out) const;
  bool isSideEffectFree() const { return true; }
  bool isSingular() const;
  void trace() const;

  static inline bool classof(const SpecializeExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == Specialize;
  }
};

/// -------------------------------------------------------------------
/// A call to a global or member function.
class FnCallExpr : public ArglistExpr {
private:
  FunctionDefn * function_;
  Expr * selfArg_;

public:
  FnCallExpr(ExprType k, const SourceLocation & loc, FunctionDefn * function,
      Expr * self)
    : ArglistExpr(k, loc, NULL)
    , function_(function)
    , selfArg_(self)
  {}

  /** The function expression being called. */
  FunctionDefn * function() { return function_; }
  const FunctionDefn * function() const { return function_; }
  void setFunction(FunctionDefn * function) { function_ = function; }

  /** The 'self' argument. */
  Expr * selfArg() const { return selfArg_; }
  void setSelfArg(Expr * self) { selfArg_ = self; }

  // Overridden methods

  void format(FormatStream & out) const;
  bool isSideEffectFree() const { return false; }
  bool isSingular() const;
  void trace() const;

  static inline bool classof(const FnCallExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == FnCall || ex->exprType() == CtorCall;
  }
};

/// -------------------------------------------------------------------
/// A call through a variable.
class IndirectCallExpr : public ArglistExpr {
private:
  Expr * function_;

public:
  IndirectCallExpr(ExprType k, const SourceLocation & loc, Expr * function)
    : ArglistExpr(k, loc, NULL)
    , function_(function)
  {}

  /** The function expression being called. */
  Expr * function() { return function_; }
  const Expr * function() const { return function_; }

  // Overridden methods

  void format(FormatStream & out) const;
  bool isSideEffectFree() const { return false; }
  bool isSingular() const;
  void trace() const;

  static inline bool classof(const IndirectCallExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == IndirectCall;
  }
};

/// -------------------------------------------------------------------
/// A 'new object' expression
class NewExpr : public Expr {
public:
  NewExpr(const SourceLocation & loc, const Type * type)
    : Expr(New, loc, type)
  {}

  // Overridden methods
  void format(FormatStream & out) const;
  bool isSideEffectFree() const { return true; }
  bool isSingular() const;
  static inline bool classof(const NewExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == New;
  }
};

/// -------------------------------------------------------------------
/// A typecast operator
class CastExpr : public UnaryExpr {
public:
  static CastExpr * bitCast(Expr * value, const Type * toType);
  static CastExpr * upCast(Expr * value, const Type * toType);
  static CastExpr * tryCast(Expr * value, const Type * toType);
  static CastExpr * dynamicCast(Expr * value, const Type * toType);

  /** Constructor. */
  CastExpr(ExprType k, const SourceLocation & loc, const Type * type, Expr * a)
    : UnaryExpr(k, loc, type, a)
    , typeIndex_(0)
  {
  }

  // Type discriminator index used in union types
  int typeIndex() const { return typeIndex_; }
  void setTypeIndex(int index) { typeIndex_ = index; }

  void format(FormatStream & out) const;
  static inline bool classof(const CastExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() >= ImplicitCast && ex->exprType() <= ZeroExtend;
  }

private:
  int typeIndex_;
};

/// -------------------------------------------------------------------
/// A low-level binary machine opcode
class BinaryOpcodeExpr : public BinaryExpr {
private:
  llvm::Instruction::BinaryOps opCode_;

public:
  /** Constructor. */
  BinaryOpcodeExpr(
      llvm::Instruction::BinaryOps op,
      const SourceLocation & loc, const Type * type)
    : BinaryExpr(BinaryOpcode, loc, type)
    , opCode_(op)
  {}

  /** Constructor. */
  BinaryOpcodeExpr(
      llvm::Instruction::BinaryOps op,
      const SourceLocation & loc, const Type * type,
      Expr * a0, Expr * a1)
    : BinaryExpr(BinaryOpcode, loc, type, a0, a1)
    , opCode_(op)
  {}

  /** The LLVM opcode for this binary expression. */
  llvm::Instruction::BinaryOps opCode() const { return opCode_; }

  // Overrides

  bool isSingular() const;
  bool isSideEffectFree() const;
  void format(FormatStream & out) const;
};

/// -------------------------------------------------------------------
/// A comparison operator
class CompareExpr : public BinaryExpr {
public:
  typedef llvm::CmpInst::Predicate Predicate;

private:
  Predicate predicate_;

public:
  /** Constructor. */
  CompareExpr(const SourceLocation & loc, Predicate pred);

  /** Constructor. */
  CompareExpr(const SourceLocation & loc, Predicate pred, Expr * f, Expr * s);

  Predicate predicate() const { return predicate_; }

  // Overrides

  void format(FormatStream & out) const;
};

/// -------------------------------------------------------------------
/// An IsInstanceOf test
class InstanceOfExpr : public Expr {
private:
  Expr * value_;
  const Type * toType_;

public:
  /** Constructor. */
  InstanceOfExpr(const SourceLocation & loc, Expr * value, const Type * ty);

  /* The instance value we are testing. */
  const Expr * value() const { return value_; }
  Expr * value() { return value_; }
  void setValue(Expr * value) { value_ = value; }

  /* The type we are testing against. */
  const Type * toType() const { return toType_; }
  void setToType(const Type * ty) { toType_ = ty; }

  // Overrides

  bool isSideEffectFree() const { return true; }
  bool isSingular() const;
  void format(FormatStream & out) const;
  void trace() const;
};

/// -------------------------------------------------------------------
/// An expression that directly represents an IR value.
class IRValueExpr : public Expr {
public:
  /** Constructor. */
  IRValueExpr(const SourceLocation & loc, const Type * type, llvm::Value * value = NULL)
    : Expr(IRValue, loc, type)
    , value_(value)
  {}

  /** The argument expression. */
  llvm::Value * value() const { return value_; }
  void setValue(llvm::Value * value) { value_ = value; }

  // Overrides

  bool isSideEffectFree() const { return true; }
  bool isConstant() const { return false; }
  bool isSingular() const { return true; }
  void format(FormatStream & out) const;

  static inline bool classof(const IRValueExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == IRValue;
  }

private:
  llvm::Value * value_;
};

/// -------------------------------------------------------------------
/// A statement that executes a local jump and return within a function.
/// This is used for cleanup handlers.
class LocalCallExpr : public Expr {
private:
  Block * target_;
  int returnState_;

public:
  /** Constructor. */
  LocalCallExpr(Block * target)
    : Expr(LocalCall, SourceLocation(), NULL)
    , target_(target)
    , returnState_(-1)
  {}

  /** The target of the call. */
  Block * target() const { return target_; }
  void setTarget(Block * target) { target_ = target; }

  /** Used in generating the call - sets a state variable before the branch. */
  int returnState() const { return returnState_; }
  void setReturnState(int state) { returnState_ = state; }

  // Overrides

  bool isSideEffectFree() const { return true; }
  bool isConstant() const { return false; }
  bool isSingular() const { return true; }
  void format(FormatStream & out) const;

  static inline bool classof(const LocalCallExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == LocalCall;
  }
};

/// -------------------------------------------------------------------
/// An array literal.
class ArrayLiteralExpr : public ArglistExpr {
public:
  ArrayLiteralExpr(const SourceLocation & loc)
    : ArglistExpr(ArrayLiteral, loc, NULL)
  {}

  // Overrides

  bool isSideEffectFree() const {
    return areArgsSideEffectFree();
  }

  static inline bool classof(const ArrayLiteralExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ArrayLiteral;
  }
};

/// -------------------------------------------------------------------
/// A tuple constructor expression.
class TupleCtorExpr : public ArglistExpr {
public:
  TupleCtorExpr(const SourceLocation & loc, const Type * type)
    : ArglistExpr(TupleCtor, loc, type)
  {}

  TupleCtorExpr(const SourceLocation & loc, const Type * type, const ExprList & argList)
    : ArglistExpr(TupleCtor, loc, type)
  {
    args().append(argList.begin(), argList.end());
  }

  // Overrides

  bool isConstant() const { return areArgsConstant(); }
  bool isSideEffectFree() const { return areArgsSideEffectFree(); }
  void format(FormatStream & out) const;

  static inline bool classof(const TupleCtorExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == TupleCtor;
  }
};

/// -------------------------------------------------------------------
/// An expression which may occur at several places in the CFG, but
/// which should only be evaluated once.
class SharedValueExpr : public UnaryExpr {
public:
  static SharedValueExpr * get(Expr * ex) {
    return new SharedValueExpr(ex);
  }

  /** Constructor. */
  SharedValueExpr(Expr * arg)
    : UnaryExpr(SharedValue, arg->location(), arg->type(), arg)
    , value_(NULL)
  {}

  /** The cached IR value. */
  llvm::Value * value() const { return value_; }
  void setValue(llvm::Value * value) const { value_ = value; }

  // Overrides

  static inline bool classof(const SharedValueExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == SharedValue;
  }

private:
  mutable llvm::Value * value_;
};

/// -------------------------------------------------------------------
/// Utility functions

/** Return the text name of a node class. */
const char * exprTypeName(Expr::ExprType type);

/** Format a list of expressions as comma-separated values. */
void formatExprList(FormatStream & out, const ExprList & exprs);

/** Format a list of expression types as comma-separated values. */
void formatExprTypeList(FormatStream & out, const ExprList & exprs);

/** Format a list of types as comma-separated values. */
void formatTypeList(FormatStream & out, const TypeList & types);

/** Return true if the expression is an error result. */
inline bool isErrorResult(const Expr * ex) {
  return ex == NULL || ex->exprType() == Expr::Invalid;
}

bool isErrorResult(const Type * ty);

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_EXPR_EXPR_H
#define TART_EXPR_EXPR_H

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

namespace tart {

class ErrorExpr;
class UnaryExpr;

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
  static UnaryExpr VoidVal;
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
  typedef ExprList::iterator iterator;
  typedef ExprList::const_iterator const_iterator;

  /** The argument list. */
  ExprList & args() { return args_; }
  const ExprList & args() const { return args_; }
  const Expr * arg(size_t index) const { return args_[index]; }
  Expr * arg(size_t index) { return args_[index]; }
  size_t argCount() const { return args_.size(); }
  void appendArg(Expr * arg);

  iterator begin() { return args_.begin(); }
  iterator end() { return args_.end(); }

  const_iterator begin() const { return args_.begin(); }
  const_iterator end() const { return args_.end(); }

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

  ArglistExpr(ExprType k, const SourceLocation & loc, const ExprList & exprs, const Type * type)
    : Expr(k, loc, type)
  {
    args_.append(exprs.begin(), exprs.end());
  }
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

bool any(ExprList::const_iterator first, ExprList::const_iterator last, bool (Expr::*func)() const);
bool all(ExprList::const_iterator first, ExprList::const_iterator last, bool (Expr::*func)() const);

} // namespace tart

#endif

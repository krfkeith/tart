/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_CONSTANT_H
#define TART_CFG_CONSTANT_H

#ifndef TART_CFG_EXPR_H
#include "tart/CFG/Expr.h"
#endif

#include <llvm/Constants.h>

namespace tart {

class PrimitiveType;
class NativeArrayType;

// Note that the names of these classes has been chosen not to collide
// with the LLVM constant classes.

/// -------------------------------------------------------------------
/// Base class for constant expressions.
class ConstantExpr : public Expr {
public:
  ConstantExpr(ExprType k, SourceLocation l, const Type * t)
    : Expr(k, l, t)
  {}

  virtual bool isEqual(const ConstantExpr * cexpr) const = 0;

  // Overrides

  bool isConstant() const { return true; }
  bool isSideEffectFree() const { return true; }
  bool isSingular() const { return true; }
  static inline bool classof(const ConstantExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ConstInt ||
        ex->exprType() == ConstFloat ||
        ex->exprType() == ConstString ||
        ex->exprType() == ConstNull ||
        ex->exprType() == ConstObjRef ||
        ex->exprType() == ConstNArray ||
        ex->exprType() == TypeName ||
        ex->exprType() == TypeLiteral;
  }
};

/// -------------------------------------------------------------------
/// Integer constant (includes chars, bools and enums as well.)
class ConstantInteger : public ConstantExpr {
private:
  llvm::ConstantInt * value_;

public:
  ConstantInteger(SourceLocation l, const Type * t, llvm::ConstantInt * val)
    : ConstantExpr(ConstInt, l, t)
    , value_(val)
  {}

  llvm::ConstantInt * value() const { return value_; }
  const llvm::APInt & intValue() const { return value_->getValue(); }

  /** Return the primitive type of this constant. If the type is an enum, it
      will return an integer type that is the same size as the enum. */
  const PrimitiveType * primitiveType() const;

  static ConstantInteger * getConstantBool(const SourceLocation & loc, bool value);
  static ConstantInteger * get(const SourceLocation & loc, const Type * type, int32_t value);
  static ConstantInteger * get(const SourceLocation & loc, const Type * type,
      llvm::ConstantInt * value);
  static ConstantInteger * getSigned(const llvm::APInt & value, const PrimitiveType * type);
  static ConstantInteger * getUnsigned(const llvm::APInt & value, const PrimitiveType * type);

  // Overrides

  bool isNegative() const;
  bool isEqual(const ConstantExpr * cexpr) const;
  void format(FormatStream & out) const;
  static inline bool classof(const ConstantInteger *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ConstInt;
  }
};

/// -------------------------------------------------------------------
/// Float constant
class ConstantFloat : public ConstantExpr {
private:
  llvm::ConstantFP * value_;

public:
  ConstantFloat(SourceLocation l, const Type * t, llvm::ConstantFP * val)
    : ConstantExpr(ConstFloat, l, t)
    , value_(val)
  {}

  llvm::ConstantFP * value() const { return value_; }

  // Overrides

  bool isEqual(const ConstantExpr * cexpr) const;
  void format(FormatStream & out) const;
  static inline bool classof(const ConstantFloat *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ConstFloat;
  }
};

/// -------------------------------------------------------------------
/// String constant
class ConstantString : public ConstantExpr {
public:
  ConstantString(SourceLocation l, const std::string & val);

  const std::string & value() const { return value_; }

  // Overrides

  bool isEqual(const ConstantExpr * cexpr) const;
  void format(FormatStream & out) const;
  static inline bool classof(const ConstantString *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ConstString;
  }

private:
  std::string value_;
};

/// -------------------------------------------------------------------
/// Null constant
class ConstantNull : public ConstantExpr {
public:
  ConstantNull(SourceLocation l);
  ConstantNull(SourceLocation l, const Type * t);

  static ConstantNull * get(const SourceLocation & l, const Type * t) {
    return new ConstantNull(l, t);
  }

  // Overrides

  bool isEqual(const ConstantExpr * cexpr) const;
  void format(FormatStream & out) const;
  static inline bool classof(const ConstantNull *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ConstNull;
  }
};

/// -------------------------------------------------------------------
/// An constant expression which refers to a type. Used for template
/// arguments and reflection.
class TypeLiteralExpr : public ConstantExpr {
private:
  const Type * value_;

public:
  TypeLiteralExpr(SourceLocation l, const Type * val);

  const Type * value() const { return value_; }

  // Overrides

  bool isEqual(const ConstantExpr * cexpr) const;
  bool isSingular() const;
  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const TypeLiteralExpr *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == TypeLiteral;
  }
};

/// -------------------------------------------------------------------
/// A constant reference to an object.
class ConstantObjectRef : public Expr {
private:
  ExprList members_;

public:
  ConstantObjectRef(SourceLocation l, const CompositeType * val);

  const ExprList & members() const { return members_; }
  ExprList & members() { return members_; }

  Expr * getMemberValue(VariableDefn * member) const;
  Expr * getMemberValue(const char * name) const;
  void setMemberValue(VariableDefn * member, Expr * value);
  void setMemberValue(const char * name, Expr * value);

  int32_t memberValueAsInt(const char * name);

  // Overrides

  bool isConstant() const { return true; }
  bool isSideEffectFree() const { return true; }
  bool isSingular() const;

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ConstantObjectRef *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ConstObjRef;
  }
};

/// -------------------------------------------------------------------
/// A constant native array.
class ConstantNativeArray : public Expr {
private:
  ExprList elements_;

public:
  ConstantNativeArray(SourceLocation l, NativeArrayType * type);

  const ExprList & elements() const { return elements_; }
  ExprList & elements() { return elements_; }

  // Overrides

  bool isConstant() const { return true; }
  bool isSideEffectFree() const { return true; }
  bool isSingular() const;

  void format(FormatStream & out) const;
  void trace() const;
  static inline bool classof(const ConstantNativeArray *) { return true; }
  static inline bool classof(const Expr * ex) {
    return ex->exprType() == ConstNArray;
  }
};

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_UNITTYPE_H
#define TART_CFG_UNITTYPE_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A type which consists of a single value, used for value template
/// arguments.
class UnitType : public Type {
public:
  // Static factory function.
  static UnitType * get(ConstantExpr * value);

  // The constant value.
  ConstantExpr * value() const { return value_; }

  // Overrides

  bool isSingular() const { return value_->isSingular(); }
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const { return isEqual(other); }
  bool isReferenceType() const { return false; }
  const llvm::Type * irType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  Expr * nullInitValue() const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const UnitType *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() == Unit;
  }

private:
  ConstantExpr * value_;

  /** Construct a new typealias. */
  UnitType(ConstantExpr * value)
    : Type(Unit)
    , value_(value)
  {}
};

} // namespace tart

#endif

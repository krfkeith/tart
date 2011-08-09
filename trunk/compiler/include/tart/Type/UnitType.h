/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_UNITTYPE_H
#define TART_TYPE_UNITTYPE_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef LLVM_ADT_FOLDINGSET_H
#include "llvm/ADT/FoldingSet.h"
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
  bool isReferenceType() const { return false; }
  TypeShape typeShape() const { return Shape_None; }
  llvm::Type * irType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  Expr * nullInitValue() const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const UnitType *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() == Unit;
  }

  typedef llvm::FoldingSetNodeWrapper<UnitType *> UniqueTypeNode;
  typedef llvm::FoldingSet<UniqueTypeNode> UniqueTypes;

private:
  static UniqueTypes uniqueTypes_;

  ConstantExpr * value_;

  /** Construct a new UnitType. */
  UnitType(ConstantExpr * value)
    : Type(Unit)
    , value_(value)
  {}
};

} // namespace tart

namespace llvm {

// Definition of folding set traits for constant expressions.
template<> struct FoldingSetTrait<tart::UnitType *> {
  static inline void Profile(const tart::UnitType * ut, FoldingSetNodeID &ID) {
    ID.Add(ut->value());
  }
};

} // namespace llvm

#endif // TART_TYPE_UNITTYPE_H

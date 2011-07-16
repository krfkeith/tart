/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPEALIAS_H
#define TART_TYPE_TYPEALIAS_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A named alias of a type.
class TypeAlias : public Type {
public:
  /** Construct a new typealias. */
  TypeAlias(const Type * val, TypeDefn * defn);

  /** The target of this alias. */
  const Type * value() const { return value_; }
  void setValue(const Type * value) { value_ = value; }

  // Overrides

  TypeDefn * typeDefn() const { return defn_; }
  bool isSingular() const { return value_->isSingular(); }
  bool isEqual(const Type * other) const { return value_->isEqual(other); }
  bool isSubtypeOf(const Type * other) const { return value_->isSubtypeOf(other); }
  bool includes(const Type * other) const { return value_->includes(other); }
  bool isReferenceType() const { return value_->isReferenceType(); }
  TypeShape typeShape() const { return value_->typeShape(); }
  const llvm::Type * irType() const;
  const llvm::Type * irEmbeddedType() const;
  const llvm::Type * irParameterType() const;
  const llvm::Type * irReturnType() const;
  void expand(TypeExpansion & out) const { value_->expand(out); }
  ConversionRank convertImpl(const Conversion & conversion) const;
  Expr * nullInitValue() const { return value_->nullInitValue(); }
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const TypeAlias *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() == Alias;
  }

private:
  const Type * value_;
  TypeDefn * defn_;
};

}

#endif

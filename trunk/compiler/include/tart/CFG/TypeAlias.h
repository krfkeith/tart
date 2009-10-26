/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TYPEALIAS_H
#define TART_CFG_TYPEALIAS_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A named alias of a type.
class TypeAlias : public Type {
public:
  /** Construct a new typealias. */
  TypeAlias(const TypeRef & val);

  /** The target of this alias. */
  TypeRef value() const { return value_; }
  TypeRef & setValue(const TypeRef & value) { value_ = value; }

  // Overrides

  bool isSingular() const { return value_.isSingular(); }
  bool isEqual(const Type * other) const { return value_.type()->isEqual(other); }
  bool isSubtype(const Type * other) const { return value_.type()->isSubtype(other); }
  bool includes(const Type * other) const { return value_.type()->includes(other); }
  bool isReferenceType() const { return value_.isReferenceType(); }
  const llvm::Type * irType() const;
  const llvm::Type * irEmbeddedType() const;
  const llvm::Type * irParameterType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  Expr * nullInitValue() const { return value_.type()->nullInitValue(); }
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const TypeAlias *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() == Alias;
  }

private:
  TypeRef value_;
};

}

#endif

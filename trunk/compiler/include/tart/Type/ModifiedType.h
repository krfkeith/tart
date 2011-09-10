/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_MODIFIEDTYPE_H
#define TART_TYPE_MODIFIEDTYPE_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A base type plus a type modifier such as volatile, readonly, etc.
class ModifiedType : public Type {
public:

  /** Derive a ModifiedType type from a base type. */
  static ModifiedType * get(TypeClass cls, const Type * baseType);

  const Type * baseType() const { return baseType_; }

  // Overrides

  size_t numTypeParams() const { return 0; }
  llvm::Type * irType() const { return baseType_->irType(); }
  bool isSingular() const { return baseType_->isSingular(); }
  bool isReferenceType() const { return baseType_->isReferenceType(); }
  void format(FormatStream & out) const;
  TypeShape typeShape() const { return baseType_->typeShape(); }

  static inline bool classof(const ModifiedType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() >= ModifiersBegin && t->typeClass() <= ModifiersEnd;
  }

private:
  ModifiedType(TypeClass cls, const Type * elemType);

  const Type * baseType_;
};

} // namespace tart

#endif // TART_TYPE_MODIFIEDTYPE_H

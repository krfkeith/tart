/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPECONSTRAINT_H
#define TART_TYPE_TYPECONSTRAINT_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef LLVM_ADT_SMALLPTRSET_H
#include "llvm/ADT/SmallPtrSet.h"
#endif

namespace tart {

// Forward declarations
class CallExpr;
class CallCandidate;
class TupleCtorExpr;

/// -------------------------------------------------------------------
/// Abstract base class for type constraints. A type constraint represents
/// some subset of possible types.
class TypeConstraint : public Type {
public:

  virtual const Type * singularValue() const = 0;

  // Overrides

  llvm::Type * irType() const;
  TypeShape typeShape() const { return Shape_Unset; }

  static inline bool classof(const TypeConstraint *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() >= AmbiguousResult;
  }

protected:
  TypeConstraint(TypeClass tcls) : Type(tcls) {}
};

/// -------------------------------------------------------------------
/// A constraint that represents a set of types that is the result of
/// some type expression. The set of types may grow or shrink during
/// type inferencing as a result of partial solutions.
class TypeSetConstraint : public TypeConstraint {
public:

  const Type * singularValue() const;
  bool isEqual(const Type * other) const;
  bool isSingular() const;
  bool isReferenceType() const;
  void format(FormatStream & out) const;

protected:
  TypeSetConstraint(TypeClass tcls) : TypeConstraint(tcls) {}
};

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPEVARIABLE_H
#define TART_TYPE_TYPEVARIABLE_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A template pattern variable which can be bound to a type expression.
class TypeVariable : public TypeImpl, public Locatable {
public:
  enum Target {
    TYPE_EXPRESSION,        // Type variable can bind to a type expression
    TYPE_QUALIFIER,         // Type variable representing a qualifier
    TYPE_CONSTRUCTOR,       // Type variable representing a unary type constructor
    CONSTANT,               // Type variable representing a non-type constant
  };

  /** Construct a type pattern variable. */
  TypeVariable(const SourceLocation & loc, StringRef name, Target target,
      const QualifiedType metaType = QualifiedType());

  /** Location where this variable was defined. */
  const SourceLocation & location() const { return location_; }

  /** The variable name. */
  StringRef name() const { return name_; }

  /** The target of this type variable */
  Target target() const { return target_; }

  /** Set the type of types which can be bound to this type variable. */
  void setTarget(Target target) { target_ = target; }

  /** The type of types which can be bound to this variable, which will usually be 'Type' */
  QualifiedType metaType() const { return metaType_; }

  /** Set the type of types which can be bound to this type variable. */
  void setMetaType(QualifiedType type) { metaType_ = type; }

  /** The upper bound of this type var - if non-null, any type bound to this type
      must be equal to upperBound or a subtype of it. */
  const QualifiedTypeList & upperBounds() const { return upperBounds_; }
  QualifiedTypeList & upperBounds() { return upperBounds_; }

  /** The lower bound of this type var - if non-null, any type bound to this type
      must be equal to lowerBound or a supertype of it. */
  const QualifiedTypeList & lowerBounds() const { return lowerBounds_; }
  QualifiedTypeList & lowerBounds() { return lowerBounds_; }

  /** Whether this is a variadic template parameter. */
  bool isVariadic() const { return isVariadic_; }
  void setIsVariadic(bool isVariadic) { isVariadic_ = isVariadic; }

  /** Return true if the specified type value can be bound to this type. */
  bool canBindTo(QualifiedType value) const;

  // Overrides

  llvm::Type * createIRType() const;
  void trace() const;
  bool isReferenceType() const;
  bool isSingular() const;
  TypeShape typeShape() const { return Shape_None; }
  void format(FormatStream & out) const;

  static inline bool classof(const TypeVariable *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == TypeVar;
  }

private:
  const SourceLocation location_;
  Target target_;
  QualifiedType metaType_;
  QualifiedTypeList upperBounds_;
  QualifiedTypeList lowerBounds_;
  StringRef name_;
  bool isVariadic_;
};

typedef llvm::SmallVector<TypeVariable *, 4> TypeVariableList;
//typedef llvm::DenseMap<const TypeVariable *, const Type *> TypeVarMap;
typedef llvm::DenseMap<const TypeVariable *, QualifiedType> QualifiedTypeVarMap;

} // namespace tart

#endif

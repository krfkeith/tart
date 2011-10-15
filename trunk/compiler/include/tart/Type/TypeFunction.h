/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPEFUNCTION_H
#define TART_TYPE_TYPEFUNCTION_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_TYPE_QUALIFIEDTYPE_H
#include "tart/Type/QualifiedType.h"
#endif

namespace tart {

class TypeVariable;
class TupleType;

/// -------------------------------------------------------------------
/// A type function is a function which is used in type expressions.
/// It accepts a type as input, and returns a type as the result.
class TypeFunction : public Type {
public:
  /** Construct a new type function. */
  TypeFunction(TypeClass cls) : Type(cls) {}

  /** Apply the function to an input type. */
  virtual const QualifiedType apply(const TupleType * args) const = 0;

  /** Format the function, with arguments. */
  virtual void formatArgs(FormatStream & out, const TupleType * args) const = 0;

  // Overrides

  bool isSingular() const { return true; }
  bool isReferenceType() const { return false; }
  TypeShape typeShape() const { return Shape_None; }
  llvm::Type * irType() const;

  static inline bool classof(const TypeFunction *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() >= TypeFunctionsBegin && ty->typeClass() <= TypeFunctionsEnd;
  }
};

/// -------------------------------------------------------------------
/// A type function that adds qualifiers to its input type.
class QualifyingTypeFunction : public TypeFunction {
public:
  QualifyingTypeFunction(unsigned qualifiers)
    : TypeFunction(TypeFnQual)
    , qualifiers_(qualifiers)
  {}

  const QualifiedType apply(const TupleType * args) const;

  // Overrides

  void format(FormatStream & out) const;
  void formatArgs(FormatStream & out, const TupleType * args) const;

  static inline bool classof(const QualifyingTypeFunction *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() == TypeFnQual;
  }
private:
  unsigned qualifiers_;
};

/// -------------------------------------------------------------------
/// A type expression that represents the application of a type function
/// upon a type argument.
class TypeFunctionCall : public Type {
public:
  /** Construct a new type function call. */
  TypeFunctionCall(const Type * var, const TupleType * args)
    : Type(TypeFnCall)
    , fnVal_(var)
    , args_(args)
  {}

  /** The type variable which names the function to apply. */
  const Type * fnVal() const { return fnVal_; }

  /** The arguments to the function. */
  const TupleType * args() const { return args_; }

  // Overrides

  bool isSingular() const { return false; }
  bool isReferenceType() const { return false; }
  TypeShape typeShape() const { return Shape_None; }
  llvm::Type * irType() const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const TypeFunctionCall *) { return true; }
  static inline bool classof(const Type * ty) {
    return ty->typeClass() == TypeFnCall;
  }

private:
  const Type * fnVal_;
  const TupleType * args_;
};

}

#endif

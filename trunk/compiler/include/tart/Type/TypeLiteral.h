/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPELITERAL_H
#define TART_TYPE_TYPELITERAL_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

namespace tart {

// -------------------------------------------------------------------
// TypeLiteralType - the type of a type literal. It has a single type
// variable which is the literal type.
class TypeLiteralType : public TypeImpl {
public:

  /** Construct a native pointer type for the specified element type. */
  static TypeLiteralType * get(const Type * literalType);

  ~TypeLiteralType();

  const Type * literalType() const { return literalType_; }

  /** Initialize the built-in template for this type. */
  static void initBuiltin();

  // Overrides

  size_t numTypeParams() const { return 1; }
  virtual QualifiedType typeParam(int index) const { return literalType_; }
  llvm::Type * irType() const { return createIRType(); }
  llvm::Type * createIRType() const;
  bool isSingular() const;
  bool isReferenceType() const { return false; }
  void format(FormatStream & out) const;
  TypeShape typeShape() const { return Shape_None; }

  static inline bool classof(const TypeLiteralType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == TypeLiteral;
  }

  /** Singleton instance. */
  static TypeDefn typedefn;
  static TypeLiteralType prototype;

private:
  typedef llvm::DenseMap<const Type * , TypeLiteralType *, Type::KeyInfo> TypeMap;
  static TypeMap uniqueTypes_;

  TypeLiteralType(const Type * elemType);
  TypeLiteralType();

  const Type * literalType_;
};

} // namespace tart

#endif // TART_TYPE_TYPELITERAL_H

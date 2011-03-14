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

  /** Initialize the built-in template for this type. */
  static void initBuiltin();

  // Overrides

  size_t numTypeParams() const { return 1; }
  virtual const Type * typeParam(int index) const { return literalType_; }
  const llvm::Type * irType() const { return createIRType(); }
  const llvm::Type * createIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSingular() const;
  bool isEqual(const Type * other) const;
  bool isSubtype(const Type * other) const;
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

  const Type* literalType_;
};

} // namespace tart

#endif // TART_TYPE_TYPELITERAL_H
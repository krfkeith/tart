/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_ENUMTYPE_H
#define TART_CFG_ENUMTYPE_H

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Enumeration type
class EnumType : public DeclaredType {
private:
  friend class EnumAnalyzer;

  const Type * baseType_;
  bool isFlags_;

public:
  EnumType(TypeDefn * de, Scope * parentScope)
      : DeclaredType(Type::Enum, de, parentScope, Shape_Primitive)
      , baseType_(NULL)
      , isFlags_(false)
  {}

  const Type * baseType() const { return baseType_; }
  void setBaseType(const Type * value) { baseType_ = value; }

  bool isFlags() const { return isFlags_; }
  void setIsFlags(bool value) { isFlags_ = value; }

  // Overrides

  const llvm::Type * createIRType() const;
  bool isSubtype(const Type * other) const;
  bool isReferenceType() const { return false; }
  bool isSingular() const { return true; }
  bool includes(const Type * other) const;
  ConversionRank convertImpl(const Conversion & conversion) const;

  static inline bool classof(const EnumType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Type::Enum;
  }
};

}

#endif

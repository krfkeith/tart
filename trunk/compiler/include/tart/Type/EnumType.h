/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_ENUMTYPE_H
#define TART_TYPE_ENUMTYPE_H

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_COMMON_PASSMGR_H
#include "tart/Common/PassMgr.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Enumeration type
class EnumType : public DeclaredType {
  friend class EnumAnalyzer;
public:
  enum AnalysisPass {
    AttributePass,
    BaseTypePass,
    ScopeCreationPass,
    OperatorCreationPass,
    PassCount
  };

  typedef tart::PassMgr<AnalysisPass, PassCount> PassMgr;
  typedef PassMgr::PassSet PassSet;

  EnumType(TypeDefn * de, Scope * parentScope)
      : DeclaredType(Type::Enum, de, parentScope, Shape_Primitive)
      , baseType_(NULL)
      , isFlags_(false)
  {}

  const Type * baseType() const { return baseType_; }
  void setBaseType(const Type * value) { baseType_ = value; }

  bool isFlags() const { return isFlags_; }
  void setIsFlags(bool value) { isFlags_ = value; }

  /** The current passes state. */
  const PassMgr & passes() const { return passes_; }
  PassMgr & passes() { return passes_; }

  // Overrides

  llvm::Type * createIRType() const;
  bool isReferenceType() const { return false; }
  bool isSingular() const { return true; }
  ConversionRank convertImpl(const Conversion & conversion) const;

  static inline bool classof(const EnumType *) { return true; }
  static inline bool classof(const Type * t) {
    return t->typeClass() == Type::Enum;
  }

private:
  const Type * baseType_;
  bool isFlags_;
  PassMgr passes_;
};

}

#endif

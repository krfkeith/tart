/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/CFG/Constant.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/EnumType.h"
#include "tart/Objects/Builtins.h"
#include "tart/Common/Diagnostics.h"
#include <llvm/ADT/StringExtras.h>

namespace tart {
  
// -------------------------------------------------------------------
// ConstantInteger

const PrimitiveType * ConstantInteger::primitiveType() const {
  const Type * ty = type();
  while (const EnumType * etype = dyn_cast<EnumType>(ty)) {
    ty = etype->baseType();
  }
  
  return cast<PrimitiveType>(ty);
}

bool ConstantInteger::isEqual(const ConstantExpr * cexpr) const {
  if (const ConstantInteger * ci = dyn_cast<ConstantInteger>(cexpr)) {
    return value_ == ci->value();
  }
  
  return false;
}

bool ConstantInteger::isNegative() const {
  const PrimitiveType * ptype = primitiveType();
  if (isUnsignedIntegerType(ptype->getTypeId())) {
    return false;
  }
  
  return value_->getValue().isNegative();
}

void ConstantInteger::format(FormatStream & out) const {
  if (getType()->isEqual(&BoolType::instance)) {
    out << (value_->isZero() ? "false" : "true");
  } else {
    const PrimitiveType * ptype = primitiveType();
    out << value_->getValue().toString(10, isSignedIntegerType(ptype->getTypeId()));
  }
}

ConstantInteger * ConstantInteger::getConstantBool(const SourceLocation & loc, bool value) {
  return new ConstantInteger(loc, &BoolType::instance,
      value ? llvm::ConstantInt::getTrue() : llvm::ConstantInt::getFalse());
}

ConstantInteger * ConstantInteger::get(const SourceLocation & loc, Type * type, int32_t value) {
  return new ConstantInteger(loc, type, llvm::ConstantInt::get(type->getIRType(), value));
}

ConstantInteger * ConstantInteger::get(const SourceLocation & loc, Type * type,
    llvm::ConstantInt * value) {
  return new ConstantInteger(loc, type, value);
}

ConstantInteger * ConstantInteger::getSigned(const llvm::APInt & value, PrimitiveType * type) {
  DASSERT(type->numBits() == value.getBitWidth());
  return new ConstantInteger(SourceLocation(), type, llvm::ConstantInt::get(value));
}

ConstantInteger * ConstantInteger::getUnsigned(const llvm::APInt & value, PrimitiveType * type) {
  DASSERT(type->numBits() == value.getBitWidth());
  return new ConstantInteger(SourceLocation(), type, llvm::ConstantInt::get(value));
}

// -------------------------------------------------------------------
// ConstantFloat
bool ConstantFloat::isEqual(const ConstantExpr * cexpr) const {
  if (const ConstantFloat * cf = dyn_cast<ConstantFloat>(cexpr)) {
    DFAIL("Implement");
  }
  
  return false;
}

void ConstantFloat::format(FormatStream & out) const {
  out << llvm::ftostr(value_->getValueAPF());
}

// -------------------------------------------------------------------
// ConstantString
ConstantString::ConstantString(SourceLocation l, const std::string & val)
  : ConstantExpr(ConstString, l, Builtins::typeString)
  , value_(val)
{
}

bool ConstantString::isEqual(const ConstantExpr * cexpr) const {
  if (const ConstantString * cs = dyn_cast<ConstantString>(cexpr)) {
    return cs->value() == value_;
  }
  
  return false;
}

void ConstantString::format(FormatStream & out) const {
  out << "\"" << value_ << "\"";
}

// -------------------------------------------------------------------
// ConstantNull
ConstantNull::ConstantNull(SourceLocation l)
  : ConstantExpr(ConstNull, l, &NullType::instance)
{}

ConstantNull::ConstantNull(SourceLocation l, Type * t)
  : ConstantExpr(ConstNull, l, t)
{}

bool ConstantNull::isEqual(const ConstantExpr * cexpr) const {
  return cexpr->exprType() == ConstNull;
}

void ConstantNull::format(FormatStream & out) const {
  out << "null";
}

/// -------------------------------------------------------------------
/// ConstantType
ConstantType::ConstantType(SourceLocation l, Type * val)
  : ConstantExpr(ConstType, l, Builtins::typeType)
  , value_(val)
{
  DASSERT(value_ != NULL);
  //DASSERT_OBJ(getType() != NULL, this);
}

ConstantType::ConstantType(SourceLocation l, TypeDefn * valDefn)
  : ConstantExpr(ConstType, l, Builtins::typeType)
  , value_(valDefn->getTypeValue())
{
  DASSERT(value_ != NULL);
  DASSERT_OBJ(getType() != NULL, this);
}

bool ConstantType::isSingular() const {
  return value_->isSingular();
}

bool ConstantType::isEqual(const ConstantExpr * cexpr) const {
  if (const ConstantType * ct = dyn_cast<ConstantType>(cexpr)) {
    return value()->isEqual(ct->value());
  }
  
  return false;
}

void ConstantType::format(FormatStream & out) const {
  out << value_;
}
  
} // namespace tart

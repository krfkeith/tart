/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Constant.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
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
  if (isUnsignedIntegerType(ptype->typeId())) {
    return false;
  }

  return value_->getValue().isNegative();
}

void ConstantInteger::format(FormatStream & out) const {
  if (type()->isEqual(&BoolType::instance)) {
    out << (value_->isZero() ? "false" : "true");
  } else {
    const PrimitiveType * ptype = primitiveType();
    out << value_->getValue().toString(10, isSignedIntegerType(ptype->typeId()));
  }
}

ConstantInteger * ConstantInteger::getConstantBool(const SourceLocation & loc, bool value) {
  return new ConstantInteger(loc, &BoolType::instance, value
      ? llvm::ConstantInt::getTrue(llvm::getGlobalContext())
      : llvm::ConstantInt::getFalse(llvm::getGlobalContext()));
}

ConstantInteger * ConstantInteger::get(const SourceLocation & loc, Type * type, int32_t value) {
  const llvm::Type * intType = type->irType();
  return new ConstantInteger(loc, type,
      cast<llvm::ConstantInt>(
          llvm::ConstantInt::get(intType,
              llvm::APInt(intType->getPrimitiveSizeInBits(), value, true))));
}

ConstantInteger * ConstantInteger::get(const SourceLocation & loc, Type * type,
    llvm::ConstantInt * value) {
  return new ConstantInteger(loc, type, value);
}

ConstantInteger * ConstantInteger::getSigned(const llvm::APInt & value, PrimitiveType * type) {
  DASSERT(type->numBits() == value.getBitWidth());
  return new ConstantInteger(SourceLocation(), type,
      cast<llvm::ConstantInt>(llvm::ConstantInt::get(type->irType(), value)));
}

ConstantInteger * ConstantInteger::getUnsigned(const llvm::APInt & value, PrimitiveType * type) {
  DASSERT(type->numBits() == value.getBitWidth());
  return new ConstantInteger(SourceLocation(), type,
      cast<llvm::ConstantInt>(llvm::ConstantInt::get(type->irType(), value)));
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
  //DASSERT_OBJ(type() != NULL, this);
}

ConstantType::ConstantType(SourceLocation l, TypeDefn * valDefn)
  : ConstantExpr(ConstType, l, Builtins::typeType)
  , value_(valDefn->typeValue())
{
  DASSERT(value_ != NULL);
  DASSERT_OBJ(type() != NULL, this);
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

void ConstantType::trace() const {
  Expr::trace();
  value_->mark();
}

void ConstantType::format(FormatStream & out) const {
  out << value_;
}

// -------------------------------------------------------------------
// ConstantObjectRef

ConstantObjectRef::ConstantObjectRef(SourceLocation l, Type * type)
  : Expr(ConstObjRef, l, type)
{
  CompositeType * ctype = cast<CompositeType>(type);
  DASSERT_OBJ(ctype->typeDefn()->isPassFinished(Pass_AnalyzeFields), ctype);
  members_.resize(ctype->instanceFieldCountRecursive());
  std::fill(members_.begin(), members_.end(), (Expr *)NULL);
}

bool ConstantObjectRef::isSingular() const {
  return type()->isSingular();
}

Expr * ConstantObjectRef::getMemberValue(VariableDefn * member) {
  DASSERT(member->memberIndexRecursive() < members_.size());
  return members_[member->memberIndexRecursive()];
}

void ConstantObjectRef::setMemberValue(VariableDefn * member, Expr * value) {
  DASSERT(member->memberIndexRecursive() < members_.size());
  members_[member->memberIndexRecursive()] = value;
}

Expr * ConstantObjectRef::getMemberValue(const char * name) {
  DefnList defs;
  type()->memberScope()->lookupMember(name, defs, true);
  if (defs.size() == 1) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(defs.front())) {
      return getMemberValue(var);
    }
  }

  return NULL;
}

int32_t ConstantObjectRef::getMemberValueAsInt(const char * name) {
  Expr * value = getMemberValue(name);
  if (ConstantInteger * cint = dyn_cast_or_null<ConstantInteger>(value)) {
    return int32_t(cint->value()->getSExtValue());
  }

  return 0;
}

void ConstantObjectRef::trace() const {
  Expr::trace();
  markList(members_.begin(), members_.end());
}

void ConstantObjectRef::format(FormatStream & out) const {
  out << "<" << type()->typeDefn()->qualifiedName() << ">";
}

} // namespace tart

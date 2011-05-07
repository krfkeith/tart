/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Constant.h"
#include "tart/Defn/VariableDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"
#include "tart/Objects/TargetSelection.h"
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
  if (isUnsignedIntegerTypeId(ptype->typeId())) {
    return false;
  }

  return value_->getValue().isNegative();
}

void ConstantInteger::format(FormatStream & out) const {
  if (type()->isEqual(&BoolType::instance)) {
    out << (value_->isZero() ? "false" : "true");
  } else {
    const PrimitiveType * ptype = primitiveType();
    out << value_->getValue().toString(10, isSignedIntegerTypeId(ptype->typeId()));
  }
}

ConstantInteger * ConstantInteger::getConstantBool(const SourceLocation & loc, bool value) {
  return new ConstantInteger(loc, &BoolType::instance, value
      ? llvm::ConstantInt::getTrue(llvm::getGlobalContext())
      : llvm::ConstantInt::getFalse(llvm::getGlobalContext()));
}

ConstantInteger * ConstantInteger::get(const SourceLocation & loc, const Type * type, int32_t value) {
  const llvm::Type * intType = type->irType();
  return new ConstantInteger(loc, type,
      cast<llvm::ConstantInt>(
          llvm::ConstantInt::get(intType,
              llvm::APInt(intType->getPrimitiveSizeInBits(), value, true))));
}

ConstantInteger * ConstantInteger::get(const SourceLocation & loc, const Type * type,
    llvm::ConstantInt * value) {
  return new ConstantInteger(loc, type, value);
}

ConstantInteger * ConstantInteger::getSInt32(int32_t value) {
  return get(SourceLocation(), &Int32Type::instance, value);
}

ConstantInteger * ConstantInteger::getUInt32(uint32_t value) {
  return get(SourceLocation(), &UInt32Type::instance, value);
}

ConstantInteger * ConstantInteger::getSInt(int32_t value) {
  const llvm::TargetData * td = TargetSelection::instance.targetData();
  const Type * intType = td != NULL && td->getPointerSizeInBits() >= 64 ?
      static_cast<const PrimitiveType *>(&Int64Type::instance) :
      static_cast<const PrimitiveType *>(&Int32Type::instance);
  return get(SourceLocation(), intType, value);
}

ConstantInteger * ConstantInteger::getUInt(uint32_t value) {
  const llvm::TargetData * td = TargetSelection::instance.targetData();
  const Type * intType = td != NULL && td->getPointerSizeInBits() >= 64 ?
      static_cast<const PrimitiveType *>(&UInt64Type::instance) :
      static_cast<const PrimitiveType *>(&UInt32Type::instance);
  return get(SourceLocation(), intType, value);
}

ConstantInteger * ConstantInteger::getSigned(const llvm::APInt & value,
    const PrimitiveType * type) {
  DASSERT(type->numBits() == value.getBitWidth());
  return new ConstantInteger(SourceLocation(), type,
      cast<llvm::ConstantInt>(llvm::ConstantInt::get(type->irType(), value)));
}

ConstantInteger * ConstantInteger::getUnsigned(const llvm::APInt & value,
    const PrimitiveType * type) {
  DASSERT(type->numBits() == value.getBitWidth());
  return new ConstantInteger(SourceLocation(), type,
      cast<llvm::ConstantInt>(llvm::ConstantInt::get(type->irType(), value)));
}

// -------------------------------------------------------------------
// ConstantFloat
bool ConstantFloat::isEqual(const ConstantExpr * cexpr) const {
  if (const ConstantFloat * cf = dyn_cast<ConstantFloat>(cexpr)) {
    (void)cf;
    DFAIL("Implement");
  }

  return false;
}

void ConstantFloat::format(FormatStream & out) const {
  out << llvm::ftostr(value_->getValueAPF());
}

// -------------------------------------------------------------------
// ConstantString
ConstantString::ConstantString(SourceLocation l, llvm::StringRef val)
  : ConstantExpr(ConstString, l, Builtins::typeString.peek())
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

ConstantNull::ConstantNull(SourceLocation l, const Type * t)
  : ConstantExpr(ConstNull, l, t)
{}

bool ConstantNull::isEqual(const ConstantExpr * cexpr) const {
  return cexpr->exprType() == ConstNull;
}

void ConstantNull::format(FormatStream & out) const {
  out << "null";
}

/// -------------------------------------------------------------------
/// TypeLiteralExpr
TypeLiteralExpr::TypeLiteralExpr(SourceLocation l, const Type * val)
  : ConstantExpr(TypeLiteral, l, TypeLiteralType::get(val))
  , value_(val)
{
  DASSERT(value_ != NULL);
}

bool TypeLiteralExpr::isSingular() const {
  return value_->isSingular();
}

bool TypeLiteralExpr::isEqual(const ConstantExpr * cexpr) const {
  if (const TypeLiteralExpr * ct = dyn_cast<TypeLiteralExpr>(cexpr)) {
    return value()->isEqual(ct->value());
  }

  return false;
}

void TypeLiteralExpr::trace() const {
  Expr::trace();
  value_->mark();
}

void TypeLiteralExpr::format(FormatStream & out) const {
  out << value_;
}

// -------------------------------------------------------------------
// ConstantObjectRef

ConstantObjectRef::ConstantObjectRef(SourceLocation l, const CompositeType * type)
  : Expr(ConstObjRef, l, type)
{
  const CompositeType * ctype = cast<CompositeType>(type);
  DASSERT_OBJ(ctype->passes().isFinished(CompositeType::FieldPass), ctype);
  members_.resize(ctype->instanceFieldCountRecursive());
  std::fill(members_.begin(), members_.end(), (Expr *)NULL);
}

bool ConstantObjectRef::isSingular() const {
  return type()->isSingular();
}

Expr * ConstantObjectRef::getMemberValue(VariableDefn * member) const {
  DASSERT((size_t) member->memberIndexRecursive() < members_.size());
  return members_[member->memberIndexRecursive()];
}

void ConstantObjectRef::setMemberValue(VariableDefn * member, Expr * value) {
  DASSERT((size_t) member->memberIndexRecursive() < members_.size());
  members_[member->memberIndexRecursive()] = value;
}

Expr * ConstantObjectRef::getMemberValue(const char * name) const {
  DefnList defs;
  type()->memberScope()->lookupMember(name, defs, true);
  if (defs.size() == 1) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(defs.front())) {
      return getMemberValue(var);
    }
  }

  return NULL;
}

void ConstantObjectRef::setMemberValue(const char * name, Expr * value) {
  DefnList defs;
  type()->memberScope()->lookupMember(name, defs, true);
  if (defs.size() == 1) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(defs.front())) {
      return setMemberValue(var, value);
    }
  }
}

int32_t ConstantObjectRef::memberValueAsInt(const char * name) {
  Expr * value = getMemberValue(name);
  if (ConstantInteger * cint = dyn_cast_or_null<ConstantInteger>(value)) {
    return int32_t(cint->value()->getSExtValue());
  }

  return 0;
}

void ConstantObjectRef::trace() const {
  Expr::trace();
  safeMarkList(members_.begin(), members_.end());
}

void ConstantObjectRef::format(FormatStream & out) const {
  out << "<" << type()->typeDefn()->qualifiedName() << ">";
}

// -------------------------------------------------------------------
// ConstantEmptyArray

ConstantEmptyArray::ConstantEmptyArray(SourceLocation l, const CompositeType * type)
  : Expr(ConstEmptyArray, l, type)
{
  const CompositeType * ctype = cast<CompositeType>(type);
  DASSERT_OBJ(ctype->passes().isFinished(CompositeType::FieldPass), ctype);
}

bool ConstantEmptyArray::isSingular() const {
  return type()->isSingular();
}

void ConstantEmptyArray::trace() const {
  Expr::trace();
}

void ConstantEmptyArray::format(FormatStream & out) const {
  out << "<" << type()->typeDefn()->qualifiedName() << ">";
}

/// -------------------------------------------------------------------
/// ConstantNativeArray
ConstantNativeArray::ConstantNativeArray(SourceLocation l, NativeArrayType * type)
  : Expr(ConstNArray, l, type)
{}

bool ConstantNativeArray::isSingular() const {
  // TODO: Check elements as well?
  return type()->isSingular();
}

void ConstantNativeArray::format(FormatStream & out) const {
  out << "[";
  for (ExprList::const_iterator it = elements_.begin(); it != elements_.end(); ++it) {
    if (it != elements_.begin()) {
      out << ", ";
    }

    out << *it;
  }

  out << "]";
}

void ConstantNativeArray::trace() const {
  Expr::trace();
  markList(elements_.begin(), elements_.end());
}

} // namespace tart

namespace llvm {

void FoldingSetTrait<tart::ConstantExpr *>::Profile(
    const tart::ConstantExpr * expr,
    FoldingSetNodeID &ID) {
  ID.AddInteger(expr->exprType());
  switch (int(expr->exprType())) {
    case tart::Expr::ConstInt: {
      const tart::ConstantInteger * cint = static_cast<const tart::ConstantInteger *>(expr);
      ID.Add(cint->value()->getValue());
      break;
    }

    case tart::Expr::ConstNull:
      break;

    case tart::Expr::ConstString: {
      const tart::ConstantString * cstr = static_cast<const tart::ConstantString *>(expr);
      ID.AddString(cstr->value());
      break;
    }

    default:
      tart::diag.fatal(expr) << "Unsupported type for folding set: " << expr->exprType();
  }
}

}

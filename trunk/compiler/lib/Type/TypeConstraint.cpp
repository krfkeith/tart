/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Defn/FunctionDefn.h"

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/TypeConstraint.h"
#include "tart/Type/TupleType.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/CallCandidate.h"

namespace tart {

// -------------------------------------------------------------------
// TypeConstraint

const llvm::Type * TypeConstraint::irType() const {
  DFAIL("TypeConstraint does not have an IRType");
}

/// -------------------------------------------------------------------
/// TypeSetConstraint

const Type * TypeSetConstraint::singularValue() const {
  TypeExpansion expansion;
  expand(expansion);
  if (expansion.size() == 1) {
    return *expansion.begin();
  }

  return NULL;
}

ConversionRank TypeSetConstraint::convertTo(const Type * toType, const Conversion & cn) const {
  // Makes no sense to return a value when converting to a constraint.
  if (cn.resultValue != NULL) {
    return Incompatible;
  }

  TypeExpansion expansion;
  expand(expansion);
  ConversionRank best = Incompatible;
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    ConversionRank rank = toType->canConvert(ty);
    if (rank > best) {
      best = rank;
      if (rank == IdenticalTypes) {
        break;
      }
    }
  }

  return best;
}

ConversionRank TypeSetConstraint::convertImpl(const Conversion & conversion) const {
  static int recursionCheck = 0;
  ++recursionCheck;
  DASSERT(recursionCheck < 50);
  TypeExpansion expansion;
  expand(expansion);
  ConversionRank best = Incompatible;
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    ConversionRank rank = ty->convert(conversion);
    if (rank > best) {
      best = rank;
      if (rank == IdenticalTypes) {
        break;
      }
    }
  }

  --recursionCheck;
  return best;
}

bool TypeSetConstraint::isEqual(const Type * other) const {
  // It's equal only if it's a equal to every member
  TypeExpansion expansion;
  expand(expansion);
  if (expansion.empty()) {
    return false;
  }
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    if (!ty->isEqual(other)) {
      return false;
    }
  }

  return true;
}

bool TypeSetConstraint::isSubtypeOf(const Type * other) const {
  DFAIL("Check");
  // It's a subtype only if it's a subtype of every member
  TypeExpansion expansion;
  expand(expansion);
  if (expansion.empty()) {
    return false;
  }
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    if (!ty->isSubtypeOf(other)) {
      diag.debug() << this << " is not a subtype of " << other;
      return false;
    }
  }

  return true;
}

bool TypeSetConstraint::includes(const Type * other) const {
  // Includes is true if any member of the set includes 'other'.
  TypeExpansion expansion;
  expand(expansion);
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    if (ty->includes(other)) {
      return true;
    }
  }

  return false;
}

bool TypeSetConstraint::isSingular() const {
  const Type * ty = singularValue();
  return ty != NULL; // && ty->isSingular();
}

bool TypeSetConstraint::isReferenceType() const {
  TypeExpansion expansion;
  expand(expansion);
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    if (!ty->isReferenceType()) {
      return false;
    }
  }

  return true;
}

void TypeSetConstraint::format(FormatStream & out) const {
  static int recursionCheck = 0;
  ++recursionCheck;
  DASSERT(recursionCheck < 50);

  TypeExpansion expansion;
  expand(expansion);
  out << "{";
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    if (it != expansion.begin()) {
      out << "|";
    }

    out << *it;
  }
  out << "}";
  --recursionCheck;
}

// -------------------------------------------------------------------
// ResultOfConstraint

void ResultOfConstraint::expand(TypeExpansion & out) const {
  const Candidates & cd = callExpr_->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    candidateResultType(*it)->expand(out);
  }
}

const Candidates & ResultOfConstraint::candidates() const {
  return callExpr_->candidates();
}

const Type * ResultOfConstraint::candidateResultType(const CallCandidate * cc) const {
  if (callExpr_->exprType() == Expr::Construct && cc->method()->isCtor()) {
    return cc->method()->functionType()->selfParam()->type();
  }

  return cc->resultType();
}

void ResultOfConstraint::trace() const {
  Type::trace();
  callExpr_->mark();
}

void ResultOfConstraint::format(FormatStream & out) const {
  const Type * singularType = callExpr_->singularResultType();
  if (singularType != NULL) {
    out << singularType;
    return;
  }

  out << "{Result: ";
  TypeSetConstraint::format(out);
  out << "}";
}

// -------------------------------------------------------------------
// ParameterOfConstraint

void ParameterOfConstraint::expand(TypeExpansion & out) const {
  const Candidates & cd = callExpr_->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    (*it)->paramType(argIndex_)->expand(out);
  }
}

const Candidates & ParameterOfConstraint::candidates() const {
  return callExpr_->candidates();
}

const Type * ParameterOfConstraint::candidateParamType(const CallCandidate * cc) const {
  return cc->paramType(argIndex_);
}

void ParameterOfConstraint::trace() const {
  Type::trace();
  callExpr_->mark();
}

void ParameterOfConstraint::format(FormatStream & out) const {
  out << "{" << callExpr_->candidates().front()->method()->name() << ".param[" <<
      argIndex_ << "]: ";
  TypeSetConstraint::format(out);
  out << "}";
}

// -------------------------------------------------------------------
// TypeParamOfConstraint

void TypeParamOfConstraint::expand(TypeExpansion & out) const {
  TypeExpansion baseExpansion;
  base_->expand(baseExpansion);
  for (TypeExpansion::const_iterator it = baseExpansion.begin(); it != baseExpansion.end(); ++it) {
    const Type * ty = dealias(*it);
    if (ty->typeClass() == cls_ && ty->numTypeParams() > paramIndex_) {
      ty->typeParam(paramIndex_)->expand(out);
    } else {
      out.insert(&BadType::instance);
    }
  }
}

const Type * TypeParamOfConstraint::forType(const Type * ty) const {
  switch (cls_) {
    case Type::Class:
    case Type::Interface:
    case Type::Struct:
    case Type::Protocol:
      if (isa<CompositeType>(ty) && ty->numTypeParams() > paramIndex_) {
        return ty->typeParam(paramIndex_);
      }
      break;

    case Type::NAddress:
    case Type::NArray:
    case Type::FlexibleArray:
      if (paramIndex_ == 0 && ty->typeClass() == cls_) {
        return ty->typeParam(0);
      }
      break;

    default:
      if (const TypeConstraint * tc = dyn_cast<TypeConstraint>(ty)) {
        return new TypeParamOfConstraint(tc, cls_, paramIndex_);
      }
      break;
  }

  return NULL;
}

void TypeParamOfConstraint::trace() const {
  Type::trace();
  base_->mark();
}

void TypeParamOfConstraint::format(FormatStream & out) const {
  out << "{TypeParam(" << paramIndex_ << "): ";
  base_->format(out);
  //TypeSetConstraint::format(out);
  out << "}";
}

// -------------------------------------------------------------------
// TupleOfConstraint

ConversionRank TupleOfConstraint::convertTo(const Type * toType, const Conversion & cn) const {
  DFAIL("Check");
  // Makes no sense to return a value when converting to a constraint.
  if (cn.resultValue != NULL) {
    return Incompatible;
  }

  ConversionRank rank = IdenticalTypes;
  if (const TupleType * ttype = dyn_cast<TupleType>(toType)) {
    if (ttype->numTypeParams() != tuple_->argCount()) {
      return Incompatible;
    }

    if (cn.resultValue != NULL) {
      DFAIL("Implement");
    }

    for (size_t i = 0; i < tuple_->argCount(); ++i) {
      Conversion argCn(tuple_->arg(i));
      rank = std::min(rank, ttype->members()[i]->convert(argCn));
    }

    return rank;
  } else if (const TupleOfConstraint * tctype = cast<TupleOfConstraint>(toType)) {
    if (tctype->numTypeParams() != tuple_->argCount()) {
      return Incompatible;
    }

    DFAIL("Implement");
  } else {
    return Incompatible;
  }

  return rank;

#if 0


  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * paramType = (*it)->paramType(argIndex);
    ConversionRank rank = toType->canConvert(paramType);
    if (rank > best) {
      best = rank;
      if (rank == IdenticalTypes) {
        break;
      }
    }
  }

  return best;
#endif
}

ConversionRank TupleOfConstraint::convertImpl(const Conversion & conversion) const {
  DFAIL("Check");
  ConversionRank rank = IdenticalTypes;
  if (const TupleType * ttype = dyn_cast<TupleType>(conversion.fromType)) {
    if (ttype->numTypeParams() != tuple_->argCount()) {
      return Incompatible;
    }

    DFAIL("Implement");
  } else if (const TupleOfConstraint * tctype = cast<TupleOfConstraint>(conversion.fromType)) {
    if (tctype->numTypeParams() != tuple_->argCount()) {
      return Incompatible;
    }

    DFAIL("Implement");
  } else {
    return Incompatible;
  }

  return rank;
}

const Type * TupleOfConstraint::singularValue() const {
  return tuple_->type();
}

bool TupleOfConstraint::isSingular() const {
  DFAIL("Check");
  return tuple_->isSingular();
}

bool TupleOfConstraint::isSubtypeOf(const Type * other) const {
  DFAIL("Implement");
}

bool TupleOfConstraint::includes(const Type * other) const {
  DFAIL("Check");
  if (const TupleType * ttype = dyn_cast<TupleType>(other)) {
    if (ttype->numTypeParams() != tuple_->argCount()) {
      return false;
    }

    DFAIL("Implement");
  } else if (const TupleOfConstraint * tctype = cast<TupleOfConstraint>(other)) {
    if (tctype->numTypeParams() != tuple_->argCount()) {
      return false;
    }

    DFAIL("Implement");
  } else {
    return false;
  }

  return false;

#if 0
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * paramType = (*it)->paramType(argIndex);
    if (paramType->includes(other)) {
      return true;
    }
  }

  return false;
#endif
}

bool TupleOfConstraint::isReferenceType() const {
  DFAIL("Illegal State");
}

void TupleOfConstraint::trace() const {
  Type::trace();
  tuple_->mark();
}

void TupleOfConstraint::format(FormatStream & out) const {
  out << tuple_;
}

// -------------------------------------------------------------------
// SizingOfConstraint

unsigned SizingOfConstraint::signedBitsRequired() const {
  const llvm::APInt & intVal = intVal_->value()->getValue();
  return intVal.getMinSignedBits();
}

unsigned SizingOfConstraint::unsignedBitsRequired() const {
  const llvm::APInt & intVal = intVal_->value()->getValue();
  return isNegative_ ? INT32_MAX : intVal.getActiveBits();
}

bool SizingOfConstraint::isEqual(const Type * other) const {
  if (const SizingOfConstraint * soc = dyn_cast_or_null<SizingOfConstraint>(other)) {
    return isNegative_ == soc->isNegative() && intVal_->isEqual(soc->intVal());
  } else {
    return false;
  }
}

const Type * SizingOfConstraint::singularValue() const {
  DFAIL("Implement");
}

ConversionRank SizingOfConstraint::convertTo(const Type * toType, const Conversion & cn) const {
  return Incompatible;
//  Conversion c2(const_cast<ConstantInteger *>(intVal_), cn.resultValue, cn.options);
//  return toType->convert(c2);
}

ConversionRank SizingOfConstraint::convertImpl(const Conversion & conversion) const {
  // Can't convert *to* an unsized type.
  return Incompatible;
}

bool SizingOfConstraint::includes(const Type * other) const {
  return intVal_->type()->includes(other);
}

bool SizingOfConstraint::isSubtypeOf(const Type * other) const {
  if (const SizingOfConstraint * soc = dyn_cast<SizingOfConstraint>(other)) {
    if (soc == other) {
      return true;
    }
    diag.debug() << this;
    diag.debug() << soc;
    DFAIL("Implement");
    (void)soc;
  } else if (const PrimitiveType * pty = dyn_cast<PrimitiveType>(other)) {
    if (isNegative() && pty->isUnsignedType()) {
      return false;
    }

    if (pty->isUnsignedType()) {
      return unsignedBitsRequired() <= pty->numBits();
    } else {
      return signedBitsRequired() <= pty->numBits();
    }
  } else {
    return false;
  }
}

void SizingOfConstraint::trace() const {
  Type::trace();
  intVal_->mark();
}

void SizingOfConstraint::format(FormatStream & out) const {
  out << intVal_->value()->getValue().toString(10, true);
}

// -------------------------------------------------------------------
// PhiTypeConstraint

void PHIConstraint::add(const Type * type) {
  DASSERT(type != NULL);
  types_.push_back(type);
}

void PHIConstraint::expand(TypeExpansion & out) const {
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    const Type * ty = *it;
    ty->expand(out);
  }
}

const Type * PHIConstraint::singularValue() const {
  DFAIL("Implement");
}

ConversionRank PHIConstraint::convertTo(const Type * toType, const Conversion & cn) const {
  // Makes no sense to return a value when converting to a constraint.
  if (cn.resultValue != NULL) {
    return Incompatible;
  }

  if (toType == this) {
    return IdenticalTypes;
  }

  ConversionRank worstRank = IdenticalTypes;
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    const Type * ty = *it;
    ConversionRank rank = toType->canConvert(ty);
    if (rank < worstRank) {
      worstRank = rank;
      if (rank == Incompatible) {
        break;
      }
    }
  }

  return worstRank;
}

ConversionRank PHIConstraint::convertImpl(const Conversion & conversion) const {
  ConversionRank worstRank = IdenticalTypes;

  if (conversion.fromType == this) {
    return IdenticalTypes;
  }

  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    const Type * ty = *it;
    ConversionRank rank = ty->convert(conversion);
    if (rank < worstRank) {
      worstRank = rank;
      if (rank == Incompatible) {
        break;
      }
    }
  }

  return worstRank;
}

bool PHIConstraint::includes(const Type * other) const {
  DFAIL("Check");
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    if (!(*it)->includes(other)) {
      return false;
    }
  }

  return true;
}

bool PHIConstraint::isSubtypeOf(const Type * other) const {
  DFAIL("Check");
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    if (!(*it)->isSubtypeOf(other)) {
      return false;
    }
  }

  return true;
}

bool PHIConstraint::isSingular() const {
  return common_ != NULL && common_->isSingular();
}

bool PHIConstraint::isReferenceType() const {
  DFAIL("Check");
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    if ((*it)->isReferenceType()) {
      return true;
    }
  }

  return false;
}

void PHIConstraint::trace() const {
  TypeConstraint::trace();
  markList(types_.begin(), types_.end());
}

void PHIConstraint::format(FormatStream & out) const {
  out << "{PHI: ";
  for (ConstTypeList::const_iterator it = types_.begin(), itEnd = types_.end(); it != itEnd; ++it) {
    if (it != types_.begin()) {
      out << "|";
    }
    out << *it;
  }
  out << "}";
}

} // namespace tart

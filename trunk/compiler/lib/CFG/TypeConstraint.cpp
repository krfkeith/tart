/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Exprs.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeConstraint.h"
#include "tart/CFG/TupleType.h"
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

  return best;
}

bool TypeSetConstraint::isSubtype(const Type * other) const {
  // It's a subtype only if it's a subtype of every member
  TypeExpansion expansion;
  expand(expansion);
  if (expansion.empty()) {
    return false;
  }
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    const Type * ty = *it;
    if (!ty->isSubtype(other)) {
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
  DFAIL("Illegal State");
}

void TypeSetConstraint::format(FormatStream & out) const {
  TypeExpansion expansion;
  expand(expansion);
  for (TypeExpansion::const_iterator it = expansion.begin(); it != expansion.end(); ++it) {
    if (it != expansion.begin()) {
      out << "|";
    }

    out << *it;
  }
}

// -------------------------------------------------------------------
// ResultOfConstraint

void ResultOfConstraint::expand(TypeExpansion & out) const {
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    out.insert(candidateResultType(*it));
  }
}

bool ResultOfConstraint::unifyWithPattern(BindingEnv &env, const Type * pattern) const {
  Substitution * saveSub = env.substitutions();
  Candidates & cd = callExpr->candidates();
  SourceContext callSite(callExpr, NULL, callExpr);
  bool match = false;
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
//    if ((*it)->isCulled()) {
//      continue;
//    }

    const Type * resultType = candidateResultType(*it);
    SourceContext candidateSite((*it)->method(), &callSite, (*it)->method(), Format_Type);
    if (env.unify(&candidateSite, pattern, resultType, Invariant)) {
      if (match) {
        env.setSubstitutions(saveSub);
        return false;
      } else {
        match = true;
      }
    }

    env.setSubstitutions(saveSub);
  }

  return match;
}

const Type * ResultOfConstraint::candidateResultType(const CallCandidate * cc) const {
  if (callExpr->exprType() == Expr::Construct && cc->method()->isCtor()) {
    return cc->method()->functionType()->selfParam()->type();
  }

  return cc->resultType();
}

void ResultOfConstraint::trace() const {
  Type::trace();
  callExpr->mark();
}

void ResultOfConstraint::format(FormatStream & out) const {
  const Type * singularType = callExpr->singularResultType();
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
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    out.insert((*it)->paramType(argIndex));
  }
}

bool ParameterOfConstraint::unifyWithPattern(BindingEnv &env, const Type * pattern) const {
  Candidates & cd = callExpr->candidates();
  Substitution * saveSub = env.substitutions();
  SourceContext callSite(callExpr, NULL, callExpr);
  bool match = false;
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
//    if ((*it)->isCulled()) {
//      continue;
//    }

    const Type * paramType = (*it)->paramType(argIndex);
    SourceContext candidateSite((*it)->method(), &callSite, (*it)->method(), Format_Type);
    if (env.unify(&candidateSite, pattern, paramType, Contravariant)) {
      if (match) {
        env.setSubstitutions(saveSub);
        return true;
        //Substitution * saveSub = env.substitutions();
      } else {
        match = true;
      }
    }

    env.setSubstitutions(saveSub);
  }

  return match;
}

void ParameterOfConstraint::trace() const {
  Type::trace();
  callExpr->mark();
}

void ParameterOfConstraint::format(FormatStream & out) const {
  out << "{Param(" << argIndex << "): ";
  TypeSetConstraint::format(out);
  out << "}";
}

// -------------------------------------------------------------------
// SingleTypeParamOfConstraint

void SingleTypeParamOfConstraint::expand(TypeExpansion & out) const {
  TypeExpansion baseExpansion;
  base_->expand(baseExpansion);
  for (TypeExpansion::const_iterator it = baseExpansion.begin(); it != baseExpansion.end(); ++it) {
    const Type * ty = dealias(*it);
    if (ty->typeClass() == cls_) {
      out.insert(ty->typeParam(0));
    }
  }
}

bool SingleTypeParamOfConstraint::unifyWithPattern(BindingEnv &env, const Type * pattern) const {
  diag.debug() << "Can't unify " << pattern << " with " << this;
  return false;
}

void SingleTypeParamOfConstraint::trace() const {
  Type::trace();
  base_->mark();
}

void SingleTypeParamOfConstraint::format(FormatStream & out) const {
  out << "{TypeParam(0): ";
  base_->format(out);
  //TypeSetConstraint::format(out);
  out << "}";
}

// -------------------------------------------------------------------
// TupleOfConstraint

void TupleOfConstraint::expand(TypeExpansion & out) const {
  out.insert(this);
}

ConversionRank TupleOfConstraint::convertTo(const Type * toType, const Conversion & cn) const {
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
    if (ttype->numTypeParams() != tuple_->argCount()) {
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
  ConversionRank rank = IdenticalTypes;
  if (const TupleType * ttype = dyn_cast<TupleType>(conversion.fromType)) {
    if (ttype->numTypeParams() != tuple_->argCount()) {
      return Incompatible;
    }

    DFAIL("Implement");
  } else if (const TupleOfConstraint * tctype = cast<TupleOfConstraint>(conversion.fromType)) {
    if (ttype->numTypeParams() != tuple_->argCount()) {
      return Incompatible;
    }

    DFAIL("Implement");
  } else {
    return Incompatible;
  }

  return rank;

#if 0
  ConversionRank rank = IdenticalTypes;
  if (TupleType * ttype = dyn_cast<TupleType>(toType)) {

  }

  ConversionRank best = Incompatible;
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * paramType = (*it)->paramType(argIndex);
    ConversionRank rank = paramType->convert(conversion);
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

const Type * TupleOfConstraint::singularValue() const {
  return tuple_->type();
}

bool TupleOfConstraint::unifyWithPattern(BindingEnv &env, const Type * pattern) const {
  DFAIL("Implement");
#if 0
  Candidates & cd = callExpr->candidates();
  Substitution * saveSub = env.substitutions();
  SourceContext callSite(callExpr, NULL, callExpr);
  bool match = false;
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * paramType = (*it)->paramType(argIndex);
    SourceContext candidateSite((*it)->method(), &callSite, (*it)->method(), Format_Type);
    if (env.unify(&candidateSite, pattern, paramType, Invariant)) {
      if (match) {
        env.setSubstitutions(saveSub);
        return true;
        //Substitution * saveSub = env.substitutions();
      } else {
        match = true;
      }
    }

    env.setSubstitutions(saveSub);
  }

  return match;
#endif
}

bool TupleOfConstraint::isSingular() const {
  return tuple_->isSingular();
}

bool TupleOfConstraint::isSubtype(const Type * other) const {
  DFAIL("Implement");
}

bool TupleOfConstraint::includes(const Type * other) const {
  ConversionRank rank = IdenticalTypes;
  if (const TupleType * ttype = dyn_cast<TupleType>(other)) {
    if (ttype->numTypeParams() != tuple_->argCount()) {
      return false;
    }

    DFAIL("Implement");
  } else if (const TupleOfConstraint * tctype = cast<TupleOfConstraint>(other)) {
    if (ttype->numTypeParams() != tuple_->argCount()) {
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

} // namespace tart

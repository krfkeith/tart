/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Expr.h"
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

// -------------------------------------------------------------------
// ResultOfConstraint

ConversionRank ResultOfConstraint::convertTo(const Type * toType) const {
  ConversionRank best = Incompatible;
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * resultType = (*it)->resultType();
    ConversionRank rank = toType->canConvert(resultType);
    if (rank > best) {
      best = rank;
      if (rank == IdenticalTypes) {
        break;
      }
    }
  }

  return best;
}

ConversionRank ResultOfConstraint::convertImpl(const Conversion & conversion) const {
  ConversionRank best = Incompatible;
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * resultType = (*it)->resultType();
    ConversionRank rank = resultType->convert(conversion);
    if (rank > best) {
      best = rank;
      if (rank == IdenticalTypes) {
        break;
      }
    }
  }

  return best;
}

bool ResultOfConstraint::unifyWithPattern(BindingEnv &env, const Type * pattern) const {
  Substitution * saveSub = env.substitutions();
  Candidates & cd = callExpr->candidates();
  SourceContext callSite(callExpr, NULL, callExpr);
  bool match = false;
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * resultType = (*it)->resultType();
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

const Type * ResultOfConstraint::singularValue() const {
  const Type * result = NULL;
  Candidates & cd = callExpr->candidates();
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    if (result != NULL) {
      return NULL;
    }

    result = (*it)->resultType();
  }

  return result;
}

bool ResultOfConstraint::isSubtype(const Type * other) const {
  // It's a subtype only if it's a subtype of every member
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * resultType = (*it)->resultType();
    if (!resultType->isSubtype(other)) {
      return false;
    }
  }

  return true;
}

bool ResultOfConstraint::includes(const Type * other) const {
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * resultType = (*it)->resultType();
    if (resultType->includes(other)) {
      return true;
    }
  }

  return false;
}

bool ResultOfConstraint::isSingular() const {
  return callExpr->singularResultType() != NULL;
}

bool ResultOfConstraint::isReferenceType() const {
  DFAIL("Illegal State");
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
  const Candidates & cd = callExpr->candidates();
  bool first = true;
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * resultType = (*it)->resultType();
    if (!first) {
      out << "|";
    }

    out << resultType;
    first = false;
  }

  out << "}";
}

// -------------------------------------------------------------------
// ParameterOfConstraint

ConversionRank ParameterOfConstraint::convertTo(const Type * toType) const {
  ConversionRank best = Incompatible;
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
}

ConversionRank ParameterOfConstraint::convertImpl(const Conversion & conversion) const {
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
}

const Type * ParameterOfConstraint::singularValue() const {
  const Type * result = NULL;
  Candidates & cd = callExpr->candidates();
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    if (result != NULL) {
      return NULL;
    }

    result = (*it)->paramType(argIndex);
  }

  return result;
}

bool ParameterOfConstraint::unifyWithPattern(BindingEnv &env, const Type * pattern) const {
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
}

bool ParameterOfConstraint::isSingular() const {
  return callExpr->singularParamType(argIndex) != NULL;
}

bool ParameterOfConstraint::isSubtype(const Type * other) const {
  DFAIL("Implement");
}

bool ParameterOfConstraint::includes(const Type * other) const {
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
}

bool ParameterOfConstraint::isReferenceType() const {
  DFAIL("Illegal State");
}

void ParameterOfConstraint::trace() const {
  Type::trace();
  callExpr->mark();
}

void ParameterOfConstraint::format(FormatStream & out) const {
  out << "{Param(" << argIndex << "): ";
  const Candidates & cd = callExpr->candidates();
  bool first = true;
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    const Type * paramType = (*it)->paramType(argIndex);
    if (!first) {
      out << "|";
    }

    out << paramType;
    first = false;
  }
  out << "}";
}

// -------------------------------------------------------------------
// TupleOfConstraint

ConversionRank TupleOfConstraint::convertTo(const Type * toType) const {
  ConversionRank rank = IdenticalTypes;
  if (const TupleType * ttype = dyn_cast<TupleType>(toType)) {
    if (ttype->numTypeParams() != tuple_->argCount()) {
      return Incompatible;
    }

    DFAIL("Implement");
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

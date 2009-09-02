/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Expr.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeConstraint.h"
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

    Type * resultType = (*it)->resultType();
    ConversionRank rank = toType->convert(resultType);
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

    Type * resultType = (*it)->resultType();
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

bool ResultOfConstraint::unifyWithPattern(BindingEnv &env, Type * pattern) {
  Substitution * saveSub = env.substitutions();
  Candidates & cd = callExpr->candidates();
  SourceContext callSite(callExpr, NULL, callExpr);
  bool match = false;
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    Type * resultType = (*it)->resultType();
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

bool ResultOfConstraint::isSubtype(const Type * other) const {
  // It's a subtype only if it's a subtype of every member
  const Candidates & cd = callExpr->candidates();
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    Type * resultType = (*it)->resultType();
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

    Type * resultType = (*it)->resultType();
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
  out << "{Result: ";
  const Candidates & cd = callExpr->candidates();
  bool first = true;
  for (Candidates::const_iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    Type * resultType = (*it)->resultType();
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

    Type * paramType = (*it)->paramType(argIndex);
    ConversionRank rank = toType->convert(paramType);
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

    Type * paramType = (*it)->paramType(argIndex);
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

bool ParameterOfConstraint::unifyWithPattern(BindingEnv &env, Type * pattern) {
  Candidates & cd = callExpr->candidates();
  Substitution * saveSub = env.substitutions();
  SourceContext callSite(callExpr, NULL, callExpr);
  bool match = false;
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->isCulled()) {
      continue;
    }

    Type * paramType = (*it)->paramType(argIndex);
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

    Type * paramType = (*it)->paramType(argIndex);
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

    Type * paramType = (*it)->paramType(argIndex);
    if (!first) {
      out << "|";
    }

    out << paramType;
    first = false;
  }
  out << "}";
}

} // namespace tart

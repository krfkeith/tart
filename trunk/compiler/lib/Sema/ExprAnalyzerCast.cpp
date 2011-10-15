/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/NamespaceDefn.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/TupleType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/SpCandidate.h"

namespace tart {

extern bool unifyVerbose;

Expr * ExprAnalyzer::doImplicitCast(Expr * in, QualifiedType toType, unsigned options) {
  DASSERT(in != NULL);
  if (isErrorResult(toType)) {
    return in;
  }

  // Special case handling for expressions that simply pass through an underlying type.
  switch (in->exprType()) {
    case Expr::Seq: {
      SeqExpr * seq = static_cast<SeqExpr *>(in);
      Expr * lastArg = doImplicitCast(seq->args().back(), toType, options);
      seq->setType(lastArg->type());
      seq->args().back() = lastArg;
      return in;
    }

    case Expr::Case: {
      CaseExpr * ce = static_cast<CaseExpr *>(in);
      ce->setBody(doImplicitCast(ce->body(), toType, options));
      ce->setType(ce->body()->type());
      return in;
    }

    case Expr::MatchAs: {
      MatchAsExpr * me = static_cast<MatchAsExpr *>(in);
      me->setBody(doImplicitCast(me->body(), toType, options));
      me->setType(me->body()->type());
      return in;
    }

    case Expr::Catch: {
      CatchExpr * ce = static_cast<CatchExpr *>(in);
      ce->setBody(doImplicitCast(ce->body(), toType, options));
      ce->setType(ce->body()->type());
      return in;
    }

    case Expr::Try: {
      TryExpr * te = static_cast<TryExpr *>(in);
      te->setBody(doImplicitCast(te->body(), toType, options));
      te->setType(te->body()->type());
      return in;
    }

    case Expr::Return:
    case Expr::Yield:
    case Expr::Break:
    case Expr::Continue:
    case Expr::Throw:
      // Non-local exits don't have a result type and thus cannot be cast.
      return in;

    default:
      break;
  }

  in = LValueExpr::constValue(in);
  if (!AnalyzerBase::analyzeType(toType, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  Expr * castExpr = NULL;
  unsigned conversionOpts = 0;
  if (options & AO_IMPLICIT_CAST) {
    conversionOpts |= TypeConversion::COERCE;
  }
  if (options & AO_EXPLICIT_CAST) {
    conversionOpts |= TypeConversion::EXPLICIT;
  }
  if (options & AO_IGNORE_QUALIFIERS) {
    conversionOpts |= TypeConversion::UNQUAL;
  }
  ConversionRank rank;
  llvm::tie(rank, castExpr) = TypeConversion::convert(in, toType, conversionOpts);
  DASSERT(rank <= QualifierLoss || castExpr != NULL) << "NULL result attempting to cast '" << in <<
      "' from type '" << in->type() << "' to type '" << toType << "'.";

  if (rank == Incompatible && (options & (AO_IMPLICIT_CAST | AO_EXPLICIT_CAST))) {
    // Try a coercive cast. Note that we don't do this in 'convert' because it
    // can't handle building the actual call expression.
    castExpr = tryCoerciveCast(in, toType.type());
    if (castExpr != NULL) {
      Expr * result = inferTypes(subject_, castExpr, toType, false);
      return result;
    }
  }

  // Call again - for debugging purposes (set breakpoint here.)
  if (rank <= QualifierLoss) {
    TypeConversion::convert(in, toType, conversionOpts);
  }

  compatibilityWarning(in->location(), rank, in, toType);
  if (isErrorResult(castExpr)) {
    return &Expr::ErrorVal;
  }

  return castExpr;
}

Expr * ExprAnalyzer::doBoxCast(Expr * in) {
  QualifiedType fromType = dealias(in->type());
  FunctionDefn * coerceFn = coerceToObjectFn(fromType.type());
  FnCallExpr * call = new FnCallExpr(Expr::FnCall, in->location(), coerceFn, NULL);
  call->appendArg(in);
  call->setType(Builtins::typeObject.get());
  return call;
}

/** Given a type, return the coercion function to convert it to a reference type. */
FunctionDefn * ExprAnalyzer::coerceToObjectFn(const Type * type) {
  DASSERT(!type->isReferenceType());
  DASSERT(type->typeClass() != Type::NAddress);
  DASSERT(type->typeClass() != Type::NArray);
  DASSERT(type->typeClass() != Type::FlexibleArray);
  DASSERT(!type->isVoidType());
  DASSERT(!type->isNullType());
  DASSERT(type->isSingular());

  TypePair conversionKey(type, Builtins::typeObject.get());
  ConverterMap::iterator it = Builtins::module.converters().find(conversionKey);
  if (it != Builtins::module.converters().end()) {
    module()->addSymbol(it->second);
    return it->second;
  }

  FunctionDefn * coerceFn = Builtins::objectCoerceFn();
  Template * coerceTemplate = coerceFn->templateSignature();

  DASSERT_OBJ(coerceTemplate->paramScope().count() == 1, type);
  // Do analysis on template if needed.
  if (coerceTemplate->ast() != NULL) {
    DefnAnalyzer da(&Builtins::module, &Builtins::module, &Builtins::module, NULL);
    da.analyzeTemplateSignature(coerceFn);
  }

  QualifiedTypeVarMap vars;
  vars[coerceTemplate->patternVar(0)] = type;
  FunctionDefn * coercer = cast<FunctionDefn>(coerceTemplate->instantiate(SourceLocation(), vars));
  analyzeFunction(coercer, Task_PrepTypeComparison);
  DASSERT(coercer->isSingular());
  Builtins::module.converters()[conversionKey] = coercer;
  module()->addSymbol(coercer);
  return coercer;
}

Expr * ExprAnalyzer::doUnboxCast(Expr * in, QualifiedType toType) {
  FunctionDefn * valueOfMethod = getUnboxFn(in->location(), toType.unqualified());
  if (valueOfMethod == NULL) {
    return NULL;
  }

  DASSERT(valueOfMethod->isSingular());
  FnCallExpr * call = new FnCallExpr(Expr::FnCall, in->location(), valueOfMethod, NULL);
  call->appendArg(doImplicitCast(in, Builtins::typeObject.get()));
  call->setType(valueOfMethod->returnType() | toType.qualifiers());
  return call;
}

FunctionDefn * ExprAnalyzer::getUnboxFn(SLC & loc, const Type * toType) {
  DASSERT(!toType->isNullType());
  DASSERT(!toType->isVoidType());

  //diag.debug(loc) << Format_Type << "Defining unbox function for " << toType << " in module " << module()->linkageName();
  TypePair conversionKey(Builtins::typeObject.get(), toType);
  ConverterMap::iterator it = Builtins::module.converters().find(conversionKey);
  if (it != Builtins::module.converters().end()) {
    module()->addSymbol(it->second);
    return it->second;
  }

  //diag.debug(loc) << Format_Type << "Defining unbox function for " << toType;
  LookupResults methods;
  analyzeDefn(Builtins::nsRefs, Task_PrepMemberLookup);
  findInScope(methods, "valueOf", &Builtins::nsRefs->memberScope(), NULL, loc, NO_PREFERENCE);
  DASSERT(!methods.empty());
  Expr * valueOf = specialize(loc, methods, TupleType::get(QualifiedType(toType)));
  FunctionDefn * valueOfMethod;
  if (SpecializeExpr * spe = dyn_cast<SpecializeExpr>(valueOf)) {
    valueOfMethod = cast_or_null<FunctionDefn>(findBestSpecialization(spe));
    if (valueOfMethod == NULL) {
      return NULL;
    }
  } else if (LValueExpr * lval = dyn_cast<LValueExpr>(valueOf)) {
    valueOfMethod = cast<FunctionDefn>(lval->value());
  } else {
    diag.error(loc) << "Unknown expression " << valueOf;
    DFAIL("IllegalState");
  }

  analyzeFunction(valueOfMethod, Task_PrepTypeComparison);
  Builtins::module.converters()[conversionKey] = valueOfMethod;
  module()->addSymbol(valueOfMethod);
  //diag.info() << Format_Verbose << "Generated boxer " << valueOfMethod;
  return valueOfMethod;
}

FunctionDefn * ExprAnalyzer::getDowncastFn(SLC & loc, const Type * toType) {
  TypePair conversionKey(Builtins::typeObject.get(), toType);
  ConverterMap::iterator it = Builtins::module.converters().find(conversionKey);
  if (it != Builtins::module.converters().end()) {
    module()->addSymbol(it->second);
    return it->second;
  }

  //diag.debug(loc) << Format_Type << "Defining downcast function for " << toType << " in module " << module()->linkageName();
  LookupResults methods;
  analyzeTypeDefn(Builtins::typeObject->typeDefn(), Task_PrepMemberLookup);
  findInScope(methods, "__downcast", Builtins::typeObject->memberScope(), NULL, loc, NO_PREFERENCE);
  DASSERT(!methods.empty());
  Expr * downCast = specialize(loc, methods, TupleType::get(QualifiedType(toType)));
  FunctionDefn * downCastMethod;
  if (SpecializeExpr * spe = dyn_cast<SpecializeExpr>(downCast)) {
    downCastMethod = cast_or_null<FunctionDefn>(findBestSpecialization(spe));
    if (downCastMethod == NULL) {
      return NULL;
    }
  } else if (LValueExpr * lval = dyn_cast<LValueExpr>(downCast)) {
    downCastMethod = cast<FunctionDefn>(lval->value());
  } else {
    diag.error(loc) << "Unknown expression " << downCast;
    DFAIL("IllegalState");
  }

  analyzeFunction(downCastMethod, Task_PrepTypeComparison);
  Builtins::module.converters()[conversionKey] = downCastMethod;
  module()->addSymbol(downCastMethod);
  //diag.info() << Format_Verbose << "Generated boxer " << downCastMethod;
  return downCastMethod;
}

Defn * ExprAnalyzer::findBestSpecialization(SpecializeExpr * spe) {
  const SpCandidateList & candidates = spe->candidates();
  ConversionRank bestRank = Incompatible;
  BindingEnv env;

  // Now, for each parameter attempt unification.
  SourceContext callSite(spe, NULL, spe);
  for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
    SpCandidate * sp = *it;
    SourceContext candidateSite(sp->def()->location(), &callSite, sp->def(), Format_Type);
    sp->relabelTypeVars(env);
    if (sp->unify(&candidateSite, env)) {
      bestRank = std::max(bestRank, sp->updateConversionRank());
    }
  }

  env.reset();

  if (bestRank == Incompatible) {
    unifyVerbose = true;
    if (!spe->args()->containsBadType()) {
      SpCandidate * front = *candidates.begin();
      diag.error(spe) << "No candidate found for '" << front->def()->name() <<
          "' which matches template arguments [" << spe->args() << "]:";
      diag.info(spe) << "Candidates are:";
      for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
        SpCandidate * sp = *it;
        SourceContext candidateSite(sp->def()->location(), &callSite, sp->def(), Format_Type);
        if (sp->unify(&candidateSite, env)) {
          sp->updateConversionRank(); // Helps with debugging.
        }
        diag.info(sp->def()) << Format_Type << sp->def() << " [" << sp->conversionRank() << "]";
      }
    }
    unifyVerbose = false;
    return NULL;
  }

  SpCandidateList bestCandidates;
  for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
    SpCandidate * sp = *it;
    if (sp->conversionRank() == bestRank) {
      bool addNew = true;
      for (SpCandidateList::iterator bc = bestCandidates.begin(); bc != bestCandidates.end();) {
        bool newIsBetter = sp->isMoreSpecific(*bc);
        bool oldIsBetter = (*bc)->isMoreSpecific(sp);

        if (newIsBetter) {
          if (!oldIsBetter) {
            /*if (ShowInference) {
              diag.debug() << Format_Type << "Culling [" << (*ms)->method() <<
                  "] because [" << call->method() << "] is more specific";
            }*/
            bc = bestCandidates.erase(bc);
            continue;
          }
        } else if (oldIsBetter) {
          /*if (ShowInference) {
            diag.debug() << Format_Type << "Culling [" << call->method() << "] because [" <<
                (*ms)->method() << "] is more specific";
          }*/

          addNew = false;
        }

        ++bc;
      }

      if (addNew) {
        bestCandidates.push_back(sp);
      }
    }
  }

  if (bestCandidates.size() > 1) {
    SpCandidate * front = *candidates.begin();
    diag.error(spe) << "Ambiguous matches for '" << front->def()->name() <<
        "' which matches template arguments [" << spe->args() << "]:";
    diag.info(spe) << "Candidates are:";
    for (SpCandidateList::const_iterator it = bestCandidates.begin(); it != bestCandidates.end();
        ++it) {
      SpCandidate * sp = *it;
      diag.info(sp->def()) << Format_Type << sp->def() << " [" << sp->conversionRank() << "]";
    }
    DFAIL("Implement better culling of SpCandidates");
  }

  SpCandidate * spBest = *bestCandidates.begin();
  QualifiedTypeVarMap vars;
  env.updateAssignments(SourceLocation(), spBest);
  env.toTypeVarMap(vars, spBest);
  if (spBest->def()->hasUnboundTypeParams()) {
    return spBest->def()->templateSignature()->instantiate(spe->location(), vars);
  }

  return spBest->def();
}

}

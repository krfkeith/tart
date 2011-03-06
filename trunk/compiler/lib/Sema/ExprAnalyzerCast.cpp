/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Exprs.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PropertyDefn.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/NamespaceDefn.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/SpCandidate.h"
#include "tart/Sema/FinalizeTypesPass.h"

namespace tart {

Expr * ExprAnalyzer::doImplicitCast(Expr * in, const Type * toType, bool tryCoerce) {
  DASSERT(in != NULL);
  if (isErrorResult(toType)) {
    return in;
  }

  in = LValueExpr::constValue(in);
  if (!AnalyzerBase::analyzeType(toType, Task_PrepTypeComparison)) {
    return in;
  }

  Expr * castExpr = NULL;
  ConversionRank rank = toType->convert(Conversion(in, &castExpr));
  DASSERT(rank == Incompatible || castExpr != NULL);

  if (rank == Incompatible && tryCoerce) {
    // Try a coercive cast. Note that we don't do this in 'convert' because it
    // can't handle building the actual call expression.
    castExpr = tryCoerciveCast(in, toType);
    if (castExpr != NULL) {
      Expr * result = inferTypes(subject_, castExpr, toType, false);
      return result;
    }
  }

  compatibilityWarning(in->location(), rank, in, toType);
  if (isErrorResult(castExpr)) {
    return in;
  }

  return castExpr;
}

Expr * ExprAnalyzer::doBoxCast(Expr * in) {
  const Type * fromType = dealias(in->type());
  FunctionDefn * coerceFn = coerceToObjectFn(fromType);
  FnCallExpr * call = new FnCallExpr(Expr::FnCall, in->location(), coerceFn, NULL);
  call->appendArg(in);
  call->setType(Builtins::typeObject);
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
  TemplateSignature * coerceTemplate = coerceFn->templateSignature();

  DASSERT_OBJ(coerceTemplate->paramScope().count() == 1, type);
  // Do analysis on template if needed.
  if (coerceTemplate->ast() != NULL) {
    DefnAnalyzer da(&Builtins::module, &Builtins::module, &Builtins::module, NULL);
    da.analyzeTemplateSignature(coerceFn);
  }

  BindingEnv env;
  env.addSubstitution(coerceTemplate->patternVar(0), type);
  FunctionDefn * coercer = cast<FunctionDefn>(coerceTemplate->instantiate(SourceLocation(), env));
  analyzeFunction(coercer, Task_PrepTypeComparison);
  DASSERT(coercer->isSingular());
  Builtins::module.converters()[conversionKey] = coercer;
  module()->addSymbol(coercer);
  //diag.info() << Format_Verbose << "Generated coercer " << coercer;
  return coercer;
}

Expr * ExprAnalyzer::doUnboxCast(Expr * in, const Type * toType) {
  FunctionDefn * valueOfMethod = getUnboxFn(in->location(), toType);
  if (valueOfMethod == NULL) {
    return NULL;
  }

  DASSERT(valueOfMethod->isSingular());
  FnCallExpr * call = new FnCallExpr(Expr::FnCall, in->location(), valueOfMethod, NULL);
  call->appendArg(doImplicitCast(in, Builtins::typeObject));
  call->setType(valueOfMethod->returnType());
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
  ExprList methods;
  analyzeDefn(Builtins::nsRefs, Task_PrepMemberLookup);
  findInScope(methods, "valueOf", &Builtins::nsRefs->memberScope(), NULL, loc, NO_PREFERENCE);
  DASSERT(!methods.empty());
  Expr * valueOf = specialize(loc, methods, TupleType::get(toType));
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
  ExprList methods;
  analyzeTypeDefn(Builtins::typeObject->typeDefn(), Task_PrepMemberLookup);
  findInScope(methods, "__downcast", Builtins::typeObject->memberScope(), NULL, loc, NO_PREFERENCE);
  DASSERT(!methods.empty());
  Expr * downCast = specialize(loc, methods, TupleType::get(toType));
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
  for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
    SpCandidate * sp = *it;
    bestRank = std::max(bestRank, sp->updateConversionRank());
  }

  if (bestRank == Incompatible) {
    if (!spe->args()->containsBadType()) {
      SpCandidate * front = *candidates.begin();
      diag.error(spe) << "No candidate found for '" << front->def()->name() <<
          "' which matches template arguments [" << spe->args() << "]:";
      diag.info(spe) << "Candidates are:";
      for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
        SpCandidate * sp = *it;
        sp->updateConversionRank(); // Helps with debugging.
        diag.info(sp->def()) << Format_Type << sp->def() << " [" << sp->conversionRank() << "]";
      }
    }
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
  if (spBest->def()->hasUnboundTypeParams()) {
    return spBest->def()->templateSignature()->instantiate(spe->location(), spBest->env());
  }

  return spBest->def();
}

}

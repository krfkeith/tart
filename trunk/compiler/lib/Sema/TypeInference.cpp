/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/TypeConstraint.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/FoldConstantsPass.h"
#include "tart/Common/Diagnostics.h"

#include <llvm/Support/CommandLine.h>

namespace tart {

static llvm::cl::opt<bool>
ShowInference("show-inference",
    llvm::cl::desc("Display debugging information for type inference"));

bool unifyVerbose = false;

/// -------------------------------------------------------------------
/// CallSite

void CallSite::update() {
  lowestRank_ = IdenticalTypes;
  remaining_ = 0;
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    CallCandidate * cc = *it;
    if (!cc->isCulled()) {
      ConversionRank rank = cc->updateConversionRank();
      lowestRank_ = std::min(lowestRank_, rank);
      ++remaining_;
    }
  }
}

bool CallSite::isCulled(int ch) const {
  return callExpr_->candidates()[ch]-> isCulled();
}

void CallSite::removeCulled() {
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator it = cd.begin(); it != cd.end();) {
    if ((*it)->isCulled()) {
      it = cd.erase(it);
    } else {
      ++it;
    }
  }
}

size_t CallSite::count() const {
  return callExpr_->candidates().size();
}

int CallSite::cullByConversionRank(ConversionRank lowerLimit, int searchDepth) {
  int cullCount = 0;
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
    CallCandidate * cc = *c;
    if (!cc->isCulled() && cc->conversionRank() < lowerLimit) {
      cc->cull(searchDepth);
      ++cullCount;
    }
  }

  return cullCount;
}

void CallSite::cullBySpecificity(int searchDepth) {
  Candidates mostSpecific;
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator cc = cd.begin(); cc != cd.end(); ++cc) {
    if ((*cc)->isCulled()) {
      continue;
    }

    CallCandidate * call = *cc;
    bool addNew = true;
    bool trace = AnalyzerBase::isTraceEnabled(call->method());
    for (Candidates::iterator ms = mostSpecific.begin(); ms != mostSpecific.end();) {
      if ((*ms)->isCulled()) {
        continue;
      }

      bool newIsBetter = call->isMoreSpecific(*ms) && call->conversionRank() >= (*ms)->conversionRank();
      bool oldIsBetter = (*ms)->isMoreSpecific(call) && (*ms)->conversionRank() >= call->conversionRank();
      if (newIsBetter) {
        if (!oldIsBetter) {
          if (ShowInference || trace) {
            diag.debug() << Format_Type << "Culling [" << (*ms)->method() <<
                "] because [" << call->method() << "] is more specific";
          }
          ms = mostSpecific.erase(ms);
          continue;
        }
      } else if (oldIsBetter) {
        if (ShowInference || trace) {
          diag.debug() << Format_Type << "Culling [" << call->method() << "] because [" <<
              (*ms)->method() << "] is more specific";
        }
        addNew = false;
      }

      ++ms;
    }

    if (addNew) {
      mostSpecific.push_back(call);
    }
  }

  for (Candidates::iterator cc = cd.begin(); cc != cd.end(); ++cc) {
    if ((*cc)->isCulled()) {
      continue;
    }

    if (std::find(mostSpecific.begin(), mostSpecific.end(), *cc) == mostSpecific.end()) {
      (*cc)->cull(searchDepth);
    }
  }
}

void CallSite::cullAllExcept(int choice, int searchDepth) {
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator c2 = cd.begin(); c2 != cd.end(); ++c2, --choice) {
    if (!(*c2)->isCulled() && choice != 0) {
      (*c2)->cull(searchDepth);
    }
  }
}

void CallSite::cullAllExceptBest(int searchDepth) {
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator c2 = cd.begin(); c2 != cd.end(); ++c2) {
    if (!(*c2)->isCulled() && *c2 == best_) {
      (*c2)->cull(searchDepth);
    }
  }
}

void CallSite::backtrack(int searchDepth) {
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
    CallCandidate * cc = *c;
    cc->decull(searchDepth);
  }
}

void CallSite::reportRanks() {
  diag.indent();
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
    CallCandidate * cc = *c;
    if (!cc->isCulled()) {
      diag.debug() << Format_Type << cc->method() << " [" << cc->conversionRank() << "]";
    }
  }
  diag.unindent();
}

bool CallSite::unify(int searchDepth) {
  Candidates & cclist = callExpr_->candidates();
  bool canUnify = false;
  for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
    CallCandidate * c = *cc;
    if (c->unify(callExpr_)) {
      canUnify = true;
    } else {
      c->cull(searchDepth);
    }
  }

  if (!canUnify) {
    // This code is here for debugging unification failures so that you can step through
    // unify after a unification failure.
    for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
      CallCandidate * c = *cc;
      unifyVerbose = true;
      if (c->unify(callExpr_)) {
        canUnify = true;
      }
      unifyVerbose = false;
    }

    reportErrors("No methods match calling signature: ");
    return false;
  }

  return true;
}

void CallSite::finish() {
  Candidates & cd = callExpr_->candidates();

  // Find the best of the remaining candidates.
  ConversionRank best = Incompatible;
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    // Ignore culled candidates unless there were no un-culled ones.
    CallCandidate * cc = *it;
    if (!cc->isCulled() || remaining_ == 0) {
      ConversionRank rank = cc->conversionRank();
      if (rank > best) {
        best = rank;
      }
    }
  }

  if (remaining_ != 1) {
    if (best == Incompatible) {
      reportErrors("No method found matching arguments ");
    } else {
      reportErrors("Ambiguous overloaded methods for call to ");
    }
  } else if (isConversionWarning(best)) {
    std::string msg(compatibilityError(best));
    msg.append(" attempting to call ");
    reportErrors(msg.c_str());
  }

  // Regardless of whether there was an ambiguity or not, just pick the
  // first candidate.
  removeCulled();
  CallCandidate * c = cd.front();
  if (c->method() != NULL) {
    callExpr_->setFunction(LValueExpr::get(callExpr_->location(), c->base(), c->method()));
  }
}

void CallSite::saveBest() {
  best_ = callExpr_->singularCandidate();
}

void CallSite::formatCallSignature(FormatStream & out) {
  Expr * functionExpr = callExpr_->function();
  Candidates & cd = callExpr_->candidates();
  DASSERT_OBJ(!cd.empty(), callExpr_);
  out << cd.front()->method()->name() << "(";
  formatExprTypeList(out, callExpr_->args());
  out << ")";
  if (callExpr_->expectedReturnType() != NULL) {
    out << " -> " << callExpr_->expectedReturnType();
  }
}

void CallSite::reportErrors(const char * msg) {
  // Create a representation of the calling signature
  std::stringstream callsig;
  FormatStream fs(callsig);
  fs << Format_Dealias;
  formatCallSignature(fs);
  fs.flush();
  diag.error(callExpr_->location()) << msg << callsig.str() << ".";
  diag.info(callExpr_->location()) << "Candidates are:";
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    CallCandidate * cc = *it;
    unifyVerbose = true;
    cc->unify(callExpr_);
    cc->updateConversionRank();
    unifyVerbose = false;
    if (cc->env().empty()) {
      diag.info(cc->method()) << Format_Type << cc->method() << " [" << cc->conversionRank() << "*"
          << cc->conversionCount() << "]";
    } else {
      diag.info(cc->method()) << Format_Type  << cc->method() << " with " <<
          Format_Dealias << cc->env() <<" [" << cc->conversionRank() << "] ";
    }
  }
}

/// -------------------------------------------------------------------
/// TupleCtorSite

TupleCtorSite::TupleCtorSite(TupleCtorExpr * in)
  : expr_(in)
  , rank_(IdenticalTypes)
{
}

bool TupleCtorSite::unify(int cullingDepth) {
/*  const TupleType * tt = cast<TupleType>(expr_->type());
  size_t size = tt->size();
  for (size_t i = 0; i < size; ++i) {
    const Type * memberType = tt->member(i);
    const Type * argType = expr_->arg(i)->type();
    //rank_ = std::min(rank_, tt->member(i)->canConvert(expr_->arg(i)));
  }

  DFAIL("IMP"); */
  return true;
}

// Update conversion rankings
void TupleCtorSite::update() {
  //diag.debug() << "Updating tuple ranking.";
  rank_ = IdenticalTypes;
  if (const TupleType * tt = dyn_cast<TupleType>(expr_->type())) {
    size_t size = tt->size();
    for (size_t i = 0; i < size; ++i) {
      rank_ = std::min(rank_, tt->member(i)->canConvert(expr_->arg(i)));
    }
  }
}

void TupleCtorSite::reportRanks() {
  DFAIL("IMP");
}

/// -------------------------------------------------------------------
/// ConstraintSite

void ConstraintSite::update() {
  switch (expr->exprType()) {
    case Expr::Assign:
    case Expr::PostAssign: {
      const AssignmentExpr * assign = static_cast<const AssignmentExpr *>(expr);
      rank = assign->toExpr()->type()->canConvert(assign->fromExpr());
      //diag.debug(expr) << "Conversion rank for " << expr << " is " << rank;
      //diag.debug(expr) << "Conversion rank " << rank;
      break;
    }

    case Expr::TupleCtor: {
      diag.debug() << "Updating tuple ranking.";
      const TupleCtorExpr * tce = static_cast<const TupleCtorExpr *>(expr);
      rank = IdenticalTypes;
      if (const TupleType * tt = dyn_cast<TupleType>(expr->type())) {
        size_t size = tt->size();
        for (size_t i = 0; i < size; ++i) {
          rank = std::min(rank, tt->member(i)->canConvert(tce->arg(i)));
        }
      }

      break;
    }

    default:
      break;
  }
}

/// -------------------------------------------------------------------
/// GatherConstraintsPass

Expr * GatherConstraintsPass::visitCall(CallExpr * in) {
  if (!in->candidates().empty() && visited_.insert(in)) {
    callSites_.push_back(new CallSite(in));

    // If the function is NULL, it means that the function reference is
    // in the individual candidates.
    if (in->function() == NULL) {
      Candidates & cd = in->candidates();
      for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
        visitExpr((*it)->base());
      }
    }
  }

  return CFGPass::visitCall(in);
}


Expr * GatherConstraintsPass::visitAssign(AssignmentExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    cstrSites_.push_back(ConstraintSite(in));
  }

  CFGPass::visitAssign(in);
  return in;
}

Expr * GatherConstraintsPass::visitPostAssign(AssignmentExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    cstrSites_.push_back(ConstraintSite(in));
  }

  CFGPass::visitPostAssign(in);
  return in;
}

Expr * GatherConstraintsPass::visitTupleCtor(TupleCtorExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    callSites_.push_back(new TupleCtorSite(in));
  }

  CFGPass::visitTupleCtor(in);
  return in;
}

/// -------------------------------------------------------------------
/// TypeInference

Expr * TypeInferencePass::run(Expr * in, const Type * expected, bool strict) {
  TypeInferencePass instance(in, expected, strict);
  return instance.runImpl();
}

Expr * TypeInferencePass::runImpl() {
  rootExpr_ = FoldConstantsPass().visitExpr(rootExpr_);
  if (isErrorResult(rootExpr_) || rootExpr_->isSingular()) {
    return rootExpr_;
  }

  GatherConstraintsPass(callSites_, cstrSites_).visitExpr(rootExpr_);
  if (callSites_.empty()) {
    diag.fatal(rootExpr_) << "Can't solve '" << rootExpr_ << "'";
    return rootExpr_;
  }

  bestSolutionRank_ = Incompatible;
  bestSolutionCount_ = 0;
  if (!unifyCalls()) {
    return rootExpr_;
  }

  update();
  reportRanks(false);

  cullByConversionRank();
  cullBySpecificity();
  cullByElimination();

  reportRanks(true);

  // Remove all culled candidates
  for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
    (*it)->finish();
  }

  return rootExpr_;
}

void TypeInferencePass::update() {
  lowestRank_ = IdenticalTypes;
  underconstrained_ = false;
  overconstrained_ = false;

  if (expectedType_ != NULL) {
    ConversionRank rootRank = expectedType_->canConvert(rootExpr_);
    if (!strict_ && rootRank < NonPreferred) {
      rootRank = NonPreferred;
    }

    lowestRank_ = rootRank;
  }

  // TODO: How do we get the explicit arguments?

  for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
    ChoicePoint * cs = *it;
    cs->update();
    lowestRank_ = std::min(lowestRank_, cs->rank());
    int choices = cs->remaining();
    if (choices == 0) {
      overconstrained_ = true;
    } else if (choices > 1) {
      underconstrained_ = true;
    }
  }

  for (ConstraintSiteList::iterator it = cstrSites_.begin(); it != cstrSites_.end(); ++it) {
    it->update();
    lowestRank_ = std::min(lowestRank_, it->rank);
    /*if (it->remaining == 0) {
      overconstrained_ = true;
    } else if (it->remaining > 1) {
      underconstrained_ = true;
    }*/
  }
}

void TypeInferencePass::checkSolution() {
  if (!overconstrained_ && !underconstrained_) {
    if (lowestRank_ == bestSolutionRank_) {
      ++bestSolutionCount_;
      if (ShowInference) {
        diag.debug() << "Discovered equivalent solution with rank " << bestSolutionRank_;
      }
    } else if (lowestRank_ > bestSolutionRank_) {
      bestSolutionCount_ = 1;
      bestSolutionRank_ = lowestRank_;
      for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
        if (CullableChoicePoint * ccp = (*it)->asCullable()) {
          ccp->saveBest();
        }
      }

      if (ShowInference) {
        diag.debug() << "Discovered new best solution with rank [" << bestSolutionRank_ << "]";
      }
    }
  }
}

void TypeInferencePass::reportRanks(bool final) {
  if (!ShowInference) {
    return;
  }

  size_t numSites = callSites_.size();
  if (final) {
    diag.debug() << "=== Final conversion rankings for " << numSites << " call sites: ===";
  } else {
    diag.debug() << "=== Initial conversion rankings for " << numSites << " call sites: ===";
  }

  diag.indent();
  diag.debug() << "lowest: " << lowestRank_;
  int siteIndex = 1;
  for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it, ++siteIndex) {
    diag.debug() << "Call site #" << siteIndex << ": " << (*it)->remaining() <<
        " candidates for " << Format_Type << (*it)->expr();
    (*it)->reportRanks();
  }
  diag.unindent();
}

bool TypeInferencePass::unifyCalls() {
  ++searchDepth_;
  bool success = true;
  for (CallSiteList::iterator site = callSites_.begin(); site != callSites_.end(); ++site) {
    if (!(*site)->unify(searchDepth_)) {
      success = false;
    }
  }

  return success;
}

void TypeInferencePass::cullByConversionRank() {
  if (underconstrained_ && lowestRank_ < Truncation) {
    if (ShowInference) {
      diag.debug() << "All sites: Culling overloads of rank < Truncation";
    }
    cullByConversionRank(Truncation);
  }

  if (!overconstrained_ && underconstrained_ && lowestRank_ < ExactConversion) {
    if (ShowInference) {
      diag.debug() << "All sites: Culling overloads of rank < Exact";
    }
    cullByConversionRank(ExactConversion);
  }

  if (!overconstrained_ && underconstrained_ && lowestRank_ < IdenticalTypes) {
    if (ShowInference) {
      diag.debug() << "All sites: Culling overloads of rank < Identical";
    }
    cullByConversionRank(IdenticalTypes);
  }

  if (overconstrained_) {
    if (ShowInference) {
      diag.debug() << "  Backtracking";
    }
    backtrack();
  }

  if (underconstrained_) {
    int siteIndex = 1;
    for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it, ++siteIndex) {
      ChoicePoint * site = *it;
      while (site->rank() < IdenticalTypes) {
        ConversionRank limit = ConversionRank(site->rank() + 1);
        if (ShowInference) {
          diag.debug() << "Site #" << siteIndex << ": culling overloads of rank < " << limit;
        }
        ++searchDepth_;
        if (CullableChoicePoint * ccp = site->asCullable()) {
          cullCount_ = ccp->cullByConversionRank(limit, searchDepth_);
          update();

          if (ShowInference) {
            diag.indent();
            diag.debug() << cullCount_ << " methods culled, " << site->remaining() <<
                " remaining:";
            site->reportRanks();
            diag.unindent();
          }
        }

        if (overconstrained_) {
          if (ShowInference) {
            diag.debug() << "  Backtracking";
          }
          backtrack();
          break;
        }
      }

      if (!underconstrained_) {
        break;
      }
    }
  }

  checkSolution();
}

void TypeInferencePass::cullByConversionRank(ConversionRank lowerLimit) {
  ++searchDepth_;
  cullCount_ = 0;
  for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
    if (CullableChoicePoint * ccp = (*it)->asCullable()) {
      cullCount_ += ccp->cullByConversionRank(lowerLimit, searchDepth_);
    }
  }

  if (ShowInference) {
    diag.debug() << "  " << cullCount_ << " methods culled.";
  }

  update();
}

void TypeInferencePass::cullBySpecificity() {
  if (underconstrained_) {
    if (ShowInference) {
      diag.debug() << "Culling by specificity";
    }
    diag.indent();
    ++searchDepth_;
    for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
      ChoicePoint * cp = *it;
      if (cp->remaining() > 1) {
        if (CullableChoicePoint * ccp = cp->asCullable()) {
          ccp->cullBySpecificity(searchDepth_);
        }
      }
    }

    update();
    checkSolution();
    diag.unindent();
  }
}

void TypeInferencePass::cullByElimination() {
  if (underconstrained_) {
    cullByElimination(callSites_.begin(), callSites_.end());
    if (bestSolutionCount_ == 1) {
      for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
        if (CullableChoicePoint * ccp = (*it)->asCullable()) {
          ccp->cullAllExceptBest(searchDepth_);
        }
      }
    }

    update();
  }
}

void TypeInferencePass::cullByElimination(
  CallSiteList::iterator first, CallSiteList::iterator last) {
  while (first < last && underconstrained_) {
    ChoicePoint * pt = *first;
    CullableChoicePoint * ccp = pt->asCullable();
    if (pt->remaining() <= 1 || ccp == NULL) {
      ++first;
    } else {
      int numChoices = pt->count();
      for (int ch = 0; ch < numChoices; ++ch) {
        if (ccp->isCulled(ch)) {
          continue;
        }

        //if (ShowInference) {
        //  diag.debug() << Format_Type << "Trying " << (*ci)->method();
        //}
        diag.indent();

        ++searchDepth_;
        ccp->cullAllExcept(ch, searchDepth_);
        cullByElimination(first + 1, last);
        backtrack();

        diag.unindent();
      }

      return;
    }
  }

  update();
  checkSolution();
}

void TypeInferencePass::cullAllButOne(CallSite * site, int choice) {
  site->cullAllExcept(choice, searchDepth_);
}

void TypeInferencePass::backtrack() {
  for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
    if (CullableChoicePoint * ccp = (*it)->asCullable()) {
      ccp->backtrack(searchDepth_);
    }
  }

  --searchDepth_;
  update();
}

} // namespace tart

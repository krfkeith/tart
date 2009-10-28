/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/TypeConstraint.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/FoldConstantsPass.h"
#include "tart/Common/Diagnostics.h"

#include <llvm/Support/CommandLine.h>

namespace tart {

static llvm::cl::opt<bool>
ShowInference("show-inference",
    llvm::cl::desc("Display debugging information for type inference"));

/// -------------------------------------------------------------------
/// CallSite

void CallSite::update() {
  lowestRank = IdenticalTypes;
  numRemaining = 0;
  Candidates & cd = callExpr->candidates();
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    CallCandidate * cc = *it;
    if (!cc->isCulled()) {
      ConversionRank rank = cc->updateConversionRank();
      lowestRank = std::min(lowestRank, rank);
      ++numRemaining;
    }
  }
}

void CallSite::removeCulled() {
  Candidates & cd = callExpr->candidates();
  for (Candidates::iterator it = cd.begin(); it != cd.end();) {
    if ((*it)->isCulled()) {
      it = cd.erase(it);
    } else {
      ++it;
    }
  }
}

void CallSite::finish() {
  Candidates & cd = callExpr->candidates();

  // Find the best of the remaining candidates.
  ConversionRank best = Incompatible;
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    // Ignore culled candidates unless there were no un-culled ones.
    CallCandidate * cc = *it;
    if (!cc->isCulled() || numRemaining == 0) {
      ConversionRank rank = cc->conversionRank();
      if (rank > best) {
        best = rank;
      }
    }
  }

  if (numRemaining != 1) {
    reportErrors("Ambiguous overloaded methods for call to ");
  } else if (isConversionWarning(best)) {
    std::string msg(compatibilityError(best));
    msg.append(" attempting to call ");
    reportErrors(msg.c_str());
  }

  // Regardless of whether there was an ambiguity or not, just pick the
  // first candidate.
  removeCulled();
  CallCandidate * c = cd.front();
  callExpr->setFunction(new LValueExpr(callExpr->location(), c->base(), c->method()));
}

void CallSite::formatCallSignature(FormatStream & out) {
  Expr * functionExpr = callExpr->function();
  Candidates & cd = callExpr->candidates();
  DASSERT_OBJ(!cd.empty(), callExpr);
  out << cd.front()->method()->name() << "(";
  formatExprTypeList(out, callExpr->args());
  out << ")";
  if (callExpr->expectedReturnType() != NULL) {
    out << " -> " << callExpr->expectedReturnType();
  }
}

void CallSite::reportErrors(const char * msg) {
  // Create a representation of the calling signature
  std::stringstream callsig;
  FormatStream fs(callsig);
  fs << Format_Dealias;
  formatCallSignature(fs);
  diag.error(callExpr->location()) << msg << callsig.str() << ".";
  diag.info(callExpr->location()) << "Candidates are:";
  Candidates & cd = callExpr->candidates();
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    CallCandidate * cc = *it;
    cc->updateConversionRank();
    if (cc->env().empty()) {
      diag.info(cc->method()) << Format_Type << cc->method() << " [" << cc->conversionRank() << "]";
    } else {
      diag.info(cc->method()) << Format_Type  << cc->method() << " with " <<
          Format_Dealias << cc->env() <<" [" << cc->conversionRank() << "] ";
    }
  }
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
  }
}

/// -------------------------------------------------------------------
/// GatherConstraintsPass

Expr * GatherConstraintsPass::visitCall(CallExpr * in) {
  if (!in->candidates().empty() && visited_.insert(in)) {
    callSites_.push_back(CallSite(in));

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

/// -------------------------------------------------------------------
/// TypeInference

Expr * TypeInferencePass::run(Expr * in, Type * expected, bool strict) {
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
    it->finish();
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
    it->update();
    lowestRank_ = std::min(lowestRank_, it->lowestRank);
    if (it->numRemaining == 0) {
      overconstrained_ = true;
    } else if (it->numRemaining > 1) {
      underconstrained_ = true;
    }
  }

  for (ConstraintSiteList::iterator it = cstrSites_.begin(); it != cstrSites_.end(); ++it) {
    it->update();
    lowestRank_ = std::min(lowestRank_, it->rank);
    /*if (it->numRemaining == 0) {
      overconstrained_ = true;
    } else if (it->numRemaining > 1) {
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
        it->best = it->callExpr->singularCandidate();
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
    diag.debug() << "Call site #" << siteIndex << ": " << it->numRemaining << " candidates for " <<
        Format_Type << it->callExpr;
    diag.indent();
    Candidates & cd = it->callExpr->candidates();
    for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
      CallCandidate * cc = *c;
      if (!cc->isCulled()) {
        diag.debug() << Format_Type << cc->method() << " [" << cc->conversionRank() << "]";
      }
    }
    diag.unindent();
  }
  diag.unindent();
}

bool TypeInferencePass::unifyCalls() {
  ++searchDepth_;
  bool success = true;
  for (CallSiteList::iterator site = callSites_.begin(); site != callSites_.end(); ++site) {
    Candidates & cclist = site->callExpr->candidates();
    bool canUnify = false;
    for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
      CallCandidate * c = *cc;
      if (c->unify(site->callExpr)) {
        canUnify = true;
      } else {
        c->cull(searchDepth_);
      }
    }

    if (!canUnify) {
      // This code is here for debugging unification failures so that you can step through
      // unify after a unification failure.
      for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
        CallCandidate * c = *cc;
        if (c->unify(site->callExpr)) {
          canUnify = true;
        } else {
          c->cull(searchDepth_);
        }
      }

      site->reportErrors("No methods match calling signature: ");
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
      CallSite & site = *it;
      while (site.lowestRank < IdenticalTypes) {
        ConversionRank limit = ConversionRank(site.lowestRank + 1);
        if (ShowInference) {
          diag.debug() << "Site #" << siteIndex << ": culling overloads of rank < " << limit;
        }
        ++searchDepth_;
        cullCount_ = 0;
        cullByConversionRank(limit, site);
        update();
        if (ShowInference) {
          diag.indent();
          diag.debug() << cullCount_ << " methods culled, " << site.numRemaining << " remaining:";
          diag.indent();
          Candidates & cd = site.callExpr->candidates();
          for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
            CallCandidate * cc = *c;
            if (!cc->isCulled()) {
              diag.debug() << Format_Type << cc->method() << " [" << cc->conversionRank() << "]";
            }
          }
          diag.unindent();
          diag.unindent();
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
    cullByConversionRank(lowerLimit, *it);
  }

  if (ShowInference) {
    diag.debug() << "  " << cullCount_ << " methods culled.";
  }

  update();
}

void TypeInferencePass::cullByConversionRank(ConversionRank lowerLimit, CallSite & site) {
  Candidates & cd = site.callExpr->candidates();
  for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
    CallCandidate * cc = *c;
    if (!cc->isCulled() && cc->conversionRank() < lowerLimit) {
      cc->cull(searchDepth_);
      ++cullCount_;
    }
  }
}

void TypeInferencePass::cullBySpecificity() {
  if (underconstrained_) {
    if (ShowInference) {
      diag.debug() << "Culling by specificity";
    }
    diag.indent();
    ++searchDepth_;
    for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
      if (it->numRemaining > 1) {
        cullBySpecificity(*it);
      }
    }

    update();
    checkSolution();
    diag.unindent();
  }
}

void TypeInferencePass::cullBySpecificity(CallSite & site) {
  Candidates mostSpecific;
  Candidates & cd = site.callExpr->candidates();
  for (Candidates::iterator cc = cd.begin(); cc != cd.end(); ++cc) {
    if ((*cc)->isCulled()) {
      continue;
    }

    CallCandidate * call = *cc;
    bool addNew = true;
    for (Candidates::iterator ms = mostSpecific.begin(); ms != mostSpecific.end();) {
      if ((*ms)->isCulled()) {
        continue;
      }

      bool newIsBetter = call->isMoreSpecific(*ms) && call->conversionRank() >= (*ms)->conversionRank();
      bool oldIsBetter = (*ms)->isMoreSpecific(call) && (*ms)->conversionRank() >= call->conversionRank();
      if (newIsBetter) {
        if (!oldIsBetter) {
          if (ShowInference) {
            diag.debug() << Format_Type << "Culling [" << (*ms)->method() <<
                "] because [" << call->method() << "] is more specific";
          }
          ms = mostSpecific.erase(ms);
          continue;
        }
      } else if (oldIsBetter) {
        if (ShowInference) {
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
      (*cc)->cull(searchDepth_);
    }
  }
}

void TypeInferencePass::cullByElimination() {
  if (underconstrained_) {
    cullByElimination(callSites_.begin(), callSites_.end());
    if (bestSolutionCount_ == 1) {
      for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
        cullAllButOne(*it, it->best);
      }
    }

    update();
  }
}

void TypeInferencePass::cullByElimination(
  CallSiteList::iterator first, CallSiteList::iterator last) {
  while (first < last && underconstrained_) {
    if (first->numRemaining <= 1) {
      ++first;
    } else {
      Candidates & cd = first->callExpr->candidates();
      for (Candidates::iterator ci = cd.begin(); ci != cd.end(); ++ci) {
        if ((*ci)->isCulled()) {
          continue;
        }

        if (ShowInference) {
          diag.debug() << Format_Type << "Trying " << (*ci)->method();
        }
        diag.indent();

        ++searchDepth_;
        cullAllButOne(*first, *ci);
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

void TypeInferencePass::cullAllButOne(CallSite & site, CallCandidate * cc) {
  Candidates & cd = site.callExpr->candidates();
  for (Candidates::iterator c2 = cd.begin(); c2 != cd.end(); ++c2) {
    if (!(*c2)->isCulled() && *c2 != cc) {
      (*c2)->cull(searchDepth_);
    }
  }
}

void TypeInferencePass::backtrack() {
  for (CallSiteList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
    Candidates & cd = it->callExpr->candidates();
    for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
      CallCandidate * cc = *c;
      cc->decull(searchDepth_);
    }
  }

  --searchDepth_;
  update();
}

} // namespace tart

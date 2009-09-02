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

  if (numRemaining != 1 || isConversionWarning(best)) {
    // Create a representation of the calling signature
    std::stringstream callsig;
    FormatStream fs(callsig);
    fs << Format_Dealias;
    formatCallSignature(fs);

    if (isConversionWarning(best)) {
      diag.error(callExpr->location()) << compatibilityError(best) <<
          " attempting to call " << callsig.str();
    } else {
      diag.error(callExpr) << "Ambiguous overloaded methods for call to " << callsig.str() << ":";
    }

    diag.info(callExpr->location()) << "Candidates are:";
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

/// -------------------------------------------------------------------
/// GatherConstraintsPass

Expr * GatherConstraintsPass::visitCall(CallExpr * in) {
  if (!in->candidates().empty()) {
    callSites.push_back(CallSite(in));
  }

  return CFGPass::visitCall(in);
}

/// -------------------------------------------------------------------
/// TypeInference

Expr * TypeInferencePass::run(Expr * in, Type * expected, bool strict) {
  TypeInferencePass instance(in, expected, strict);
  return instance.runImpl();
}

Expr * TypeInferencePass::runImpl() {
  rootExpr = FoldConstantsPass().visitExpr(rootExpr);
  if (isErrorResult(rootExpr) || rootExpr->isSingular()) {
    return rootExpr;
  }

  GatherConstraintsPass(callSites).visitExpr(rootExpr);
  if (callSites.empty()) {
    diag.fatal(rootExpr) << "Can't solve '" << rootExpr << "'";
    return rootExpr;
  }

  bestSolutionRank = Incompatible;
  bestSolutionCount = 0;
  unifyCalls();
  update();
  reportRanks();

  cullByConversionRank();
  cullBySpecificity();
  cullByElimination();

  reportRanks();

  /*else {
    // Report ambiguity.
    //diag.fatal()
    DFAIL("Implement error messages");
  }*/

  // Remove all culled candidates
  for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it) {
    it->finish();
  }

  return rootExpr;
}

void TypeInferencePass::update() {
  lowestRank = IdenticalTypes;
  underconstrained = false;
  overconstrained = false;

  if (expectedType != NULL) {
    ConversionRank rootRank = expectedType->convert(rootExpr);
    if (!strict_ && rootRank < NonPreferred) {
      rootRank = NonPreferred;
    }

    lowestRank = rootRank;
  }

  // TODO: How do we get the explicit arguments?

  for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it) {
    it->update();
    lowestRank = std::min(lowestRank, it->lowestRank);
    if (it->numRemaining == 0) {
      overconstrained = true;
    } else if (it->numRemaining > 1) {
      underconstrained = true;
    }
  }
}

void TypeInferencePass::checkSolution() {
  if (!overconstrained && !underconstrained) {
    if (lowestRank == bestSolutionRank) {
      ++bestSolutionCount;
      if (ShowInference) {
        diag.debug() << "Discovered equivalent solution with rank " <<
            bestSolutionRank;
      }
    } else if (lowestRank > bestSolutionRank) {
      bestSolutionCount = 1;
      bestSolutionRank = lowestRank;
      for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it) {
        it->best = it->callExpr->singularCandidate();
      }

      if (ShowInference) {
        diag.debug() << "Discovered new best solution with rank [" <<
            bestSolutionRank << "]";
      }
    }
  }
}

void TypeInferencePass::reportRanks() {
  if (!ShowInference) {
    return;
  }

  diag.debug() << "=== Conversion rankings for " << callSites.size() << " call sites: ===";
  diag.indent();
  diag.debug() << "lowest: " << lowestRank;
  int siteIndex = 1;
  for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it, ++siteIndex) {
    diag.debug() << "Call site #" << siteIndex << ": " << it->numRemaining
      << " candidates for " << it->callExpr;
    diag.indent();
    Candidates & cd = it->callExpr->candidates();
    for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
      CallCandidate * cc = *c;
      if (!cc->isCulled()) {
        diag.debug() << cc->method() << " [" << cc->conversionRank() << "]";
      }
    }
    diag.unindent();
  }
  diag.unindent();
}

void TypeInferencePass::unifyCalls() {
  ++searchDepth;
  for (CallSiteList::iterator site = callSites.begin(); site != callSites.end(); ++site) {
    Candidates & cclist = site->callExpr->candidates();
    for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
      CallCandidate * c = *cc;
      if (!c->unify(site->callExpr)) {
        c->cull(searchDepth);
      }
    }
  }
}

void TypeInferencePass::cullByConversionRank() {
  if (underconstrained && lowestRank < Truncation) {
    if (ShowInference) {
      diag.debug() << "All sites: Culling overloads of rank < Truncation";
    }
    cullByConversionRank(Truncation);
  }

  if (!overconstrained && underconstrained && lowestRank < ExactConversion) {
    if (ShowInference) {
      diag.debug() << "All sites: Culling overloads of rank < Exact";
    }
    cullByConversionRank(ExactConversion);
  }

  if (!overconstrained && underconstrained && lowestRank < IdenticalTypes) {
    if (ShowInference) {
      diag.debug() << "All sites: Culling overloads of rank < Identical";
    }
    cullByConversionRank(IdenticalTypes);
  }

  if (overconstrained) {
    if (ShowInference) {
      diag.debug() << "  Backtracking";
    }
    backtrack();
  }

  if (underconstrained) {
    int siteIndex = 1;
    for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it, ++siteIndex) {
      CallSite & site = *it;
      while (site.lowestRank < IdenticalTypes) {
        ConversionRank limit = ConversionRank(site.lowestRank + 1);
        if (ShowInference) {
          diag.debug() << "Site #" << siteIndex <<
              ": culling overloads of rank < " << limit;
        }
        ++searchDepth;
        cullCount = 0;
        cullByConversionRank(limit, site);
        if (ShowInference) {
          diag.debug() << "  " << cullCount << " methods culled.";
        }
        update();

        if (overconstrained) {
          if (ShowInference) {
            diag.debug() << "  Backtracking";
          }
          backtrack();
          break;
        }
      }

      if (!underconstrained) {
        break;
      }
    }
  }

  checkSolution();
}

void TypeInferencePass::cullByConversionRank(ConversionRank lowerLimit) {
  ++searchDepth;
  cullCount = 0;
  for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it) {
    cullByConversionRank(lowerLimit, *it);
  }

  if (ShowInference) {
    diag.debug() << "  " << cullCount << " methods culled.";
  }
  update();
}

void TypeInferencePass::cullByConversionRank(ConversionRank lowerLimit,
    CallSite & site) {
  Candidates & cd = site.callExpr->candidates();
  for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
    CallCandidate * cc = *c;
    if (!cc->isCulled() && cc->conversionRank() < lowerLimit) {
      cc->cull(searchDepth);
      ++cullCount;
    }
  }
}

void TypeInferencePass::cullBySpecificity() {
  if (underconstrained) {
    if (ShowInference) {
      diag.debug() << "Culling by specificity";
    }
    diag.indent();
    ++searchDepth;
    for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it) {
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

      bool newIsBetter = call->isMoreSpecific(*ms);
      bool oldIsBetter = (*ms)->isMoreSpecific(call);
      if (newIsBetter) {
        if (!oldIsBetter) {
          if (ShowInference) {
            diag.debug() << "Culling [" << (*ms)->method() <<
                "] because [" << call->method() << "] is more specific";
          }
          ms = mostSpecific.erase(ms);
          continue;
        }
      } else if (oldIsBetter) {
        if (ShowInference) {
          diag.debug() << "Culling [" << call->method() << "] because [" <<
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

void TypeInferencePass::cullByElimination() {
  if (underconstrained) {
    cullByElimination(callSites.begin(), callSites.end());
    if (bestSolutionCount == 1) {
      for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it) {
        cullAllButOne(*it, it->best);
      }
    }

    update();
  }
}

void TypeInferencePass::cullByElimination(
  CallSiteList::iterator first, CallSiteList::iterator last) {
  while (first < last && underconstrained) {
    if (first->numRemaining <= 1) {
      ++first;
    } else {
      Candidates & cd = first->callExpr->candidates();
      for (Candidates::iterator ci = cd.begin(); ci != cd.end(); ++ci) {
        if ((*ci)->isCulled()) {
          continue;
        }

        if (ShowInference) {
          diag.debug() << "Trying " << (*ci)->method();
        }
        diag.indent();

        ++searchDepth;
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
      (*c2)->cull(searchDepth);
    }
  }
}

void TypeInferencePass::backtrack() {
  for (CallSiteList::iterator it = callSites.begin(); it != callSites.end(); ++it) {
    Candidates & cd = it->callExpr->candidates();
    for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
      CallCandidate * cc = *c;
      cc->decull(searchDepth);
    }
  }

  --searchDepth;
  update();
}

} // namespace tart

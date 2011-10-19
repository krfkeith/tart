/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/Infer/CallSite.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/Support/CommandLine.h"

namespace tart {

extern bool unifyVerbose;
extern bool showInference;

// -------------------------------------------------------------------
// CallSite

void CallSite::update() {
  lowestRank_ = IdenticalTypes;
  remaining_ = 0;
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    if (!cc->isCulled()) {
      ConversionRank rank = cc->updateConversionRank();
      lowestRank_ = std::min(lowestRank_, rank);
      ++remaining_;
    }
  }
}

bool CallSite::isCulled(int index) const {
  return callExpr_->candidates()[index]->isCulled();
}

void CallSite::cull(int choice, int searchDepth) {
  callExpr_->candidates()[choice]->cull(searchDepth);
}

int CallSite::cullByConversionRank(ConversionRank lowerLimit, int searchDepth) {
  if (count() == 1) {
    return 0;
  }
  int cullCount = 0;
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    if (!cc->isCulled() && cc->conversionRank() < lowerLimit) {
      cc->cull(searchDepth);
      ++cullCount;
    }
  }

  return cullCount;
}

void CallSite::cullBySpecificity(int searchDepth) {
  Candidates mostSpecific;
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    if (cc->isCulled()) {
      continue;
    }

    bool addNew = true;
    bool trace = AnalyzerBase::isTraceEnabled(cc->method());
    for (iterator ms = mostSpecific.begin(); ms != mostSpecific.end();) {
      bool newIsBetter = cc->isMoreSpecific(*ms) &&
          cc->conversionRank() >= (*ms)->conversionRank();
      bool oldIsBetter = (*ms)->isMoreSpecific(cc) &&
          (*ms)->conversionRank() >= cc->conversionRank();
      if (newIsBetter) {
        if (!oldIsBetter) {
          if (showInference || trace) {
            diag.debug() << Format_Type << "Culling [" << (*ms)->method() <<
                "] because [" << cc->method() << "] is more specific";
          }
          ms = mostSpecific.erase(ms);
          continue;
        }
      } else if (oldIsBetter) {
        if (showInference || trace) {
          diag.debug() << Format_Type << "Culling [" << cc->method() << "] because [" <<
              (*ms)->method() << "] is more specific";
        }
        addNew = false;
      }

      ++ms;
    }

    if (addNew) {
      mostSpecific.push_back(cc);
    }
  }

  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    if ((*ci)->isCulled()) {
      continue;
    }

    if (std::find(mostSpecific.begin(), mostSpecific.end(), *ci) == mostSpecific.end()) {
      (*ci)->cull(searchDepth);
    }
  }
}

void CallSite::cullAllExcept(int choice, int searchDepth) {
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci, --choice) {
    CallCandidate * cc = *ci;
    if (!cc->isCulled() && choice != 0) {
      cc->cull(searchDepth);
    }
  }
}

void CallSite::cullAllExceptBest(int searchDepth) {
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    if (!cc->isCulled() && *ci != best_) {
      cc->cull(searchDepth);
    }
  }
}

void CallSite::backtrack(int searchDepth) {
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    cc->decull(searchDepth);
  }
}

bool CallSite::dependsOn(int choice, const ProvisionSet & pset) const {
  return pset.count(callExpr_->candidates()[choice]->primaryProvision()) != 0;
}

bool CallSite::hasErrors() const {
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    if (cc->hasErrors()) {
      return true;
    }
  }
  return false;
}

void CallSite::reportRanks() {
  diag.indent();
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    if (!cc->isCulled()) {
      diag.debug() << cc << " [" << cc->conversionRank() << "]";
      diag.indent();
      cc->dumpTypeParams();
      diag.unindent();
    }
  }
  diag.unindent();
}

void CallSite::relabelVars(BindingEnv & env) {
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    cc->relabelTypeVars(env);
  }
}

bool CallSite::unify(BindingEnv & env, int searchDepth) {
  bool canUnify = false;
  unsigned stateBeforeAny = env.stateCount();
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * c = *ci;
    unsigned stateBeforeNext = env.stateCount();
    if (c->unify(callExpr_, env)) {
      canUnify = true;
    } else {
      if (showInference) {
        diag.debug() << Format_Type << "Unification failed for " << c->method() << ":";
      }
      env.backtrack(stateBeforeNext);
      c->cull(searchDepth);
    }
  }

  if (!canUnify) {
    reportErrors("No methods match calling signature: ");
    backtrack(stateBeforeAny);
    DBREAK;
    if (showInference) {
      unifyVerbose = true;
      for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
        CallCandidate * c = *ci;
        unsigned stateBeforeNext = env.stateCount();
        c->unify(callExpr_, env);
        env.backtrack(stateBeforeNext);
      }
      unifyVerbose = false;
    }
    return false;
  }

  return true;
}

void CallSite::finish() {

  // Find the best of the remaining candidates.
  ConversionRank best = Incompatible;
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    // Ignore culled candidates unless there were no un-culled ones.
    CallCandidate * cc = *ci;
    // Don't bother producing a report if any of the candidates have errors.
    if (cc->hasErrors()) {
      return;
    }
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

  // Remove all culled candidates from the list.
  Candidates & cd = callExpr_->candidates();
  for (iterator ci = cd.begin(); ci != cd.end();) {
    if ((*ci)->isCulled()) {
      ci = cd.erase(ci);
    } else {
      ++ci;
    }
  }

  // Regardless of whether there was an ambiguity or not, just pick the
  // first candidate.
  CallCandidate * c = cd.front();
  if (c->method() != NULL) {
    callExpr_->setFunction(LValueExpr::get(callExpr_->location(), c->base(), c->method()));
  }
}

void CallSite::saveBest() {
  best_ = callExpr_->singularCandidate();
}

void CallSite::formatCallSignature(FormatStream & out) {
  Candidates & cd = callExpr_->candidates();
  DASSERT_OBJ(!cd.empty(), callExpr_);
  out << cd.front()->method()->name() << "(";
  formatExprTypeList(out, callExpr_->args());
  out << ")";
  if (callExpr_->expectedReturnType()) {
    out << " -> " << callExpr_->expectedReturnType();
  }
}

void CallSite::reportErrors(const char * msg) {
  // If any of the candidates contains a parameter of type badType, then forego the report.
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    if ((*ci)->method()->functionType()->hasErrors()) {
      return;
    }
  }

  // Create a representation of the calling signature
  StrFormatStream fs;
  fs << Format_Dealias;
  formatCallSignature(fs);
  diag.error(callExpr_->location()) << msg << fs.str() << ".";
  diag.info(callExpr_->location()) << "Candidates are:";
  for (const_iterator ci = begin(), ciEnd = end(); ci != ciEnd; ++ci) {
    CallCandidate * cc = *ci;
    backtrack(0);
    cc->updateConversionRank();
    cc->reportConversionErrors();
  }
}

} // namespace tart

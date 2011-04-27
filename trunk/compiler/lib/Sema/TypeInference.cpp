/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TypeConstraint.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/TupleType.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"
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

// -------------------------------------------------------------------
// CallSite

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
      if (ShowInference) {
        diag.debug() << Format_Type << "Unification failed for " << c->method() << ":";
        unifyVerbose = true;
        c->unify(callExpr_);
        unifyVerbose = false;
      }
      c->cull(searchDepth);
    }
  }

  if (!canUnify) {
    // This code is here for debugging unification failures so that you can step through
    // unify after a unification failure.
    for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
      CallCandidate * c = *cc;
      c->unify(callExpr_);
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
  Candidates & cd = callExpr_->candidates();

  // If any of the candidates contains a parameter of type badType, then forego the report.
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    if ((*it)->method()->functionType()->hasErrors()) {
      return;
    }
  }

  // Create a representation of the calling signature
  std::stringstream callsig;
  FormatStream fs(callsig);
  fs << Format_Dealias;
  formatCallSignature(fs);
  fs.flush();
  diag.error(callExpr_->location()) << msg << callsig.str() << ".";
  diag.info(callExpr_->location()) << "Candidates are:";
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    std::stringstream errMsg;
    FormatStream errStrm(errMsg);
    CallCandidate * cc = *it;
    //unifyVerbose = true;
    cc->unify(callExpr_, &errStrm);
    cc->updateConversionRank();
    //unifyVerbose = false;
    errStrm.flush();
    if (!errMsg.str().empty()) {
      diag.info(cc->method()) << Format_Type << cc->method() << " : " << errMsg.str();
    } else if (cc->env().empty()) {
      diag.info(cc->method()) << Format_Type << cc->method() << " [" << cc->conversionRank() << "*"
          << cc->conversionCount() << "]";
    } else {
      diag.info(cc->method()) << Format_Type  << cc->method() << " with " <<
          Format_Dealias << cc->env() <<" [" << cc->conversionRank() << "] ";
    }
  }
}

// -------------------------------------------------------------------
// ConstantIntegerSite

#if 0
void ConstantIntegerSite::update() {
  lowestRank_ = IdenticalTypes;
  remaining_ = 0;
  const UnsizedIntConstraint * uic = static_cast<const UnsizedIntConstraint *>(expr_->type());
  for (int i = 0; i < UnsizedIntConstraint::NUM_TYPES; ++i) {
    if (!uic->isCulled(i)) {
      //ConversionRank rank = uic->types()[i]->updateConversionRank();
      //lowestRank_ = std::min(lowestRank_, rank);
      ++remaining_;
    }
  }
}

size_t ConstantIntegerSite::count() const {
  return UnsizedIntConstraint::NUM_TYPES;
}

int ConstantIntegerSite::remaining() const {
  return remaining_;
}

bool ConstantIntegerSite::unify(int cullingDepth) {
  return true;
}

ConversionRank ConstantIntegerSite::rank() const {
  return lowestRank_;
}

bool ConstantIntegerSite::isCulled(int index) const {
  const UnsizedIntConstraint * uic = static_cast<const UnsizedIntConstraint *>(expr_->type());
  return uic->isCulled(index);
}

int ConstantIntegerSite::cullByConversionRank(ConversionRank lowerLimit, int searchDepth) {
  return 0;
}

void ConstantIntegerSite::cullBySpecificity(int searchDepth) {
  //DFAIL("Implement");
}

void ConstantIntegerSite::cullAllExcept(int choice, int searchDepth) {
  //DFAIL("Implement");
}

void ConstantIntegerSite::cullAllExceptBest(int searchDepth) {
  //DFAIL("Implement");
}

void ConstantIntegerSite::finish() {
  //DFAIL("Implement");
}

void ConstantIntegerSite::reportErrors(const char * msg) {
  //DFAIL("Implement");
}

void ConstantIntegerSite::reportRanks() {
  //DFAIL("Implement");
}

void ConstantIntegerSite::saveBest() {
  //DFAIL("Implement");
}

void ConstantIntegerSite::backtrack(int searchDepth) {
  //DFAIL("Implement");
}
#endif

// -------------------------------------------------------------------
// AssignmentSite

void AssignmentSite::update() {
  const AssignmentExpr * assign = static_cast<const AssignmentExpr *>(expr_);
  rank_ = assign->toExpr()->type()->canConvert(assign->fromExpr());
}

void AssignmentSite::report() {

}

// -------------------------------------------------------------------
// TupleCtorSite

// Update conversion rankings
void TupleCtorSite::update() {
  rank_ = IdenticalTypes;
  TupleCtorExpr * tct = static_cast<TupleCtorExpr *>(expr_);
  if (const TupleType * tt = dyn_cast<TupleType>(tct->type())) {
    size_t size = tt->size();
    for (size_t i = 0; i < size; ++i) {
      rank_ = std::min(rank_, tt->member(i)->canConvert(tct->arg(i)));
    }
  }
}

void TupleCtorSite::report() {

}

// -------------------------------------------------------------------
// PHISite

void PHISite::update() {
  const PHIConstraint * phiType = cast<PHIConstraint>(expr_->type());
  TypeExpansion types;
  ConstTypeList solutions;

  phiType->expand(types);
  if (phiType->expected() != NULL) {
    solutions.push_back(phiType->expected());
  } else {
    // Try to find a common type that encompasses all input types.
    // TODO: Use coercions of needed.
    for (TypeExpansion::const_iterator it = types.begin(); it != types.end(); ++it) {
      const Type * newType = *it;
      DASSERT(newType != NULL);
      bool addNew = true;
      for (ConstTypeList::iterator s = solutions.begin(); s != solutions.end();) {
        const Type * oldType = *s;
        if (oldType->includes(newType)) {
          addNew = false;
          break;
        } else if (newType->includes(oldType)) {
          s = solutions.erase(s);
        } else {
          ++s;
        }
      }

      if (addNew) {
        solutions.push_back(newType);
      }
    }
  }

  if (solutions.size() == 1) {
    // Compute the lowest conversion ranking of all input types to the solution.
    rank_ = IdenticalTypes;
    const Type * solution = solutions.front();
//    if (solution->isUnsignedType()) {
//      solution = &Int32Type::instance;
//    }
    for (TypeExpansion::const_iterator it = types.begin(); it != types.end(); ++it) {
      rank_ = std::min(rank_, solution->canConvert(*it, Conversion::Coerce));
    }

    phiType->setCommon(solution);
  } else {
    phiType->setCommon(NULL);
  }
}

void PHISite::report() {
  const PHIConstraint * phiType = cast<PHIConstraint>(expr_->type());
  TypeExpansion types;
  phiType->expand(types);
  diag.info() << "expression: " << expr_;
  if (phiType->expected() != NULL) {
    diag.info() << "expected result type: " << phiType->expected();
  }
  for (TypeExpansion::const_iterator it = types.begin(); it != types.end(); ++it) {
    diag.info() << "expression type: " << *it;
  }
}

// -------------------------------------------------------------------
// GatherConstraintsPass

Expr * GatherConstraintsPass::visitCall(CallExpr * in) {
  if (!in->candidates().empty() && visited_.insert(in)) {
    // Note: pre-order traversal.
    Expr * result = CFGPass::visitCall(in);
    choicePoints_.push_back(new CallSite(in));

    // If the function is NULL, it means that the function reference is
    // in the individual candidates.
    if (in->function() == NULL) {
      Candidates & cd = in->candidates();
      for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
        visitExpr((*it)->base());
      }
    }
    return result;
  } else {
    return CFGPass::visitCall(in);
  }
}


Expr * GatherConstraintsPass::visitAssign(AssignmentExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    constraints_.push_back(new AssignmentSite(in));
  }

  CFGPass::visitAssign(in);
  return in;
}

Expr * GatherConstraintsPass::visitPostAssign(AssignmentExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    constraints_.push_back(new AssignmentSite(in));
  }

  CFGPass::visitPostAssign(in);
  return in;
}

Expr * GatherConstraintsPass::visitTupleCtor(TupleCtorExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    constraints_.push_back(new TupleCtorSite(in));
  }

  CFGPass::visitTupleCtor(in);
  return in;
}

Expr * GatherConstraintsPass::visitConstantInteger(ConstantInteger * in) {
#if 0
  if (in->type()->isUnsizedIntType()) {
    in->setType(new UnsizedIntConstraint(in));
    choicePoints_.push_back(new ConstantIntegerSite(in));
  }
#endif

  return in;
}

Expr * GatherConstraintsPass::visitIf(IfExpr * in) {
  CFGPass::visitIf(in);
  visitPHI(in);
  return in;
}

Expr * GatherConstraintsPass::visitSwitch(SwitchExpr * in) {
  CFGPass::visitSwitch(in);
  visitPHI(in);
  return in;
}

Expr * GatherConstraintsPass::visitMatch(MatchExpr * in) {
  CFGPass::visitMatch(in);
  visitPHI(in);
  return in;
}

void GatherConstraintsPass::visitPHI(Expr * in) {
  if (const PHIConstraint * phi = dyn_cast<PHIConstraint>(in->type())) {
    constraints_.push_back(new PHISite(in));
  }
}

// -------------------------------------------------------------------
// TypeInference

Expr * TypeInferencePass::run(Module * module, Expr * in, const Type * expected, bool strict) {
  TypeInferencePass instance(module, in, expected, strict);
  return instance.runImpl();
}

Expr * TypeInferencePass::runImpl() {
  rootExpr_ = FoldConstantsPass(module_).visitExpr(rootExpr_);
  if (isErrorResult(rootExpr_) || rootExpr_->isSingular()) {
    return rootExpr_;
  }

  GatherConstraintsPass(callSites_, cstrSites_).visitExpr(rootExpr_);
  if (callSites_.empty()) {
    // Try running the constraints anyway, just to see what happens.
    for (ConstraintList::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
        it != itEnd; ++it) {
      (*it)->update();
    }
    if (rootExpr_->isSingular()) {
      return rootExpr_;
    }

    for (ConstraintList::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
        it != itEnd; ++it) {
      const Expr * e = (*it)->expr();
      if (!e->isSingular()) {
        diag.error(e) << "Can't find an unambiguous solution for '" << e << "'";
        (*it)->report();
        return rootExpr_;
      }
    }

    diag.error(rootExpr_) << "Can't find an unambiguous solution for '" << rootExpr_ << "'";
    diag.info() << "Total constraint count: " << cstrSites_.size();
    diag.indent();
    for (ConstraintList::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
        it != itEnd; ++it) {
      (*it)->report();
    }
    diag.unindent();

    return rootExpr_;
  }

  bestSolutionRank_ = Incompatible;
  bestSolutionCount_ = 0;
  if (!unifyCalls()) {
    return rootExpr_;
  }

  update();
  reportRanks(INITIAL);

  cullByConversionRank();
  reportRanks(INTERMEDIATE);
  cullBySpecificity();
  cullByElimination();

  reportRanks(FINAL);

  // Remove all culled candidates
  for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
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

  for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
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

  for (ConstraintList::iterator it = cstrSites_.begin(); it != cstrSites_.end(); ++it) {
    (*it)->update();
    lowestRank_ = std::min(lowestRank_, (*it)->rank());
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
      for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
        (*it)->saveBest();
      }

      if (ShowInference) {
        diag.debug() << "Discovered new best solution with rank [" << bestSolutionRank_ << "]";
      }
    }
  }
}

void TypeInferencePass::reportRanks(ReportLabel label) {
  if (!ShowInference) {
    return;
  }

  size_t numSites = callSites_.size();
  switch (label) {
    case INITIAL:
      diag.debug() << "=== Initial conversion rankings for " << numSites << " call sites: ===";
      break;

    case INTERMEDIATE:
      diag.debug() << "=== Intermediate conversion rankings for " << numSites << " call sites: ===";
      break;

    case FINAL:
      diag.debug() << "=== Final conversion rankings for " << numSites << " call sites: ===";
      break;
  }

  diag.indent();
  diag.debug() << "lowest: " << lowestRank_;
  int siteIndex = 1;
  for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it, ++siteIndex) {
    diag.debug() << "Call site #" << siteIndex << ": " << (*it)->remaining() <<
        " candidates for " << Format_Type << (*it)->expr();
    (*it)->reportRanks();
  }
  diag.unindent();
}

bool TypeInferencePass::unifyCalls() {
  ++searchDepth_;
  bool success = true;
  for (ChoicePointList::iterator site = callSites_.begin(); site != callSites_.end(); ++site) {
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
      diag.indent();
      diag.debug() << "Backtracking";
      diag.unindent();
    }
    backtrack();
  }

  while (underconstrained_ && cullEachSiteByConversionRank()) {}
  checkSolution();
}

bool TypeInferencePass::cullEachSiteByConversionRank() {
  int siteIndex = 1;
  bool siteChanged = false;
  for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it, ++siteIndex) {
    ChoicePoint * site = *it;
    while (site->rank() < IdenticalTypes) {
      ConversionRank limit = ConversionRank(site->rank() + 1);
      if (ShowInference) {
        diag.debug() << "Site #" << siteIndex << ": culling overloads of rank < " << limit;
      }
      ++searchDepth_;
      cullCount_ = site->cullByConversionRank(limit, searchDepth_);
      update();

      if (ShowInference) {
        diag.indent();
        diag.debug() << cullCount_ << " methods culled, " << site->remaining() <<
            " remaining:";
        site->reportRanks();
        diag.unindent();
      }

      if (overconstrained_) {
        if (ShowInference) {
          diag.indent();
          diag.debug() << "Backtracking";
          diag.unindent();
        }
        backtrack();
        break;
      } else {
        siteChanged = true;
      }
    }

    if (!underconstrained_) {
      break;
    }
  }

  return siteChanged;
}

void TypeInferencePass::cullByConversionRank(ConversionRank lowerLimit) {
  ++searchDepth_;
  cullCount_ = 0;
  for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
    cullCount_ += (*it)->cullByConversionRank(lowerLimit, searchDepth_);
  }

  if (ShowInference) {
    diag.indent();
    diag.debug() << cullCount_ << " methods culled.";
    diag.unindent();
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
    for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
      ChoicePoint * cp = *it;
      if (cp->remaining() > 1) {
        cp->cullBySpecificity(searchDepth_);
      }
    }

    update();
    checkSolution();
    diag.unindent();
  }
}

void TypeInferencePass::cullByElimination() {
  if (underconstrained_) {
    if (ShowInference) {
      diag.debug() << "Culling by elimination";
      reportRanks(INTERMEDIATE);
      diag.indent();
    }
    cullByElimination(callSites_.begin(), callSites_.end());
    if (bestSolutionCount_ == 1) {
      for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
        (*it)->cullAllExceptBest(searchDepth_);
      }
    }

    update();

    if (ShowInference) {
      diag.unindent();
    }
  }
}

void TypeInferencePass::cullByElimination(
  ChoicePointList::iterator first, ChoicePointList::iterator last) {
  while (first < last && underconstrained_) {
    ChoicePoint * pt = *first;
    if (pt->remaining() <= 1) {
      ++first;
    } else {
      int numChoices = pt->count();
      for (int ch = 0; ch < numChoices; ++ch) {
        if (pt->isCulled(ch)) {
          continue;
        }

        if (ShowInference) {
          diag.debug() << Format_Type << "Trying " << pt->expr();
          diag.indent();
        }

        ++searchDepth_;
        pt->cullAllExcept(ch, searchDepth_);
        cullByElimination(first + 1, last);
        backtrack();

        if (ShowInference) {
          diag.unindent();
        }
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
  for (ChoicePointList::iterator it = callSites_.begin(); it != callSites_.end(); ++it) {
    (*it)->backtrack(searchDepth_);
  }

  --searchDepth_;
  update();
}

} // namespace tart

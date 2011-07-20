/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TypeConstraint.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/AmbiguousPhiType.h"

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/FoldConstantsPass.h"
#include "tart/Sema/Infer/ConstraintExpansion.h"
#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/Support/CommandLine.h"

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

bool CallSite::isCulled(int index) const {
  return callExpr_->candidates()[index]->isCulled();
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

void CallSite::cull(int choice, int searchDepth) {
  callExpr_->candidates()[choice]->cull(searchDepth);
}

int CallSite::cullByConversionRank(ConversionRank lowerLimit, int searchDepth) {
  int cullCount = 0;
  Candidates & cd = callExpr_->candidates();
  if (cd.size() == 1) {
    return 0;
  }
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
      bool newIsBetter = call->isMoreSpecific(*ms) &&
          call->conversionRank() >= (*ms)->conversionRank();
      bool oldIsBetter = (*ms)->isMoreSpecific(call) &&
          (*ms)->conversionRank() >= call->conversionRank();
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
    if (!(*c2)->isCulled() && *c2 != best_) {
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

bool CallSite::dependsOn(int choice, const ProvisionSet & pset) const {
  return pset.count(callExpr_->candidates()[choice]->primaryProvision()) != 0;
}

void CallSite::reportRanks() {
  diag.indent();
  Candidates & cd = callExpr_->candidates();
  for (Candidates::iterator c = cd.begin(); c != cd.end(); ++c) {
    CallCandidate * cc = *c;
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
  Candidates & cclist = callExpr_->candidates();
  for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
    (*cc)->relabelTypeVars(env);
  }
}

bool CallSite::unify(BindingEnv & env, int searchDepth) {
  Candidates & cclist = callExpr_->candidates();
  bool canUnify = false;
  unsigned stateBeforeAny = env.stateCount();
  for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
    CallCandidate * c = *cc;
    unsigned stateBeforeNext = env.stateCount();
    if (c->unify(callExpr_, env)) {
      canUnify = true;
    } else {
      if (ShowInference) {
        diag.debug() << Format_Type << "Unification failed for " << c->method() << ":";
        env.backtrack(stateBeforeNext);
        unifyVerbose = true;
        c->unify(callExpr_, env);
        unifyVerbose = false;
      }
      env.backtrack(stateBeforeNext);
      c->cull(searchDepth);
    }
  }

  if (!canUnify) {
    backtrack(stateBeforeAny);
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
  //Expr * functionExpr = callExpr_->function();
  Candidates & cd = callExpr_->candidates();
  DASSERT_OBJ(!cd.empty(), callExpr_);
  out << cd.front()->method()->name() << "(";
  formatExprTypeList(out, callExpr_->args());
  out << ")";
  if (callExpr_->expectedReturnType() != NULL) {
    out << " -> " << callExpr_->expectedReturnType();
  }
}

void CallSite::formatExpression(FormatStream & out) {
  formatCallSignature(out);
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
  StrFormatStream fs;
  fs << Format_Dealias;
  formatCallSignature(fs);
  diag.error(callExpr_->location()) << msg << fs.str() << ".";
  diag.info(callExpr_->location()) << "Candidates are:";
  for (Candidates::iterator it = cd.begin(); it != cd.end(); ++it) {
    CallCandidate * cc = *it;
    StrFormatStream errStrm;
    backtrack(0);
    DBREAK;
    cc->updateConversionRank();
    if (!errStrm.str().empty()) {
      diag.info(cc->method()) << Format_Type << cc->method() << " : " << errStrm.str();
    } else {
      diag.info(cc->method()) << Format_Type  << cc->method() << Format_Dealias <<
          " [" << cc->conversionRank() << "] ";
    }
  }
}

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
  const AmbiguousPhiType * phiType = cast<AmbiguousPhiType>(expr_->type());
  TypeExpansion types;
  const Type * solution = NULL;

  phiType->expand(types);
  if (phiType->expected() != NULL) {
    solution = phiType->expected();
  } else {
    // Try to find a common type that encompasses all input types.
    // TODO: Use coercions if needed.
    for (TypeExpansion::const_iterator it = types.begin(); it != types.end(); ++it) {
      const Type * ty = *it;
      if (solution == NULL) {
        solution = ty;
      } else {
        solution = Type::commonBase(solution, ty);
        if (solution == NULL) {
          break;
        }
      }
    }
  }

  phiType->setCommon(solution);
  if (solution != NULL) {
    // Compute the lowest conversion ranking of all input types to the solution.
    rank_ = IdenticalTypes;
    for (TypeExpansion::const_iterator it = types.begin(); it != types.end(); ++it) {
      rank_ = std::min(rank_, solution->canConvert(*it, Conversion::Coerce));
    }
  }
}

void PHISite::report() {
  const AmbiguousPhiType * phiType = cast<AmbiguousPhiType>(expr_->type());
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

  return CFGPass::visitAssign(in);
}

Expr * GatherConstraintsPass::visitPostAssign(AssignmentExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    constraints_.push_back(new AssignmentSite(in));
  }

  return CFGPass::visitPostAssign(in);
}

Expr * GatherConstraintsPass::visitTupleCtor(TupleCtorExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    constraints_.push_back(new TupleCtorSite(in));
  }

  return CFGPass::visitTupleCtor(in);
}

Expr * GatherConstraintsPass::visitConstantInteger(ConstantInteger * in) {
  if (in->type()->isUnsizedIntType()) {
    in->setType(new SizingOfConstraint(in));
  }

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
  if (isa<AmbiguousPhiType>(in->type())) {
    constraints_.push_back(new PHISite(in));
  }
}

// -------------------------------------------------------------------
// TypeInference

Expr * TypeInferencePass::run(Module * module, Expr * in, BindingEnv & env,
    const Type * expected, bool strict) {
  TypeInferencePass instance(module, in, env, expected, strict);
  return instance.runImpl();
}

Expr * TypeInferencePass::runImpl() {
  rootExpr_ = FoldConstantsPass(module_).visitExpr(rootExpr_);
  if (isErrorResult(rootExpr_) || rootExpr_->isSingular()) {
    return rootExpr_;
  }

  GatherConstraintsPass(choicePoints_, cstrSites_).visitExpr(rootExpr_);
  if (choicePoints_.empty()) {
    // Try running the constraints anyway, just to see what happens.
    for (ConstraintSiteSet::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
        it != itEnd; ++it) {
      (*it)->update();
    }
    if (rootExpr_->isSingular()) {
      return rootExpr_;
    }

    for (ConstraintSiteSet::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
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
    for (ConstraintSiteSet::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
        it != itEnd; ++it) {
      (*it)->report();
    }
    diag.unindent();

    return rootExpr_;
  }

  if (ShowInference) {
    diag.debug() << "\n## Begin Type Inference";
    diag.indent();
    diag.debug() << "Expression: " << rootExpr_;
    diag.unindent();
  }

  bestSolutionRank_ = Incompatible;
  bestSolutionCount_ = 0;
  relabelVars();
  if (!unify()) {
    return rootExpr_;
  }

  simplifyConstraints();
  update();
  reportRanks(INITIAL);

  if (cullByContradiction()) {
    reportRanks(INTERMEDIATE);
  }
  if (cullByConversionRank()) {
    reportRanks(INTERMEDIATE);
  }
  cullBySpecificity();
  cullByElimination();

  reportRanks(FINAL);

  // Remove all culled candidates
  for (ChoicePointList::iterator pt = choicePoints_.begin(); pt != choicePoints_.end(); ++pt) {
    (*pt)->finish();
  }

  env_.updateAssignments(rootExpr_->location(), NULL);
  if (ShowInference) {
    diag.debug() << "=== Normalized substitutions ===";
    diag.indent();
    env_.dump();
    diag.unindent();
  }
  return rootExpr_;
}

void TypeInferencePass::relabelVars() {
  for (ChoicePointList::iterator pt = choicePoints_.begin(); pt != choicePoints_.end(); ++pt) {
    (*pt)->relabelVars(env_);
  }
}

bool TypeInferencePass::unify() {
  ++searchDepth_;
  bool success = true;
  for (ChoicePointList::iterator pt = choicePoints_.begin(); pt != choicePoints_.end(); ++pt) {
    if (!(*pt)->unify(env_, searchDepth_)) {
      success = false;
    }
  }

  return success;
}

void TypeInferencePass::simplifyConstraints() {
  if (env_.empty()) {
    return;
  }

  bool hasConstraints = false;
  for (TypeAssignment * ta = env_.assignments(); ta != NULL; ta = ta->next()) {
    if (!ta->constraints().empty()) {
      hasConstraints = true;
      break;
    }
  }
  if (!hasConstraints) {
    return;
  }

  if (ShowInference) {
    diag.debug() << "=== Initial constraints ===";
    diag.indent();
    env_.dump();
    diag.unindent();
  }

  // Optimize constraints
  for (TypeAssignment * ta = env_.assignments(); ta != NULL; ta = ta->next()) {
    ta->constraints().minimize();
  }

  // Simpify by expansion.
  bool changed;
  unsigned loopCount = 0;
  do {
    if (ShowInference && loopCount > 0) {
      diag.debug() << "=== Result after simplification pass " << loopCount << " ===";
      diag.indent();
      env_.dump();
      diag.unindent();
    }

    changed = false;
    for (TypeAssignment * ta = env_.assignments(); ta != NULL; ta = ta->next()) {
      ConstraintExpansion expansion(ta);
      expansion.expandAll(ta->constraints());

      // See if it changed
      expansion.result().swap(ta->constraints());
      if (!changed && !expansion.result().equals(ta->constraints())) {
        changed = true;
      }
    }

    if (loopCount++ > 5) {
      diag.debug() << "## Infinite Loop in simplifySubstitutions";
      diag.indent();
      env_.dump();
      diag.unindent();
      exit(-1);
    }
  } while (changed);

  selectConstantIntegerTypes();
  env_.sortAssignments();

  if (ShowInference) {
    diag.debug() << "=== After constraint simplification pass ===";
    diag.indent();
    env_.dump();
    diag.unindent();
  }
}

void TypeInferencePass::selectConstantIntegerTypes() {
  // Replace unconstrained integer constants with plain integer types.
  for (TypeAssignment * ta = env_.assignments(); ta != NULL; ta = ta->next()) {
    bool hasUnsizedIntTypes = false;
    bool hasSizedIntTypes = false;
    for (ConstraintSet::const_iterator si = ta->begin(), sEnd = ta->end(); si != sEnd; ++si) {
      const Type * ty = (*si)->value();
      if (isa<SizingOfConstraint>(ty)) {
        hasUnsizedIntTypes = true;
      } else if (ty->isIntType()) {
        hasSizedIntTypes = true;
      }
    }

    bool changed = false;
    if (hasUnsizedIntTypes && !hasSizedIntTypes) {
      for (ConstraintSet::const_iterator si = ta->begin(), sEnd = ta->end(); si != sEnd; ++si) {
        Constraint * s = *si;
        if (const SizingOfConstraint * soc = dyn_cast<SizingOfConstraint>(s->value())) {
          changed = true;
          if (soc->signedBitsRequired() <= 32) {
            s->setValue(&Int32Type::instance);
          } else {
            s->setValue(&Int64Type::instance);
          }
        }
      }
    }

    if (changed) {
      ta->constraints().minimize();
    }
  }
}

void TypeInferencePass::update() {
  lowestRank_ = IdenticalTypes;
  underconstrained_ = false;
  overconstrained_ = false;

  env_.reconcileConstraints(NULL);

  if (expectedType_ != NULL) {
    ConversionRank rootRank = expectedType_->canConvert(rootExpr_);
    if (!strict_ && rootRank < NonPreferred) {
      rootRank = NonPreferred;
    }

    lowestRank_ = rootRank;
  }

  for (ChoicePointList::iterator it = choicePoints_.begin(); it != choicePoints_.end(); ++it) {
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

  for (ConstraintSiteSet::iterator it = cstrSites_.begin(); it != cstrSites_.end(); ++it) {
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
      for (ChoicePointList::iterator it = choicePoints_.begin(); it != choicePoints_.end(); ++it) {
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

  size_t numSites = choicePoints_.size();
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
  for (ChoicePointList::iterator pt = choicePoints_.begin(); pt != choicePoints_.end();
      ++pt, ++siteIndex) {
    StrFormatStream fs;
    (*pt)->formatExpression(fs);
    diag.debug() << "Call site #" << siteIndex << ": " << (*pt)->remaining() <<
        " candidates for " << Format_Type << fs.str();
    (*pt)->reportRanks();
  }
  diag.unindent();
}

bool TypeInferencePass::cullByContradiction() {
  // The set of unsatisfiable provisions
  ProvisionSet unsatisfiable;

  // Optimize constraints
  for (TypeAssignment * ta = env_.assignments(); ta != NULL; ta = ta->next()) {
    const ConstraintSet & cs = ta->constraints();

    // For now, we're only interested in constraints that have a single provision.
    for (ConstraintSet::const_iterator ci = cs.begin(), ciEnd = cs.end(); ci != ciEnd; ++ci) {
      Constraint * c = *ci;
      if (c->provisions().size() == 1) {
        const Provision * prov = *c->provisions().begin();
        bool foundContradiction = false;
        for (ConstraintSet::const_iterator si = cs.begin(), siEnd = cs.end(); si != siEnd; ++si) {
          if (ci != si) {
            Constraint * s = *si;
            if (Constraint::contradicts(c, s) && c->provisions().implies(s->provisions())) {
              foundContradiction = true;
              if (ShowInference) {
                if (unsatisfiable.empty()) {
                  diag.debug() << "=== Culling candidates with unsatisfiable constraints ===";
                }
                diag.indent();
                diag.debug() << "Candidate " << prov << " culled because of constraints: [" <<
                    c->kind() << " " << c->value() << "] and [" <<
                    s->kind() << " " << s->value() << "]";
                diag.unindent();
              }
              break;
            }
          }
        }

        if (foundContradiction) {
          unsatisfiable.insert(prov);
        }
      }
    }
  }

  if (unsatisfiable.empty()) {
    return false;
  }

  ++searchDepth_;

  // First determine if there are contradictions with no overloads culled.
  int siteIndex = 1;
  for (ChoicePointList::iterator pt = choicePoints_.begin(); pt != choicePoints_.end();
      ++pt, ++siteIndex) {
    ChoicePoint * cp = *pt;
    int count = cp->count();
    if (count > 1) {
      for (int i = 0; i < count; ++i) {
        if (cp->dependsOn(i, unsatisfiable)) {
          cp->cull(i, searchDepth_);
        }
      }
    }
  }

  return true;
}

bool TypeInferencePass::cullByConversionRank() {
  int initialSearchDepth = searchDepth_;

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
  return searchDepth_ != initialSearchDepth;
}

bool TypeInferencePass::cullEachSiteByConversionRank() {
  int siteIndex = 1;
  bool siteChanged = false;
  for (ChoicePointList::iterator pt = choicePoints_.begin(); pt != choicePoints_.end();
      ++pt, ++siteIndex) {
    ChoicePoint * site = *pt;
    if (site->count() > 1) {
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
  }

  return siteChanged;
}

void TypeInferencePass::cullByConversionRank(ConversionRank lowerLimit) {
  ++searchDepth_;
  cullCount_ = 0;
  for (ChoicePointList::iterator it = choicePoints_.begin(); it != choicePoints_.end(); ++it) {
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
    //ConversionRank savedRank = lowestRank_;
    diag.indent();
    ++searchDepth_;
    for (ChoicePointList::iterator it = choicePoints_.begin(); it != choicePoints_.end(); ++it) {
      ChoicePoint * cp = *it;
      if (cp->remaining() > 1) {
        cp->cullBySpecificity(searchDepth_);
      }
    }

    update();
    checkSolution();
    diag.unindent();

    if (/*savedRank > SignedUnsigned &&*/ lowestRank_ <= SignedUnsigned) {
      if (ShowInference) {
        diag.indent();
        diag.debug() << "Backtracking";
        diag.unindent();
      }
      backtrack();
    }
  }
}

void TypeInferencePass::cullByElimination() {
  if (underconstrained_) {
    if (ShowInference) {
      diag.debug() << "Culling by elimination";
      reportRanks(INTERMEDIATE);
      diag.indent();
    }
    cullByElimination(choicePoints_.begin(), choicePoints_.end());
    if (bestSolutionCount_ == 1) {
      for (ChoicePointList::iterator it = choicePoints_.begin(); it != choicePoints_.end(); ++it) {
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
      DASSERT(pt->remaining() > 0);
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
  DASSERT(!overconstrained_);
  checkSolution();
}

void TypeInferencePass::backtrack() {
  for (ChoicePointList::iterator it = choicePoints_.begin(); it != choicePoints_.end(); ++it) {
    (*it)->backtrack(searchDepth_);
  }

  --searchDepth_;
  update();
}

} // namespace tart

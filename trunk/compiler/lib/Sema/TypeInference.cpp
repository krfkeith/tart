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
#include "tart/Type/TypeConversion.h"
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
optShowInference("show-inference",
    llvm::cl::desc("Display debugging information for type inference"));

bool unifyVerbose = false;
bool showInference = false;

// -------------------------------------------------------------------
// GatherConstraintsPass

Expr * GatherConstraintsPass::visitCall(CallExpr * in) {
  if (!in->candidates().empty() && visited_.insert(in)) {
    // Note: pre-order traversal.
    Expr * result = CFGPass::visitCall(in);
    calls_.push_back(new CallSite(in));

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
    conversions_.push_back(new AssignmentSite(in));
  }

  return CFGPass::visitAssign(in);
}

Expr * GatherConstraintsPass::visitPostAssign(AssignmentExpr * in) {
  if (!in->isSingular() && visited_.insert(in)) {
    conversions_.push_back(new AssignmentSite(in));
  }

  return CFGPass::visitPostAssign(in);
}

Expr * GatherConstraintsPass::visitTupleCtor(TupleCtorExpr * in) {
  in = cast<TupleCtorExpr>(CFGPass::visitTupleCtor(in));
  if (!in->isSingular() && visited_.insert(in)) {
    conversions_.push_back(new TupleCtorSite(in));
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
  if (in->type().isa<AmbiguousPhiType>()) {
    conversions_.push_back(new PHISite(in));
  }
}

// -------------------------------------------------------------------
// TypeInference

Expr * TypeInferencePass::run(Module * module, Expr * in, BindingEnv & env,
    QualifiedType expected, bool strict) {
  TypeInferencePass instance(module, in, env, expected, strict);
  return instance.runImpl();
}

Expr * TypeInferencePass::runImpl() {
  showInference = optShowInference;

  rootExpr_ = FoldConstantsPass(module_).visitExpr(rootExpr_);
  if (isErrorResult(rootExpr_) || rootExpr_->isSingular()) {
    return rootExpr_;
  }

  GatherConstraintsPass(calls_, cstrSites_).visitExpr(rootExpr_);
  if (calls_.empty()) {
    // Try running the constraints anyway, just to see what happens.
    for (ConversionSites::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
        it != itEnd; ++it) {
      (*it)->update();
    }
    if (rootExpr_->isSingular()) {
      return rootExpr_;
    }

    for (ConversionSites::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
        it != itEnd; ++it) {
      const Expr * e = (*it)->expr();
      if (!e->isSingular()) {
        diag.error(e) << "Can't find an unambiguous solution for '" << e << "'";
        //(*it)->report();
        return rootExpr_;
      }
    }

    diag.error(rootExpr_) << "Can't find an unambiguous solution for '" << rootExpr_ << "'";
    diag.info() << "Total constraint count: " << cstrSites_.size();
    diag.indent();
    for (ConversionSites::const_iterator it = cstrSites_.begin(), itEnd = cstrSites_.end();
        it != itEnd; ++it) {
      //(*it)->report();
    }
    diag.unindent();

    return rootExpr_;
  }

  if (showInference) {
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
  for (CallSites::iterator pt = calls_.begin(); pt != calls_.end(); ++pt) {
    (*pt)->finish();
  }

  // Report constraint errors.
  for (ConversionSites::iterator cs = cstrSites_.begin(); cs != cstrSites_.end(); ++cs) {
    ConversionSite * site = *cs;
    if (site->rank() <= QualifierLoss) {
      diag.info(site->expr()) << compatibilityError(site->rank()) << Format_Dealias <<
          " executing " << site->expr();
    }
  }

  env_.updateAssignments(rootExpr_->location(), NULL);
  if (showInference) {
    diag.debug() << "=== Normalized substitutions ===";
    diag.indent();
    env_.dump();
    diag.unindent();

    if (diag.getErrorCount() > 0) {
      exit(-1);
    }
  }
  return rootExpr_;
}

void TypeInferencePass::relabelVars() {
  for (CallSites::iterator pt = calls_.begin(); pt != calls_.end(); ++pt) {
    (*pt)->relabelVars(env_);
  }
}

bool TypeInferencePass::unify() {
  ++searchDepth_;
  bool success = true;
  for (CallSites::iterator pt = calls_.begin(); pt != calls_.end(); ++pt) {
    CallSite * cp = *pt;
    if (cp->hasErrors()) {
      return false;
    }
    if (!cp->unify(env_, searchDepth_)) {
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

  if (showInference) {
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
    if (showInference && loopCount > 0) {
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

  // Remove primary provisions, because they are now redundant.
  for (TypeAssignment * ta = env_.assignments(); ta != NULL; ta = ta->next()) {
    if (ta->primaryProvision() != NULL) {
      for (ConstraintSet::iterator csi = ta->constraints().begin(); csi != ta->constraints().end();
          ++csi) {
        (*csi)->provisions().erase(ta->primaryProvision());
      }
    }
  }

  selectConstantIntegerTypes();
  env_.sortAssignments();

  if (showInference) {
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
      QualifiedType ty = (*si)->value();
      if (ty->isUnsizedIntType()) {
        hasUnsizedIntTypes = true;
      } else if (ty->isIntType()) {
        hasSizedIntTypes = true;
      }
    }

    bool changed = false;
    if (hasUnsizedIntTypes && !hasSizedIntTypes) {
      for (ConstraintSet::const_iterator si = ta->begin(), sEnd = ta->end(); si != sEnd; ++si) {
        Constraint * s = *si;
        if (Qualified<UnsizedIntType> uint = s->value().dyn_cast<UnsizedIntType>()) {
          changed = true;
          if (uint->signedBitsRequired() <= 32) {
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

  if (expectedType_) {
    ConversionRank rootRank = TypeConversion::check(rootExpr_, expectedType_);
    if (!strict_ && rootRank < NonPreferred) {
      rootRank = NonPreferred;
    }

    lowestRank_ = rootRank;
  }

  for (CallSites::iterator it = calls_.begin(); it != calls_.end(); ++it) {
    CallSite * cs = *it;
    cs->update();
    lowestRank_ = std::min(lowestRank_, cs->rank());
    int choices = cs->remaining();
    if (choices == 0) {
      overconstrained_ = true;
    } else if (choices > 1) {
      underconstrained_ = true;
    }
  }

  for (ConversionSites::iterator it = cstrSites_.begin(); it != cstrSites_.end(); ++it) {
    (*it)->update();
    lowestRank_ = std::min(lowestRank_, (*it)->rank());
  }
}

void TypeInferencePass::checkSolution() {
  if (!overconstrained_ && !underconstrained_) {
    if (lowestRank_ == bestSolutionRank_) {
      ++bestSolutionCount_;
      if (showInference) {
        diag.debug() << "Discovered equivalent solution with rank " << bestSolutionRank_;
      }
    } else if (lowestRank_ > bestSolutionRank_) {
      bestSolutionCount_ = 1;
      bestSolutionRank_ = lowestRank_;
      for (CallSites::iterator it = calls_.begin(); it != calls_.end(); ++it) {
        (*it)->saveBest();
      }

      if (showInference) {
        diag.debug() << "Discovered new best solution with rank [" << bestSolutionRank_ << "]";
      }
    }
  }
}

void TypeInferencePass::reportRanks(ReportLabel label) {
  if (!showInference) {
    return;
  }

  size_t numSites = calls_.size();
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
  for (CallSites::iterator pt = calls_.begin(); pt != calls_.end();
      ++pt, ++siteIndex) {
    StrFormatStream fs;
    (*pt)->formatCallSignature(fs);
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
              if (showInference) {
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
      } else if (c->provisions().empty() && ta->primaryProvision() != NULL) {
        // Constraints that have zero provisions are always in effect.
        bool foundContradiction = false;
        for (ConstraintSet::const_iterator si = cs.begin(), siEnd = cs.end(); si != siEnd; ++si) {
          if (ci != si && (*si)->provisions().empty()) {
            Constraint * s = *si;
            if (Constraint::contradicts(c, s)) {
              foundContradiction = true;
              if (showInference) {
                if (unsatisfiable.empty()) {
                  diag.debug() << "=== Culling candidates with unsatisfiable constraints ===";
                }
                diag.indent();
                diag.debug() << "Candidate " << ta->primaryProvision() <<
                    " culled because of constraints: [" <<
                    c->kind() << " " << c->value() << "] and [" <<
                    s->kind() << " " << s->value() << "]";
                diag.unindent();
              }
              break;
            }
          }
        }

        if (foundContradiction) {
          unsatisfiable.insert(ta->primaryProvision());
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
  for (CallSites::iterator pt = calls_.begin(); pt != calls_.end();
      ++pt, ++siteIndex) {
    CallSite * cp = *pt;
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
    if (showInference) {
      diag.debug() << "All sites: Culling overloads of rank < Truncation";
    }
    cullByConversionRank(Truncation);
  }

  if (!overconstrained_ && underconstrained_ && lowestRank_ < ExactConversion) {
    if (showInference) {
      diag.debug() << "All sites: Culling overloads of rank < Exact";
    }
    cullByConversionRank(ExactConversion);
  }

  if (!overconstrained_ && underconstrained_ && lowestRank_ < IdenticalTypes) {
    if (showInference) {
      diag.debug() << "All sites: Culling overloads of rank < Identical";
    }
    cullByConversionRank(IdenticalTypes);
  }

  if (overconstrained_) {
    if (showInference) {
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
  for (CallSites::iterator pt = calls_.begin(); pt != calls_.end();
      ++pt, ++siteIndex) {
    CallSite * site = *pt;
    if (site->count() > 1) {
      while (site->rank() < IdenticalTypes) {
        ConversionRank limit = ConversionRank(site->rank() + 1);
        if (showInference) {
          diag.debug() << "Site #" << siteIndex << ": culling overloads of rank < " << limit;
        }
        ++searchDepth_;
        cullCount_ = site->cullByConversionRank(limit, searchDepth_);
        update();

        if (showInference) {
          diag.indent();
          diag.debug() << cullCount_ << " methods culled, " << site->remaining() <<
              " remaining:";
          site->reportRanks();
          diag.unindent();
        }

        if (overconstrained_) {
          if (showInference) {
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
  for (CallSites::iterator it = calls_.begin(); it != calls_.end(); ++it) {
    cullCount_ += (*it)->cullByConversionRank(lowerLimit, searchDepth_);
  }

  if (showInference) {
    diag.indent();
    diag.debug() << cullCount_ << " methods culled.";
    diag.unindent();
  }

  update();
}

void TypeInferencePass::cullBySpecificity() {
  if (underconstrained_) {
    if (showInference) {
      diag.debug() << "Culling by specificity";
    }
    //ConversionRank savedRank = lowestRank_;
    diag.indent();
    ++searchDepth_;
    for (CallSites::iterator it = calls_.begin(); it != calls_.end(); ++it) {
      CallSite * cp = *it;
      if (cp->remaining() > 1) {
        cp->cullBySpecificity(searchDepth_);
      }
    }

    update();
    checkSolution();
    diag.unindent();

    if (/*savedRank > SignedUnsigned &&*/ lowestRank_ <= SignedUnsigned) {
      if (showInference) {
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
    if (showInference) {
      diag.debug() << "Culling by elimination";
      reportRanks(INTERMEDIATE);
      diag.indent();
    }
    cullByElimination(calls_.begin(), calls_.end());
    if (bestSolutionCount_ == 1) {
      for (CallSites::iterator it = calls_.begin(); it != calls_.end(); ++it) {
        (*it)->cullAllExceptBest(searchDepth_);
      }
    }

    update();

    if (showInference) {
      diag.unindent();
    }
  }
}

void TypeInferencePass::cullByElimination(
  CallSites::iterator first, CallSites::iterator last) {
  while (first < last && underconstrained_) {
    CallSite * pt = *first;
    if (pt->remaining() <= 1) {
      DASSERT(pt->remaining() > 0);
      ++first;
    } else {
      int numChoices = pt->count();
      for (int ch = 0; ch < numChoices; ++ch) {
        if (pt->isCulled(ch)) {
          continue;
        }

        if (showInference) {
          diag.debug() << Format_Type << "Trying " << pt->expr();
          diag.indent();
        }

        ++searchDepth_;
        pt->cullAllExcept(ch, searchDepth_);
        cullByElimination(first + 1, last);
        backtrack();

        if (showInference) {
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
  for (CallSites::iterator it = calls_.begin(); it != calls_.end(); ++it) {
    (*it)->backtrack(searchDepth_);
  }

  --searchDepth_;
  update();
}

} // namespace tart

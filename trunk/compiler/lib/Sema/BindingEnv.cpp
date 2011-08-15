/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/NativeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Type/AmbiguousType.h"

#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/TypeTransform.h"
#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/ImmutableSet.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/CommandLine.h"

static llvm::cl::opt<bool>
DebugUnify("debug-unify", llvm::cl::desc("Debug unification"), llvm::cl::init(false));

namespace tart {

typedef llvm::DenseSet<const Type *, Type::KeyInfo> TypeSet;
typedef llvm::SmallSetVector<TypeAssignment *, 16> TypeAssignmentSet;

namespace {

/** Special version of dealias that doesn't dereference type assignments. */
const Type * dereferenceAlias(const Type * type) {
  if (const TypeAlias * alias = dyn_cast<TypeAlias>(type)) {
    type = alias->value();
    DASSERT_OBJ(type != NULL, alias);
  }

  return type;
}

void findTypeAssignments(TypeAssignmentSet & result, const Type * ty) {
  if (const TypeAssignment * ta = dyn_cast<TypeAssignment>(ty)) {
    TypeAssignment * taMutable = const_cast<TypeAssignment *>(ta);
    if (!result.count(taMutable)) {
      for (ConstraintSet::const_iterator it = ta->begin(), itEnd = ta->end(); it != itEnd; ++it) {
        Constraint * cst = *it;
        if (!cst->visited()) {
          cst->setVisited(true);
          findTypeAssignments(result, cst->value());
          cst->setVisited(false);
        }
      }
      result.insert(taMutable);
    }
  } else if (ty->numTypeParams() > 0) {
    for (size_t i = 0, numTypeParams = ty->numTypeParams(); i < numTypeParams; ++i) {
      findTypeAssignments(result, ty->typeParam(i));
    }
  }
}

}

extern bool unifyVerbose;

// -------------------------------------------------------------------
// BindingEnv

void BindingEnv::reset() {
  backtrack(0);
}

bool BindingEnv::unify(SourceContext * source, const Type * left, const Type * right,
    Constraint::Kind kind, const ProvisionSet & provisions) {
  if (left == &BadType::instance) {
    return false;
  }

  // Dealias but don't dereference type variables
  left = dereferenceAlias(left);
  right = dereferenceAlias(right);

  if (DebugUnify || unifyVerbose) {
    if (diag.getIndentLevel() == 0) {
      diag.debug(source->location()) << "## Begin unification of: " << left << " with: " << right;
      if (stateCount_ > 0) {
        diag.debug() << "   Current bindings: " << Format_Verbose << *this;
      }
    } else {
      diag.debug() << "unify " << left << " with " << right;
    }
    diag.indent();
  }

  bool result = unifyImpl(source, left, right, kind, provisions);

  if (DebugUnify || unifyVerbose) {
    if (!result) {
      diag.debug() << "unification failed!";
    }

    diag.unindent();
    if (diag.getIndentLevel() == 0) {
      if (result) {
        diag.debug() << "## End (success): " << *this;
      } else {
        diag.debug() << "## End (failure)";
      }
    }
  }

  return result;
}

bool BindingEnv::unifyImpl(SourceContext * source, const Type * left, const Type * right,
    Constraint::Kind kind, const ProvisionSet & provisions) {
  if (left == right) {
    return true;
  } else if (isErrorResult(left)) {
    return false;
  }

  // Make sure that if either left or right is a type variable, it is not one that
  // this environment knows about, otherwise it should have been replaced with
  // a type assignment.
  DASSERT_OBJ(!isAssigned(left), left);
  DASSERT_OBJ(!isAssigned(right), right);

  // Ambiguous type on the left side.
  switch (left->typeClass()) {
    case Type::AmbiguousResult:
    case Type::AmbiguousParameter:
    case Type::AmbiguousTypeParam:
      return unifyWithAmbiguousType(source, left, right, kind, provisions);
    default:
      break;
  }

  // Ambiguous type on the right.
  switch (right->typeClass()) {
    case Type::AmbiguousResult:
    case Type::AmbiguousParameter:
    case Type::AmbiguousTypeParam:
      return unifyWithAmbiguousType(source, right, left, Constraint::reverse(kind), provisions);
    default:
      break;
  }

  // Type variable on left side
  if (const TypeAssignment * ta = dyn_cast<TypeAssignment>(left)) {
    return unifyWithTypeVar(source, ta, right, kind, provisions);
  }

  // Type variable on right side
  if (const TypeAssignment * ta = dyn_cast<TypeAssignment>(right)) {
    return unifyWithTypeVar(source, ta, left, Constraint::reverse(kind), provisions);
  }

  // Address type
  if (const AddressType * npp = dyn_cast<AddressType>(left)) {
    return unifyAddressType(source, npp, right);
  }

  // Native array type
  if (const NativeArrayType * nap = dyn_cast<NativeArrayType>(left)) {
    return unifyNativeArrayType(source, nap, right);
  }

  // Flexible array type
  if (const FlexibleArrayType * nap = dyn_cast<FlexibleArrayType>(left)) {
    return unifyFlexibleArrayType(source, nap, right);
  }

  // Tuple type
  if (const TupleType * tPattern = dyn_cast<TupleType>(left)) {
    if (const TupleType * tValue = dyn_cast<TupleType>(right)) {
      return unifyTupleType(source, tPattern, tValue);
    }

    return false;
  }

  // Type literal type
  if (const TypeLiteralType * npp = dyn_cast<TypeLiteralType>(left)) {
    return unifyTypeLiteralType(source, npp, right);
  }

  if (const CompositeType * ctPattern = dyn_cast<CompositeType>(left)) {
    if (const CompositeType * ctValue = dyn_cast<CompositeType>(right)) {
      return unifyCompositeType(source, ctPattern, ctValue);
    } else if (const NativeArrayType * natValue = dyn_cast<NativeArrayType>(right)) {
      // Special case for assigning Array to NativeArray in initializers
      if (ctPattern->typeDefn()->ast() == Builtins::typeArray->typeDefn()->ast()) {
        return unify(source, ctPattern->typeParam(0), natValue->typeParam(0), Constraint::EXACT);
      }
//    } else if (const FunctionType * fnValue = dyn_cast<FunctionType>(value)) {
//      const CompositeType * fnPattern =
//      if (!fnValue->isStatic()) {
//      }
    }

    if (const UnionType * rut = dyn_cast<UnionType>(right)) {
      return unifyUnionMemberType(source, rut, ctPattern);
    }

    // The type on the left may have a coercion method, so assume that unification
    // succeeds.
    if (right->isSingular()) {
      return true;
    }

    return false;
  }

  if (const UnionType * lut = dyn_cast<UnionType>(left)) {
    if (const UnionType * rut = dyn_cast<UnionType>(right)) {
      return unifyUnionType(source, lut, rut);
    } else if (unifyUnionMemberType(source, lut, right)) {
      return true;
    }

    return false;
  } else if (const UnionType * rut = dyn_cast<UnionType>(right)) {
    if (unifyUnionMemberType(source, rut, left)) {
      return true;
    }
  }

  if (isa<PrimitiveType>(left)) {
    // Go ahead and unify - type inference will see if it can convert.
    return true;
  } else {
    return false;
  }
}

bool BindingEnv::unifyAddressType(
    SourceContext * source, const AddressType * pat, const Type * value) {
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (const AddressType * npv = dyn_cast<AddressType>(value)) {
    if (!AnalyzerBase::analyzeType(npv, Task_PrepTypeComparison)) {
      return false;
    }

    return unify(source, pat->typeParam(0), npv->typeParam(0), Constraint::EXACT);
  } else {
    return false;
  }
}

bool BindingEnv::unifyNativeArrayType(SourceContext * source, const NativeArrayType * pat,
    const Type * value) {
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (const NativeArrayType * nav = dyn_cast<NativeArrayType>(value)) {
    if (!AnalyzerBase::analyzeType(nav, Task_PrepTypeComparison)) {
      return false;
    }

    if (pat->size() != nav->size()) {
      return false;
    }

    return unify(source, pat->typeParam(0), nav->typeParam(0), Constraint::EXACT);
  } else {
    return false;
  }
}

bool BindingEnv::unifyFlexibleArrayType(SourceContext * source, const FlexibleArrayType * pat,
    const Type * value) {
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (const FlexibleArrayType * fav = dyn_cast<FlexibleArrayType>(value)) {
    if (!AnalyzerBase::analyzeType(fav, Task_PrepTypeComparison)) {
      return false;
    }

    return unify(source, pat->typeParam(0), fav->typeParam(0), Constraint::EXACT);
  } else {
    return false;
  }
}

bool BindingEnv::unifyTypeLiteralType(
    SourceContext * source, const TypeLiteralType * pat, const Type * value) {
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (const TypeLiteralType * npv = dyn_cast<TypeLiteralType>(value)) {
    if (!AnalyzerBase::analyzeType(npv, Task_PrepTypeComparison)) {
      return false;
    }

    return unify(source, pat->typeParam(0), npv->typeParam(0), Constraint::EXACT);
  } else {
    return false;
  }
}

bool BindingEnv::unifyCompositeType(
    SourceContext * source, const CompositeType * left, const CompositeType * right) {
  if (left->isEqual(right)) {
    return true;
  }

  if (!AnalyzerBase::analyzeType(left, Task_PrepTypeComparison)) {
    return false;
  }

  if (!AnalyzerBase::analyzeType(right, Task_PrepTypeComparison)) {
    return false;
  }

  TypeDefn * tdLeft = left->typeDefn();
  TypeDefn * tdRight = right->typeDefn();

  // Compare the ASTs to see if they derive from the same original symbol.
  if (tdLeft->ast() == tdRight->ast()) {
    // Now we have to see if we can bind the type variables.
    const TupleType * typeParamsLeft = NULL;
    const TupleType * typeParamsRight = NULL;

    if (tdLeft->isTemplate()) {
      typeParamsLeft = tdLeft->templateSignature()->typeParams();
    } else if (tdLeft->isTemplateInstance()) {
      typeParamsLeft = tdLeft->templateInstance()->typeArgs();
    }

    if (tdRight->isTemplate()) {
      typeParamsRight = tdRight->templateSignature()->typeParams();
    } else if (tdRight->isTemplateInstance()) {
      typeParamsRight = tdRight->templateInstance()->typeArgs();
    }

    if (typeParamsLeft == typeParamsRight) {
      return true;
    }

    if (typeParamsLeft == NULL ||
        typeParamsRight == NULL ||
        typeParamsLeft->size() != typeParamsRight->size()) {
      return false;
    }

    size_t numParams = typeParamsLeft->size();
    unsigned savedState = stateCount_;
    for (size_t i = 0; i < numParams; ++i) {
      if (!unify(source, (*typeParamsLeft)[i], (*typeParamsRight)[i], Constraint::EXACT)) {
        backtrack(savedState);
        return false;
      }
    }

    return true;
  }

  // See if we can find a match in the superclasses.
  // This may produce a false positive, which will be caught later.

  unsigned savedState = stateCount_;
  for (ClassList::const_iterator it = right->bases().begin(); it != right->bases().end(); ++it) {
    if (unifyCompositeType(source, left, *it)) {
      return true;
    }

    backtrack(savedState);
  }

  for (ClassList::const_iterator it = left->bases().begin(); it != left->bases().end(); ++it) {
    if (unifyCompositeType(source, *it, right)) {
      return true;
    }

    backtrack(savedState);
  }

  return false;
}

bool BindingEnv::unifyUnionType(
    SourceContext * source, const UnionType * left, const UnionType * right) {
  if (left->isEqual(right)) {
    return true;
  }

  if (!AnalyzerBase::analyzeType(left, Task_PrepTypeComparison)) {
    return false;
  }

  if (!AnalyzerBase::analyzeType(right, Task_PrepTypeComparison)) {
    return false;
  }

  if (left->members().size() != right->members().size()) {
    return false;
  }

  TypeSet membersLeft;
  for (TupleType::const_iterator it = left->members().begin(); it != left->members().end();
      ++it) {
    membersLeft.insert(dereferenceAlias(*it));
  }

  TypeSet memberRight;
  for (TupleType::const_iterator it = right->members().begin(); it != right->members().end();
      ++it) {
    memberRight.insert(dereferenceAlias(*it));
    membersLeft.erase(dereferenceAlias(*it));
  }

  for (TupleType::const_iterator it = left->members().begin(); it != left->members().end();
      ++it) {
    memberRight.erase(dereferenceAlias(*it));
  }

  if (memberRight.size() != membersLeft.size()) {
    return false;
  }

  if (memberRight.size() == 0) {
    return true;
  }

  if (memberRight.size() > 1) {
    DFAIL("Implement more than one pattern var per union");
  }

  return unify(source, *membersLeft.begin(), *memberRight.begin(), Constraint::EXACT);
}

bool BindingEnv::unifyUnionMemberType(
    SourceContext * source, const UnionType * left, const Type * right) {
  // If we're assigning to a union type, try each of the union members.
  unsigned savedState = stateCount_;
  for (TupleType::const_iterator it = left->members().begin(); it != left->members().end();
      ++it) {
    if (unify(source, right, *it, Constraint::EXACT)) {
      return true;
    }

    backtrack(savedState);
  }

  return false;
}

bool BindingEnv::unifyTupleType(
    SourceContext * source, const TupleType * left, const TupleType * right) {
  if (left->isEqual(right)) {
    return true;
  }

  if (!AnalyzerBase::analyzeType(left, Task_PrepTypeComparison)) {
    return false;
  }

  if (!AnalyzerBase::analyzeType(right, Task_PrepTypeComparison)) {
    return false;
  }

  if (left->size() != right->size()) {
    return false;
  }

  size_t size = left->size();
  for (size_t i = 0; i < size; ++i) {
    if (!unify(source, left->member(i), right->member(i), Constraint::EXACT)) {
      return false;
    }
  }

  return true;
}

bool BindingEnv::unifyWithTypeVar(
    SourceContext * source, const TypeAssignment * ta, const Type * value,
    Constraint::Kind kind, const ProvisionSet & provisions) {

  // Dereference the value as well.
  value = TypeAssignment::deref(value);

  // Don't bind a type assignment to itself.
  if (ta == value) {
    return true;
  }

  // Check template conditions
  if (!ta->target()->canBindTo(value)) {
    return false;
  }

  // Calculate the set of all provisions
  ProvisionSet combinedProvisions(provisions);
  combinedProvisions.insertIfValid(ta->primaryProvision());
  if (const TypeAssignment * taValue = dyn_cast<TypeAssignment>(value)) {
    combinedProvisions.insertIfValid(taValue->primaryProvision());
  }

  // Early out - already bound to this same value.
  for (ConstraintSet::const_iterator si = ta->begin(); si != ta->end(); ++si) {
    Constraint * cst = *si;
    if (cst->value()->isEqual(value) &&
        cst->kind() == kind &&
        cst->provisions().equals(combinedProvisions)) {
      return true;
    }

    DASSERT((*si)->value() != ta) << "Pattern " << ta << " bound to itself";
  }

  // Add a new constraint onto the type assignment.
  if (combinedProvisions.isConsistent()) {
    TypeAssignment * taMutable = const_cast<TypeAssignment *>(ta);
    taMutable->constraints().insert(
        source->location(), value, nextState(), kind, combinedProvisions);
    if (DebugUnify || unifyVerbose) {
      diag.debug() << "bind " << ta << " " << kind << " " << value;
      dumpProvisions(combinedProvisions);
    }

    // If the other side is also a type variable, add a reverse constraint to it.
    if (const TypeAssignment * taValue = dyn_cast<TypeAssignment>(value)) {
      const_cast<TypeAssignment *>(taValue)->constraints().insert(
          source->location(), ta, nextState(), Constraint::reverse(kind), combinedProvisions);
      if (DebugUnify || unifyVerbose) {
        diag.debug() << "bind " << value << " " << kind << " " << ta;
        dumpProvisions(combinedProvisions);
      }
    }
    // TODO: This doesn't accommodate the case where there might be type vars buried inside.
  }

  return true;
}

bool BindingEnv::unifyWithAmbiguousType(SourceContext * source, const Type * amb,
    const Type * value, Constraint::Kind kind, const ProvisionSet & provisions) {
  ProspectList prospects;
  AmbiguousType::listProspects(prospects, amb, provisions);
  bool success = false;
  for (ProspectList::const_iterator it = prospects.begin(), itEnd = prospects.end(); it != itEnd;
      ++it) {
    unsigned savedState = stateCount_;
    if (unify(source, it->type(), value, kind, it->provisions())) {
      success = true;
    } else {
      backtrack(savedState);
    }
  }
  return success;
}

TypeAssignment * BindingEnv::assign(const TypeVariable * target, const Type * value, GC * scope) {
  int sequenceNum = 1;
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next_) {
    DASSERT(ta->target() != target || ta->scope() != scope);
    if (ta->target()->name() == target->name()) {
      DASSERT(ta->scope() != scope);
      if (sequenceNum == 1) {
        sequenceNum = ta->sequenceNum_ + 1;
        break;
      }
    }
  }

  TypeAssignment * result = new TypeAssignment(target, scope);
  result->next_ = assignments_;
  DASSERT(result->next_ != result);
  assignments_ = result;
  result->sequenceNum_ = sequenceNum;

  if (DebugUnify || unifyVerbose) {
    diag.debug() << "Assign: " << result << " in " << *this;
  }
  return result;
}

void BindingEnv::backtrack(unsigned state) {
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
    while (!ta->constraints_.empty() && ta->constraints_.back()->stateCount() > state) {
      ta->constraints_.pop_back();
    }
  }

  stateCount_ = state;

  if (DebugUnify || unifyVerbose) {
    diag.debug() << "Backtracking to state: " << *this;
  }
}

const TypeAssignment * BindingEnv::getAssignment(const TypeVariable * var, const GC * context) const {
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
    if (ta->target() == var && ta->scope() == context) {
      return ta;
    }
  }

  return NULL;
}

void BindingEnv::sortAssignments() {
  // Do a post-order graph traversal.
  TypeAssignmentSet assignmentOrder;
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
    findTypeAssignments(assignmentOrder, ta);
  }

  // Now iterate in reverse order and re-insert
  assignments_ = NULL;
  TypeAssignmentSet::const_iterator it = assignmentOrder.end();
  while (it != assignmentOrder.begin()) {
    TypeAssignment * ta = *--it;
    ta->next_ = assignments_;
    assignments_ = ta;
  }
}

bool BindingEnv::updateAssignments(SourceLocation loc, GC * context) {
  if (!reconcileConstraints(context)) {
    diag.error(loc) << "Type inference cannot find a solution that satisfies all constraints.";
    diag.indent();
    dump();
    diag.unindent();
    reconcileConstraints(context); // For debugging - set breakpoint here
    return false;
  }

  // One final thing - transform the TAs values to get rid of references to other TAs.
  NormalizeTransform nxform;
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
    if (ta->value() != NULL &&
        ta->checkPrimaryProvision() &&
        (ta->scope() == context || context == NULL)) {
      ta->setValue(nxform(ta->value()));
    }
  }

  return true;
}

bool BindingEnv::reconcileConstraints(GC * context) {
  typedef llvm::SmallVector<TypeAssignment *, 32> TypeAssignmentList;
  TypeAssignmentList unsolved;

  // Reset all TAs having constraints and collect those that need solving.
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
    if (!ta->constraints().empty() &&
        ta->checkPrimaryProvision() &&
        (ta->scope() == context || context == NULL)) {
      ta->setValue(NULL);
      unsolved.push_back(ta);
    }
  }

  // Sort here?

  // Iteratively attempt to assign a value to every TA.
  unsigned prevUnsolvedCount = 0;
  unsigned unsolvedCount = unsigned(-1);
  do {
    prevUnsolvedCount = unsolvedCount;
    unsolvedCount = 0;
    for (TypeAssignmentList::const_iterator it = unsolved.begin(); it != unsolved.end(); ++it) {
      TypeAssignment * ta = *it;
      if (ta->value() == NULL) {
        ta->setValue(ta->findSingularSolution());
      }
      if (ta->value() == NULL) {
        ++unsolvedCount;
      }
    }
  } while (unsolvedCount > 0 && unsolvedCount != prevUnsolvedCount);

  return unsolvedCount == 0;
}

void BindingEnv::toTypeVarMap(TypeVarMap & map, GC * context) {
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
    if (ta->scope() == context && ta->value() != NULL) {
      DASSERT_OBJ(ta->value() != NULL, ta);
      map[ta->target()] = ta->value();
    }
  }
}

bool BindingEnv::isAssigned(const Type * ty) const {
  if (const TypeVariable * tv = dyn_cast<TypeVariable>(ty)) {
    for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
      if (ta->target() == tv) {
        return true;
      }
    }
  }

  return false;
}

void BindingEnv::dumpAssignment(TypeAssignment * ta) const {
  if (ta->constraints().empty()) {
    if (ta->value() != NULL) {
      diag.debug() << ta << " [" << ta->value() << "]";
    } else {
      diag.debug() << ta << " == {}";
    }
  } else if (ta->constraints().size() == 1) {
    Constraint * s = ta->constraints().front();
    if (ta->value() != NULL) {
      diag.debug() << ta << " " << s->kind() << " " << s->value() << " [" << ta->value() << "]";
    } else {
      diag.debug() << ta << " " << s->kind() << " " << s->value();
    }
    diag.indent();
    dumpProvisions(s->provisions());
    diag.unindent();
  } else {
    if (ta->value() != NULL) {
      diag.debug() << ta << " [" << ta->value() << "]:";
    } else {
      diag.debug() << ta << ":";
    }
    diag.indent();
    for (ConstraintSet::const_iterator si = ta->begin(); si != ta->end(); ++si) {
      Constraint * s = *si;
      diag.debug() << "" << s->kind() << " " << s->value();
      dumpProvisions(s->provisions());
    }
    diag.unindent();
  }
}

void BindingEnv::dumpProvisions(const ProvisionSet & provisions) const {
  diag.indent();
  for (ProvisionSet::const_iterator p = provisions.begin(); p != provisions.end(); ++p) {
    diag.debug() << "provided: " << *p;
  }
  diag.unindent();
}

void BindingEnv::dump() const {
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
    dumpAssignment(ta);
  }
}

void BindingEnv::trace() const {
  GC::safeMark(assignments_);
}

FormatStream & operator<<(FormatStream & out, const BindingEnv & env) {
  out << "{";
  for (TypeAssignment * ta = env.assignments(); ta != NULL; ta = ta->next()) {
    if (ta != env.assignments()) {
      out << ", ";
    }
    out << ta;
  }
  out << "}";
  return out;
}

} // namespace tart

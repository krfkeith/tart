/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/AmbiguousType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeFunction.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Type/TypeRelation.h"
#include "tart/Type/UnionType.h"

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

typedef llvm::SmallSetVector<TypeAssignment *, 16> TypeAssignmentSet;

namespace {

/** Special version of dealias that doesn't dereference type assignments. */
QualifiedType dereferenceAlias(QualifiedType type) {
  if (type.isa<TypeAlias>()) {
    type = type.as<TypeAlias>()->value();
    DASSERT(!type.isNull());
  }
  return type;
}

// Return all type assignments reachable from ty.
void findTypeAssignments(TypeAssignmentSet & result, QualifiedType ty) {
  if (Qualified<TypeAssignment> ta = ty.dyn_cast<TypeAssignment>()) {
    TypeAssignment * taMutable = const_cast<TypeAssignment *>(ta.unqualified());
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

bool BindingEnv::unify(SourceContext * source, QualifiedType left, QualifiedType right,
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
        diag.indent();
        diag.debug() << "Current bindings:";
        diag.indent();
        dump();
        diag.unindent();
        diag.unindent();
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

bool BindingEnv::unifyImpl(SourceContext * source, QualifiedType left, QualifiedType right,
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
  if (Qualified<TypeAssignment> ta = left.dyn_cast<TypeAssignment>()) {
    return unifyWithTypeVar(source, ta, right, kind, provisions);
  }

  // Type variable on right side
  if (Qualified<TypeAssignment> ta = right.dyn_cast<TypeAssignment>()) {
    return unifyWithTypeVar(source, ta, left, Constraint::reverse(kind), provisions);
  }

  // Type function on left side
  if (Qualified<TypeFunctionCall> tfc = left.dyn_cast<TypeFunctionCall>()) {
    return unifyWithTypeFunctionCall(source, tfc, right, kind, provisions);
  }

  // Type function on right side
  if (Qualified<TypeFunctionCall> tfc = right.dyn_cast<TypeFunctionCall>()) {
    return unifyWithTypeFunctionCall(source, tfc, left, Constraint::reverse(kind), provisions);
  }

  const Type * lty = left.unqualified();
  const Type * rty = right.unqualified();

  // Address type
  if (const AddressType * npp = dyn_cast<AddressType>(lty)) {
    return unifyAddressType(source, npp, rty);
  }

  // Native array type
  if (const NativeArrayType * nap = dyn_cast<NativeArrayType>(lty)) {
    return unifyNativeArrayType(source, nap, rty);
  }

  // Flexible array type
  if (const FlexibleArrayType * nap = dyn_cast<FlexibleArrayType>(lty)) {
    return unifyFlexibleArrayType(source, nap, rty);
  }

  // Tuple type
  if (const TupleType * tPattern = dyn_cast<TupleType>(lty)) {
    if (const TupleType * tValue = dyn_cast<TupleType>(rty)) {
      return unifyTupleType(source, tPattern, tValue);
    }

    return false;
  }

  // Type literal type
  if (const TypeLiteralType * npp = dyn_cast<TypeLiteralType>(lty)) {
    return unifyTypeLiteralType(source, npp, rty);
  }

  if (const CompositeType * ctPattern = dyn_cast<CompositeType>(lty)) {
    if (const CompositeType * ctValue = dyn_cast<CompositeType>(rty)) {
      return unifyCompositeType(source, ctPattern, ctValue);
    } else if (const NativeArrayType * natValue = dyn_cast<NativeArrayType>(rty)) {
      // Special case for assigning Array to NativeArray in initializers
      if (ctPattern->typeDefn()->ast() == Builtins::typeArray->typeDefn()->ast()) {
        return unify(source, ctPattern->typeParam(0), natValue->typeParam(0), Constraint::EXACT);
      }
//    } else if (const FunctionType * fnValue = dyn_cast<FunctionType>(value)) {
//      const CompositeType * fnPattern =
//      if (!fnValue->isStatic()) {
//      }
    }

    if (const UnionType * rut = dyn_cast<UnionType>(rty)) {
      return unifyUnionMemberType(source, rut, ctPattern);
    }

    // The type on the left may have a coercion method, so assume that unification
    // succeeds.
    if (right->isSingular()) {
      return true;
    }

    return false;
  }

  if (const UnionType * lut = dyn_cast<UnionType>(lty)) {
    if (const UnionType * rut = dyn_cast<UnionType>(rty)) {
      return unifyUnionType(source, lut, rut);
    } else if (unifyUnionMemberType(source, lut, right)) {
      return true;
    }

    return false;
  } else if (const UnionType * rut = dyn_cast<UnionType>(rty)) {
    if (unifyUnionMemberType(source, rut, left)) {
      return true;
    }
  }

  if (isa<PrimitiveType>(lty)) {
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
  if (TypeRelation::isEqual(left, right)) {
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
  if (TypeRelation::isEqual(left, right)) {
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

  QualifiedTypeSet membersLeft;
  for (TupleType::const_iterator it = left->members().begin(); it != left->members().end();
      ++it) {
    membersLeft.insert(dereferenceAlias(*it));
  }

  QualifiedTypeSet memberRight;
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
  SourceContext * source, const UnionType * left, QualifiedType right) {
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
  if (TypeRelation::isEqual(left, right)) {
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
    SourceContext * source, Qualified<TypeAssignment> ta, QualifiedType value,
    Constraint::Kind kind, const ProvisionSet & provisions) {

  // Dereference the value as well.
  value = TypeAssignment::deref(value);

  // Don't bind a type assignment to itself.
  if (ta.unqualified() == value.unqualified()) {
    return true;
  }

  // Check template conditions
  if (!ta->target()->canBindTo(value)) {
    return false;
  }

  // Figure out which qualifiers are different, remove ones which are common to both sides.
//  unsigned commonQualifiers = ta.qualifiers() & value.qualifiers();
//  ta.removeQualifiers(commonQualifiers);
//  value.removeQualifiers(commonQualifiers);

  // Calculate the set of all provisions
  ProvisionSet combinedProvisions(provisions);
  combinedProvisions.insertIfValid(ta->primaryProvision());
  if (const TypeAssignment * taValue = dyn_cast<TypeAssignment>(value.unqualified())) {
    combinedProvisions.insertIfValid(taValue->primaryProvision());
  }

  // Early out - already bound to this same value.
  for (ConstraintSet::const_iterator si = ta->begin(); si != ta->end(); ++si) {
    Constraint * cst = *si;
    if (TypeRelation::isEqual(cst->value(), value) &&
        cst->kind() == kind &&
        cst->provisions().equals(combinedProvisions)) {
      return true;
    }

    DASSERT(cst->value() != ta.as<Type>()) << "Pattern " << ta << " bound to itself";
  }

  // Add a new constraint onto the type assignment.
  if (combinedProvisions.isConsistent()) {
    ta->mutableConstraints().insert(
        source->location(), value, nextState(), kind, combinedProvisions);
    if (DebugUnify || unifyVerbose) {
      diag.debug() << "bind " << ta << " " << kind << " " << value;
      dumpProvisions(combinedProvisions);
    }

    // If the other side is also a type variable, add a reverse constraint to it.
    if (value.isa<TypeAssignment>()) {
      value.as<TypeAssignment>().unqualified()->mutableConstraints().insert(
          source->location(), ta.as<Type>(), nextState(), Constraint::reverse(kind),
          combinedProvisions);
      if (DebugUnify || unifyVerbose) {
        diag.debug() << "bind " << value << " " << kind << " " << ta;
        dumpProvisions(combinedProvisions);
      }
    }
    // TODO: This doesn't accommodate the case where there might be type vars buried inside.
  }

  return true;
}

bool BindingEnv::unifyWithTypeFunctionCall(
    SourceContext * source, Qualified<TypeFunctionCall> tfc, QualifiedType value,
    Constraint::Kind kind, const ProvisionSet & provisions) {

  value = TypeAssignment::deref(value);
  const TupleType * args = tfc->args();

  if (Qualified<TypeFunctionCall> vfc = value.dyn_cast<TypeFunctionCall>()) {
    return true;
//    diag.debug() << tfc << " <=> " << vfc;
//    DFAIL("Implement");
  }

  // Unify with a type qualifier
  if (const TypeAssignment * ta = dyn_cast<TypeAssignment>(tfc->fnVal())) {
    if (ta->target()->target() == TypeVariable::TYPE_QUALIFIER && args->size() == 1) {
      QualifiedType arg = args->member(0);
      if (unify(source, arg.unqualified(), value.unqualified(), kind, provisions)) {
        // Bind the type variable for the callable to a qualifying function.
        TypeFunction * fnVal = new QualifyingTypeFunction(value.qualifiers());
        ta->mutableConstraints().insert(source->location(), fnVal, nextState(),
            kind, provisions);
        return true;
      }
      return false;
    } else {
      // The variable isn't a qualifier.
      diag.debug() << "NumArgs: " << args->size();
      diag.debug() << "Target type: " << ta->target()->target();
      DFAIL("Implement");
    }
  } else {
    // The function value isn't a type variable.
    DFAIL("Implement");
  }

  return false;
}

bool BindingEnv::unifyWithAmbiguousType(SourceContext * source, QualifiedType amb,
    QualifiedType value, Constraint::Kind kind, const ProvisionSet & provisions) {
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

TypeAssignment * BindingEnv::assign(const TypeVariable * target, GC * scope) {
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

void BindingEnv::setAssignmentBounds(QualifiedTypeVarMap & assignments) {
  // We need a relabeling transform to convert the bounding types (which may be type
  // variables) to the corresponding relabeled types.
  RelabelTransform relabel(assignments);

  // Convert all upper and lower bounds on the type variable into constraints on the assignment.
  bool first = true;
  for (QualifiedTypeVarMap::const_iterator it = assignments.begin(), itEnd = assignments.end();
      it != itEnd; ++it) {
    Qualified<TypeVariable> tv = it->first;
    Qualified<TypeAssignment> ta = it->second.as<TypeAssignment>();
    const QualifiedTypeList & upper = tv->upperBounds();
    const QualifiedTypeList & lower = tv->lowerBounds();

    if (!upper.empty() || !lower.empty()) {
      if (DebugUnify || unifyVerbose) {
        if (first) {
          diag.debug() << "Computing type assignment bounds:";
          first = false;
        }
        diag.indent();
        diag.debug() << "For variable: " << tv;
        diag.indent();
      }

      ProvisionSet provisions;
      provisions.insertIfValid(ta->primaryProvision());

      // Upper bounds
      for (QualifiedTypeList::const_iterator ui = upper.begin(); ui != upper.end(); ++ui) {
        QualifiedType ty = relabel(*ui);
        if (DebugUnify || unifyVerbose) {
          diag.debug() << "<= " << ty << " [" << *ui << "]";
        }
        ta->mutableConstraints().insert(tv->location(), ty, Constraint::UPPER_BOUND, provisions);
      }

      // Lower bounds
      for (QualifiedTypeList::const_iterator li = lower.begin(); li != lower.end(); ++li) {
        QualifiedType ty = relabel(*li);
        if (DebugUnify || unifyVerbose) {
          diag.debug() << ">= " << ty << " [" << *li << "]";
        }
        ta->mutableConstraints().insert(tv->location(), ty, Constraint::LOWER_BOUND, provisions);
      }

      if (DebugUnify || unifyVerbose) {
        diag.unindent();
        diag.unindent();
      }
    }
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
    if (ta->value() &&
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

  // Iteratively attempt to assign a value to every TA.
  unsigned prevUnsolvedCount = 0;
  unsigned unsolvedCount = unsigned(-1);
  do {
    prevUnsolvedCount = unsolvedCount;
    unsolvedCount = 0;
    for (TypeAssignmentList::const_iterator it = unsolved.begin(); it != unsolved.end(); ++it) {
      TypeAssignment * ta = *it;
      if (!ta->value()) {
        ta->setValue(ta->findSingularSolution());
      }
      if (!ta->value()) {
        ++unsolvedCount;
      }
    }
  } while (unsolvedCount > 0 && unsolvedCount != prevUnsolvedCount);

  return unsolvedCount == 0;
}

void BindingEnv::toTypeVarMap(QualifiedTypeVarMap & map, GC * context) {
  for (TypeAssignment * ta = assignments_; ta != NULL; ta = ta->next()) {
    if (ta->scope() == context && ta->value()) {
      map[ta->target()] = ta->value();
    }
  }
}

bool BindingEnv::isAssigned(QualifiedType ty) const {
  if (const TypeVariable * tv = dyn_cast<TypeVariable>(ty.unqualified())) {
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
    if (ta->value()) {
      diag.debug() << ta << " [" << ta->value() << "]";
    } else {
      diag.debug() << ta << " == {}";
    }
  } else if (ta->constraints().size() == 1) {
    Constraint * s = ta->constraints().front();
    if (ta->value()) {
      diag.debug() << ta << " " << s->kind() << " " << s->value() << " [" << ta->value() << "]";
    } else {
      diag.debug() << ta << " " << s->kind() << " " << s->value();
    }
    diag.indent();
    dumpProvisions(s->provisions());
    diag.unindent();
  } else {
    if (ta->value()) {
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

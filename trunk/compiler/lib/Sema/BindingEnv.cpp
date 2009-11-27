/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/NativeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/TypeTransform.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"
#include "llvm/Support/CommandLine.h"

static llvm::cl::opt<bool>
DebugUnify("debug-unify", llvm::cl::desc("Debug unification"), llvm::cl::init(false));

namespace tart {

static void assureNoPatternVars(Type * t) {
  for (int i = 0; i < t->numTypeParams(); ++i) {
    if (isa<PatternVar>(t->typeParam(i))) {
      diag.fatal() << "What's a type param doing here?" << t;
      DFAIL("Unexpected pattern var");
    }
  }

  if (CompositeType * ctype = dyn_cast<CompositeType>(t)) {
    for (ClassList::iterator it = ctype->bases().begin(); it != ctype->bases().end(); ++it) {
      assureNoPatternVars(*it);
    }
  }
}

// -------------------------------------------------------------------
// TypeBinding

void Substitution::trace() const {
  safeMark(left_);
  safeMark(right_);
  safeMark(prev_);
}

// -------------------------------------------------------------------
// PatternValue

bool PatternValue::isSingular() const {
  if (const Type * val = value()) {
    return val->isSingular();
  }

  return false;
}

bool PatternValue::isEqual(const Type * other) const {
  if (Type * val = value()) {
    return val->isEqual(other);
  }

  return false;
}

bool PatternValue::isReferenceType() const {
  if (Type * val = value()) {
    return val->isReferenceType();
  }

  return false;
}

bool PatternValue::isSubtype(const Type * other) const {
  if (Type * val = value()) {
    return val->isSubtype(other);
  }

  return false;
}

bool PatternValue::includes(const Type * other) const {
  return isEqual(other);

  /*if (Type * val = value()) {
    return val->includes(other);
  }

  return false;*/
}

ConversionRank PatternValue::convertImpl(const Conversion & conversion) const {
  if (Type * val = value()) {
    return val->convert(conversion);
  }

  return Incompatible;
}

Expr * PatternValue::nullInitValue() const {
  DFAIL("IllegalState");
}

void PatternValue::trace() const {
  var_->mark();
}

void PatternValue::format(FormatStream & out) const {
  out << "%%" << var_->name();
  if (Type * val = value()) {
    out << "=" << val;
  }
}

const llvm::Type * PatternValue::irType() const {
  DFAIL("IllegalState");
}

Type * PatternValue::value() const {
  Type * result = env_->get(var_);
  return (result == this) ? NULL : result;
}

// -------------------------------------------------------------------
// BindingEnv

const char * BindingEnv::str() const {
  static std::string temp;
  std::stringstream ss;
  FormatStream stream(ss);
  stream.setFormatOptions(Format_Verbose);
  stream << *this;
  temp = ss.str();
  return temp.c_str();
}

void BindingEnv::reset() {
  substitutions_ = NULL;
}

bool BindingEnv::unify(SourceContext * source, const Type * pattern, const Type * value,
    Variance variance) {
  // Dealias but don't depattern pattern
  while (pattern->typeClass() == Type::Alias) {
    if (const TypeAlias * alias = dyn_cast<TypeAlias>(pattern)) {
      pattern = alias->value().type();
      DASSERT_OBJ(pattern != NULL, alias);
    } else {
      break;
    }
  }

  //pattern = dealias(pattern);
  value = dealias(value);

  if (DebugUnify) {
    diag.debug(source) << "Unify? " << pattern << " == " << value << " with " << *this;
    diag.indent();
  }

  bool result = unifyImpl(source, pattern, value, variance);

  if (DebugUnify) {
    if (!result) {
      diag.debug(source) << "Unify: " << pattern << " != " << value << " with " << *this;;
    } else {
      diag.debug(source) << "Unify: " << pattern << " == " << value << " with " << *this;;
    }

    diag.unindent();
  }

  return result;
}

bool BindingEnv::unifyImpl(SourceContext * source, const Type * pattern, const Type * value,
    Variance variance) {
  if (pattern == value) {
    return true;
  } else if (isErrorResult(pattern)) {
    return false;
  } else if (const PatternVar * pv = dyn_cast<PatternVar>(pattern)) {
    return unifyPattern(source, pv, value, variance);
  } else if (const PatternValue * pval = dyn_cast<PatternValue>(pattern)) {
    if (pval->env() == this) {
      return unifyPattern(source, pval->var(), value, variance);
    } else if (pval->value() != NULL) {
      return unifyImpl(source, pval->value(), value, variance);
    } else {
      addSubstitution(pattern, value);
      diag.debug(source) << "Unify error: " << pval << " : " << value;
      DFAIL("Why is there a pattern val on the lhs?");
      //return false;
      return true;
    }
  } else if (const PointerType * npp = dyn_cast<PointerType>(pattern)) {
    return unifyPointerType(source, npp, value);
  } else if (const AddressType * npp = dyn_cast<AddressType>(pattern)) {
    return unifyAddressType(source, npp, value);
  } else if (const NativeArrayType * nap = dyn_cast<NativeArrayType>(pattern)) {
    return unifyNativeArrayType(source, nap, value);
  } else if (const PatternVar * pv = dyn_cast<PatternVar>(value)) {
    diag.debug(source) << "Unify error: " << pattern << " : " << pv;
    DFAIL("Should not be unifying with a value that is a pattern var.");
  } else if (const TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pattern);
  } else if (const PatternValue * pval = dyn_cast<PatternValue>(value)) {
    Type * boundValue = pval->value();
    if (boundValue != NULL) {
      return unify(source, pattern, boundValue, variance);
    }

    diag.debug(source) << "Unbound pattern value found in value " << value <<
        " for pattern " << pattern;
    addSubstitution(pval, value);
    return true;
  } else if (const CompositeType * ctPattern = dyn_cast<CompositeType>(pattern)) {
    if (const CompositeType * ctValue = dyn_cast<CompositeType>(value)) {
      return unifyCompositeType(source, ctPattern, ctValue, variance);
    }

    return false;
  } else if (const PrimitiveType * pval = dyn_cast<PrimitiveType>(pattern)) {
    // Go ahead and unify - type inference will see if it can convert.
    return true;
  } else {
    //diag.error() << Format_Dealias << "Implement unification of " << pattern << " and " << value;
    return false;
    //DFAIL("Implement");
  }
}

bool BindingEnv::unifyPointerType(
    SourceContext * source, const PointerType * pat, const Type * value) {
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (const PointerType * npv = dyn_cast<PointerType>(value)) {
    if (!AnalyzerBase::analyzeType(npv, Task_PrepTypeComparison)) {
      return false;
    }

    return unify(source, pat->typeParam(0), npv->typeParam(0), Invariant);
  } else if (const TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pat);
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

    return unify(source, pat->typeParam(0), npv->typeParam(0), Invariant);
  } else if (const TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pat);
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

    return unify(source, pat->typeParam(0), nav->typeParam(0), Invariant);
  } else if (const TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pat);
  } else {
    return false;
  }
}

bool BindingEnv::unifyCompositeType(
    SourceContext * source, const CompositeType * pattern, const CompositeType * value,
    Variance variance) {
  if (pattern->isEqual(value)) {
    return true;
  }

  if (!AnalyzerBase::analyzeType(pattern, Task_PrepTypeComparison)) {
    return false;
  }

  if (!AnalyzerBase::analyzeType(value, Task_PrepTypeComparison)) {
    return false;
  }

  TypeDefn * patternDefn = pattern->typeDefn();
  TypeDefn * valueDefn = value->typeDefn();

  // Compare the ASTs to see if they derive from the same original symbol.
  if (patternDefn->ast() == valueDefn->ast()) {
    // Now we have to see if we can bind the type variables.
    const TupleType * patternTypeParams = NULL;
    const TupleType * valueTypeParams = NULL;

    if (patternDefn->isTemplate()) {
      patternTypeParams = patternDefn->templateSignature()->typeParams();
    } else if (patternDefn->isTemplateInstance()) {
      patternTypeParams = patternDefn->templateInstance()->typeArgs();
    }

    if (valueDefn->isTemplate()) {
      valueTypeParams = valueDefn->templateSignature()->typeParams();
    } else if (valueDefn->isTemplateInstance()) {
      valueTypeParams = valueDefn->templateInstance()->typeArgs();
    }

    if (patternTypeParams == valueTypeParams) {
      return true;
    }

    if (patternTypeParams == NULL ||
        valueTypeParams == NULL ||
        patternTypeParams->size() != valueTypeParams->size()) {
      return false;
    }

    size_t numParams = patternTypeParams->size();
    Substitution * savedState = substitutions();
    for (size_t i = 0; i < numParams; ++i) {
      if (!unify(source, (*patternTypeParams)[i], (*valueTypeParams)[i], Invariant)) {
        setSubstitutions(savedState);
        return false;
      }
    }

    return true;
  }

  if (variance == Contravariant) {
    Substitution * savedState = substitutions();
    for (ClassList::const_iterator it = value->bases().begin(); it != value->bases().end(); ++it) {
      if (unifyCompositeType(source, pattern, *it, Invariant)) {
        return true;
      }

      setSubstitutions(savedState);
    }

    return false;
  } else if (variance == Covariant) {
    Substitution * savedState = substitutions();
    for (ClassList::const_iterator it = pattern->bases().begin(); it != pattern->bases().end(); ++it) {
      if (unifyCompositeType(source, *it, value, Invariant)) {
        return true;
      }

      setSubstitutions(savedState);
    }

    return false;
  } else {
    return false;
  }
}

bool BindingEnv::unifyPattern(
    SourceContext * source, const PatternVar * pattern, const Type * value, Variance variance) {

  if (pattern == value) {
    // Don't bind a pattern to itself.
    return true;
  }

  /*if (PatternVar * pvar = dyn_cast<PatternVar>(value)) {
    diag.debug() << pattern->templateDefn()->name() << pattern << " <- " << pvar->templateDefn()->name() << pvar << " in " << *this;
    //DFAIL("Abort");
  }*/

  // If there is already a value bound to this pattern variable
  Substitution * s = getSubstitutionFor(pattern);
  if (s != NULL) {
    // Early out - already bound to this same value.
    if (s->right() == value) {
      return true;
    }

    // Early out
    if (s->right() == pattern) {
      return true;
    }

    if (const PatternValue * pval = dyn_cast<PatternValue>(s->right())) {
      // If the value that is already bound is a pattern variable from some other
      // environment (it should never be from this one), then bind it to
      // this variable.
      // TODO: Might want to check if the value is bindable to this var.
      if (pval->value() == value /*|| pval->var() == pattern && pval->env() == this*/) {
        return true;
      }

      addSubstitution(pattern, value);
      return true;
    }

    if (isa<PatternValue>(value)) {
      // If the value that we're trying to bind is a pattern value, and we already
      // have a value, then leave the current value as is, assuming that it can
      // be bound to that pattern variable later.
      return true;
    }

    if (!pattern->canBindTo(value)) {
      return false;
    }

    if (const PatternVar * svar = dyn_cast<PatternVar>(s->right())) {
      //DFAIL("Should not happen");
      // If 'pattern' is bound to another pattern variable, then go ahead and override
      // that binding.
      if (svar != value && svar->canBindTo(value)) {
        addSubstitution(svar, value);
        addSubstitution(pattern, value);
      }

      return true;
    }

    const Type * upperBound = value;
    const Type * lowerBound = value;

    if (value->isEqual(&UnsizedIntType::instance)) {
      //upperBound =
    }

    const Type * commonType = Type::selectLessSpecificType(s->right(), value);
    if (commonType == s->right()) {
      return true;
    }

    /*if (variance == Invariant) {
      diag.info() << "Invariant " << pattern;
      diag.info() << "Pattern = " << pattern;
      diag.info() << "Substitution = " << s->left() << " -> " << s->right();
      diag.info() << "Value = " << value;
      diag.info() << "NewValue = " << newValue;
      diag.printContextStack(source);
    }*/

    /*switch (variance) {
      case Invariant:
        newValue = value;
        return unify(source, s->right(), value, variance);

      case Covariant:
        //newValue = Type::selectLessSpecificType(s->right(), value);
        break;

      case Contravariant:
        break;
    }*/

    // Override the old substitution with a new one.
    if (commonType != NULL) {
      if (commonType == value) {
        // No need to rebind if same as before.
        //diag.debug() << "Skipping rebind of " << pattern << " <- " << commonType << " because the new value is " << value << " and the old value is " << s->right() << " with variance " << variance;
        return true;
      }

      //diag.debug() << "Adding substitution of " << pattern << " <- " << commonType << " because the new value is " << value << " and the old value is " << s->right() << " with variance " << variance;
      addSubstitution(pattern, commonType);
      return true;
    }

    return false;
  } else if (pattern->canBindTo(value)) {
    // Don't bother binding a pattern value to its own variable.
    if (const PatternValue * pval = dyn_cast<PatternValue>(value)) {
      if (pval->var() == pattern) {
        return true;
      }
    }

    if (DebugUnify) {
      diag.debug(source->location()) << "Unify: " << pattern << " <- " << value << " in environment " << *this;
    }

    // Add a new substitution of value for pattern
    addSubstitution(pattern, value);
    return true;
  } else {
    return false;
  }
}

Substitution * BindingEnv::addSubstitution(const Type * left, const Type * right) {
  for (Substitution * s = substitutions_; s != NULL; s = s->prev()) {
    if (s->left() == left && s->right() == right) {
      diag.error() << "Redundant substitution: " << s->left() << " -> " << s->right();
      DFAIL("BadState");
    }
  }

  DASSERT(left != right);
  substitutions_ = new Substitution(left, right, substitutions_);
  return substitutions_;
}

Substitution * BindingEnv::addSubstitution(
    const Type * left, const Type * upper, const Type * lower) {
  DASSERT(left != upper);
  DASSERT(left != lower);
  substitutions_ = new Substitution(left, upper, lower, substitutions_);
  return substitutions_;
}

Type * BindingEnv::get(const PatternVar * type) const {
  Substitution * s = getSubstitutionFor(type);
  if (s != NULL) {
    return const_cast<Type *>(s->right());
  }

  return NULL;
}

Type * BindingEnv::dereference(Type * type) const {
  while (type != NULL) {
    if (PatternVar * var = dyn_cast<PatternVar>(type)) {
      Substitution * s = getSubstitutionFor(type);
      if (s != NULL) {
        type = const_cast<Type *>(s->right());
      } else {
        return NULL;
      }
    } else if (PatternValue * val = dyn_cast<PatternValue>(type)) {
      type = val->value();
      if (type == NULL) {
        return val;
      }
    } else {
      break;
    }
  }

  return type;
}

const Type * BindingEnv::subst(const Type * in) const {
  if (substitutions_ == NULL || isErrorResult(in)) {
    return in;
  }

  return SubstitutionTransform(*this).transform(in);
}

/*const Type * BindingEnv::relabel(const Type * in) {
  return RelabelTransform(*this).transform(in);
}*/

void BindingEnv::trace() const {
  GC::safeMark(substitutions_);
}

FormatStream & operator<<(FormatStream & out, const BindingEnv & env) {
  out << "{";
  for (Substitution * s = env.substitutions(); s != NULL; s = s->prev()) {
    if (s != env.substitutions()) {
      out << ", ";
    }

    out << s->left() << ":" << s->right();
  }

  out << "}";
  return out;
}

} // namespace tart

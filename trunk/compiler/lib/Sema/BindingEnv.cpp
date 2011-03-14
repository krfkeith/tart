/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/NativeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Defn/Template.h"

#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/TypeTransform.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/CommandLine.h"

static llvm::cl::opt<bool>
DebugUnify("debug-unify", llvm::cl::desc("Debug unification"), llvm::cl::init(false));

namespace tart {

extern bool unifyVerbose;

static void assureNoTypeVars(Type * t) {
  for (size_t i = 0; i < t->numTypeParams(); ++i) {
    if (isa<TypeVariable>(t->typeParam(i))) {
      diag.fatal() << "What's a type param doing here?" << t;
      DFAIL("Unexpected type variable");
    }
  }

  if (CompositeType * ctype = dyn_cast<CompositeType>(t)) {
    for (ClassList::iterator it = ctype->bases().begin(); it != ctype->bases().end(); ++it) {
      assureNoTypeVars(*it);
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
// TypeBinding

bool TypeBinding::isSingular() const {
  if (const Type * val = value()) {
    return val->isSingular();
  }

  return false;
}

bool TypeBinding::isEqual(const Type * other) const {
  if (this == other) {
    return true;
  }
  if (Type * val = value()) {
    return val->isEqual(other);
  }

  return false;
}

bool TypeBinding::isReferenceType() const {
  if (Type * val = value()) {
    return val->isReferenceType();
  }

  return false;
}

bool TypeBinding::isSubtype(const Type * other) const {
  if (Type * val = value()) {
    return val->isSubtype(other);
  }

  return false;
}

bool TypeBinding::includes(const Type * other) const {
  return isEqual(other);

  /*if (Type * val = value()) {
    return val->includes(other);
  }

  return false;*/
}

ConversionRank TypeBinding::convertImpl(const Conversion & conversion) const {
  if (Type * val = value()) {
    return val->convert(conversion);
  }

  return Incompatible;
}

Expr * TypeBinding::nullInitValue() const {
  DFAIL("IllegalState");
}

void TypeBinding::trace() const {
  var_->mark();
}

void TypeBinding::format(FormatStream & out) const {
  var_->format(out);
  out << "." << env_->index_;
  if (Type * val = value()) {
    out << "=" << val;
  }
}

const llvm::Type * TypeBinding::irType() const {
  DFAIL("IllegalState");
}

Type * TypeBinding::value() const {
  //Type * result = env_->get(this);
  Type * result = env_->get(var_);
  return (result == this) ? NULL : result;
}

// -------------------------------------------------------------------
// BindingEnv

int BindingEnv::nextIndex_ = 1;

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
  if (pattern == &BadType::instance) {
    return false;
  }

  // Dealias but don't depattern pattern
  while (pattern->typeClass() == Type::Alias) {
    if (const TypeAlias * alias = dyn_cast<TypeAlias>(pattern)) {
      pattern = alias->value();
      DASSERT_OBJ(pattern != NULL, alias);
    } else {
      break;
    }
  }

  //pattern = dealias(pattern);
  value = dealias(value);

  if (DebugUnify || unifyVerbose) {
    diag.debug(source) << "Unify? " << pattern << " == " << value << " with " << *this;
    diag.indent();
  }

  bool result = unifyImpl(source, pattern, value, variance);

  if (DebugUnify || unifyVerbose) {
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
  } else if (const TypeVariable * pv = dyn_cast<TypeVariable>(pattern)) {
    return unifyPattern(source, pv, value, variance);
  } else if (const TypeBinding * pval = dyn_cast<TypeBinding>(pattern)) {
    if (pval->env() == this) {
      return unifyPattern(source, pval->var(), value, variance);
    } else if (pval->value() != NULL) {
      return unifyImpl(source, pval->value(), value, variance);
    } else {
      //addSubstitution(pattern, value);
      //diag.debug(source) << "Unify error: " << pval << " : " << value;
      //DFAIL("Why is there a pattern val on the lhs?");
      //return false;
      return true;
    }
  } else if (const AddressType * npp = dyn_cast<AddressType>(pattern)) {
    return unifyAddressType(source, npp, value);
  } else if (const NativeArrayType * nap = dyn_cast<NativeArrayType>(pattern)) {
    return unifyNativeArrayType(source, nap, value);
  } else if (const FlexibleArrayType * nap = dyn_cast<FlexibleArrayType>(pattern)) {
    return unifyFlexibleArrayType(source, nap, value);
  } else if (const TypeLiteralType * npp = dyn_cast<TypeLiteralType>(pattern)) {
    return unifyTypeLiteralType(source, npp, value);
  } else if (const TypeVariable * pv = dyn_cast<TypeVariable>(value)) {
    diag.debug(source) << "Unify error: " << pattern << " : " << pv;
    DFAIL("Should not be unifying with a value that is a pattern var.");
  } else if (const TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    if (pattern->isSingular()) {
      // Let conversion pass take care of it.
      return true;
    }
    return tc->unifyWithPattern(*this, pattern);
  } else if (const TypeBinding * pval = dyn_cast<TypeBinding>(value)) {
    Type * boundValue = pval->value();
    if (boundValue != NULL) {
      return unify(source, pattern, boundValue, variance);
    }

    return true;
//    diag.error(source) << "Unbound pattern value found in value " << value <<
//        " for pattern " << pattern;
//    //addSubstitution(pval, value);
//    return true;
  } else if (const CompositeType * ctPattern = dyn_cast<CompositeType>(pattern)) {
    if (const CompositeType * ctValue = dyn_cast<CompositeType>(value)) {
      return unifyCompositeType(source, ctPattern, ctValue, variance);
    } else if (const NativeArrayType * natValue = dyn_cast<NativeArrayType>(value)) {
      // Special case for assigning Array to NativeArray in initializers
      if (ctPattern->typeDefn()->ast() == Builtins::typeArray->typeDefn()->ast()) {
        return unify(source, ctPattern->typeParam(0), natValue->typeParam(0), Invariant);
      }
    }

    if (variance == Covariant) {
      if (const UnionType * utValue = dyn_cast<UnionType>(value)) {
        return unifyToUnionType(source, ctPattern, utValue, variance);
      }
    }

    return false;
  } else if (const UnionType * uPattern = dyn_cast<UnionType>(pattern)) {
    if (const UnionType * uValue = dyn_cast<UnionType>(value)) {
      return unifyUnionType(source, uPattern, uValue, variance);
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
    if (const TypeBinding * elemVar = dyn_cast<TypeBinding>(pat->typeParam(0))) {
      return unify(source, elemVar, new SingleTypeParamOfConstraint(tc, Type::NAddress), Invariant);
    }

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

bool BindingEnv::unifyFlexibleArrayType(SourceContext * source, const FlexibleArrayType * pat,
    const Type * value) {
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (const FlexibleArrayType * fav = dyn_cast<FlexibleArrayType>(value)) {
    if (!AnalyzerBase::analyzeType(fav, Task_PrepTypeComparison)) {
      return false;
    }

    return unify(source, pat->typeParam(0), fav->typeParam(0), Invariant);
  } else if (const TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pat);
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

    return unify(source, pat->typeParam(0), npv->typeParam(0), Invariant);
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

  // See if we can find a match in the superclasses.
  // This may produce a false positive, which will be caught later.

  Substitution * savedState = substitutions();
  for (ClassList::const_iterator it = value->bases().begin(); it != value->bases().end(); ++it) {
    if (unifyCompositeType(source, pattern, *it, Invariant)) {
      return true;
    }

    setSubstitutions(savedState);
  }

  for (ClassList::const_iterator it = pattern->bases().begin(); it != pattern->bases().end(); ++it) {
    if (unifyCompositeType(source, *it, value, Invariant)) {
      return true;
    }

    setSubstitutions(savedState);
  }

  return false;
}

bool BindingEnv::unifyUnionType(
    SourceContext * source, const UnionType * pattern, const UnionType * value,
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

  if (pattern->members().size() != value->members().size()) {
    return false;
  }

  typedef llvm::DenseSet<const Type *, Type::KeyInfo> TypeSet;
  TypeSet patternTypes;
  for (TupleType::const_iterator it = pattern->members().begin(); it != pattern->members().end();
      ++it) {
    patternTypes.insert(dealias(*it));
  }

  TypeSet valueTypes;
  for (TupleType::const_iterator it = value->members().begin(); it != value->members().end();
      ++it) {
    valueTypes.insert(dealias(*it));
    patternTypes.erase(dealias(*it));
  }

  for (TupleType::const_iterator it = pattern->members().begin(); it != pattern->members().end();
      ++it) {
    valueTypes.erase(dealias(*it));
  }

  if (valueTypes.size() != patternTypes.size()) {
    return false;
  }

  if (valueTypes.size() == 0) {
    return true;
  }

  if (valueTypes.size() > 1) {
    DFAIL("Implement more than one pattern var per union");
  }

  return unify(source, *patternTypes.begin(), *valueTypes.begin(), Invariant);
}

bool BindingEnv::unifyToUnionType(
    SourceContext * source, const Type * pattern, const UnionType * value,
    Variance variance) {
  // If we're assigning to a union type, try each of the union members.
  Substitution * savedState = substitutions();
  for (TupleType::const_iterator it = value->members().begin(); it != value->members().end();
      ++it) {
    if (unify(source, pattern, *it, variance)) {
      return true;
    }

    setSubstitutions(savedState);
  }

  return false;
}

bool BindingEnv::unifyPattern(
    SourceContext * source, const TypeVariable * pattern, const Type * value, Variance variance) {

  if (pattern == value) {
    // Don't bind a pattern to itself.
    return true;
  }

  /*if (TypeVariable * pvar = dyn_cast<TypeVariable>(value)) {
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
      DFAIL("Pattern bound to itself?");
      return true;
    }

    if (const TypeBinding * pval = dyn_cast<TypeBinding>(s->right())) {
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

    if (isa<TypeBinding>(value)) {
      // If the value that we're trying to bind is a pattern value, and we already
      // have a value, then leave the current value as is, assuming that it can
      // be bound to that pattern variable later.
      return true;
    }

    if (!pattern->canBindTo(value)) {
      return false;
    }

    if (const TypeVariable * svar = dyn_cast<TypeVariable>(s->right())) {
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

    if (value->isUnsizedIntType()) {
      //upperBound =
    }

    if (!s->right()->includes(value) && !value->includes(s->right())) {
      if (unifyWithBoundValue(source, s->right(), value, Invariant)) {
        return true;
      }

      // Hack: For now, don't even try to unify a constraint with another constraint.
      // TODO: Figure out how this really ought to work, but in the mean time
      // this makes the unit tests compile and run.
      if (isa<TypeConstraint>(s->right()) && isa<TypeConstraint>(value)) {
        return true;
      }
    }

    const Type * commonType = selectLessSpecificType(source, s->right(), value);
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
        //newValue = Type::selectLesSpecificType(s->right(), value);
        break;

      case Contravariant:
        break;
    }*/

    // Override the old substitution with a new one.
    if (commonType != NULL) {
      if (commonType == s->right()) {
        return true;
      }

      if (commonType != s->right() && commonType->isSingular()) {
        if (unifyVerbose) {
          diag.debug() << "Adding substitution of " << pattern << " <- " << commonType <<
              " because the new value is " << value <<
              " and the old value is " << s->right() <<
              " with variance " << variance;
        }

        addSubstitution(pattern, commonType);
      } else {
        // No need to rebind if same as before.
        if (unifyVerbose) {
          diag.debug() << "Skipping rebind of " << pattern << " <- " << commonType <<
              " because the new value is " << value <<
              " and the old value is " << s->right() <<
              " with variance " << variance;
        }
      }

      return true;
    }

    return false;
  } else if (pattern->canBindTo(value)) {
    // Don't bother binding a pattern value to its own variable.
    if (const TypeBinding * pval = dyn_cast<TypeBinding>(value)) {
      if (pval->var() == pattern) {
        return true;
      }
    }

    if (DebugUnify || unifyVerbose) {
      diag.debug(source->location()) << "Unify: " << pattern << " <- " << value << " in environment " << *this;
    }

    // Add a new substitution of value for pattern
    addSubstitution(pattern, value);
    return true;
  } else {
    return false;
  }
}

bool BindingEnv::unifyWithBoundValue(
    SourceContext * source, const Type * prevValue, const Type * newValue, Variance variance) {
  if (unifyVerbose) {
    diag.debug() << "Attempting to reconcile existing substitution: ";
    diag.debug() << "   " << prevValue;
    diag.debug() << "with new value: ";
    diag.debug() << "   " << newValue;
  }

  if (const TypeConstraint * tc = dyn_cast<TypeConstraint>(prevValue)) {
    // For now, unification of a previous constraint with a new value always succeeds.
    // This will get further constrained later when we rank conversions.
    return true;
  } else {
    return unify(source, prevValue, newValue, variance);
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

Type * BindingEnv::get(const TypeVariable * type) const {
  Substitution * s = getSubstitutionFor(type);
  if (s != NULL) {
    return const_cast<Type *>(s->right());
  }

  return NULL;
}

Type * BindingEnv::get(const TypeBinding * type) const {
  Substitution * s = getSubstitutionFor(type);
  if (s != NULL) {
    return const_cast<Type *>(s->right());
  }

  return NULL;
}

Type * BindingEnv::dereference(Type * type) const {
  while (type != NULL) {
    if (TypeVariable * var = dyn_cast<TypeVariable>(type)) {
      Substitution * s = getSubstitutionFor(type);
      if (s != NULL) {
        type = const_cast<Type *>(s->right());
      } else {
        return NULL;
      }
    } else if (TypeBinding * val = dyn_cast<TypeBinding>(type)) {
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


const Type * BindingEnv::selectLessSpecificType(SourceContext * source, const Type * type1,
    const Type * type2) {
  if (type2->includes(type1)) {
    return type2;
  } else if (type1->includes(type2)) {
    return type1;
  } else {
    const Type * t1 = PrimitiveType::derefEnumType(type1);
    const Type * t2 = PrimitiveType::derefEnumType(type2);
    if (t1->typeClass() == Type::Primitive && t2->typeClass() == Type::Primitive) {
      const PrimitiveType * p1 = static_cast<const PrimitiveType *>(t1);
      const PrimitiveType * p2 = static_cast<const PrimitiveType *>(t2);

      if (isIntegerTypeId(p1->typeId()) && isIntegerTypeId(p2->typeId())) {
        bool isSignedResult = isSignedIntegerTypeId(p1->typeId())
            || isSignedIntegerTypeId(p2->typeId());
        int type1Bits = p1->numBits() + (isSignedResult && isUnsignedIntegerTypeId(p1->typeId()) ? 1 : 0);
        int type2Bits = p2->numBits() + (isSignedResult && isUnsignedIntegerTypeId(p2->typeId()) ? 1 : 0);
        int resultBits = std::max(type1Bits, type2Bits);

        if (isSignedResult) {
          if (resultBits <= 8) {
            return &Int8Type::instance;
          } else if (resultBits <= 16) {
            return &Int16Type::instance;
          } else if (resultBits <= 32) {
            return &Int32Type::instance;
          } else if (resultBits <= 64) {
            return &Int64Type::instance;
          }

          diag.error(source) << "Integer value requires " << resultBits << " bits, too large.";
          diag.info() << "p1 = " << p1;
          diag.info() << "p2 = " << p2;
          //DFAIL("Integer value too large to be represented as native type.");
        } else {
          if (resultBits <= 8) {
            return &UInt8Type::instance;
          } else if (resultBits <= 16) {
            return &UInt16Type::instance;
          } else if (resultBits <= 32) {
            return &UInt32Type::instance;
          } else if (resultBits <= 64) {
            return &UInt64Type::instance;
          }

          diag.error(source) << "Integer value requires " << resultBits << " bits, too large.";
          diag.info() << "p1 = " << p1;
          diag.info() << "p2 = " << p2;
          //DFAIL("Integer value too large to be represented as native type.");
        }
      }
    }

    diag.debug() << "Neither " << type1 << " nor " << type2 << " is more specific than the other.";
    return NULL;
  }
}

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

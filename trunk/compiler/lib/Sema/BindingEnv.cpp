/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/NativeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"
#include <llvm/Support/CommandLine.h>

static llvm::cl::opt<bool>
DebugUnify("debug-unify", llvm::cl::desc("Debug unification"), llvm::cl::init(false));

namespace tart {

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
BindingEnv::BindingEnv(const TemplateSignature * ts) : substitutions_(NULL) {}

void BindingEnv::reset() {
  substitutions_ = NULL;
}

bool BindingEnv::unify(SourceContext * source, Type * pattern, Type * value, Variance variance) {
  // Dealias but don't depattern pattern
  while (pattern->typeClass() == Type::Alias) {
    if (TypeAlias * alias = dyn_cast<TypeAlias>(pattern)) {
      pattern = alias->value();
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

bool BindingEnv::unifyImpl(SourceContext * source, Type * pattern, Type * value, Variance variance) {
  if (pattern == value) {
    return true;
  } else if (isErrorResult(pattern)) {
    return false;
  } else if (PatternVar * pv = dyn_cast<PatternVar>(pattern)) {
    return unifyPattern(source, pv, value, variance);
  } else if (PatternValue * pval = dyn_cast<PatternValue>(pattern)) {
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
  } else if (NativePointerType * npp = dyn_cast<NativePointerType>(pattern)) {
    return unifyNativePointerType(source, npp, value);
  } else if (NativeArrayType * nap = dyn_cast<NativeArrayType>(pattern)) {
    return unifyNativeArrayType(source, nap, value);
  } else if (PatternVar * pv = dyn_cast<PatternVar>(value)) {
    diag.debug(source) << "Unify error: " << pattern << " : " << pv;
    DFAIL("Should not be unifying with a value that is a pattern var.");
  } else if (TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pattern);
  } else if (PatternValue * pval = dyn_cast<PatternValue>(value)) {
    Type * boundValue = pval->value();
    if (boundValue != NULL) {
      return unify(source, pattern, boundValue, variance);
    }

    diag.debug(source) << "Unbound pattern value found in value " << value << " for pattern " << pattern;
    addSubstitution(pval, value);
    return true;
  } else {
    //diag.error() << Format_Dealias << "Implement unification of " << pattern << " and " << value;
    return false;
    //DFAIL("Implement");
  }
}

bool BindingEnv::unifyNativePointerType(
    SourceContext * source, NativePointerType * pat, Type * value) {
  if (!AnalyzerBase::analyzeTypeDefn(pat->typeDefn(), Task_InferType)) {
    return false;
  }

  if (NativePointerType * npv = dyn_cast<NativePointerType>(value)) {
    if (!AnalyzerBase::analyzeTypeDefn(npv->typeDefn(), Task_InferType)) {
      return false;
    }

    return unify(source, pat->typeParam(0), npv->typeParam(0), Invariant);
  } else if (TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pat);
  } else {
    return false;
  }
}

bool BindingEnv::unifyNativeArrayType(SourceContext * source, NativeArrayType * pat, Type * value) {
  if (!AnalyzerBase::analyzeTypeDefn(pat->typeDefn(), Task_InferType)) {
    return false;
  }

  if (NativeArrayType * nav = dyn_cast<NativeArrayType>(value)) {
    if (!AnalyzerBase::analyzeTypeDefn(nav->typeDefn(), Task_InferType)) {
      return false;
    }

    if (pat->getSize() != nav->getSize()) {
      return false;
    }

    return unify(source, pat->typeParam(0), nav->typeParam(0), Invariant);
  } else if (TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pat);
  } else {
    return false;
  }
}

bool BindingEnv::unifyPattern(
    SourceContext * source, PatternVar * pattern, Type * value, Variance variance) {

  // If there is already a value bound to this pattern variable
  Substitution * s = getSubstitutionFor(pattern);
  if (s != NULL) {
    // Early out
    if (s->right() == pattern) {
      return true;
    }

    if (PatternValue * pval = dyn_cast<PatternValue>(s->right())) {
      // If the value that is already bound is a pattern variable from some other
      // environment (it should never be from this one), then bind it to
      // this variable.
      // TODO: Might want to check if the value is bindable to this var.
      bind(pattern, value);
      return true;
    }

    if (PatternValue * pval = dyn_cast<PatternValue>(value)) {
      // If the value that we're trying to bind is a pattern value, and we already
      // have a value, then leave the current value as is, assuming that it can
      // be bound to that pattern variable later.
      return true;
    }

    if (!pattern->canBindTo(value)) {
      return false;
    }

    Type * upperBound = value;
    Type * lowerBound = value;

    if (value->isEqual(&UnsizedIntType::instance)) {
      //upperBound =
    }

    Type * newValue = Type::selectLessSpecificType(s->right(), value);

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
    if (newValue != NULL) {
      if (newValue == value) {
        // No need to rebind if same as before.
        return true;
      }

      bind(pattern, newValue);
      return true;
    }

    return false;
  } else if (pattern->canBindTo(value)) {
    // Don't bother binding a pattern value to its own variable.
    if (PatternValue * pval = dyn_cast<PatternValue>(value)) {
      if (pval->var() == pattern) {
        return true;
      }
    }

    if (DebugUnify) {
      diag.debug(source->location()) << "Unify: " << pattern << " <- " << value << " in environment " << *this;
    }

    // Add a new substitution of value for pattern
    bind(pattern, value);
    return true;
  } else {
    return false;
  }
}

Substitution * BindingEnv::addSubstitution(const Type * left, Type * right) {
  DASSERT(left != right);
  substitutions_ = new Substitution(left, right, substitutions_);
  return substitutions_;
}

Substitution * BindingEnv::addSubstitution(const Type * left, Type * upper, Type * lower) {
  DASSERT(left != upper);
  DASSERT(left != lower);
  substitutions_ = new Substitution(left, upper, lower, substitutions_);
  return substitutions_;
}

void BindingEnv::bind(const PatternVar * var, Type * value) {
  addSubstitution(var, value);
}

Type * BindingEnv::get(const PatternVar * type) const {
  Substitution * s = getSubstitutionFor(type);
  if (s != NULL) {
    return s->right();
  }

  return NULL;
}

Type * BindingEnv::subst(Type * in, bool finalize) const {
  if (substitutions_ == NULL) {
    return in;
  }

  in = dealias(in);

  switch (in->typeClass()) {
    case Type::Pattern: {
      PatternVar * var = static_cast<const PatternVar *>(in);
      Type * value = get(var);
      if (value != NULL) {
        return subst(value, finalize);
      }

      return in;
    }

    case Type::PatternVal: {
      return in;
    }

    case Type::NativePointer: {
      const NativePointerType * np = static_cast<const NativePointerType *>(in);
      if (np->typeParam(0) == NULL) {
        return in;
      }

      Type * elemType = subst(np->typeParam(0), finalize);
      if (elemType == np->typeParam(0)) {
        return in;
      }

      return NativePointerType::create(elemType);
    }

    case Type::NativeArray: {
      const NativeArrayType * nt = static_cast<const NativeArrayType *>(in);
      if (nt->typeParam(0) == NULL) {
        return in;
      }

      Type * elemType = subst(nt->typeParam(0), finalize);
      if (elemType == nt->typeParam(0)) {
        return in;
      }

      return NativeArrayType::create(elemType, nt->getSize());
    }

    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol:
    case Type::Function:
    case Type::NonType:
      DASSERT(in->isSingular());
      return in;

    case Type::Primitive:
      return in;

    default:
      diag.fatal() << "Type class not handled: " << in->typeClass();
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

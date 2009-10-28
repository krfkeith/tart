/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/NativeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
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

static void assureNoPatternVars(Type * t) {
  for (int i = 0; i < t->numTypeParams(); ++i) {
    if (isa<PatternVar>(t->typeParam(i).type())) {
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

bool BindingEnv::unify(SourceContext * source, const TypeRef & pattern, const TypeRef & value,
    Variance variance) {
  return unify(source, pattern.type(), value.type(), variance);
}

bool BindingEnv::unify(SourceContext * source, Type * pattern, Type * value, Variance variance) {
  // Dealias but don't depattern pattern
  while (pattern->typeClass() == Type::Alias) {
    if (TypeAlias * alias = dyn_cast<TypeAlias>(pattern)) {
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
  } else if (PointerType * npp = dyn_cast<PointerType>(pattern)) {
    return unifyPointerType(source, npp, value);
  } else if (AddressType * npp = dyn_cast<AddressType>(pattern)) {
    return unifyAddressType(source, npp, value);
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

    diag.debug(source) << "Unbound pattern value found in value " << value <<
        " for pattern " << pattern;
    addSubstitution(pval, value);
    return true;
  } else if (CompositeType * ctPattern = dyn_cast<CompositeType>(pattern)) {
    if (CompositeType * ctValue = dyn_cast<CompositeType>(value)) {
      return unifyCompositeType(source, ctPattern, ctValue, variance);
    }

    return false;
  } else if (PrimitiveType * pval = dyn_cast<PrimitiveType>(pattern)) {
    // Go ahead and unify - type inference will see if it can convert.
    return true;
  } else {
    //diag.error() << Format_Dealias << "Implement unification of " << pattern << " and " << value;
    return false;
    //DFAIL("Implement");
  }
}

bool BindingEnv::unifyPointerType(
    SourceContext * source, PointerType * pat, Type * value) {
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (PointerType * npv = dyn_cast<PointerType>(value)) {
    if (!AnalyzerBase::analyzeType(npv, Task_PrepTypeComparison)) {
      return false;
    }

    return unify(source, pat->typeParam(0), npv->typeParam(0), Invariant);
  } else if (TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pat);
  } else {
    return false;
  }
}

bool BindingEnv::unifyAddressType(
    SourceContext * source, AddressType * pat, Type * value) {
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (AddressType * npv = dyn_cast<AddressType>(value)) {
    if (!AnalyzerBase::analyzeType(npv, Task_PrepTypeComparison)) {
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
  if (!AnalyzerBase::analyzeType(pat, Task_PrepTypeComparison)) {
    return false;
  }

  if (NativeArrayType * nav = dyn_cast<NativeArrayType>(value)) {
    if (!AnalyzerBase::analyzeType(nav, Task_PrepTypeComparison)) {
      return false;
    }

    if (pat->size() != nav->size()) {
      return false;
    }

    return unify(source, pat->typeParam(0), nav->typeParam(0), Invariant);
  } else if (TypeConstraint * tc = dyn_cast<TypeConstraint>(value)) {
    return tc->unifyWithPattern(*this, pat);
  } else {
    return false;
  }
}

bool BindingEnv::unifyCompositeType(
    SourceContext * source, CompositeType * pattern, CompositeType * value, Variance variance) {
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
    TypeList * patternTypeParams = NULL;
    TypeList * valueTypeParams = NULL;

    if (patternDefn->isTemplate()) {
      patternTypeParams = &patternDefn->templateSignature()->params();
    } else if (patternDefn->isTemplateInstance()) {
      patternTypeParams = &patternDefn->templateInstance()->paramValues();
    }

    if (valueDefn->isTemplate()) {
      valueTypeParams = &valueDefn->templateSignature()->params();
    } else if (valueDefn->isTemplateInstance()) {
      valueTypeParams = &valueDefn->templateInstance()->paramValues();
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
    for (ClassList::iterator it = value->bases().begin(); it != value->bases().end(); ++it) {
      if (unifyCompositeType(source, pattern, *it, Invariant)) {
        return true;
      }

      setSubstitutions(savedState);
    }

    return false;
  } else if (variance == Covariant) {
    Substitution * savedState = substitutions();
    for (ClassList::iterator it = pattern->bases().begin(); it != pattern->bases().end(); ++it) {
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
    SourceContext * source, PatternVar * pattern, Type * value, Variance variance) {

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

    if (PatternValue * pval = dyn_cast<PatternValue>(s->right())) {
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

    if (PatternVar * svar = dyn_cast<PatternVar>(s->right())) {
      //DFAIL("Should not happen");
      // If 'pattern' is bound to another pattern variable, then go ahead and override
      // that binding.
      if (svar != value && svar->canBindTo(value)) {
        addSubstitution(svar, value);
        addSubstitution(pattern, value);
      }

      return true;
    }

    Type * upperBound = value;
    Type * lowerBound = value;

    if (value->isEqual(&UnsizedIntType::instance)) {
      //upperBound =
    }

    Type * commonType = Type::selectLessSpecificType(s->right(), value);
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
    if (PatternValue * pval = dyn_cast<PatternValue>(value)) {
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

Substitution * BindingEnv::addSubstitution(const Type * left, Type * right) {
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

Substitution * BindingEnv::addSubstitution(const Type * left, Type * upper, Type * lower) {
  DASSERT(left != upper);
  DASSERT(left != lower);
  substitutions_ = new Substitution(left, upper, lower, substitutions_);
  return substitutions_;
}

Type * BindingEnv::get(const PatternVar * type) const {
  Substitution * s = getSubstitutionFor(type);
  if (s != NULL) {
    return s->right();
  }

  return NULL;
}

Type * BindingEnv::dereference(Type * type) const {
  while (type != NULL) {
    if (PatternVar * var = dyn_cast<PatternVar>(type)) {
      Substitution * s = getSubstitutionFor(type);
      if (s != NULL) {
        type = s->right();
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

TypeRef BindingEnv::subst(const TypeRef & in) const {
  TypeRef result(in);
  result.setType(subst(in.type()));
  return result;
}

Type * BindingEnv::subst(Type * in) const {
  if (substitutions_ == NULL || isErrorResult(in)) {
    return in;
  }

  in = dealias(in);

  switch (in->typeClass()) {
    case Type::Pattern: {
      PatternVar * var = static_cast<const PatternVar *>(in);
      Type * value = get(var);
      if (value != NULL) {
        return subst(value);
      }

      return in;
    }

    case Type::PatternVal: {
      PatternValue * value = static_cast<const PatternValue *>(in);
      Type * result = value->value();
      if (result != NULL) {
        return result;
      }

      return in;
    }

    case Type::Address: {
      const AddressType * np = static_cast<const AddressType *>(in);
      if (!np->typeParam(0).isDefined()) {
        return in;
      }

      TypeRef elemType = subst(np->typeParam(0));
      if (elemType == np->typeParam(0)) {
        return in;
      }

      return AddressType::get(elemType);
    }

    case Type::Pointer: {
      const PointerType * np = static_cast<const PointerType *>(in);
      if (!np->typeParam(0).isDefined()) {
        return in;
      }

      TypeRef elemType = subst(np->typeParam(0));
      if (elemType == np->typeParam(0)) {
        return in;
      }

      return PointerType::get(elemType);
    }

    case Type::NativeArray: {
      const NativeArrayType * nt = static_cast<const NativeArrayType *>(in);
      if (!nt->typeParam(0).isDefined()) {
        return in;
      }

      TypeRef elemType = subst(nt->typeParam(0));
      if (elemType == nt->typeParam(0)) {
        return in;
      }

      return NativeArrayType::get(elemType, nt->size());
    }

    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol: {
      if (in->typeDefn() == NULL) {
        return in;
      } else if (in->typeDefn()->isTemplate()) {
        Defn * def = in->typeDefn()->templateSignature()->instantiate(SourceLocation(), *this);
        if (def != NULL) {
          return cast<TypeDefn>(def)->typeValue();
        } else {
          return NULL;
        }
      } else if (in->typeDefn()->isTemplateMember()) {
        DFAIL("Implement");
      } else if (in->typeDefn()->isPartialInstantiation()) {
        DFAIL("Implement");
      }

      return in;
    }

    case Type::Function:
    case Type::NonType:
      //DASSERT(in->isSingular());
      return in;

    case Type::Primitive:
      return in;

    case Type::Constraint: {
      TypeConstraint * constraint = static_cast<TypeConstraint *>(in);
      if (constraint->isSingular()) {
        return constraint->singularValue().type();
      }

      DFAIL("Type constraint not handled");
      break;
    }

    default:
      diag.fatal() << "Type class not handled: " << in->typeClass();
      return NULL;
  }
}

TypeRef BindingEnv::relabel(const TypeRef & in) {
  TypeRef result(in);
  result.setType(relabel(in.type()));
  return result;
}

Type * BindingEnv::relabel(Type * in) {
  if (substitutions_ == NULL) {
    return in;
  }

  in = dealias(in);

  switch (in->typeClass()) {
    case Type::Pattern: {
      PatternVar * var = static_cast<const PatternVar *>(in);
      Type * value = get(var);
      if (value != NULL) {
        return relabel(value);
      }

      diag.debug() << "Pattern variable " << var << " not found in environment " << *this;
      DFAIL("Missing substitution");

      return in;
    }

    case Type::PatternVal: {
      PatternValue * value = static_cast<const PatternValue *>(in);
      DASSERT(value->env() == this);
      Type * result = value->value();
      if (result != NULL) {
        return result;
      }

      return in;
    }

    case Type::Address: {
      const AddressType * np = static_cast<const AddressType *>(in);
      if (!np->typeParam(0).isDefined()) {
        return in;
      }

      TypeRef elemType = relabel(np->typeParam(0));
      if (elemType == np->typeParam(0)) {
        return in;
      }

      return AddressType::get(elemType);
    }

    case Type::Pointer: {
      const PointerType * np = static_cast<const PointerType *>(in);
      if (!np->typeParam(0).isDefined()) {
        return in;
      }

      TypeRef elemType = relabel(np->typeParam(0));
      if (elemType == np->typeParam(0)) {
        return in;
      }

      return PointerType::get(elemType);
    }

    case Type::NativeArray: {
      const NativeArrayType * nt = static_cast<const NativeArrayType *>(in);
      if (!nt->typeParam(0).isDefined()) {
        return in;
      }

      TypeRef elemType = relabel(nt->typeParam(0));
      if (elemType == nt->typeParam(0)) {
        return in;
      }

      return NativeArrayType::get(elemType, nt->size());
    }

    case Type::Struct:
    case Type::Class:
    case Type::Interface:
    case Type::Protocol: {
      if (in->typeDefn() == NULL) {
        return in;
      } else if (in->typeDefn()->isTemplate()) {
        Defn * def = in->typeDefn()->templateSignature()->instantiate(SourceLocation(), *this);
        if (def != NULL) {
          assureNoPatternVars(cast<TypeDefn>(def)->typeValue());
          return cast<TypeDefn>(def)->typeValue();
        } else {
          return NULL;
        }
      } else if (in->typeDefn()->isTemplateMember()) {
        DFAIL("Implement");
      } else if (in->typeDefn()->isPartialInstantiation()) {
        TemplateInstance * tinst = in->typeDefn()->templateInstance();
        TemplateSignature * tsig = tinst->templateDefn()->templateSignature();
        Substitution * savedState = substitutions_;
        // Add type param mappings.
        size_t numParams = tsig->patternVarCount();
        for (size_t i = 0; i < numParams; ++i) {
          PatternVar * param = tsig->patternVar(i);
          Type * value = tinst->paramValues()[i];
          Type * svalue = relabel(value);
          if (svalue != NULL) {
            addSubstitution(param, svalue);
          }
        }

        Defn * def = tsig->instantiate(SourceLocation(), *this);
        substitutions_ = savedState;
        if (def != NULL) {
          assureNoPatternVars(cast<TypeDefn>(def)->typeValue());
          return cast<TypeDefn>(def)->typeValue();
        } else {
          return NULL;
        }
      }

      assureNoPatternVars(in);
      return in;
    }

    case Type::Function:
    case Type::NonType:
      //DASSERT(in->isSingular());
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

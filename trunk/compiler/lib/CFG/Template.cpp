/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Template.h"
#include "tart/CFG/Constant.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/NativeType.h"
#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"

namespace tart {

/** The set of traits that should be copied from the template to its
    instantiation. */
static const Defn::Traits INSTANTIABLE_TRAITS = Defn::Traits::of(
  Defn::Final,
  Defn::Abstract,
  Defn::ReadOnly,
  Defn::Extern,
  Defn::Ctor
);

// -------------------------------------------------------------------
// PatternVar

PatternVar::PatternVar(const SourceLocation & location,
    TypeDefn * defn, Scope * parentScope, TemplateSignature * temp)
  : DeclaredType(Pattern, defn, parentScope)
  , location_(location)
  , valueType_(Builtins::typeTypeDescriptor)
  , template_(temp)
{}

const llvm::Type * PatternVar::createIRType() const {
  DFAIL("Invalid");
}

ConversionRank PatternVar::convertImpl(const Conversion & cn) const {
  // The only place where this conversion function is called is when attempting to
  // determine if a custom coercion can be done (without actually doing it.)
  if (cn.resultValue != NULL) {
    DFAIL("Shouldn't be attempting to call convert on a Pattern Var (I think).");
  }

  return NonPreferred;
}

const Defn * PatternVar::templateDefn() const {
  return template_->value();
}

bool PatternVar::canBindTo(const Type * value) const {
  if (const SingleValueType * nt = dyn_cast<SingleValueType>(value)) {
    ConstantExpr * expr = nt->value();
    return valueType_->canConvert(expr);
  } else if (valueType_ == NULL || valueType_->isSubtype(Builtins::typeTypeDescriptor)) {
    return true;
  } else {
    return false;
  }
}

bool PatternVar::isSubtype(const Type * other) const {
  return false;
}

bool PatternVar::isReferenceType() const {
  return false;
}

bool PatternVar::isSingular() const {
  return false;
}

void PatternVar::trace() const {
  DeclaredType::trace();
  location_.trace();
}

void PatternVar::format(FormatStream & out) const {
  if (out.getShowQualifiedName()) {
    out << template_->value()->name() << "%" << name();
  } else {
    out << "%" << name();
  }
}

/// -------------------------------------------------------------------
/// TemplateSignature

TemplateSignature * TemplateSignature::get(Defn * v, Scope * parent) {
  if (v->templateSignature() == NULL) {
    v->setTemplateSignature(new TemplateSignature(v, parent));
  }

  return v->templateSignature();
}

TemplateSignature::TemplateSignature(Defn * v, Scope * parentScope)
  : value_(v)
  , ast_(NULL)
  , paramScope_(parentScope)
{
  paramScope_.setScopeName("template-params");
}

PatternVar * TemplateSignature::addPatternVar(const SourceLocation & loc,
    const char * name, Type * type) {
  TypeDefn * tdef = new TypeDefn(value_->module(), name, NULL);
  PatternVar * var = new PatternVar(loc, tdef, &paramScope_, this);
  if (type != NULL) {
    var->setValueType(type);
  }
  tdef->setTypeValue(var);
  vars_.push_back(var);
  DASSERT_OBJ(paramScope_.lookupSingleMember(name) == NULL, tdef);
  paramScope_.addMember(tdef);
  return var;
}

void TemplateSignature::addParameter(const SourceLocation & loc, const char * name, Type * type) {
  PatternVar * var = addPatternVar(loc, name, type);
  params_.push_back(var);
}

PatternVar * TemplateSignature::patternVar(const char * name) const {
  Defn * de = paramScope_.lookupSingleMember(name);
  if (TypeDefn * tdef = dyn_cast_or_null<TypeDefn>(de)) {
    return cast<PatternVar>(tdef->typeValue());
  }

  return NULL;
}

size_t TemplateSignature::getVarIndex(const PatternVar * var) const {
  for (PatternVarList::const_iterator it = vars_.begin(); it != vars_.end(); ++it) {
    if (var == *it) {
      return it - vars_.begin();
    }
  }

  return size_t(-1);
}

PatternVar * TemplateSignature::patternVar(int index) const {
  return vars_[index];
}

size_t TemplateSignature::patternVarCount() const {
  return vars_.size();
}

void TemplateSignature::trace() const {
  safeMark(ast_);
  markList(params_.begin(), params_.end());
  markList(requirements_.begin(), requirements_.end());
  markList(specializations.begin(), specializations.end());
  paramScope_.trace();
}

void TemplateSignature::format(FormatStream & out) const {
  out << "[";
  formatTypeList(out, params_);
  out << "]";
}

Defn * TemplateSignature::instantiate(const SourceLocation & loc, const BindingEnv & env,
    bool singular) {
  bool isPartial = false;

  // Check to make sure that the parameters are of the correct type.
  TypeList paramValues;
  bool noCache = false;
  for (PatternVarList::iterator it = vars_.begin(); it != vars_.end(); ++it) {
    PatternVar * var = *it;
    if (var->valueType() == NULL) {
      // TODO: This is needed because some pattern vars are created before
      // typeTypeDescriptor is loaded. We should fix that.
      DASSERT(Builtins::typeTypeDescriptor != NULL);
      var->setValueType(Builtins::typeTypeDescriptor);
    }

    Type * value = env.subst(var);
    DASSERT_OBJ(value != NULL, var);
    if (!var->canBindTo(value)) {
      diag.fatal(loc) << "Type of expression " << value <<
          " incompatible with template parameter " << var << ":" << var->valueType();
      DASSERT(var->canBindTo(value));
    }

    if (!value->isSingular()) {
      if (singular) {
        diag.fatal(loc) << "Non-singular parameter '" << var << "' = '" << value << "'";
      }

      isPartial = true;
    }

    if (isa<PatternValue>(value)) {
      noCache = true;
    }

    //DASSERT(!isa<PatternValue>(value));

    // We might need to do some coercion here...
    paramValues.push_back(value);
  }

  TypeRefList typeArgs;
  // Substitute into the template args to create the arg list
  for (TypeList::iterator it = params_.begin(); it != params_.end(); ++it) {
    typeArgs.push_back(env.subst(*it));
  }

  TypeVector * typeArgVector = TypeVector::get(typeArgs);

  //TypeVector * tv = TypeVector::get(paramValues);

  // See if we can find an existing specialization that matches the arguments.
  // TODO: Canonicalize and create a key from the args.
  if (!noCache) {
    for (DefnList::const_iterator it = specializations.begin(); it != specializations.end(); ++it) {
      Defn * spec = *it;
      TemplateInstance * ti = spec->templateInstance();
      DASSERT_OBJ(ti != NULL, value_);
      DASSERT_OBJ(ti->paramValues().size() == paramValues.size(), value_);
      if (std::equal(paramValues.begin(), paramValues.end(), ti->paramValues().begin(),
          TypeEquals())) {

        if (singular && !ti->value()->isSingular()) {
          diag.fatal(loc) << Format_Verbose << "Expected " << ti->value() << " to be singular, why isn't it?";
          DFAIL("Non-singular");
        }

        return ti->value();
      }
    }
  }

  // Create the template instance
  DASSERT(value_->definingScope() != NULL);
  TemplateInstance * tinst = new TemplateInstance(value_, typeArgVector);
  tinst->paramValues().append(paramValues.begin(), paramValues.end());
  tinst->instantiatedFrom() = loc;

  // Substitute into the template args to create the arg list
  /*for (TypeList::iterator it = params_.begin(); it != params_.end(); ++it) {
    Type * argValue = env.subst(*it);
    tinst->templateArgs().push_back(argValue);
  }*/

  // Create the definition
  Defn * result = NULL;
  if (value_->ast() != NULL) {
    result = ScopeBuilder::createDefn(tinst, &Builtins::syntheticModule, value_->ast());
    tinst->setValue(result);
    result->setQualifiedName(value_->qualifiedName());
    result->addTrait(Defn::Synthetic);
    result->traits().addAll(value_->traits() & INSTANTIABLE_TRAITS);
    result->setParentDefn(value_->parentDefn());
    result->setDefiningScope(tinst);
    if (isPartial) {
      result->addTrait(Defn::PartialInstantiation);
    }
  } else if (value_->defnType() == Defn::Typedef) {
    TypeDefn * tdef = static_cast<TypeDefn *>(value_);

    if (tdef == &AddressType::typedefn) {
      DFAIL("Implement");
    } else if (tdef == &PointerType::typedefn) {
      DFAIL("Implement");
    }

    TypeDefn * newDef = new TypeDefn(value_->module(), tdef->name());

    switch (tdef->typeValue()->typeClass()) {
      case Type::NativeArray: {
        SingleValueType * ntc = cast<SingleValueType>(paramValues[1]);
        ConstantInteger * intVal = cast<ConstantInteger>(ntc->value());
        uint64_t size = intVal->value()->getZExtValue();
        NativeArrayType * na = new NativeArrayType(paramValues[0], size, newDef, tinst);
        newDef->setTypeValue(na);
        newDef->createQualifiedName(NULL);
        break;
      }

      default:
        DFAIL("Invalid template type");
        break;
    }

    result = newDef;
    tinst->setValue(result);
  } else {
    DFAIL("Invalid template type");
  }

  // Copy over certain attributes
  if (FunctionDefn * fdef = dyn_cast<FunctionDefn>(result)) {
    fdef->setIntrinsic(static_cast<FunctionDefn *>(value_)->intrinsic());
  }

  if (!noCache) {
    specializations.push_back(result);
  }

  // Create a symbol for each template parameter.
  bool isSingular = true;

  for (int i = 0; i < vars_.size(); ++i) {
    PatternVar * var = vars_[i];
    Type * value = paramValues[i];

    Defn * argDefn;
    if (SingleValueType * ntc = dyn_cast<SingleValueType>(value)) {
      argDefn = new VariableDefn(Defn::Let, result->module(), var->name(), ntc->value());
    } else {
      argDefn = new TypeDefn(result->module(), var->name(), value);
    }

    argDefn->setSingular(value->isSingular());
    isSingular &= value->isSingular();
    argDefn->addTrait(Defn::Synthetic);
    tinst->addMember(argDefn);
  }

  if (singular && !isSingular) {
    diag.fatal(loc) << Format_Verbose << "Expected " << result << " to be singular, why isn't it?";
  }

  // One additional parameter, which is the name of the instantiated symbol.
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(result)) {
    if (CompositeType * ctype = dyn_cast<CompositeType>(tdef->typeValue())) {
      TypeDefn * nameAlias = new TypeDefn(result->module(), tdef->name(), ctype);
      nameAlias->setSingular(ctype->isSingular());
      nameAlias->addTrait(Defn::Synthetic);
      tinst->addMember(nameAlias);
    }
  }

  result->setSingular(isSingular);
  result->setTemplateInstance(tinst);

  DASSERT(isSingular == result->isSingular());

  if (singular && !result->isSingular()) {
    diag.fatal(loc) << Format_Verbose << "Expected " << result << " to be singular, why isn't it?";
    DFAIL("Non-singular");
  }

  return result;
}

Type * TemplateSignature::instantiateType(const SourceLocation & loc, const BindingEnv & env,
    bool singular) {

  if (value_->ast() != NULL) {
    TypeDefn * tdef = cast<TypeDefn>(instantiate(loc, env, singular));
    return tdef->typeValue();
  }

  // Create the definition
  TypeDefn * tdef = static_cast<TypeDefn *>(value_);
  Type * proto = tdef->typeValue();
  if (proto->typeClass() != Type::Address && proto->typeClass() != Type::Pointer) {
    TypeDefn * tdef = cast<TypeDefn>(instantiate(loc, env, singular));
    return tdef->typeValue();
  }

  // TODO: This needs major refactoring.
  // Check to make sure that the parameters are of the correct type.
  TypeList paramValues;
  for (PatternVarList::iterator it = vars_.begin(); it != vars_.end(); ++it) {
    PatternVar * var = *it;
    Type * value = env.subst(var);
    DASSERT_OBJ(value != NULL, var);
    if (!var->canBindTo(value)) {
      diag.fatal(loc) << "Type of expression " << value <<
          " incompatible with template parameter " << var << ":" << var->valueType();
      DASSERT(var->canBindTo(value));
    }

    if (!value->isSingular()) {
      if (singular) {
        diag.fatal(loc) << "Non-singular parameter '" << var << "' = '" << value << "'";
      }
    }

    // We might need to do some coercion here...
    paramValues.push_back(value);
  }

  switch (tdef->typeValue()->typeClass()) {
    case Type::Address: {
      return AddressType::get(paramValues[0]);
      break;
    }

    case Type::Pointer: {
      return PointerType::get(paramValues[0]);
      break;
    }

#if 0
    case Type::NativeArray: {
      SingleValueType * ntc = cast<SingleValueType>(paramValues[1]);
      ConstantInteger * intVal = cast<ConstantInteger>(ntc->value());
      uint64_t size = intVal->value()->getZExtValue();
      NativeArrayType * na = new NativeArrayType(paramValues[0], size, newDef, tinst);
      newDef->setTypeValue(na);
      newDef->createQualifiedName(NULL);
      break;
    }
#endif

    default:
      DFAIL("Invalid template type");
      break;
  }

  return NULL;
}

/// -------------------------------------------------------------------
/// TemplateInstance

TemplateInstance::TemplateInstance(Defn * templateDefn, TypeVector * templateArgs)
  : value_(NULL)
  , templateDefn_(templateDefn)
  , templateArgs_(templateArgs)
  , parentScope_(templateDefn->definingScope())
{
}

void TemplateInstance::addMember(Defn * d) {
  DASSERT(d->storageClass() != Storage_Local);
  DASSERT(d->definingScope() == NULL);
  SymbolTable::Entry * entry = paramDefns_.add(d);
  d->setDefiningScope(this);
}

bool TemplateInstance::lookupMember(const char * ident, DefnList & defs, bool inherit) const {
  const SymbolTable::Entry * entry = paramDefns_.findSymbol(ident);
  if (entry != NULL) {
    defs.append(entry->begin(), entry->end());
    return true;
  }

  return false;
}

void TemplateInstance::dumpHierarchy(bool full) const {
  std::string out;
  out.append("[template-instance] ");
  paramDefns_.getDebugSummary(out);
  diag.writeLnIndent(out);
}

void TemplateInstance::format(FormatStream & out) const {
  out << "[";
  for (TypeVector::iterator it = templateArgs_->begin(); it != templateArgs_->end(); ++it) {
    if (it != templateArgs_->begin()) {
      out << ", ";
    }

    out << *it;
  }

  out << "]";
}

void TemplateInstance::trace() const {
  paramDefns_.trace();
  safeMark(value_);
  safeMark(templateArgs_);
  markList(paramValues_.begin(), paramValues_.end());
}

} // namespace Tart

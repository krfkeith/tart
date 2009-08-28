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
  , valueType_(Builtins::typeType)
  , template_(temp)
{}

const llvm::Type * PatternVar::createIRType() const {
  DFAIL("Invalid");
}

ConversionRank PatternVar::convertImpl(const Conversion & cn) const {
  if (cn.bindingEnv != NULL) {
    const Type * ty = cn.bindingEnv->get(this);
    DFAIL("Deprecated");
    if (ty == NULL) {
      diag.fatal() << "Unbound type param " << this << " in environment " << *cn.bindingEnv;
      DFAIL("Unbound type param");
    } else {
      return ty->convertImpl(cn);
    }
  }

  return Incompatible;
}

const Defn * PatternVar::templateDefn() const {
  return template_->getValue();
}

bool PatternVar::canBindTo(const Type * value) const {
  if (const NonTypeConstant * nt = dyn_cast<NonTypeConstant>(value)) {
    ConstantExpr * expr = nt->value();
    return valueType_->canConvert(expr);
  } else if (valueType_ == NULL || valueType_->isSubtype(Builtins::typeType)) {
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
  out << "%" << name();
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
  : value(v)
  , ast(NULL)
  , paramScope_(parentScope)
{
  paramScope_.setScopeName("template-params");
}

PatternVar * TemplateSignature::addPatternVar(const SourceLocation & loc,
    const char * name, Type * type) {
  TypeDefn * tdef = new TypeDefn(value->module(), name, NULL);
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
    return cast<PatternVar>(tdef->getTypeValue());
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
  safeMark(ast);
  markList(params_.begin(), params_.end());
  markList(requirements.begin(), requirements.end());
  markList(specializations.begin(), specializations.end());
  paramScope_.trace();
}

void TemplateSignature::format(FormatStream & out) const {
  out << "[";
  formatTypeList(out, params_);
  out << "]";
}

Defn * TemplateSignature::instantiate(const SourceLocation & loc, const BindingEnv & env) {

  // Check to make sure that thre parameters are of the correct type.
  TypeList paramValues;
  for (PatternVarList::iterator it = vars_.begin(); it != vars_.end(); ++it) {
    PatternVar * var = *it;
    if (var->valueType() == NULL) {
      // TODO: This is needed because some pattern vars are created before
      // typeType is loaded. We should fix that.
      DASSERT(Builtins::typeType != NULL);
      var->setValueType(Builtins::typeType);
    }

    Type * value = env.get(var);
    DASSERT(value != NULL);
    if (!var->canBindTo(value)) {
      diag.fatal(loc) << "Type of expression " << value <<
          " incompatible with template parameter " << var << ":" << var->valueType();
      DASSERT(var->canBindTo(value));
    }

    // We might need to do some coercion here...
    paramValues.push_back(value);
  }

  // See if we can find an existing specialization that matches the arguments.
  // TODO: Canonicalize and create a key from the args.
  for (DefnList::const_iterator it = specializations.begin(); it != specializations.end(); ++it) {
    Defn * spec = *it;
    TemplateInstance * ti = spec->templateInstance();
    DASSERT_OBJ(ti != NULL, value);
    DASSERT_OBJ(ti->paramValues().size() == paramValues.size(), value);
    if (std::equal(paramValues.begin(), paramValues.end(),
            ti->paramValues().begin(), TypeEquals())) {
      return ti->value();
    }
  }

  // Create the template instance
  DASSERT(value->definingScope() != NULL);
  TemplateInstance * tinst = new TemplateInstance(value->definingScope());
  tinst->paramValues().append(paramValues.begin(), paramValues.end());

  // Substitute into the template args to create the arg list
  for (TypeList::iterator it = params_.begin(); it != params_.end(); ++it) {
    Type * argValue = env.subst(*it);
    tinst->templateArgs().push_back(argValue);
  }

  // Create the definition
  Defn * result = NULL;
  if (value->getAST() != NULL) {
    result = ScopeBuilder::createDefn(tinst, &Builtins::syntheticModule, value->getAST());
    tinst->setValue(result);
    result->setQualifiedName(value->qualifiedName());
    result->addTrait(Defn::Synthetic);
    result->getTraits().addAll(value->getTraits() & INSTANTIABLE_TRAITS);
    result->setDefiningScope(tinst);
  } else if (value->defnType() == Defn::Typedef) {
    TypeDefn * tdef = static_cast<TypeDefn *>(value);
    TypeDefn * newDef = new TypeDefn(value->module(), tdef->name());

    switch (tdef->getTypeValue()->typeClass()) {
      case Type::NativePointer: {
        NativePointerType * np = new NativePointerType(paramValues[0], newDef, tinst);
        newDef->setTypeValue(np);
        newDef->createQualifiedName(NULL);
        break;
      }

      case Type::NativeArray: {
        NonTypeConstant * ntc = cast<NonTypeConstant>(paramValues[1]);
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
    fdef->setIntrinsic(static_cast<FunctionDefn *>(value)->intrinsic());
  }

  specializations.push_back(result);

  // Create a symbol for each template parameter.
  bool isSingular = true;

  for (int i = 0; i < vars_.size(); ++i) {
    PatternVar * var = vars_[i];
    Type * value = paramValues[i];

    Defn * argDefn;
    if (NonTypeConstant * ntc = dyn_cast<NonTypeConstant>(value)) {
      argDefn = new VariableDefn(Defn::Let, result->module(), var->name(),
          ntc->value());
    } else {
      argDefn = new TypeDefn(result->module(), var->name(), value);
    }

    argDefn->setSingular(value->isSingular());
    isSingular &= value->isSingular();
    argDefn->addTrait(Defn::Synthetic);
    tinst->addMember(argDefn);
  }

  // One additional parameter, which is the name of the instantiated symbol.
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(result)) {
    if (CompositeType * ctype = dyn_cast<CompositeType>(tdef->getTypeValue())) {
      TypeDefn * nameAlias = new TypeDefn(
          result->module(), tdef->name(), ctype);
      nameAlias->setSingular(ctype->isSingular());
      nameAlias->addTrait(Defn::Synthetic);
      tinst->addMember(nameAlias);
    }
  }

  result->setSingular(isSingular);
  result->setTemplateInstance(tinst);

  return result;
}

/// -------------------------------------------------------------------
/// TemplateInstance

TemplateInstance::TemplateInstance(Scope * ps)
  : value_(NULL)
  , parentScope_(ps)
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
  for (TypeList::const_iterator it = templateArgs_.begin();
      it != templateArgs_.end(); ++it) {
    if (it != templateArgs_.begin()) {
      out << ", ";
    }

    out << *it;
  }

  out << "]";
}

void TemplateInstance::trace() const {
  paramDefns_.trace();
  value_->mark();
  markList(paramValues_.begin(), paramValues_.end());
}

} // namespace Tart

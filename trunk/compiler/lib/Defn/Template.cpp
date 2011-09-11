/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/TemplateConditions.h"

#include "tart/Expr/Constant.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeConversion.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Type/UnitType.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/TypeTransform.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

namespace tart {

// -------------------------------------------------------------------
// Class to discover all pattern variables in a type parameter list.

class FindTypeVariables : public TypeTransform {
public:
  FindTypeVariables(TypeVariableList & vars) : vars_(vars) {}

  const Type * visitTypeVariable(const TypeVariable * in) {
    vars_.push_back(const_cast<TypeVariable *>(in));
    return in;
  }

private:
  TypeVariableList & vars_;
};

// -------------------------------------------------------------------
// TypeVariable

TypeVariable::TypeVariable(const SourceLocation & location, StringRef name,
    const Type * valueType)
  : TypeImpl(TypeVar, Shape_Unset)
  , location_(location)
  , valueType_(valueType)
  , upperBound_(NULL)
  , name_(name)
  , isVariadic_(false)
{}

llvm::Type * TypeVariable::createIRType() const {
  DFAIL("Invalid");
}

bool TypeVariable::canBindTo(const Type * value) const {
  if (const UnitType * nt = dyn_cast<UnitType>(value)) {
    if (valueType_ == NULL) {
      return false;
    }

    ConstantExpr * expr = nt->value();
    return TypeConversion::check(expr, valueType_);
  } else if (value->typeClass() == Type::Assignment) {
    return true;
  } else if (valueType_ == NULL) {
    return true;
  } else {
    return false;
  }
}

bool TypeVariable::isReferenceType() const {
  return false;
}

bool TypeVariable::isSingular() const {
  return false;
}

void TypeVariable::trace() const {
  TypeImpl::trace();
  location_.trace();
}

void TypeVariable::format(FormatStream & out) const {
  out << "%" << name_;
  if (isVariadic_) {
    out << "...";
  }
}

/// -------------------------------------------------------------------
/// Template

Template * Template::get(Defn * v, Scope * parent) {
  if (v->templateSignature() == NULL) {
    v->setTemplateSignature(new Template(v, parent));
  }

  return v->templateSignature();
}

Template::Template(Defn * v, Scope * parentScope)
  : value_(v)
  , ast_(NULL)
  , typeParams_(NULL)
  , paramScope_(parentScope)
  , numRequiredArgs_(0)
  , isVariadic_(false)
{
  paramScope_.setScopeName("template-params");
}

// TODO: Insure that variadic argument is the last one? Do we care?

void Template::setTypeParams(const TupleType * typeParams) {
  DASSERT(typeParams_ == NULL);
  typeParams_ = typeParams;
  numRequiredArgs_ = typeParams->size();
  FindTypeVariables(vars_).transform(typeParams_);

  // Check for variadic type parameters
  const TypeVariable * variadicParam = NULL;
  for (size_t i = 0; i < typeParams_->size(); ++i) {
    if (const TypeVariable * tv = dyn_cast<TypeVariable>((*typeParams)[i])) {
      if (tv->isVariadic()) {
        if (i != typeParams_->size() - 1) {
          diag.error(tv) << "template variadic parameter must be last";
        }
        isVariadic_ = true;
        variadicParam = tv;
        --numRequiredArgs_;
      }
    }
  }

  for (TypeVariableList::const_iterator it = vars_.begin(); it != vars_.end(); ++it) {
    TypeVariable * var = *it;
    if (var->isVariadic() && var != variadicParam) {
      diag.error(var) << "Variadic argument not allowed here";
    }
    if (paramScope_.lookupSingleMember(var->name()) == NULL) {
      TypeDefn * tdef = new TypeDefn(value_->module(), var->name(), var);
      paramScope_.addMember(tdef);
    }
  }
}

const Type * Template::typeParam(int index) const {
  return (*typeParams_)[index];
}

TypeVariable * Template::patternVar(const char * name) const {
  Defn * de = paramScope_.lookupSingleMember(name);
  if (TypeDefn * tdef = dyn_cast_or_null<TypeDefn>(de)) {
    return cast<TypeVariable>(tdef->typeValue());
  }

  return NULL;
}

TypeVariable * Template::patternVar(int index) const {
  return vars_[index];
}

size_t Template::patternVarCount() const {
  return vars_.size();
}

void Template::trace() const {
  safeMark(ast_);
  safeMark(typeParams_);
  markList(conditions_.begin(), conditions_.end());
  safeMarkList(typeParamDefaults_.begin(), typeParamDefaults_.end());
  markList(vars_.begin(), vars_.end());
  for (SpecializationMap::const_iterator it = specializations_.begin();
      it != specializations_.end(); ++it) {
    it->first->mark();
    it->second->mark();
  }

  paramScope_.trace();
}

void Template::format(FormatStream & out) const {
  if (typeParams_) {
    out << "[";
    typeParams_->formatMembers(out);
    out << "]";
  }
}

Defn * Template::findSpecialization(const TupleType * tv) const {
  SpecializationMap::const_iterator it = specializations_.find(tv);
  if (it != specializations_.end()) {
    return it->second;
  }

  return NULL;
}

Defn * Template::instantiate(const SourceLocation & loc, const TypeVarMap & varValues,
    uint32_t expectedTraits) {
  bool isPartial = false;
  bool isScaffold = false;
  bool isExpectedSingular = (expectedTraits & Singular) != 0;
  bool trace = AnalyzerBase::isTraceEnabled(value_);
  SubstitutionTransform subst(varValues);

  // Check to make sure that the parameters are of the correct type.
  ConstTypeList paramValues;
  for (TypeVariableList::iterator it = vars_.begin(); it != vars_.end(); ++it) {
    TypeVariable * var = *it;
    const Type * value = subst(var);
    DASSERT_OBJ(value != NULL, var);
    //DASSERT_OBJ(!value->isNullType(), var);
    if (!var->canBindTo(value)) {
      diag.error(loc) << "Type of expression " << value <<
          " incompatible with template parameter " << var << ":" << var->valueType();
      DASSERT(var->canBindTo(value));
    }

    // A template made up of throwaway types is also throwaway.
    if (value->isScaffold()) {
      isScaffold = true;
    }

    // We might need to do some coercion here...
    paramValues.push_back(value);
  }

  if (isScaffold && (expectedTraits & NonScaffold)) {
    diag.fatal(loc) << "Expected non-throwaway template instantiation.";
  }

  const TupleType * typeArgs = cast<TupleType>(subst(typeParams_));
  if (!typeArgs->isSingular()) {
    if (isExpectedSingular) {
      diag.fatal(loc) << "Non-singular parameters [" << typeArgs << "]";
    }

    isPartial = true;
  }

  // See if we can find an existing specialization that matches the arguments.
  // TODO: Canonicalize and create a key from the args.
  if (!isScaffold) {
    Defn * sp = findSpecialization(typeArgs);
    if (sp != NULL) {
      if (trace) {
        diag.debug(loc) << "Found " << value_ << " with params " << typeArgs << " in cache.";
      }
      return sp;
    }
  }

  if (trace) {
    diag.debug(loc) << "Instantiating " << value_ << " with params " << typeArgs;
  }

  // Create the template instance
  DASSERT(value_->definingScope() != NULL);
  TemplateInstance * tinst = new TemplateInstance(value_, typeArgs, TupleType::get(paramValues));
  tinst->instantiatedFrom() = loc;

  // Create the definition
  DASSERT_OBJ(value_->ast() != NULL, value_);
  DASSERT_OBJ(value_->parentDefn() != NULL, value_);
  Defn * result = NULL;
  result = ScopeBuilder::createDefn(
      tinst, &Builtins::syntheticModule, value_->ast(), value_->storageClass());
  tinst->setValue(result);
  result->setQualifiedName(value_->qualifiedName());
  result->addTrait(Defn::Synthetic);
  result->setParentDefn(value_->parentDefn());
  result->setDefiningScope(tinst);
  if (isPartial) {
    result->addTrait(Defn::PartialInstantiation);
  }
  if (isScaffold) {
    result->addTrait(Defn::Scaffold);
  }

  // Copy over certain attributes
  if (FunctionDefn * fdef = dyn_cast<FunctionDefn>(result)) {
    fdef->setIntrinsic(static_cast<FunctionDefn *>(value_)->intrinsic());
  }

  if (!isScaffold) {
    specializations_[typeArgs] = result;
  }

  // Create a symbol for each template parameter.
  bool isSingular = true;

  for (size_t i = 0; i < vars_.size(); ++i) {
    TypeVariable * var = vars_[i];
    const Type * value = paramValues[i];

    Defn * argDefn;
    if (const UnitType * ut = dyn_cast<UnitType>(value)) {
      Expr * cval = ut->value();
      if (cval != NULL && var->valueType() != NULL) {
        cval = var->valueType()->implicitCast(loc, cval);
      }
      argDefn = new VariableDefn(Defn::Let, result->module(), var->name(), cval);
      argDefn->setStorageClass(Storage_Static);
    } else {
      argDefn = new TypeDefn(result->module(), var->name(), const_cast<Type *>(value));
    }

    argDefn->setSingular(value->isSingular());
    isSingular &= value->isSingular();
    argDefn->addTrait(Defn::Synthetic);
    tinst->addMember(argDefn);
  }

  if (isExpectedSingular && !isSingular) {
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
  return result;
}

Type * Template::instantiateType(
    const SourceLocation & loc, const TypeVarMap & varValues, uint32_t expectedTraits)
{
  if (value_->ast() != NULL) {
    TypeDefn * tdef = cast<TypeDefn>(instantiate(loc, varValues, expectedTraits));
    return tdef->typeValue();
  }

  // Create the definition
  TypeDefn * tdef = static_cast<TypeDefn *>(value_);
  Type * proto = tdef->typeValue();
  if (proto->typeClass() != Type::NAddress &&
      proto->typeClass() != Type::NArray &&
      proto->typeClass() != Type::FlexibleArray &&
      proto->typeClass() != Type::TypeLiteral) {
    TypeDefn * tdef = cast<TypeDefn>(instantiate(loc, varValues));
    return tdef->typeValue();
  }

  // TODO: Can TypeTransform do this?
  // Check to make sure that the parameters are of the correct type.
  TypeList paramValues;
  SubstitutionTransform subst(varValues);
  for (TypeVariableList::iterator it = vars_.begin(); it != vars_.end(); ++it) {
    TypeVariable * var = *it;
    const Type * value = subst(var);
    DASSERT_OBJ(value != NULL, var);
    if (!var->canBindTo(value)) {
      diag.fatal(loc) << "Type of expression " << value <<
          " incompatible with template parameter " << var << ":" << var->valueType();
      DASSERT(var->canBindTo(value));
    }

    if (!value->isSingular()) {
      /*if (singular) {
        diag.fatal(loc) << "Non-singular parameter '" << var << "' = '" << value << "'";
      }*/
    }

    // We might need to do some coercion here...
    paramValues.push_back(const_cast<Type *>(value));
  }

  switch (tdef->typeValue()->typeClass()) {
    case Type::NAddress:
      return AddressType::get(paramValues[0]);

    case Type::NArray:
      return NativeArrayType::get(TupleType::get(paramValues));

    case Type::FlexibleArray:
      return FlexibleArrayType::get(TupleType::get(paramValues));

    case Type::TypeLiteral:
      return TypeLiteralType::get(paramValues[0]);

    default:
      DFAIL("Invalid template type");
      break;
  }

  return NULL;
}

bool Template::canUnify(const TupleType * args) const {
  if (args->size() > typeParams_->size()) {
    return false;
  }
  for (size_t i = 0; i < args->size(); ++i) {
    if (!canUnify(typeParam(i), args->member(i))) {
      return false;
    }
  }
  return true;
}

bool Template::canUnify(const Type * param, const Type * value) const {
  value = dealias(value);
  switch (param->typeClass()) {
    case Type::TypeVar:
      return true;

    default:
      return TypeRelation::isEqual(param, value);
  }
}

/// -------------------------------------------------------------------
/// TemplateInstance

TemplateInstance::TemplateInstance(Defn * templateDefn, const TupleType * templateArgs,
    const TupleType * patternVarValues)
  : value_(NULL)
  , templateDefn_(templateDefn)
  , typeArgs_(templateArgs)
  , patternVarValues_(patternVarValues)
  , parentScope_(templateDefn->definingScope())
  , lessSpecialized_(NULL)
{
}

const Type * TemplateInstance::typeArg(int index) const {
  return typeArgs_->member(index);
}

void TemplateInstance::addMember(Defn * d) {
  DASSERT(d->storageClass() != Storage_Local);
  DASSERT(d->definingScope() == NULL);
  paramDefns_.add(d);
  d->setDefiningScope(this);
}

bool TemplateInstance::lookupMember(StringRef ident, DefnList & defs, bool inherit) const {
  const SymbolTable::Entry * entry = paramDefns_.findSymbol(ident);
  if (entry != NULL) {
    defs.append(entry->begin(), entry->end());
    return true;
  }

  return false;
}

Defn * TemplateInstance::findLessSpecializedInstance() {
  if (lessSpecialized_ != NULL) {
    return lessSpecialized_ != value_ ? lessSpecialized_ : NULL;
  }

  Template * tm = templateDefn_->templateSignature();
  TypeVarMap varValues;
  DASSERT(patternVarValues_->size() == tm->patternVarCount());
  bool canMerge = false;
  for (size_t i = 0; i < patternVarValues_->size(); ++i) {
    const Type * type = patternVarValues_->member(i);
    if (type->typeClass() == Type::Class || type->typeClass() == Type::Interface) {
      type = Builtins::typeObject.get();
    }

    // TODO: Merge primitive types, addresses, etc.

    if (type != patternVarValues_->member(i)) {
      canMerge = true;
    }

    varValues[tm->patternVar(i)] = type;
  }

  if (canMerge) {
    lessSpecialized_ = tm->instantiate(
        value_->location(), varValues, Template::Singular | Template::NonScaffold);
    return lessSpecialized_;
  } else {
    lessSpecialized_ = value_;
    return NULL;
  }
}

void TemplateInstance::dumpHierarchy(bool full) const {
  StrFormatStream out;
  out << "[template-instance] ";
  paramDefns_.getDebugSummary(out);
  diag.writeLnIndent(out.str());
}

void TemplateInstance::format(FormatStream & out) const {
  Template * tm = templateDefn_->templateSignature();
  out << "[";
  if (tm && tm->isVariadic()) {
    int index = 0;
    for (TupleType::const_iterator it = typeArgs_->begin(); it != typeArgs_->end(); ++it, ++index) {
      const TupleType * vargs = NULL;
      if (const TypeVariable * tv = dyn_cast<TypeVariable>(tm->typeParam(index))) {
        if (tv->isVariadic()) {
          vargs = cast<TupleType>(*it);
          if (vargs->size() == 0) {
            break;
          }
        }
      }

      if (it != typeArgs_->begin()) {
        out << ",";
      }

      // Special formatting for variadic template params
      if (vargs != NULL) {
        vargs->formatMembers(out);
      } else {
        (*it)->format(out);
      }
    }
  } else {
    typeArgs_->formatMembers(out);
  }
  out << "]";
}

void TemplateInstance::trace() const {
  paramDefns_.trace();
  safeMark(value_);
  safeMark(typeArgs_);
  safeMark(templateDefn_);
  safeMark(patternVarValues_);
  safeMark(lessSpecialized_);
}

} // namespace Tart

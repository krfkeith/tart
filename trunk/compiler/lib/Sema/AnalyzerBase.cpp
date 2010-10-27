/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/NamespaceAnalyzer.h"
#include "tart/Sema/ClassAnalyzer.h"
#include "tart/Sema/EnumAnalyzer.h"
#include "tart/Sema/FunctionAnalyzer.h"
#include "tart/Sema/PropertyAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/SpCandidate.h"

#include "tart/CFG/Exprs.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/NamespaceDefn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/UnitType.h"
#include "tart/CFG/TypeLiteral.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Template.h"

#include "tart/Common/PackageMgr.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"

#include "tart/Objects/Builtins.h"

namespace tart {

llvm::cl::opt<std::string> AnalyzerBase::traceDef_("trace-def",
    llvm::cl::desc("Enable debugging messages for this definition"),
    llvm::cl::value_desc("defn-name"),
    llvm::cl::init("-"));

AnalyzerBase::AnalyzerBase(Module * mod, Scope * parent, Defn * subject,
    FunctionDefn * currentFunction)
  : module_(mod)
  , activeScope_(parent)
  , subject_(subject)
  , currentFunction_(currentFunction)
{
  DASSERT(activeScope_ != NULL);
}

AnalyzerBase::AnalyzerBase(const AnalyzerBase * parent)
  : module_(parent->module())
  , activeScope_(parent->activeScope())
  , subject_(parent->subject())
  , currentFunction_(parent->currentFunction())
{
  DASSERT(activeScope_ != NULL);
}

bool AnalyzerBase::isTraceEnabled(Defn * de) {
  return de != NULL && de->name() != NULL && traceDef_.getValue() == de->name();
}

bool AnalyzerBase::lookupName(ExprList & out, const ASTNode * ast, LookupOptions lookupOptions) {
  std::string path;
  lookupNameRecurse(out, ast, path, lookupOptions);
  return !out.empty();
}

// Here's how this works:
// If we return true then it means we found something.
// If we return false, but path is non-empty, it means we found nothing,
//    but there's a chance it might be a package reference.
// If we return false and path is empty, then it means that we found nothing,
//    and there's no hope of finding anything.
bool AnalyzerBase::lookupNameRecurse(ExprList & out, const ASTNode * ast, std::string & path,
    LookupOptions lookupOptions) {

  SLC & loc = ast->location();
  bool isAbsPath = (lookupOptions & LOOKUP_ABS_PATH) != 0;
  bool isRequired = (lookupOptions & LOOKUP_REQUIRED) != 0;
  bool doResolve = (lookupOptions & LOOKUP_NO_RESOLVE) == 0;
  if (ast->nodeType() == ASTNode::Id) {
    const ASTIdent * ident = static_cast<const ASTIdent *>(ast);
    const char * name = ident->value();
    if (!isAbsPath && activeScope_ != NULL && lookupIdent(out, name, loc)) {
      return true;
    }

    path.assign(name);
    if (importName(out, path, isAbsPath, loc)) {
      return true;
    }

    if (isRequired) {
      diag.error(ast) << "Undefined symbol '" << ast << "'";
      diag.info() << "Scopes searched:";
      dumpScopeHierarchy();
    }

    return false;
  } else if (ast->nodeType() == ASTNode::Member) {
    const ASTMemberRef * mref = static_cast<const ASTMemberRef *>(ast);
    const ASTNode * qual = mref->qualifier();
    ExprList lvals;
    if (lookupNameRecurse(lvals, qual, path, LookupOptions(lookupOptions & ~LOOKUP_REQUIRED))) {
      if (lvals.size() > 1) {
        diag.error(ast) << "Multiply defined symbol " << qual;
        diag.info() << "Found in scopes:";
        dumpScopeList(lvals);
        path.clear();
        return false;
      }

      Expr * context = lvals.front();
      if (findMemberOf(out, context, mref->memberName(), loc)) {
        return true;
      }

      path.clear();
      if (isRequired) {
        diag.error(ast) << "Undefined member '" << mref->memberName() << "'";
        diag.info() << "Scoped searched:";
        dumpScopeList(lvals);
      }

      return false;
    }

    if (!path.empty()) {
      path.push_back('.');
      path.append(mref->memberName());
      if (importName(out, path, isAbsPath, loc)) {
        return true;
      }
    }

    if (isRequired) {
      diag.error(qual) << "Undefined symbol '" << qual << "'";
      diag.info() << "Scopes searched:";
      dumpScopeHierarchy();
    }

    return false;
  } else if (ast->nodeType() == ASTNode::Specialize) {
    const ASTSpecialize * spec = static_cast<const ASTSpecialize *>(ast);
    ExprList lvals;
    if (!lookupNameRecurse(lvals, spec->templateExpr(), path,
        LookupOptions(lookupOptions | LOOKUP_REQUIRED))) {
      //DASSERT(diag.inRecovery());
      return false;
    }

    Expr * expr = specialize(loc, lvals, spec->args(), doResolve);
    if (expr == NULL) {
      diag.error(spec) << "No template found matching expression: " << spec->templateExpr();
      diag.info() << "Scoped searched:";
      dumpScopeList(lvals);
      return false;
    }

    out.push_back(expr);
    return true;
  } else {
    // It's not a name or anything like that.
    path.clear();

    // See if it's an expression.
    ExprAnalyzer ea(module_, activeScope_, subject(), currentFunction_);
    Expr * result = ea.reduceExpr(ast, NULL);
    if (!isErrorResult(result)) {
      out.push_back(result);
      return true;
    }

    //DASSERT(diag.inRecovery());
    return false;
  }
}

bool AnalyzerBase::lookupIdent(ExprList & out, const char * name, SLC & loc) {
  // Search the current active scopes.
  for (Scope * sc = activeScope_; sc != NULL; sc = sc->parentScope()) {
    if (findInScope(out, name, sc, sc->baseExpr(), loc, NO_PREFERENCE)) {
      return true;
    }
  }

  return false;
}

bool AnalyzerBase::findMemberOf(ExprList & out, Expr * context, const char * name, SLC & loc) {
  if (ScopeNameExpr * scopeName = dyn_cast<ScopeNameExpr>(context)) {
    if (Module * m = dyn_cast<Module>(scopeName->value())) {
      AnalyzerBase::analyzeDefn(m, Task_PrepMemberLookup);
      if (findInScope(out, name, m, NULL, loc, NO_PREFERENCE)) {
        return true;
      }
    } else if (NamespaceDefn * ns = dyn_cast<NamespaceDefn>(scopeName->value())) {
      analyzeNamespace(ns, Task_PrepMemberLookup);
      if (findInScope(out, name, &ns->memberScope(), NULL, loc, NO_PREFERENCE)) {
        return true;
      }
    }
  }

  if (TypeLiteralExpr * typeNameExpr = dyn_cast<TypeLiteralExpr>(context)) {
    // Look for static members.
    const Type * type = dealias(typeNameExpr->value());
    TypeDefn * typeDef = type->typeDefn();
    if (typeDef != NULL && type->memberScope() != NULL) {
      if (typeDef->isTemplate()) {
        return findStaticTemplateMember(out, typeDef, name, loc);
      }

      DASSERT_OBJ(typeNameExpr->isSingular(), typeDef);
      AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
      if (findInScope(out, name, type->memberScope(), context, loc, PREFER_STATIC)) {
        return true;
      }
    }
  } else if (context->type() != NULL) {
    if (!context->isSingular()) {
      // Member lookup requires types be singular.
      Expr * singularContext = ExprAnalyzer::inferTypes(subject_, context, NULL, false);
      if (singularContext == NULL) {
        diag.error(loc) << "Base expression '" << context << "' is ambiguous.";
        return false;
      }

      context = singularContext;
    }

    const Type * contextType = context->canonicalType();
    if (LValueExpr * lvalue = dyn_cast<LValueExpr>(context)) {
      const Type * type = inferType(lvalue->value());
      if (type == NULL) {
        return false;
      }

      contextType = type;
    }

    // If it's a native pointer, then do an implicit dereference.
    if (const AddressType * nptype = dyn_cast<AddressType>(contextType)) {
      contextType = nptype->typeParam(0);
    } else if (const UnionType * utype = dyn_cast<UnionType>(contextType)) {
      // See if an implicit conversion makes sense here.
      if (utype->isSingleOptionalType()) {
        const Type * toType = utype->getFirstNonVoidType();
        Expr * newContext = NULL;
        Conversion cn(context, &newContext, 0);
        if (toType->convert(cn) > Incompatible) {
          context = newContext;
          contextType = context->canonicalType();
        }
      }
    }

    TypeDefn * typeDef = contextType->typeDefn();
    if (typeDef != NULL && contextType->memberScope() != NULL) {
      if (contextType->isUnsizedIntType()) {
        diag.error(loc) << "Attempt to access member of integer of unknown size";
        return false;
      }

      DASSERT_OBJ(typeDef->isSingular(), typeDef);
      AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
      if (findInScope(out, name, contextType->memberScope(), context, loc, PREFER_INSTANCE)) {
        return true;
      }
    }
  }

  return false;
}

bool AnalyzerBase::findInScope(ExprList & out, const char * name, const Scope * scope,
    Expr * context, SLC & loc, MemberPreference pref) {
  DefnList defns;
  if (scope->lookupMember(name, defns, true)) {
    // If there's an exclusion in effect by storage class
    if (pref != NO_PREFERENCE) {
      bool hasStaticMembers = false;
      bool hasInstanceMembers = false;
      for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
        StorageClass sc = (*it)->storageClass();
        if (sc == Storage_Instance) {
          hasInstanceMembers = true;
        } else if (sc == Storage_Static) {
          hasStaticMembers = true;
        }
      }

      if (hasStaticMembers && hasInstanceMembers) {
        StorageClass scToRemove;
        if (pref == PREFER_INSTANCE) {
          scToRemove = Storage_Static;
        } else {
          scToRemove = Storage_Instance;
        }
        for (DefnList::iterator it = defns.begin(); it != defns.end();) {
          if ((*it)->storageClass() == scToRemove) {
            it = defns.erase(it);
          } else {
            ++it;
          }
        }
      }
    }

    return getDefnListAsExprList(loc, defns, context, out);
  }

  return false;
}

bool AnalyzerBase::findStaticTemplateMember(ExprList & out, TypeDefn * typeDef, const char * name,
    SLC & loc) {
  DefnList defns;
  if (lookupTemplateMember(defns, typeDef, name, loc)) {
    int numStaticDefns = 0;
    for (DefnList::const_iterator it = defns.begin(); it != defns.end(); ++it) {
      Defn * de = *it;
      if (de->storageClass() == Storage_Static) {
        ++numStaticDefns;
        out.push_back(getDefnAsExpr(de, NULL, loc));
      }
    }

    if (numStaticDefns > 0) {
      return true;
    }

    if (defns.size() > 0) {
      diag.error(loc) << "Invalid reference to non-static member '" << name << "' of type '" <<
          typeDef << "'.";
    }

    return false;
  }

  return false;
}

bool AnalyzerBase::lookupTemplateMember(DefnList & out, TypeDefn * typeDef, const char * name,
    SLC & loc) {
  DASSERT(typeDef->isTemplate());
  AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
  if (CompositeType * ctype = dyn_cast<CompositeType>(typeDef->typeValue())) {
    if (ctype->memberScope()->lookupMember(name, out, false)) {
      return true;
    }
  }

  return false;
}

Expr * AnalyzerBase::specialize(SLC & loc, const ExprList & exprs, const ASTNodeList & args,
    bool inferArgTypes) {
  ConstTypeList argList; // Template args, not function args.
  bool isSingularArgList = true;  // True if all args are fully resolved.

  // Resolve all the arguments. Note that we don't support type inference on template args,
  // so the resolution is relatively straightforward.
  ExprAnalyzer ea(module_, activeScope_, subject(), currentFunction_);
  for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
    Expr * cb = ea.reduceTemplateArgExpr(*it, inferArgTypes);
    if (isErrorResult(cb)) {
      return NULL;
    }

    // Handle the case of a type argument that is a tuple type.
    if (TupleCtorExpr * tc = dyn_cast<TupleCtorExpr>(cb)) {
      const Type * ttype = getTupleTypesFromTupleExpr(cb);
      if (ttype != NULL) {
        DASSERT(!inferArgTypes || ttype->isSingular());
        cb = new TypeLiteralExpr(cb->location(), ttype);
      }
    }

    const Type * typeArg = NULL;
    if (TypeLiteralExpr * ctype = dyn_cast<TypeLiteralExpr>(cb)) {
      typeArg = dealias(ctype->value());
      if (TypeDefn * tdef = typeArg->typeDefn()) {
        typeArg = tdef->typeValue();
      }
    }

    if (typeArg == NULL) {
      if (!isa<ConstantExpr>(cb)) {
        diag.fatal(cb) << "Not a constant expression: " << *it;
      }

      typeArg = UnitType::get(cast<ConstantExpr>(cb));
    }

    if (!cb->isSingular()) {
      isSingularArgList = false;
    }

    argList.push_back(typeArg);
  }

  return specialize(loc, exprs, TupleType::get(argList));
}

Expr * AnalyzerBase::specialize(SLC & loc, const ExprList & exprs, TupleType * typeArgs) {
  // Examine all of the possible candidates for specialization.
  SpCandidateSet candidates;
  for (ExprList::const_iterator it = exprs.begin(); it != exprs.end(); ++it) {
    if (TypeLiteralExpr * tref = dyn_cast<TypeLiteralExpr>(*it)) {
      const Type * type = dealias(tref->value());
      TypeDefn * typeDefn = type->typeDefn();
      if (typeDefn != NULL) {
        if (typeDefn->isTemplate() || typeDefn->isTemplateInstance()) {
          addSpecCandidate(loc, candidates, NULL, typeDefn, typeArgs);
        }
      } else if (const AddressType * np = dyn_cast<AddressType>(type)) {
        addSpecCandidate(loc, candidates, NULL, &AddressType::typedefn, typeArgs);
      } else if (const NativeArrayType * np = dyn_cast<NativeArrayType>(type)) {
        addSpecCandidate(loc, candidates, NULL, &NativeArrayType::typedefn, typeArgs);
      } else if (const FlexibleArrayType * np = dyn_cast<FlexibleArrayType>(type)) {
        addSpecCandidate(loc, candidates, NULL, &FlexibleArrayType::typedefn, typeArgs);
      } else if (const TypeLiteralType * np = dyn_cast<TypeLiteralType>(type)) {
        addSpecCandidate(loc, candidates, NULL, &TypeLiteralType::typedefn, typeArgs);
      }
    } else if (LValueExpr * lv = dyn_cast<LValueExpr>(*it)) {
      ValueDefn * val = lv->value();
      if (val->isTemplate() || val->isTemplateInstance()) {
        addSpecCandidate(loc, candidates, lv->base(), val, typeArgs);
      }
    }
  }

  if (candidates.empty()) {
    diag.error(loc) << "No templates found which match template arguments [" << typeArgs << "]";
    for (ExprList::const_iterator it = exprs.begin(); it != exprs.end(); ++it) {
      diag.info(*it) << Format_Type << "candidate: " << *it;
    }

    //ea.dumpScopeHierarchy();
    return NULL;
  }

  if (candidates.size() > 1) {
    return new SpecializeExpr(loc, candidates, typeArgs);
  }

  // TODO: Do template overload resolution.
  // TODO: Use parameter assignments.
  SpCandidate * sp = *candidates.begin();
  Defn * defn = sp->def();
  TemplateSignature * tsig = defn->templateSignature();
  if (TypeDefn * typeDefn = dyn_cast<TypeDefn>(defn)) {
    Type * type = tsig->instantiateType(loc, sp->env());
    if (type != NULL) {
      return new TypeLiteralExpr(loc, type);
    }
  }

  defn = tsig->instantiate(loc, sp->env());
  if (defn != NULL) {
    return getDefnAsExpr(defn, sp->base(), loc);
  }

  DFAIL("Bad state");
}

void AnalyzerBase::addSpecCandidate(SLC & loc, SpCandidateSet & spcs, Expr * base, Defn * defn,
    TupleType * args) {
  if (defn->isTemplate()) {
    DefnAnalyzer::analyzeTemplateSignature(defn);
    const TemplateSignature * tsig = defn->templateSignature();
    if (args->size() >= tsig->numRequiredArgs() && args->size() <= tsig->typeParams()->size()) {
      // Attempt unification of type variables with template args.
      SpCandidate * spc = new SpCandidate(base, defn, args);
      SourceContext candidateSite(defn->location(), NULL, defn, Format_Type);
      if (spc->unify(&candidateSite)) {
        spcs.insert(spc);
      }
    }
  } else if (defn->isTemplateInstance()) {
    DefnList defns;
    defn->templateInstance()->parentScope()->lookupMember(defn->name(), defns, true);
    for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
      Defn * d = *it;
      if (d->isTemplate()) {
        addSpecCandidate(loc, spcs, base, d, args);
      }
    }
  } else {
    DFAIL("Not a template");
  }
}

bool AnalyzerBase::importName(ExprList & out, const std::string & path, bool absPath, SLC & loc) {
  if (module_ != NULL) {
    DefnList defns;
    if (module_->import(path.c_str(), defns, absPath)) {
      return getDefnListAsExprList(loc, defns, NULL, out);
    }
  }

  //Module * m = PackageMgr::get().getModuleForImportPath(path);
  //if (m != NULL) {
  //  out.push_back(new ScopeNameExpr(loc, m));
  //  return true;
  //}

  return false;
}

bool AnalyzerBase::getDefnListAsExprList(SLC & loc, DefnList & defs, Expr * context,
    ExprList & out) {
  for (DefnList::iterator it = defs.begin(); it != defs.end(); ++it) {
    if (ExplicitImportDefn * imp = dyn_cast<ExplicitImportDefn>(*it)) {
      // TODO: Should we allow mixing of import defns with non-imports of
      // the same name?
      ExprList & importedDefns = imp->importValues();
      out.append(importedDefns.begin(), importedDefns.end());
    } else {
      out.push_back(getDefnAsExpr(*it, context, loc));
    }
  }

  return !out.empty();
}

Expr * AnalyzerBase::getDefnAsExpr(Defn * de, Expr * context, SLC & loc) {
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(de)) {
    if (tdef->typeValue()->typeClass() == Type::Alias) {
      analyzeTypeDefn(tdef, Task_PrepTypeComparison);
    }
    return tdef->asExpr();
  } else if (ValueDefn * vdef = dyn_cast<ValueDefn>(de)) {
    if (vdef->storageClass() == Storage_Instance && context == NULL) {
      diag.fatal(loc) << "Cannot access non-static member '" <<
          vdef->name() << "' from static method.";
    }

    analyzeDefn(vdef, Task_PrepTypeComparison);
    LValueExpr * result = LValueExpr::get(loc, context, vdef);

    // If it's a variadic parameter, then the actual type is an array of the declared type.
    if (ParameterDefn * param = dyn_cast<ParameterDefn>(vdef)) {
      DASSERT_OBJ(param->internalType() != NULL, param);
      result->setType(param->internalType());
    }

    return result;
  } else if (NamespaceDefn * ns = dyn_cast<NamespaceDefn>(de)) {
    return new ScopeNameExpr(loc, ns);
  } else if (Module * m = dyn_cast<Module>(de)) {
    return new ScopeNameExpr(loc, m);
  } else {
    diag.fatal(de) << Format_Verbose << de;
    DFAIL("IllegalState");
  }
}

bool AnalyzerBase::getTypesFromExprs(SLC & loc, ExprList & in, TypeList & out) {
  int numNonTypes = 0;
  for (ExprList::iterator it = in.begin(); it != in.end(); ++it) {
    if (TypeLiteralExpr * tle = dyn_cast<TypeLiteralExpr>(*it)) {
      out.push_back(const_cast<Type *>(tle->value()));
    } else {
      numNonTypes++;
    }
  }

  if (out.empty()) {
    return false;
  }

  if (numNonTypes > 0) {
    diag.fatal(loc) << "Incompatible definitions for '" << out.front() << "'";
    for (ExprList::iterator it = in.begin(); it != in.end(); ++it) {
      diag.info(*it) << *it;
    }
  }

  return true;
}

const Type * AnalyzerBase::getTupleTypesFromTupleExpr(Expr * in) {
  if (TupleCtorExpr * tctor = dyn_cast<TupleCtorExpr>(in)) {
    // Check to see if all members are type literals.
    ConstTypeList typeMembers;
    for (ExprList::const_iterator it = tctor->args().begin(); it != tctor->args().end(); ++it) {
      TypeLiteralExpr * tl = dyn_cast<TypeLiteralExpr>(*it);
      if (tl == NULL) {
        return NULL;
      }

      typeMembers.push_back(tl->value());

    }

    return TupleType::get(typeMembers);
  }

  return NULL;
}

const Type * AnalyzerBase::inferType(ValueDefn * valueDef) {
  if (valueDef->type() == NULL) {
    if (!analyzeDefn(valueDef, Task_PrepTypeComparison)) {
      return NULL;
    }
  }

  if (valueDef->type() != NULL && valueDef->type()->isSingular()) {
    if (ParameterDefn * param = dyn_cast<ParameterDefn>(valueDef)) {
      return param->internalType();
    }

    return valueDef->type();
  }

  if (valueDef->type() != NULL) {
    diag.info(valueDef) << valueDef << ":" << valueDef->type();
  } else {
    diag.info(valueDef) << valueDef;
  }

  DFAIL("Failed to determine type of value.");
}

bool AnalyzerBase::analyzeType(const Type * in, AnalysisTask task) {
  if (in != NULL) {
    TypeDefn * de = in->typeDefn();
    if (de != NULL) {
      return analyzeTypeDefn(de, task);
    }

    switch (in->typeClass()) {
      case Type::Function: {
        const FunctionType * ftype = static_cast<const FunctionType *>(in);
        const ParameterList & params = ftype->params();
        for(ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
          analyzeType((*it)->type(), task);
        }

        if (ftype->selfParam() != NULL) {
          analyzeType(ftype->selfParam()->type(), task);
        }

        if (!ftype->returnType()->isVoidType()) {
          analyzeType(ftype->returnType(), task);
        }

        size_t numTypes = in->numTypeParams();
        for (size_t i = 0; i < numTypes; ++i) {
          analyzeType(in->typeParam(i), task);
        }

        break;
      }

      case Type::NAddress:
      case Type::NArray:
      case Type::FlexibleArray:
      case Type::Union:
      case Type::Tuple: {
        size_t numTypes = in->numTypeParams();
        for (size_t i = 0; i < numTypes; ++i) {
          analyzeType(in->typeParam(i), task);
        }

        break;
      }

      default:
        break;
    }
  }

  return true;
}

bool AnalyzerBase::analyzeModule(Module * mod) {
  DefnAnalyzer da(mod, mod, mod, NULL);
  return da.analyzeModule();
}

bool AnalyzerBase::analyzeFunction(FunctionDefn * fn, AnalysisTask task) {
  return FunctionAnalyzer(fn).analyze(task);
}

bool AnalyzerBase::analyzeVariable(VariableDefn * var, AnalysisTask task) {
  return VarAnalyzer(var).analyze(task);
}

bool AnalyzerBase::analyzeProperty(PropertyDefn * prop, AnalysisTask task) {
  return PropertyAnalyzer(prop).analyze(task);
}

bool AnalyzerBase::analyzeNamespace(NamespaceDefn * ns, AnalysisTask task) {
  return NamespaceAnalyzer(ns).analyze(task);
}

bool AnalyzerBase::analyzeCompletely(Defn * in) {
  return analyzeDefn(in, Task_PrepCodeGeneration);
}

bool AnalyzerBase::analyzeDefn(Defn * in, AnalysisTask task) {
  switch (in->defnType()) {
    case Defn::Typedef:
      return analyzeTypeDefn(static_cast<TypeDefn *>(in), task);

    case Defn::Namespace:
      return analyzeNamespace(static_cast<NamespaceDefn *>(in), task);

    case Defn::Var:
    case Defn::Let:
    case Defn::MacroArg:
    case Defn::Parameter: {
      return analyzeVariable(static_cast<VariableDefn *>(in), task);
    }

    case Defn::Property:
    case Defn::Indexer: {
      return analyzeProperty(static_cast<PropertyDefn *>(in), task);
    }

    case Defn::Function:
    case Defn::Macro: {
      return analyzeFunction(static_cast<FunctionDefn *>(in), task);
    }

    case Defn::Mod: {
      Module * m = static_cast<Module *>(in);
      return DefnAnalyzer(m, m, m, NULL).analyzeModule();
    }

    case Defn::ExplicitImport:
      return true;

    case Defn::DefnTypeCount:
    default:
      DFAIL("IllegalState");
  }
}

bool AnalyzerBase::analyzeTypeDefn(TypeDefn * in, AnalysisTask task) {
  Type * type = in->typeValue();
  switch (type->typeClass()) {
    case Type::Primitive:
      return true;

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      return ClassAnalyzer(in).analyze(task);
    }

    case Type::Enum:
      return EnumAnalyzer(in).analyze(task);

    case Type::NAddress:
    case Type::NArray:
    case Type::FlexibleArray: {
      analyzeType(type->typeParam(0), task);
      return true;
    }

    case Type::Alias: {
      TypeAlias * ta = cast<TypeAlias>(type);
      if (ta->value() == NULL) {
        Type * targetType = TypeAnalyzer(in->module(), in->definingScope())
            .typeFromAST(cast<ASTTypeDecl>(in->ast())->bases().front());
        if (isErrorResult(targetType)) {
          return false;
        }

        ta->setValue(targetType);
      }

      return true;
    }

    case Type::TypeVar:
      return true;

    default:
      diag.debug(in) << in;
      DFAIL("IllegalState");
  }
}

CompositeType * AnalyzerBase::getArrayTypeForElement(const Type * elementType) {
  // Look up the array class
  TemplateSignature * arrayTemplate = Builtins::typeArray->typeDefn()->templateSignature();

  // Do analysis on template if needed.
  if (arrayTemplate->ast() != NULL) {
    DefnAnalyzer da(&Builtins::module, &Builtins::module, &Builtins::module, NULL);
    da.analyzeTemplateSignature(Builtins::typeArray->typeDefn());
  }

  DASSERT_OBJ(arrayTemplate->paramScope().count() == 1, elementType);

  // Special case for when the elementType is Array.ElementType
  if (elementType == arrayTemplate->typeParam(0)) {
    return Builtins::typeArray.get();
  }

  BindingEnv arrayEnv;
  arrayEnv.addSubstitution(arrayTemplate->patternVar(0), elementType);
  return cast<CompositeType>(cast<TypeDefn>(
      arrayTemplate->instantiate(SourceLocation(), arrayEnv))->typeValue());
}

ArrayLiteralExpr * AnalyzerBase::createArrayLiteral(SLC & loc, const Type * elementType) {
  CompositeType * arrayType = getArrayTypeForElement(elementType);
  ArrayLiteralExpr * array = new ArrayLiteralExpr(loc);
  array->setType(arrayType);

  return array;
}

// Determine if the target is able to be accessed from the current source defn.
void AnalyzerBase::checkAccess(const SourceLocation & loc, Defn * target) {
  if (!canAccess(subject_, target)) {
    diag.fatal(loc) << "'" << target->name() << "' is " <<
        (target->visibility() == Protected ? "protected." : "private.");
  }
}

void AnalyzerBase::checkAccess(const SourceLocation & loc, Defn * source, Defn * target) {
  if (!canAccess(source, target)) {
    diag.fatal(loc) << "'" << target->name() << "' is " <<
        (target->visibility() == Protected ? "protected." : "private.");
  }
}

/** The set of definition types that represent namespaces that can grant access to their
    members. */
static const DefnTypeSet ACCESS_CONTEXTS = DefnTypeSet::of(
    Defn::Typedef, Defn::Namespace, Defn::Mod);

bool AnalyzerBase::canAccess(Defn * source, Defn * target) {
  if (target->storageClass() == Storage_Local || target->defnType() == Defn::Parameter) {
    return true;
  }

  if (target->visibility() != Public) {
    // The destination context is the scope that defines who can see the visible symbol.
    // This is never the symbol itself - it is the next outer name-space-like symbol.
    Defn * dstContext = target;
    if ((dstContext == target || !ACCESS_CONTEXTS.contains(dstContext->defnType()))
        && dstContext->parentDefn() != NULL) {
      dstContext = dstContext->parentDefn();
    }

    if (source != NULL) {
      for (Defn * de = source; de != NULL; de = de->parentDefn()) {
        if (de == dstContext) {
          return true;
        }
      }

      if (target->visibility() == Protected) {
        if (TypeDefn * dstTypeDef = dyn_cast<TypeDefn>(dstContext)) {
          if (CompositeType * dstType = dyn_cast_or_null<CompositeType>(dstTypeDef->typeValue())) {
            for (Defn * de = source; de != NULL; de = de->parentDefn()) {
              if (TypeDefn * srcTypeDef = dyn_cast<TypeDefn>(de)) {
                if (CompositeType * srcType =
                    dyn_cast_or_null<CompositeType>(srcTypeDef->typeValue())) {
                  if (srcType->isSubclassOf(dstType)) {
                    return true;
                  }
                }
              }
            }
          }
        }
      }
    }

    return false;
  }

  return true;
}

void AnalyzerBase::dumpScopeHierarchy() {
  int level = diag.getIndentLevel();
  for (Scope * s = activeScope_; s != NULL; s = s->parentScope()) {
    s->dumpHierarchy(true);
    diag.indent();
  }

  diag.setIndentLevel(level);
  diag.recovered();
}

void AnalyzerBase::dumpScopeList(const ExprList & lvals) {
  diag.indent();
  for (ExprList::const_iterator it = lvals.begin(); it != lvals.end(); ++it) {
    const Expr * ex = *it;
    if (const ScopeNameExpr * scopeName = dyn_cast<ScopeNameExpr>(ex)) {
      diag.info(ex) << ex;
    } else if (const TypeLiteralExpr * typeNameExpr = dyn_cast<TypeLiteralExpr>(ex)) {
      diag.info(ex) << ex;
    } else if (ex->type() != NULL) {
      diag.info(ex) << ex->type();
    } else {
      diag.info(ex) << ex;
    }
  }

  diag.unindent();
  diag.recovered();
}

TaskInProgress * TaskInProgress::tasks_ = NULL;

void TaskInProgress::report() {
  diag.debug() << "Current analysis tasks in progress:";
  for (TaskInProgress * task = tasks_; task != NULL; task = task->next_) {
    const char * taskName = "Unknown";
    switch (task->task_) {
      case Task_PrepTypeComparison: taskName = "Task_PrepTypeComparison"; break;
      case Task_PrepMemberLookup: taskName = "Task_PrepMemberLookup"; break;
      case Task_PrepConstruction: taskName = "Task_PrepConstruction"; break;
      case Task_PrepConversion: taskName = "Task_PrepConversion"; break;
      case Task_PrepEvaluation: taskName = "Task_PrepEvaluation"; break;
      case Task_PrepTypeGeneration: taskName = "Task_PrepTypeGeneration"; break;
      case Task_PrepCodeGeneration: taskName = "Task_PrepCodeGeneration"; break;
      case Task_PrepReflection: taskName = "Task_PrepReflection"; break;
    }

    diag.debug() << Format_QualifiedName << "  " << taskName << ": " << task->defn_;
  }
}

//diag.fatal(this) << "Infinite recursion during " << pass << " of " << this;
void reportAnalysisCycle() {
  diag.fatal() << "Infinite recursion during analysis";
  TaskInProgress::report();
  DFAIL("ABORT");
}

} // namespace tart

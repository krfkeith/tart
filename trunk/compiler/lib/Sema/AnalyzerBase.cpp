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

#include "tart/Expr/Exprs.h"

#include "tart/Defn/Defn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/NamespaceDefn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Module.h"
#include "tart/Defn/Template.h"

#include "tart/Type/FunctionType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/UnitType.h"
#include "tart/Type/TypeLiteral.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Common/PackageMgr.h"
#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

namespace tart {

llvm::cl::opt<std::string> AnalyzerBase::traceDef_("trace-def",
    llvm::cl::desc("Enable debugging messages for this definition"),
    llvm::cl::value_desc("defn-name"),
    llvm::cl::init("-"));

class RecursionGuard {
public:
  RecursionGuard() {
    ++_recursionLevel;
    if (_recursionLevel > 1000) {
      DFAIL("Recursion limit exceeded!");
    }
  }

  ~RecursionGuard() {
    --_recursionLevel;
  }

private:
  static uint32_t _recursionLevel;
};

uint32_t RecursionGuard::_recursionLevel = 0;

void LookupResult::format(FormatStream & out) const {
  if (defn_) {
    defn_->format(out);
  } else if (expr_) {
    expr_->format(out);
  } else {
    out << "<Bad Lookup Result>";
  }
}

AnalyzerBase::AnalyzerBase(Module * mod, Scope * activeScope, Defn * subject,
    FunctionDefn * currentFunction)
  : module_(mod)
  , activeScope_(activeScope)
  , subject_(subject)
  , currentFunction_(currentFunction)
{
  DASSERT(activeScope_ != NULL);
}

bool AnalyzerBase::isTraceEnabled(Defn * de) {
  return de != NULL && traceDef_.getValue() == de->name();
}

bool AnalyzerBase::lookupName(
    LookupResults & out, const ASTNode * ast, LookupOptions lookupOptions) {
  llvm::SmallString<0> path;
  lookupNameRecurse(out, ast, path, lookupOptions);
  return !out.empty();
}

// Here's how this works:
// If we return true then it means we found something.
// If we return false, but path is non-empty, it means we found nothing,
//    but there's a chance it might be a package reference.
// If we return false and path is empty, then it means that we found nothing,
//    and there's no hope of finding anything.
bool AnalyzerBase::lookupNameRecurse(LookupResults & out, const ASTNode * ast,
    llvm::SmallString<0> & path, LookupOptions lookupOptions) {
  SLC & loc = ast->location();
  bool isAbsPath = (lookupOptions & LOOKUP_ABS_PATH) != 0;
  bool isRequired = (lookupOptions & LOOKUP_REQUIRED) != 0;
  bool doResolve = (lookupOptions & LOOKUP_NO_RESOLVE) == 0;
  if (ast->nodeType() == ASTNode::Id) {
    const ASTIdent * ident = static_cast<const ASTIdent *>(ast);
    StringRef name = ident->value();
    if (!isAbsPath && activeScope_ != NULL && lookupIdent(out, name, loc)) {
      return true;
    }

    path = name;
    if (importName(out, path, isAbsPath, loc)) {
      return true;
    }

    if (isRequired) {
      diag.error(ast) << "Undefined symbol '" << ast << "'";
      if (diag.enableVerboseErrors()) {
        // TODO: Dump macro context.
        diag.info() << "Scopes searched:";
        dumpScopeHierarchy();
      }
    }

    return false;
  } else if (ast->nodeType() == ASTNode::Member) {
    const ASTMemberRef * mref = static_cast<const ASTMemberRef *>(ast);
    const ASTNode * qual = mref->qualifier();
    LookupResults lvals;
    if (lookupNameRecurse(lvals, qual, path, LookupOptions(lookupOptions & ~LOOKUP_REQUIRED))) {
      if (lvals.size() > 1) {
        diag.error(ast) << "Multiply defined symbol " << qual;
        if (diag.enableVerboseErrors()) {
          // TODO: Dump macro context.
          diag.info() << "Found in scopes:";
          dumpScopeList(lvals);
        }
        path.clear();
        return false;
      }

      if (findMemberOf(out, lvals.front(), mref->memberName(), loc)) {
        return true;
      }

      path.clear();
      if (isRequired) {
        diag.error(ast) << "Undefined member '" << mref->memberName() << "'";
        if (diag.enableVerboseErrors()) {
          // TODO: Dump macro & context.
          diag.info() << "Scopes searched:";
          dumpScopeList(lvals);
        }
      }

      return false;
    }

    if (!path.empty()) {
      path.push_back('.');
      path += mref->memberName();
      if (importName(out, path, isAbsPath, loc)) {
        return true;
      }
    }

    if (isRequired) {
#if 1
      lookupNameRecurse(lvals, qual, path, LookupOptions(lookupOptions | LOOKUP_REQUIRED));
#endif
      diag.error(qual) << "Undefined symbol '" << qual << "'";
      if (diag.enableVerboseErrors()) {
        diag.info() << "Scopes searched:";
        dumpScopeHierarchy();
      }
    }

    return false;
  } else if (ast->nodeType() == ASTNode::Specialize) {
    const ASTSpecialize * spec = static_cast<const ASTSpecialize *>(ast);
    LookupResults lvals;
    if (!lookupNameRecurse(lvals, spec->templateExpr(), path,
        LookupOptions(lookupOptions | LOOKUP_REQUIRED))) {
      return false;
    }

    Expr * expr = specialize(loc, lvals, spec->args(), doResolve);
    if (expr == NULL) {
      diag.error(spec) << "No template found matching expression: " << spec->templateExpr();
      diag.info() << "Scoped searched:";
      dumpScopeList(lvals);
      return false;
    }

    out.push_back(LookupResult(NULL, expr));
    return true;
  } else if (ast->nodeType() == ASTNode::QName) {
    // A fully qualified, absolute path.
    const ASTIdent * ident = static_cast<const ASTIdent *>(ast);
    StringRef name = ident->value();
    size_t colon = name.rfind(':');
    if (colon != name.npos) {
      StringRef modName = name.substr(0, colon);
      StringRef relName = name.substr(colon + 1, name.npos);
      if (lookupNameInModule(out, modName, relName, loc)) {
        return true;
      }
    } else if (lookupQName(out, ident->value(), loc)) {
      return true;
    }

    diag.error(ast) << "Undefined symbol '" << ast << "'";
    return false;
  } else if (ast->nodeType() == ASTNode::BuiltIn) {
    path.clear();
    out.push_back(static_cast<const ASTBuiltIn *>(ast)->value());
    return true;
  } else {
    // It's not a name or anything like that.
    path.clear();

    // See if it's an expression.
    ExprAnalyzer ea(this, currentFunction_);
    Expr * result = ea.reduceExpr(ast, NULL);
    if (!isErrorResult(result)) {
      out.push_back(LookupResult(NULL, result));
      return true;
    }

    return false;
  }
}

bool AnalyzerBase::lookupIdent(LookupResults & out, StringRef name, SLC & loc) {
  // Search the current active scopes.
  for (Scope * sc = activeScope_; sc != NULL; sc = sc->parentScope()) {
    if (findInScope(out, name, sc, sc->baseExpr(), loc, NO_PREFERENCE)) {
      return true;
    }
  }

  return false;
}

bool AnalyzerBase::lookupQName(LookupResults & out, StringRef name, SLC & loc) {
  if (importName(out, name, true, loc)) {
    return true;
  }

  size_t dot = name.rfind('.');
  if (dot == name.npos) {
    // Try builtin definitions.
    return findInScope(out, name, &Builtins::module, NULL, loc, NO_PREFERENCE);
  }

  LookupResults lvals;
  StringRef qual = name.substr(0, dot);
  if (lookupQName(lvals, qual, loc)) {
    if (lvals.size() > 1) {
      diag.error(loc) << "Multiply defined symbol " << qual;
      return false;
    }

    if (findMemberOf(out, lvals.front(), name.substr(dot + 1, name.npos), loc)) {
      return true;
    }
  }

  return false;
}

bool AnalyzerBase::lookupNameInModule(LookupResults & out, StringRef modName,
    StringRef name, SLC & loc) {
  DefnList primaries;
  if (module_->import(modName, primaries, true)) {
    DASSERT(!primaries.empty());
    Module * m = primaries.front()->module();
    size_t dot = name.rfind('.');
    if (dot == name.npos) {
      //m->createMembers();
      return findInScope(out, name, m, NULL, loc, NO_PREFERENCE);
    } else {
      DFAIL("Implement");
    }
  }

  return false;
}

bool AnalyzerBase::findMemberOf(
    LookupResults & out, LookupResult context, StringRef name, SLC & loc) {
  if (context.defn() != NULL) {
    if (Module * m = dyn_cast<Module>(context.defn())) {
      if (findInScope(out, name, m, NULL, loc, NO_PREFERENCE)) {
        return true;
      }
    } else if (NamespaceDefn * ns = dyn_cast<NamespaceDefn>(context.defn())) {
      analyzeNamespace(ns, Task_PrepMemberLookup);
      if (findInScope(out, name, &ns->memberScope(), NULL, loc, NO_PREFERENCE)) {
        return true;
      }
    } else if (TypeDefn * tdef = dyn_cast<TypeDefn>(context.defn())) {
      // Look for static members.
      return findStaticTypeMember(out, tdef->typePtr(), context.expr(), name, loc);
    } else if (ValueDefn * value = dyn_cast<ValueDefn>(context.defn())) {
      ExprAnalyzer ea(this, currentFunction_);
      Expr * base = ea.getLValue(loc, value, context.expr(), false);
      return findInstanceMember(out, base, name, loc);
    }
  } else if (context.expr() != NULL) {
    if (TypeLiteralExpr * tl = dyn_cast<TypeLiteralExpr>(context.expr())) {
      return findStaticTypeMember(out, tl->value().unqualified(), context.expr(), name, loc);
    } else {
      return findInstanceMember(out, context.expr(), name, loc);
    }
  }

  return false;
}

bool AnalyzerBase::findStaticTypeMember(
    LookupResults & out, const Type * type, Expr * base, StringRef name, SLC & loc) {
  // Look for static members.
  type = dealias(type);
  TypeDefn * typeDef = type->typeDefn();
  if (typeDef != NULL && type->memberScope() != NULL) {
    if (typeDef->isTemplate()) {
      return findStaticTemplateMember(out, typeDef, name, loc);
    }

    DASSERT_OBJ(typeDef->isSingular(), typeDef);
    AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
    if (findInScope(out, name, type->memberScope(), base, loc, PREFER_STATIC)) {
      return true;
    }
  }
  return false;
}

bool AnalyzerBase::findInstanceMember(LookupResults & out, Expr * base, StringRef name, SLC & loc) {
  if (base->type()) {
    if (!base->isSingular()) {
      // Member lookup requires types be singular.
      ExprAnalyzer ea(this, currentFunction_);
      Expr * singularContext = ea.inferTypes(subject_, base, NULL, false);
      if (singularContext == NULL) {
        diag.error(loc) << "Base expression '" << base << "' is ambiguous.";
        return false;
      }

      base = singularContext;
    }

    QualifiedType baseType = base->canonicalType();
    if (LValueExpr * lvalue = dyn_cast<LValueExpr>(base)) {
      QualifiedType type = inferType(lvalue->value());
      if (!type) {
        return false;
      }

      baseType = dealias(type);
    }

    // If it's a native pointer, then do an implicit dereference.
    if (const AddressType * nptype = dyn_cast<AddressType>(baseType.unqualified())) {
      baseType = nptype->typeParam(0).type();
    } else if (const UnionType * utype = dyn_cast<UnionType>(baseType.unqualified())) {
      // See if an implicit conversion makes sense here.
      if (utype->isSingleOptionalType()) {
        const Type * toType = utype->getFirstNonVoidType();
        Expr * newContext = NULL;
        if (TypeConversion::convert(base, toType, &newContext) > Incompatible) {
          base = newContext;
          baseType = base->canonicalType();
        }
      }
    }

    TypeDefn * typeDef = baseType->typeDefn();
    if (typeDef != NULL && baseType->memberScope() != NULL) {
      if (baseType->isUnsizedIntType()) {
        diag.error(loc) << "Attempt to access member of integer of unknown size";
        return false;
      }

      DASSERT_OBJ(typeDef->isSingular(), typeDef);
      AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
      return findInScope(out, name, baseType->memberScope(), base, loc, PREFER_INSTANCE);
    }
  }

  return false;
}

bool AnalyzerBase::findInScope(LookupResults & out, StringRef name, const Scope * scope,
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

    return defnListToLookupResults(defns, context, out);
  }

  return false;
}

bool AnalyzerBase::findStaticTemplateMember(LookupResults & out, TypeDefn * typeDef,
    StringRef name, SLC & loc) {
  DefnList defns;
  if (lookupTemplateMember(defns, typeDef, name, loc)) {
    int numStaticDefns = 0;
    for (DefnList::const_iterator it = defns.begin(); it != defns.end(); ++it) {
      Defn * de = *it;
      if (de->storageClass() == Storage_Static) {
        ++numStaticDefns;
        out.push_back(de);
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

bool AnalyzerBase::lookupTemplateMember(DefnList & out, TypeDefn * typeDef, StringRef name,
    SLC & loc) {
  DASSERT(typeDef->isTemplate());
  AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
  if (const CompositeType * ctype = dyn_cast<CompositeType>(typeDef->typePtr())) {
    if (ctype->memberScope()->lookupMember(name, out, false)) {
      return true;
    }
  }

  return false;
}

Expr * AnalyzerBase::specialize(SLC & loc, const LookupResults & exprs, const ASTNodeList & args,
    bool inferArgTypes) {
  QualifiedTypeList argList; // Template args, not function args.
  if (!AnalyzerBase::reduceSpecArgs(argList, args, inferArgTypes)) {
    return &Expr::ErrorVal;
  }
  return specialize(loc, exprs, TupleType::get(argList));
}

bool AnalyzerBase::reduceSpecArgs(
    QualifiedTypeList & argList, const ASTNodeList & args, bool inferArgTypes) {
  // Resolve all the arguments. Note that we don't support type inference on template args,
  // so the resolution is relatively straightforward.
  ExprAnalyzer ea(this, currentFunction_);
  for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
    Expr * cb = ea.reduceTemplateArgExpr(*it, inferArgTypes);
    if (isErrorResult(cb)) {
      return false;
    }

    // This next section deals with converting normal expressions into type expressions.

    // Handle the case of a type argument that is a tuple type.
    if (isa<TupleCtorExpr>(cb)) {
      const Type * ttype = getTupleTypesFromTupleExpr(cb);
      if (ttype != NULL) {
        DASSERT(!inferArgTypes || ttype->isSingular());
        cb = new TypeLiteralExpr(cb->location(), ttype);
      }
    }

    // Handle the case of a type argument that is a type literal.
    QualifiedType typeArg;
    if (TypeLiteralExpr * ctype = dyn_cast<TypeLiteralExpr>(cb)) {
      typeArg = dealias(ctype->value());
      if (TypeDefn * tdef = typeArg->typeDefn()) {
        typeArg = tdef->value() | typeArg.qualifiers();
      }
    }

    if (!typeArg) {
      if (!isa<ConstantExpr>(cb)) {
        diag.fatal(cb) << "Not a constant expression: " << *it;
      }

      typeArg = UnitType::get(cast<ConstantExpr>(cb));
    }

    argList.push_back(typeArg);
  }
  return true;
}

Expr * AnalyzerBase::specialize(SLC & loc, const LookupResults & results, TupleType * typeArgs) {
  // Go through the list of matching expressions and see which ones
  // can accept 'typeArgs' as type arguments. Add those to the set of
  // specialization candidates.
  SpCandidateSet candidates;
  for (LookupResults::const_iterator it = results.begin(); it != results.end(); ++it) {
    if (it->isErrorResult()) {
      return NULL;
    } else if (TypeDefn * typeDefn = dyn_cast_or_null<TypeDefn>(it->defn())) {
      QualifiedType type = dealias(typeDefn->value());
      typeDefn = type->typeDefn();
      if (typeDefn != NULL) {
        if (typeDefn->isTemplate() || typeDefn->isTemplateInstance()) {
          addSpecCandidate(loc, candidates, NULL, typeDefn, typeArgs);
        }
      } else if (type.isa<AddressType>()) {
        addSpecCandidate(loc, candidates, NULL, &AddressType::typedefn, typeArgs);
      } else if (type.isa<NativeArrayType>()) {
        addSpecCandidate(loc, candidates, NULL, &NativeArrayType::typedefn, typeArgs);
      } else if (type.isa<FlexibleArrayType>()) {
        addSpecCandidate(loc, candidates, NULL, &FlexibleArrayType::typedefn, typeArgs);
      } else if (type.isa<TypeLiteralType>()) {
        addSpecCandidate(loc, candidates, NULL, &TypeLiteralType::typedefn, typeArgs);
      }
    } else if (ValueDefn * val = dyn_cast<ValueDefn>(it->defn())) {
      if (val->isTemplate() || val->isTemplateInstance()) {
        addSpecCandidate(loc, candidates, it->expr(), val, typeArgs);
      }
    }
  }

  if (candidates.empty()) {
    diag.error(loc) << "No templates found which match template arguments [" << typeArgs << "]";
    for (LookupResults::const_iterator it = results.begin(); it != results.end(); ++it) {
      diag.info(it->location()) << Format_Type << "candidate: " << *it;
    }

    //ea.dumpScopeHierarchy();
    return NULL;
  }

  SpecializeExpr * result = new SpecializeExpr(loc, candidates, typeArgs);
  // Set a type so that the analyzer won't complain. The actual type doesn't matter.
  result->setType(&AnyType::instance);
  return result;
}

void AnalyzerBase::addSpecCandidate(SLC & loc, SpCandidateSet & spcs, Expr * base, Defn * defn,
    TupleType * args) {
  if (defn->isTemplate()) {
    DefnAnalyzer::analyzeTemplateSignature(defn);
    const Template * tm = defn->templateSignature();
    if (tm->isVariadic()) {
      // Attempt to match the type args against the variadic type params.
      size_t variadicIndex = tm->typeParams()->size() - 1;
      if (args->size() >= variadicIndex) {
        QualifiedTypeList typeArgs(args->begin(), args->begin() + variadicIndex);
        typeArgs.push_back(TupleType::get(args->begin() + variadicIndex, args->end()));
        args = TupleType::get(typeArgs);
        if (tm->canUnify(args)) {
          spcs.insert(new SpCandidate(base, defn, args));
        }
      }
    } else if (args->size() >= tm->numRequiredArgs() &&
        args->size() <= tm->typeParams()->size()) {
      if (tm->canUnify(args)) {
        spcs.insert(new SpCandidate(base, defn, args));
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

bool AnalyzerBase::importName(LookupResults & out, StringRef path, bool absPath, SLC & loc) {
  DefnList defns;
  if (module_ != NULL) {
    if (module_->import(path, defns, absPath)) {
      return defnListToLookupResults(defns, NULL, out);
    }
  }

  if (currentFunction_ != NULL && currentFunction_->isSynthetic()) {
    Module * source = currentFunction_->sourceModule();
    if (source != NULL && source != module_) {
      if (source->import(path, defns, absPath)) {
        return defnListToLookupResults(defns, NULL, out);
      }
    }
  }

  return false;
}

bool AnalyzerBase::defnListToLookupResults(DefnList & defs, Expr * context, LookupResults & out) {
  for (DefnList::iterator it = defs.begin(); it != defs.end(); ++it) {
    if (ExplicitImportDefn * imp = dyn_cast<ExplicitImportDefn>(*it)) {
      // TODO: Should we allow mixing of import defns with non-imports of
      // the same name?
      DefnList & importedDefns = imp->importDefs();
      DASSERT(!importedDefns.empty()) << "Import statement " << imp << " has no definitions.";
      out.append(importedDefns.begin(), importedDefns.end());
    } else {
      out.push_back(LookupResult(*it, context));
    }
  }

  return !out.empty();
}

bool AnalyzerBase::getLookupResultTypes(SLC & loc, LookupResults & in, QualifiedTypeList & out,
    unsigned defaultQualifiers) {
  int numNonTypes = 0;
  BindingEnv env;
  for (LookupResults::iterator it = in.begin(); it != in.end(); ++it) {
    if (it->isErrorResult()) {
      out.push_back(&BadType::instance);
    } else if (TypeDefn * tdef = dyn_cast_or_null<TypeDefn>(it->defn())) {
      QualifiedType value = tdef->value();
      if (isQualifiableType(value.unqualified())) {
        if (tdef->modifiers().flags & Mutable) {
          value.addQualifiers(QualifiedType::MUTABLE);
        } else if (tdef->modifiers().flags & ReadOnly) {
          value.addQualifiers(QualifiedType::READONLY);
        } else if (tdef->modifiers().flags & Immutable) {
          //value.addQualifiers(QualifiedType::IMMUTABLE);
        } else if (defaultQualifiers) {
          //value.addQualifiers(defaultQualifiers);
        }
      }
      out.push_back(value);
    } else if (SpecializeExpr * spe = dyn_cast_or_null<SpecializeExpr>(it->expr())) {
      SourceContext specSite(loc, NULL, spe);
      const SpCandidateList & candidates = spe->candidates();
      for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
        SpCandidate * sp = *it;
        if (isa<TypeDefn>(sp->def())) {
          // If unification fails, just skip over it - it's not an error.
          Type * type = sp->toType(&specSite, env);
          if (type != NULL) {
            out.push_back(type);
          }
        } else {
          numNonTypes++;
        }
      }
    } else {
      numNonTypes++;
    }
  }

  if (out.empty()) {
    return false;
  }

  if (numNonTypes > 0) {
    diag.fatal(loc) << "Incompatible definitions for '" << out.front() << "'";
    for (LookupResults::const_iterator it = in.begin(); it != in.end(); ++it) {
      diag.info(it->location()) << Format_Type << *it;
    }
  }

  return true;
}

const Type * AnalyzerBase::getTupleTypesFromTupleExpr(Expr * in) {
  if (TupleCtorExpr * tctor = dyn_cast<TupleCtorExpr>(in)) {
    // Check to see if all members are type literals.
    QualifiedTypeList typeMembers;
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

const QualifiedType AnalyzerBase::inferType(ValueDefn * valueDef) {
  if (!valueDef->type()) {
    if (!analyzeDefn(valueDef, Task_PrepTypeComparison)) {
      return NULL;
    }
  }

  if (valueDef->type() && valueDef->type()->isSingular()) {
    if (ParameterDefn * param = dyn_cast<ParameterDefn>(valueDef)) {
      return param->internalType();
    }

    return valueDef->type().type();
  }

  if (valueDef->type()) {
    diag.info(valueDef) << valueDef << ":" << valueDef->type();
  } else {
    diag.info(valueDef) << valueDef;
  }

  DFAIL("Failed to determine type of value.");
}

bool AnalyzerBase::analyzeType(QualifiedType in, AnalysisTask task) {
  return analyzeType(in.unqualified(), task);
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
  RecursionGuard guard;

  Type * type = in->mutableTypePtr();
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
      if (!ta->value()) {
        QualifiedType targetType = TypeAnalyzer(in->module(), in->definingScope())
            .qualifiedTypeFromAST(cast<ASTTypeDecl>(in->ast())->bases().front());
        if (isErrorResult(targetType)) {
          return false;
        }

        ta->setValue(targetType);
      }

      return true;
    }

    case Type::TypeVar:
    case Type::TypeLiteral:
      return true;

    default:
      diag.debug(in) << in;
      DFAIL("IllegalState");
      break;
  }
}

const CompositeType * AnalyzerBase::getArrayTypeForElement(QualifiedType elementType) {
  // Look up the array class
  Template * arrayTemplate = Builtins::typeArray->typeDefn()->templateSignature();

  // Do analysis on template if needed.
  if (arrayTemplate->ast() != NULL) {
    DefnAnalyzer da(&Builtins::module, &Builtins::module, &Builtins::module, NULL);
    da.analyzeTemplateSignature(Builtins::typeArray->typeDefn());
  }

  DASSERT(arrayTemplate->paramScope().count() == 1);

  // Special case for when the elementType is Array.ElementType
  if (elementType == arrayTemplate->typeParam(0)) {
    return Builtins::typeArray.get();
  }

  QualifiedTypeVarMap vars;
  vars[arrayTemplate->patternVar(0)] = elementType;
  return cast<CompositeType>(
      arrayTemplate->instantiateType(
          SourceLocation(), vars, Template::expectedTraits(elementType)));
}

Expr * AnalyzerBase::getEmptyArrayOfElementType(const Type * elementType) {
  const CompositeType * arrayType = getArrayTypeForElement(elementType);
  return new ConstantEmptyArray(SourceLocation(), arrayType);
}

ArrayLiteralExpr * AnalyzerBase::createArrayLiteral(SLC & loc, QualifiedType elementType) {
  const CompositeType * arrayType = getArrayTypeForElement(elementType);
  ArrayLiteralExpr * array = new ArrayLiteralExpr(loc);
  array->setType(arrayType);

  return array;
}

const CompositeType * AnalyzerBase::getMutableRefType(const Type * valueType) {
  // Look up the MutableRef class
  Template * refTemplate = Builtins::typeMutableRef->typeDefn()->templateSignature();

  // Do analysis on template if needed.
  if (refTemplate->ast() != NULL) {
    DefnAnalyzer da(&Builtins::module, &Builtins::module, &Builtins::module, NULL);
    da.analyzeTemplateSignature(Builtins::typeMutableRef->typeDefn());
  }

  DASSERT_OBJ(refTemplate->paramScope().count() == 1, valueType);

  QualifiedTypeVarMap vars;
  vars[refTemplate->patternVar(0)] = valueType;
  return cast<CompositeType>(
      refTemplate->instantiateType(
          SourceLocation(), vars, Template::expectedTraits(valueType)));
}

const CompositeType * AnalyzerBase::getFunctionInterfaceType(const FunctionType * ftype) {
  // Look up the Function interface
  Template * fnTemplate = Builtins::typeFunction->typeDefn()->templateSignature();

  // Do analysis on template if needed.
  if (fnTemplate->ast() != NULL) {
    DefnAnalyzer da(&Builtins::module, &Builtins::module, &Builtins::module, NULL);
    da.analyzeTemplateSignature(Builtins::typeFunction->typeDefn());
  }

  DASSERT_OBJ(fnTemplate->paramScope().count() == 2, ftype);

  QualifiedTypeVarMap vars;
  vars[fnTemplate->patternVar(0)] = ftype->returnType();
  vars[fnTemplate->patternVar(1)] = ftype->paramTypes();
  return cast<CompositeType>(
      fnTemplate->instantiateType(SourceLocation(), vars, Template::expectedTraits(ftype)));
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
          for (Defn * de = source; de != NULL; de = de->parentDefn()) {
            if (TypeDefn * srcTypeDef = dyn_cast<TypeDefn>(de)) {
              if (TypeRelation::isSubclass(srcTypeDef->value(), dstTypeDef->value())) {
                return true;
              }
            }
          }
        }
      }

      if (target->visibility() == Internal) {
        // TODO: This probably shouldn't simply compare the name. Also handle submodules.
        if (source->module()->packageName() == target->module()->packageName()) {
          return true;
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

void AnalyzerBase::dumpScopeList(const LookupResults & lvals) {
  diag.indent();
  for (LookupResults::const_iterator it = lvals.begin(); it != lvals.end(); ++it) {
    if (it->defn() != NULL) {
      diag.info(it->defn()) << it->defn()->name();
    } else if (it->expr() != NULL) {
      diag.info(it->expr()) << it->expr();
    }
  }

  diag.unindent();
  diag.recovered();
}

Defn * AnalyzerBase::findLessSpecializedInstance(Defn * de) {
  if (de->isTemplateInstance()) {
    return de->templateInstance()->findLessSpecializedInstance();
  } else if (de->isSynthetic()) {
    Defn * parent = findLessSpecializedInstance(de->parentDefn());
    if (parent == NULL) {
      return NULL;
    }

    return findDefnByAst(parent, de);
  }

  return NULL;
}

Defn * AnalyzerBase::findDefnByAst(Defn * parent, Defn * toFind) {
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(parent)) {
    DefnList cdefs;
    AnalyzerBase::analyzeTypeDefn(tdef, Task_PrepMemberLookup);
    if (const CompositeType * ctype = dyn_cast<CompositeType>(tdef->typePtr())) {
      if (!ctype->memberScope()->lookupMember(toFind->name(), cdefs, false)) {
        return NULL;
      }
    }

    for (DefnList::const_iterator it = cdefs.begin(); it != cdefs.end(); ++it) {
      if ((*it)->ast() == toFind->ast()) {
        return *it;
      }
    }
  } else if (PropertyDefn * prop = dyn_cast<PropertyDefn>(parent)) {
    if (prop->getter() != NULL &&  prop->getter()->ast() == toFind->ast()) {
      return prop->getter();
    }

    if (prop->setter() != NULL &&  prop->setter()->ast() == toFind->ast()) {
      return prop->setter();
    }
  }

  return NULL;
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


FunctionDefn * ExprAnalyzer::selectOverload(
    const SourceLocation & loc,
    DefnList & methods, 
    const ASTNodeList & inArgs,
    const ExprList & inTemplateArgs,
    Type * expected,
    ExprList & outArgs) {

  // Make sure that parameter types have been resolved.
  for (DefnList::iterator it = methods.begin(); it != methods.end(); ++it) {
    if (!analyzeDefn(*it, Pass_ResolveParameterTypes)) {
      return NULL;
    }
  }

  // Make sure that the return type is known.
  for (DefnList::iterator it = methods.begin(); it != methods.end(); ++it) {
    // Make sure that parameter types have been resolved.
    if (!analyzeDefn(*it, Pass_ResolveReturnType)) {
      return NULL;
    }
  }

  //if (methods.size() == 1) {
  //  return methods.front();
  //}

  // Create an overload function resolver, and try each function that
  // we find.
#if BLARG
  OverloadResolver resolver(loc, inArgs, expected);
#if 0
  resolver.setTemplateArgs(inTemplateArgs);
#endif

  for (DefnList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
    Defn * de = *it;
    /*if (de->getClass() == Expr::Template) {
      TemplateDef * tdef = cast<TemplateDef>(de);
      analyze(tdef);
      if (tdef->getBody()->getClass() == Expr::Function ||
          tdef->getBody()->getClass() == Expr::Indexer) {
        FunctionDef * method = static_cast<FunctionDef *>(tdef->getBody());
        if (method->getModule() == NULL) {
          //method->setParentDecl(tdef);
          assert(false);
        }
        // This is an open type, so we don't want to do a complete resolution yet
        resolveFunctionParameterTypes(method);
        resolver.addMethod(method, true);
      }
    } else*/
    
    if (de->defnType() == Defn::Function || de->defnType() == Defn::Indexer) {
      FunctionDefn * method = static_cast<FunctionDefn *>(de);
      //resolveType(loc, method->functionType(), false);
      resolver.addMethod(method);
    } else {
      diag.fatal(loc) << "Cannot call object of non-function type " << de;
      return NULL;
    }
  }
  
  diag.debug(loc) << resolver.candidates().size() << " candidates.";
  resolver.computeArgTypes();
  
  // Now, evaluate all of the arguments.
  size_t argCount = inArgs.size();
  ExprList reducedArgs;
  reducedArgs.resize(argCount);
  DASSERT(resolver.argTypes().size() == argCount);
  for (size_t i = 0; i < argCount; ++i) {
    const ASTNode * ast = inArgs[i];
    //TypeSelection & ts = resolver.argType(i);
    Type * paramType = resolver.argTypes()[i];

    //if (ts.getEntries().size() == 1) {
    //  paramType = ts.getEntries()[0].value;
    //} else {
    //  paramType = AmbiguousType::get(ts);
    //}

    Expr * arg = reduceExpr(inArgs[i], paramType);
    if (isErrorResult(arg)) {
      return NULL;
    }

    resolver.argTypes()[i] = arg->getType();
    reducedArgs[i] = arg;
  }
  
  resolver.chooseMethods();

#if 0
  // If we find 0 or more than 1, that's an error
  resolver.filterResults();
  ConversionRank typeCompat = resolver.getConversionRank();
  if (typeCompat < NonPreferred) {
    TypeConversion::compatibilityWarning(callLoc, typeCompat);
  }

#endif

  if (resolver.candidates().size() != 1) {
    if (resolver.candidates().size() > 1) {
      // && typeCompat >= NonPreferred
      diag.recovered();
      diag.fatal(loc) << "Ambiguous matching signatures";
      for (CandidateList::const_iterator it = resolver.candidates().begin();
      it != resolver.candidates().end(); ++it) {
        diag.info(it->method) << it->method;
      }
    }

    return NULL;
  }
  
  // The one matching method
  OverloadCandidate & oc = resolver.candidates().front();
  FunctionDefn * method = oc.method;
  FunctionType * ftype = method->functionType();
  size_t numParams = ftype->params().size();
  
#if 0
  if (!ftype->isClosed()) {
    resolveType(callLoc, ftype);
    assert(ftype->isClosed());
  }
#endif

  // Initialize the actual array of arguments to be passed.
  outArgs.resize(numParams);
  std::fill(outArgs.begin(), outArgs.end(), (Expr *)NULL);
  
  // First fill in non-default arguments.
  for (size_t i = 0; i < oc.paramAssignments.size(); ++i) {
    int paramIndex = oc.paramAssignments[i];
    ParameterDefn * param = ftype->params()[paramIndex];
    outArgs[paramIndex] = param->getType()->implicitCast(
        inArgs[i]->getLocation(), reducedArgs[i]);
  }
  
  // Now do the default args
  for (size_t i = 0; i < numParams; ++i) {
    if (outArgs[i] == NULL) {
      ParameterDefn * param = ftype->params()[i];
      outArgs[i] = param->getType()->implicitCast(
          loc, param->defaultValue());
    }
  }

#if 0
    if (param->getParameterFlag(ParameterDef::Hidden)) {
      if (arg->getClass() == Expr::NewInstance && !fromType->isClosed()) {
        // The hidden argument was a template - let's try and find the
        // concrete type.
        const TypeDef * tdef = cast<TypeDef>(fromType->getDeclaration());
        /*const TemplateDef * tmpl = tdef->getEnclosingTemplate();

        const TypeParameterList & typeParams = tmpl->getTypeParams();
        for (TypeParameterList::const_iterator it = typeParams.begin();
            it != typeParams.end(); ++it) {
            //const Type * mappedType = resolver.
        }*/

        // TODO: This isn't quite right - the type of the 'self' argument might
        // not be the type of the new instance, it might be a supertype.
        arg = new Expr(Expr::NewInstance, arg->getLocation(), toType);
      }
#endif

  return method;
#endif

  //diag.debug("Found cand %s", result->toString().c_str());
  DFAIL("Deppercate");
}

#if 0
Expr * AnalyzerBase::resolveQualifiedName(const SourceLocation & loc,
    const char * name, Expr * qualifier) {

  // Do a qualified name lookup.
  Defn * searchContext = NULL;
  
  if (ScopeNameExpr * scopeName = dyn_cast<ScopeNameExpr>(qualifier)) {
    if (scopeName->getValue()->lookupMember(name, defns, true)) {
      return searchResult(loc, defns, NULL);
    } else {
      return NULL;
    }
  }

#if 0
  if (Defn * qdef = dyn_cast<Defn>(qualifier)) {
    // If the declaration is of a type that creates a scope, then
    // that is the search context.
    if (qdef->defnType() == Defn::Namespace ||
        qdef->defnType() == Defn::Type) {
      searchContext = qdef;
    }
  }

#endif
  if (LValueExpr * lvalue = dyn_cast<LValueExpr>(qualifier)) {
    const Type * type = inferType(lvalue->getValue());
    const TypeDefn * typeDef = type->getTypeDefn();
    if (typeDef != NULL) {
      if (typeDef->lookupMember(name, defns, true)) {
        return searchResult(loc, defns, lvalue);
      } else {
        return NULL;
      }
    } else {
      // TODO: Error.
    }
  }

  if (!searchContext) {
#if 0
    // Otherwise, the search context is determined by the type of the
    // expression.
    const Type * type = inferType(qualifier);
    if (type == NULL ||
        type->getCanonicalType() == NULL ||
        type == &PrimitiveType::BadType) {
      return false;
    }
    
    // SearchDecl is the declaration of the object's type.
    searchContext = type->getDeclaration();
    if (searchContext == NULL) {
      return false;
    }

    assert(searchContext->getClass() == Expr::ASTTypeDeclaration);
    assertNotNull(searchContext->getModule(), searchContext);
#endif
  }

#if 0
  // Make sure the search context has been prepped.
  Analyzer::analyzeDecl(searchContext);
  return searchContext->lookupMember(name, out, true);
#endif
  diag.fatal(loc) << "Not found: " << name;
  DASSERT(false);
}

Expr * AnalyzerBase::resolveUnqualifiedName(const SourceLocation & loc,
    const char * name) {
      
  DefnList defns;
  Expr * basePtr = NULL;
  if (lookupUnqualifiedNames(name, defns, &basePtr)) {
    return searchResult(loc, defns, basePtr);
  }
  
  return NULL;

  // Search the current active scopes.
  Scope * foundScope = NULL;
  Expr * basePtr = NULL;
  for (Scope * sc = activeScope; sc != NULL && foundScope == NULL;
      sc = sc->getParentScope()) {
    if (sc->lookupMember(name, defns, true)) {
      foundScope = sc;
      basePtr = sc->getBase();
    }
  }
  
  // Try implicit import lookup.
  if (foundScope == NULL && !doImplicitImportLookup(name, defns)) {
    return false;
  }

  // Resolve any explicit imports
  bool found = false;
  for (DefnList::const_iterator it = defns.begin(); it != defns.end(); ++it) {
    Defn * de = *it;
#if 0
    assertNotNull(de->getParentScope(), de);
    assertNotNull(de->getModule(), de);
    if (de->getClass() == Expr::ImportedName) {
      if (!finalizeExplicitImport(static_cast<ImportedNameDef *>(de), out))
        return false;
    } else if (testDeclCondition(de)) {
      out.push_back(de);
      found = true;
    }
#endif
  }
  
  return searchResult(loc, defns, basePtr);
}
  
#endif

#if 0
/// -------------------------------------------------------------------
/// Represents an invocation of the specialization operator (![]).
/// Normally this is only used when the type arguments are incomplete;
/// For complete types we just go ahead and do the specialization directly.
class SpecializedType : public GC, public Type, public Locatable {
private:
  SpCandidates candidates;
  ConstantExprList args;
  
public:
  SpecializedType(const SourceLocation & l, SpCandidates & cdlist,
      ConstantExprList & alist)
    : Type(Specialized), loc(l), candidates(cdlist), args(alist) {}

  SpCandidates & candidates() { return candidates; }
  ConstantExprList & args() { return args; }

  // Overrides

  const llvm::Type * getIRType() const;
  ConversionRank convertImpl(const Conversion & conversion) const;
  bool isSubtype(const Type * other) const;
  void mark() const { GC::mark(); }
  void trace() const;
  bool isReferenceType() const;
  bool isSingular() const { return false; }
  void format(FormatStream & out) const;

  static inline bool classof(const SpecializedType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == Specialized;
  }
};
#endif


#if 0
// -------------------------------------------------------------------
// SpecializedType

const llvm::Type * SpecializedType::getIRType() const {
  DFAIL("Implement");
}

ConversionRank SpecializedType::convertImpl(
    const Conversion & conversion) const {
  DFAIL("Implement");
}

bool SpecializedType::isSubtype(const Type * other) const {
  DFAIL("Implement");
}

bool SpecializedType::isReferenceType() const {
  DFAIL("Implement");
}

void SpecializedType::trace() const {
  Type::trace();
  markList(candidates.begin(), candidates.end();
  markList(args.begin(), args.end());
}

void SpecializedType::format(FormatStream & out) const {
  DFAIL("Implement");
}
#endif


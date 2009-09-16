/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ClassAnalyzer.h"
#include "tart/Sema/EnumAnalyzer.h"
#include "tart/Sema/FunctionAnalyzer.h"
#include "tart/Sema/PropertyAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/BindingEnv.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Template.h"
#include "tart/Common/PackageMgr.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Objects/Builtins.h"

namespace tart {

AnalyzerBase::AnalysisQueue AnalyzerBase::queue;
size_t AnalyzerBase::queuePos = 0;

bool AnalyzerBase::lookupName(ExprList & out, const ASTNode * ast, bool absPath) {
  std::string path;
  lookupNameRecurse(out, ast, path, absPath);
  return !out.empty();
}

// Here's how this works:
// If we return true then it means we found something.
// If we return false, but path is non-empty, it means we found nothing,
//    but there's a chance it might be a package reference.
// If we return false and path is empty, then it means that we found nothing,
//    and there's no hope of finding anything.
bool AnalyzerBase::lookupNameRecurse(ExprList & out, const ASTNode * ast, std::string & path,
    bool absPath) {

  SLC & loc = ast->location();
  if (ast->nodeType() == ASTNode::Id) {
    const ASTIdent * ident = static_cast<const ASTIdent *>(ast);
    const char * name = ident->value();
    if (!absPath && activeScope != NULL && lookupIdent(out, name, loc)) {
      return true;
    }

    path.assign(name);
    return importName(out, path, absPath, loc);
  } else if (ast->nodeType() == ASTNode::Member) {
    const ASTMemberRef * mref = static_cast<const ASTMemberRef *>(ast);
    const ASTNode * qual = mref->qualifier();
    ExprList lvals;
    if (lookupNameRecurse(lvals, qual, path, absPath)) {
      if (lvals.size() > 1) {
        diag.fatal(ast) << "Multiply defined symbol " << qual;
        path.clear();
        return false;
      }

      Expr * context = lvals.front();
      if (findMemberOf(out, context, mref->memberName(), loc)) {
        return true;
      }

      path.clear();
      return false;
    }

    if (!path.empty()) {
      path.push_back('.');
      path.append(mref->memberName());
      return importName(out, path, absPath, loc);
    }

    return false;
  } else if (ast->nodeType() == ASTNode::Specialize) {
    const ASTSpecialize * spec = static_cast<const ASTSpecialize *>(ast);
    ExprList lvals;
    if (!lookupNameRecurse(lvals, spec->templateExpr(), path, absPath)) {
      diag.error(spec) << "Undefined symbol: " << spec->templateExpr();
      dumpScopeHierarchy();
      return false;
    }

    Expr * expr = resolveSpecialization(loc, lvals, spec->args());
    if (expr == NULL) {
      diag.error(spec) << "No template found matching expression: " << spec->templateExpr();
      return false;
    }

    out.push_back(expr);
    return true;
  } else {
    // It's not a name or anything like that.
    path.clear();

    // See if it's an expression.
    ExprAnalyzer ea(module, activeScope);
    Expr * result = ea.reduceExpr(ast, NULL);
    if (result != NULL) {
      out.push_back(result);
      return true;
    }

    return false;
  }
}

bool AnalyzerBase::lookupIdent(ExprList & out, const char * name, SLC & loc) {
  // Search the current active scopes.
  for (Scope * sc = activeScope; sc != NULL; sc = sc->parentScope()) {
    if (findInScope(out, name, sc, sc->baseExpr(), loc)) {
      return true;
    }
  }

  return false;
}

bool AnalyzerBase::findMemberOf(ExprList & out, Expr * context, const char * name, SLC & loc) {
  if (ScopeNameExpr * scopeName = dyn_cast<ScopeNameExpr>(context)) {
    if (Module * m = dyn_cast<Module>(scopeName->value())) {
      AnalyzerBase::analyzeDefn(m, Task_PrepMemberLookup);
      if (findInScope(out, name, m, NULL, loc)) {
        return true;
      }
    } else if (NamespaceDefn * ns = dyn_cast<NamespaceDefn>(scopeName->value())) {
      AnalyzerBase::analyzeDefn(ns, Task_PrepMemberLookup);
      if (findInScope(out, name, &ns->memberScope(), NULL, loc)) {
        return true;
      }
    }
  }

  if (LValueExpr * lvalue = dyn_cast<LValueExpr>(context)) {
    Type * type = inferType(lvalue->value());
    TypeDefn * typeDef = dealias(type)->typeDefn();
    if (typeDef != NULL && type->memberScope() != NULL) {
      DASSERT_OBJ(typeDef->isSingular(), typeDef);
      AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
      if (findInScope(out, name, type->memberScope(), context, loc)) {
        return true;
      }
    }
  } else if (ConstantType * ctype = dyn_cast<ConstantType>(context)) {
    TypeDefn * typeDef = dealias(ctype->value())->typeDefn();
    if (typeDef != NULL && ctype->value()->memberScope() != NULL) {
      if (typeDef->isTemplate()) {
        return findStaticTemplateMember(out, typeDef, name, loc);
      }

      DASSERT_OBJ(ctype->isSingular(), typeDef);
      AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
      if (findInScope(out, name, ctype->value()->memberScope(), context, loc)) {
        return true;
      }
    }
  } else if (context->type() != NULL) {
    TypeDefn * typeDef = dealias(context->type())->typeDefn();
    if (typeDef != NULL && context->type()->memberScope() != NULL) {
      if (typeDef->typeValue()->isUnsizedIntType()) {
        diag.error(loc) << "Attempt to access member of integer of unknown size";
        return false;
      }

      DASSERT_OBJ(typeDef->isSingular(), typeDef);
      AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
      if (findInScope(out, name, context->type()->memberScope(), context, loc)) {
        return true;
      }
    }
  }

  return false;
}

bool AnalyzerBase::findInScope(ExprList & out, const char * name, Scope * scope, Expr * context,
    SLC & loc) {
  DefnList defns;
  if (scope->lookupMember(name, defns, true)) {
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
  AnalyzerBase::analyzeTypeDefn(typeDef, Task_PrepMemberLookup);
  if (typeDef->beginPass(Pass_CreateMembers)) {
    if (typeDef->ast() != NULL) {
      ScopeBuilder::createScopeMembers(typeDef);
    }

    typeDef->finishPass(Pass_CreateMembers);
  }

  if (CompositeType * ctype = dyn_cast<CompositeType>(typeDef->typeValue())) {
    if (ctype->memberScope()->lookupMember(name, out, false)) {
      return true;
    }

    const ASTTypeDecl * ast = cast_or_null<const ASTTypeDecl>(ctype->typeDefn()->ast());
    if (ast == NULL) {
      return false;
    }

    const ASTNodeList & astBases = ast->bases();
    if (!astBases.empty()) {
      DFAIL("Implement search of template base classes");
    }
  }

  return false;
}

Expr * AnalyzerBase::resolveSpecialization(SLC & loc, const ExprList & exprs,
    const ASTNodeList & args) {
  DefnList defns;
  DefnAnalyzer da(module, activeScope);
  Expr * base = NULL;

  for (ExprList::const_iterator it = exprs.begin(); it != exprs.end(); ++it) {
    if (ConstantType * tref = dyn_cast<ConstantType>(*it)) {
      TypeDefn * ty = dealias(tref->value())->typeDefn();
      if (ty != NULL) {
        if (ty->isTemplate()) {
          defns.push_back(ty);
        } else if (ty->isTemplateInstance()) {
          // It's an instance, so get the original template and its siblings
          ty->templateInstance()->parentScope()->lookupMember(ty->name(), defns, true);
        }
      }
    } else if (LValueExpr * lv = dyn_cast<LValueExpr>(*it)) {
      if (lv->base() != NULL) {
        base = lv->base();
      }

      ValueDefn * val = lv->value();
      if (val->isTemplate()) {
        defns.push_back(val);
      } else if (val->isTemplateInstance()) {
        // It's an instance, so get the original template and its siblings
        val->templateInstance()->parentScope()->lookupMember(val->name(), defns, true);
      }
    }
  }

  if (defns.empty()) {
    return NULL;
  }

  // TODO: Handle expressions that have a base.
  Defn * de = da.specialize(loc, defns, args);
  if (de == NULL) {
    return NULL;
  }

  return getDefnAsExpr(de, base, loc);
}

bool AnalyzerBase::importName(ExprList & out, const std::string & path, bool absPath, SLC & loc) {

  if (module != NULL) {
    DefnList defns;
    if (module->import(path.c_str(), defns, absPath)) {
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
    return new ConstantType(loc, tdef);
  } else if (ValueDefn * vdef = dyn_cast<ValueDefn>(de)) {
    if (vdef->storageClass() == Storage_Instance && context == NULL) {
      diag.fatal(loc) << "Cannot access non-static member '" <<
          vdef->name() << "' from static method.";
    }

    LValueExpr * result = new LValueExpr(loc, context, vdef);

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

bool AnalyzerBase::getTypesFromExprs(SLC & loc, ExprList & in, DefnList & out) {
  int numNonTypes = 0;
  for (ExprList::iterator it = in.begin(); it != in.end(); ++it) {
    if (ConstantType * cty = dyn_cast<ConstantType>(*it)) {
      if (TypeDefn * tdef = cty->value()->typeDefn()) {
        out.push_back(tdef);
      }

      continue;
    }

    numNonTypes++;
  }

  if (out.empty()) {
    return false;
  }

  if (numNonTypes > 0) {
    diag.fatal(loc) << "Incompatible definitions for '" << out.front()->name() << "'";
    for (ExprList::iterator it = in.begin(); it != in.end(); ++it) {
      diag.info(*it) << *it;
    }
  }

  return true;
}

Type * AnalyzerBase::inferType(ValueDefn * valueDef) {
  if (valueDef->type() == NULL) {
    if (!analyzeDefn(valueDef, Task_InferType)) {
      return NULL;
    }
  }

  if (valueDef->type() != NULL && valueDef->type()->isSingular()) {
    if (ParameterDefn * param = dyn_cast<ParameterDefn>(valueDef)) {
      return param->internalType();
    }
    return valueDef->type();
  }

  diag.info(valueDef) << valueDef << ":" << valueDef->type();
  DFAIL("Failed to determine type of value.");
}

bool AnalyzerBase::analyzeType(Type * in, AnalysisTask pass) {
  if (in != NULL) {
    TypeDefn * de = in->typeDefn();
    if (de != NULL) {
      return analyzeTypeDefn(de, pass);
    }
  }

  return true;
}

bool AnalyzerBase::analyzeModule(Module * mod) {
  DefnAnalyzer da(mod, mod);
  return da.analyzeModule();
}

bool AnalyzerBase::analyzeDefn(Defn * in, AnalysisTask task) {
  switch (in->defnType()) {
    case Defn::Typedef:
      return analyzeTypeDefn(static_cast<TypeDefn *>(in), task);

    case Defn::Namespace:
      return analyzeNamespace(static_cast<NamespaceDefn *>(in), task);

    case Defn::Var:
    case Defn::Let:
    case Defn::Parameter:
    case Defn::TemplateParam: {
      return VarAnalyzer(static_cast<ValueDefn *>(in)).analyze(task);
    }

    case Defn::Property:
    case Defn::Indexer: {
      return PropertyAnalyzer(static_cast<PropertyDefn *>(in)).analyze(task);
    }

    case Defn::Function:
    case Defn::Macro: {
      return FunctionAnalyzer(static_cast<FunctionDefn *>(in)).analyze(task);
    }

    case Defn::Mod: {
      Module * m = static_cast<Module *>(in);
      return DefnAnalyzer(m, m).analyzeModule();
    }

    case Defn::ExplicitImport:
      return true;
      //DFAIL("IllegalState");

    case Defn::DefnTypeCount:
    default:
      DFAIL("IllegalState");
  }
}

bool AnalyzerBase::analyzeTypeDefn(TypeDefn * in, AnalysisTask pass) {
  Type * type = in->typeValue();
  switch (type->typeClass()) {
    case Type::Primitive:
      return true;

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      return ClassAnalyzer(in).analyze(pass);
    }

    case Type::Enum:
      return EnumAnalyzer(in).analyze();

    case Type::NativePointer: {
      NativePointerType * np = static_cast<NativePointerType *>(type);
      Type * elementType = np->typeParam(0);
      if (elementType != NULL && elementType->typeDefn() != NULL && elementType->isSingular()) {
        analyzeLater(elementType->typeDefn());
      }

      return true;
    }

    case Type::NativeArray: {
      NativeArrayType * na = static_cast<NativeArrayType *>(type);
      Type * elementType = na->typeParam(0);
      if (elementType != NULL && elementType->typeDefn() != NULL && elementType->isSingular()) {
        analyzeLater(elementType->typeDefn());
      }

      return true;
    }

    case Type::Alias:
      // TODO: Analyze what we are pointing to.
      return true;

    case Type::Pattern:
      return true;

    default:
      diag.debug(in) << in;
      DFAIL("IllegalState");
  }
}

bool AnalyzerBase::analyzeValueDefn(ValueDefn * in, AnalysisTask pass) {
  return analyzeDefn(in, pass);
}

void AnalyzerBase::analyzeLater(Defn * de) {
  DASSERT_OBJ(de->isSingular(), de);
  if (queue.insert(de)) {
    //diag.debug() << de << " added to queue";
  }
}

void AnalyzerBase::flushAnalysisQueue() {
  while (queuePos < queue.size()) {
    Defn * de = queue[queuePos++];
    analyzeDefn(de, Task_PrepCodeGeneration);
  }
}

bool AnalyzerBase::analyzeNamespace(NamespaceDefn * in, AnalysisTask task) {
  // Analyze namespace imports.
  if (task == Task_PrepMemberLookup && in->ast() != NULL && in->beginPass(Pass_ResolveImport)) {
    DefnAnalyzer da(in->module(), &in->memberScope());
    const ASTNodeList & imports = in->ast()->imports();
    for (ASTNodeList::const_iterator it = imports.begin(); it != imports.end(); ++it) {
      da.importIntoScope(cast<ASTImport>(*it), &in->memberScope());
    }

    in->finishPass(Pass_ResolveImport);
  }

  if (task == Task_PrepMemberLookup && in->beginPass(Pass_CreateMembers)) {
    if (in->ast() != NULL) {
      ScopeBuilder::createScopeMembers(in);
    }

    in->finishPass(Pass_CreateMembers);
  }

  return true;
}

CompositeType * AnalyzerBase::getArrayTypeForElement(Type * elementType) {
  // Look up the array class
  TemplateSignature * arrayTemplate = Builtins::typeArray->typeDefn()->templateSignature();

  // Do analysis on template if needed.
  if (arrayTemplate->ast() != NULL) {
    DefnAnalyzer da(&Builtins::module, &Builtins::module);
    da.analyzeTemplateSignature(Builtins::typeArray->typeDefn());
  }

  DASSERT_OBJ(arrayTemplate->paramScope().count() == 1, elementType);

  // Special case for when the elementType is Array.ElementType
  if (elementType == arrayTemplate->params()[0]) {
    return cast<CompositeType>(Builtins::typeArray);
  }

  BindingEnv arrayEnv(arrayTemplate);
  arrayEnv.addSubstitution(arrayTemplate->patternVar(0), elementType);
  return cast<CompositeType>(cast<TypeDefn>(
      arrayTemplate->instantiate(SourceLocation(), arrayEnv))->typeValue());
}

ArrayLiteralExpr * AnalyzerBase::createArrayLiteral(SLC & loc, Type * elementType) {
  CompositeType * arrayType = getArrayTypeForElement(elementType);
  ArrayLiteralExpr * array = new ArrayLiteralExpr(loc);
  array->setType(arrayType);

  return array;
}

void AnalyzerBase::dumpScopeHierarchy() {
  int level = diag.getIndentLevel();
  for (Scope * s = activeScope; s != NULL; s = s->parentScope()) {
    s->dumpHierarchy(true);
    diag.indent();
  }

  diag.setIndentLevel(level);
  diag.recovered();
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/NamespaceDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/Module.h"
#include "tart/Defn/Template.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/FunctionType.h"

#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/TypeAnalyzer.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Meta/MDReader.h"

#include "tart/Objects/Builtins.h"

namespace tart {

typedef ASTDeclList::const_iterator decl_iterator;

void ScopeBuilder::createScopeMembers(Defn * parent) {
  if (parent->mdNode()) {
    // Create definitions from Metadata Node.
    MDReader(parent->module(), parent).readMembers(parent);
  } else {
    // Create definitions from AST
    createScopeMembers(parent, parent->astMembers());
  }
}

void ScopeBuilder::createScopeMembers(Defn * parent, const ASTDeclList & decs) {
  // If this is a scope-defining definition, then construct the scope.
  switch (parent->defnType()) {
    case Defn::Typedef: {
      TypeDefn * tdef = static_cast<TypeDefn *>(parent);
      Type * type = tdef->typeValue();
      switch (type->typeClass()) {
        case Type::Class:
        case Type::Struct:
        case Type::Interface:
        case Type::Protocol:
          createScopeMembers(type->memberScope(), parent, decs, Storage_Instance);
          break;

        case Type::Primitive:
        case Type::Enum:
        case Type::NAddress:
        case Type::NArray:
        case Type::Alias:
        default:
          DFAIL("createScopeMembers: Invalid parent type");
      }

      break;
    }

    case Defn::Namespace:
      createScopeMembers(&static_cast<NamespaceDefn *>(parent)->memberScope(), parent, decs,
          Storage_Global);
      break;

    case Defn::Mod:
      createScopeMembers(static_cast<Module *>(parent), parent, decs, Storage_Global);
      break;

    case Defn::Property:
    case Defn::Indexer: {
      createAccessors(static_cast<PropertyDefn *>(parent));
      break;
    }

    case Defn::Parameter:
    case Defn::Var:
    case Defn::Let:
    case Defn::Function:
    case Defn::Macro:
      break;

    case Defn::ExplicitImport:
    case Defn::DefnTypeCount:
    case Defn::MacroArg:
      DFAIL("createScopeMembers: Invalid parent type");
  }
}

void ScopeBuilder::createAccessors(PropertyDefn * prop) {
  const ASTPropertyDecl * ast = cast<ASTPropertyDecl>(prop->ast());
  if (ast->getter() != NULL) {
    FunctionDefn * getter = new FunctionDefn(Defn::Function, prop->module(), ast->getter());
    prop->setGetter(getter);
    prop->accessorScope().addMember(getter);
    getter->createQualifiedName(prop);
    getter->copyTrait(prop, Defn::Synthetic);
    if (ast->modifiers().flags & tart::Final) {
      getter->setFlag(FunctionDefn::Final);
    }
    if (ast->modifiers().flags & tart::Undef) {
      getter->setFlag(FunctionDefn::Undefined);
    }
    if (ast->modifiers().flags & tart::Override) {
      getter->setFlag(FunctionDefn::Override);
    }
    getter->setParentDefn(prop);
    getter->setStorageClass(prop->storageClass());
    if (getter->templateSignature() == NULL) {
      getter->copyTrait(prop, Defn::Singular);
    }
  }

  if (ast->setter() != NULL) {
    FunctionDefn * setter = new FunctionDefn(Defn::Function, prop->module(), ast->setter());
    prop->setSetter(setter);
    prop->accessorScope().addMember(setter);
    setter->createQualifiedName(prop);
    setter->copyTrait(prop, Defn::Synthetic);
    if (ast->modifiers().flags & tart::Final) {
      setter->setFlag(FunctionDefn::Final);
    }
    if (ast->modifiers().flags & tart::Undef) {
      setter->setFlag(FunctionDefn::Undefined);
    }
    if (ast->modifiers().flags & tart::Override) {
      setter->setFlag(FunctionDefn::Override);
    }
    setter->setParentDefn(prop);
    setter->setStorageClass(prop->storageClass());
    if (setter->templateSignature() == NULL) {
      setter->copyTrait(prop, Defn::Singular);
    }
  }
}

void ScopeBuilder::createScopeMembers(
    IterableScope * scope, Defn * parent, const ASTDeclList & decs, StorageClass scDefault) {
  for (decl_iterator it = decs.begin(); it != decs.end(); ++it) {
    const ASTDecl * de = *it;
    if (de->nodeType() == ASTNode::VarList) {
      createScopeMembers(scope, parent, de->members(), scDefault);
    } else {
      createMemberDefn(scope, parent, de, scDefault);
    }
  }

  checkNameConflicts(scope);
}

Defn * ScopeBuilder::createMemberDefn(Scope * scope, Defn * parentDefn, const ASTDecl * de,
    StorageClass scDefault) {
  DASSERT(scope != NULL);
  DASSERT(parentDefn != NULL);

  // If it's a namespace, attempt to merge with existing ns of same name.
  if (de->nodeType() == ASTDecl::Namespace) {
    Defn * ns = mergeNamespace(scope, de);
    if (ns != NULL) {
      return ns;
    }
  }

  Defn * member = createDefn(scope, parentDefn->module(), de, scDefault);
  scope->addMember(member);
  member->setParentDefn(parentDefn);
  member->createQualifiedName(parentDefn);
  member->copyTrait(parentDefn, Defn::Synthetic);
  if (parentDefn->hasUnboundTypeParams() || parentDefn->isTemplateMember()) {
    member->addTrait(Defn::TemplateMember);
  }
  if (parentDefn->isPartialInstantiation()) {
    member->addTrait(Defn::PartialInstantiation);
  }
  if (parentDefn->isSingular() && member->templateSignature() == NULL) {
    member->addTrait(Defn::Singular);
  }

  return member;
}

Defn * ScopeBuilder::createLocalDefn(Scope * scope, Defn * parent, const ASTDecl * ast) {
  DASSERT(scope != NULL);
  Defn * defn = createDefn(scope, parent->module(), ast, Storage_Local);
  checkVariableHiding(scope, defn);
  scope->addMember(defn);
  DASSERT_OBJ((ast->modifiers().flags & Static) == 0, defn);
  defn->createQualifiedName(parent);
  if (defn->templateSignature() == NULL) {
    defn->addTrait(Defn::Singular);
  }

  createScopeMembers(defn);
  return defn;
}

Defn * ScopeBuilder::createTemplateDefn(Scope * scope, Module * m, const ASTTemplate * tp,
    StorageClass scDefault) {
  DASSERT(scope != NULL);
  Defn * body = createDefn(scope, m, tp->body(), scDefault);
  Scope * parentScope = scope;
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(body)) {
    Template * tm = Template::get(body, NULL /*parentScope*/);
    tm->setAST(tp);
    if (CompositeType * ctype = dyn_cast<CompositeType>(tdef->typeValue())) {
      ctype->auxScopes().insert(&tm->paramScope());
    }
  } else {
    Template * tm = Template::get(body, parentScope);
    tm->setAST(tp);
  }
  return body;
}

void ScopeBuilder::checkVariableHiding(Scope * scope, const Defn * de) {
  StringRef name = de->name();
  DASSERT(!name.empty());
  for (Scope * s = scope; s != NULL; s = s->parentScope()) {
    // Local scopes only, please.
    // TODO: Also check parameter scope.
    if (s->allowOverloads()) {
      break;
    }

    DefnList dlist;
    if (scope->lookupMember(name, dlist, false)) {
      if (s == scope) {
        diag.fatal(de) << "'" << name << "' is already defined in this scope";
      } else {
        diag.fatal(de) << "Definition of '" << name << "' hides definition in enclosing scope";
      }
    }
  }
}

void ScopeBuilder::checkNameConflicts(IterableScope * scope) {
  const SymbolTable & scopeMembers = scope->members();
  for (SymbolTable::const_iterator it = scopeMembers.begin(); it != scopeMembers.end(); ++it) {
    const SymbolTable::Entry & entry = it->second;
    if (entry.size() > 1) {
      const Defn * conflictingDefn = NULL;
      const Defn * prevDef = entry.front();
      SymbolTable::Entry::const_iterator sit = entry.begin();
      ++sit;
      for (; sit != entry.end(); ++sit) {
        const Defn * de = *sit;
        switch (prevDef->defnType()) {
          case Defn::Typedef:
            if (de->defnType() != prevDef->defnType()) {
              conflictingDefn = prevDef;
            } else if (de->templateSignature() == NULL && prevDef->templateSignature() == NULL) {
              conflictingDefn = prevDef;
            }
            break;

          case Defn::Function:
          case Defn::Macro:
          case Defn::Property:
            if (de->defnType() != prevDef->defnType()) {
              conflictingDefn = prevDef;
            }
            break;

          case Defn::Let:
          case Defn::Var:
          case Defn::Parameter:
          case Defn::Mod:
          default:
            conflictingDefn = prevDef;
            break;

          case Defn::Namespace:
            break;
        }

        if (conflictingDefn) {
          diag.fatal(de) << "Definition of '" << de << "' conflicts with earlier definition";
          diag.info(conflictingDefn) << "defined here.";
          break;
        }

        prevDef = de;
      }
    }
  }
}

Defn * ScopeBuilder::createDefn(Scope * parent, Module * m, const ASTDecl * ast, StorageClass sc) {
  if (ast->modifiers().flags & Static) {
    sc = Storage_Static;
  }
  DASSERT(parent != NULL);
  switch (ast->nodeType()) {
    case ASTDecl::Class:
    case ASTDecl::Struct:
    case ASTDecl::Interface:
    case ASTDecl::Protocol: {
      TypeDefn * tdef = new TypeDefn(m, static_cast<const ASTTypeDecl *>(ast));
      tdef->setStorageClass(sc);
      Type::TypeClass tc;

      switch (int(ast->nodeType())) {
        case ASTDecl::Class: tc = Type::Class; break;
        case ASTDecl::Struct: tc = Type::Struct; break;
        case ASTDecl::Interface: tc = Type::Interface; break;
        case ASTDecl::Protocol: tc = Type::Protocol; break;
      }

      CompositeType * ctype = new CompositeType(tc, tdef, parent, ast->modifiers().flags);
      tdef->setTypeValue(ctype);
      return tdef;
    }

    case ASTDecl::Enum: {
      TypeDefn * tdef = new TypeDefn(m, static_cast<const ASTTypeDecl *>(ast));
      tdef->setStorageClass(Storage_Static);
      tdef->setTypeValue(new EnumType(tdef, parent));
      return tdef;
    }

    #if 0
    case ASTDecl::Alias:
      break;

    #endif

    case ASTDecl::Let: {
      VariableDefn * result = new VariableDefn(Defn::Let, m, static_cast<const ASTDecl *>(ast));
      result->setStorageClass(sc);
      return result;
    }

    case ASTDecl::Var: {
      VariableDefn * result = new VariableDefn(Defn::Var, m, static_cast<const ASTDecl *>(ast));
      result->setStorageClass(sc);
      return result;
    }

    case ASTDecl::Function: {
      FunctionDefn * result = new FunctionDefn(
          Defn::Function, m, static_cast<const ASTFunctionDecl *>(ast));
      result->setStorageClass(sc);
      return result;
    }

    case ASTDecl::Macro: {
      FunctionDefn * result = new FunctionDefn(
          Defn::Macro, m, static_cast<const ASTFunctionDecl *>(ast));
      result->setStorageClass(sc);
      return result;
    }

    case ASTDecl::Prop: {
      PropertyDefn * result = new PropertyDefn(
          Defn::Property, m, static_cast<const ASTPropertyDecl *>(ast));
      result->setStorageClass(sc);
      return result;
    }

    case ASTDecl::Idx: {
      IndexerDefn * result = new IndexerDefn(
          Defn::Indexer, m, static_cast<const ASTPropertyDecl *>(ast));
      result->setStorageClass(sc);
      return result;
    }

    case ASTDecl::Template:
      return createTemplateDefn(parent, m, static_cast<const ASTTemplate *>(ast), sc);

    case ASTDecl::Namespace:
      return new NamespaceDefn(m, ast);

    case ASTDecl::TypeAlias: {
      TypeDefn * tdef = new TypeDefn(m, static_cast<const ASTTypeDecl *>(ast));
      tdef->setStorageClass(Storage_Global);
      tdef->setTypeValue(new TypeAlias(NULL, tdef));
      return tdef;
    }

    default:
      diag.fatal(ast) << "Can't create member " << nodeTypeName(ast->nodeType());
      return NULL;
  }
}

Defn * ScopeBuilder::mergeNamespace(Scope * parent, const ASTDecl * ast) {
  // Check if there's an existing namespace with this name already.
  DefnList defns;
  if (parent->lookupMember(ast->name(), defns, false)) {
    // Insure that the previous defn is also a namespace.
    NamespaceDefn * ns = NULL;
    for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
      if ((*it)->defnType() != Defn::Namespace) {
        diag.fatal(ast) << "Namespace definition '" << ast->name() <<
            "' conflicts with previous definition:";
        diag.info(*it) << "'" << *it << "' defined here";
      } else {
        ns = static_cast<NamespaceDefn *>(*it);
      }
    }

    if (ns != NULL) {
      // Go ahead and create all of the members of the scope.
      // Normally we do this lazily but in this case it's OK to do it early.
      createScopeMembers(ns, ast->members());
      return ns;
    }
  }

  return NULL;
}

}

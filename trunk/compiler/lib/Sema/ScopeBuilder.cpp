/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/ScopeBuilder.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/NamespaceDefn.h"
#include "tart/CFG/PropertyDefn.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Template.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/StmtAnalyzer.h"
#include "tart/Objects/Builtins.h"

namespace tart {

typedef ASTDeclList::const_iterator decl_iterator;

void ScopeBuilder::createScopeMembers(Defn * parent) {
  createScopeMembers(parent, parent->astMembers());
}

void ScopeBuilder::createScopeMembers(Defn * parent, const ASTDeclList & decs) {
  // If this is a scope-defining definition, then construct the scope.
  switch (parent->defnType()) {
    case Defn::Typedef: {
      TypeDefn * tdef = static_cast<TypeDefn *>(parent);
      Type * type = tdef->typeValue();
      switch (type->typeClass()) {
        case Type::Primitive:
        case Type::Class:
        case Type::Struct:
        case Type::Interface:
        case Type::Protocol:
          createScopeMembers(type->memberScope(), parent, decs);
          break;

        case Type::Enum:
        case Type::NAddress:
        case Type::NPointer:
        case Type::NArray:
        case Type::Alias:
          DFAIL("Unimplemented");
          break;

        default:
          DFAIL("Bad");
      }

      break;
    }

    case Defn::Namespace:
      createScopeMembers(&static_cast<NamespaceDefn *>(parent)->memberScope(), parent, decs);
      break;

    case Defn::Mod:
      createScopeMembers(static_cast<Module *>(parent), parent, decs);
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
    //case Defn::TypeParameter:
    case Defn::DefnTypeCount:
      DFAIL("IllegalState");
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
    getter->setParentDefn(prop);
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
    setter->setParentDefn(prop);
    if (setter->templateSignature() == NULL) {
      setter->copyTrait(prop, Defn::Singular);
    }
  }
}

void ScopeBuilder::createScopeMembers(
    IterableScope * scope, Defn * parent, const ASTDeclList & decs) {
  for (decl_iterator it = decs.begin(); it != decs.end(); ++it) {
    const ASTDecl * de = *it;
    if (de->nodeType() == ASTNode::VarList) {
      createScopeMembers(scope, parent, de->members());
    } else {
      createMemberDefn(scope, parent, de);
    }
  }

  checkNameConflicts(scope);
}

Defn * ScopeBuilder::createMemberDefn(Scope * scope, Defn * parentDefn, const ASTDecl * de) {
  DASSERT(scope != NULL);
  DASSERT(parentDefn != NULL);

  // If it's a namespace, attempt to merge with existing ns of same name.
  if (de->nodeType() == ASTDecl::Namespace) {
    Defn * ns = mergeNamespace(scope, de);
    if (ns != NULL) {
      return ns;
    }
  }

  Defn * member = createDefn(scope, parentDefn->module(), de);
  scope->addMember(member);
  member->setParentDefn(parentDefn);
  member->createQualifiedName(parentDefn);
  member->copyTrait(parentDefn, Defn::Synthetic);
  if (parentDefn->hasUnboundTypeParams() || parentDefn->isTemplateMember()) {
    member->addTrait(Defn::TemplateMember);
  }

  //if (isa<ValueDefn>(member)) {
  //  member->copyTrait(parentDefn, Defn::Final);
  //}

  if (parentDefn->isSingular() && member->templateSignature() == NULL) {
    member->addTrait(Defn::Singular);
  }

  return member;
}

Defn * ScopeBuilder::createLocalDefn(Scope * scope, Defn * parent, const ASTDecl * ast) {
  DASSERT(scope != NULL);
  Defn * defn = createDefn(scope, parent->module(), ast);
  checkVariableHiding(scope, defn);
  scope->addMember(defn);
  defn->createQualifiedName(parent);
  if (defn->templateSignature() == NULL) {
    defn->addTrait(Defn::Singular);
  }

  createScopeMembers(defn);
  return defn;
}

Defn * ScopeBuilder::createTemplateDefn(Scope * scope, Module * m, const ASTTemplate * tp) {
  DASSERT(scope != NULL);
  Defn * body = createDefn(scope, m, tp->body());
  TemplateSignature * tsig = TemplateSignature::get(body, scope);
  tsig->setAST(tp);
  return body;
}

void ScopeBuilder::checkVariableHiding(Scope * scope, const Defn * de) {
  const char * name = de->name();
  DASSERT(name != NULL);
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

Defn * ScopeBuilder::createDefn(Scope * parent, Module * m, const ASTDecl * ast) {
  DASSERT(parent != NULL);
  switch (ast->nodeType()) {
    case ASTDecl::Class:
    case ASTDecl::Struct:
    case ASTDecl::Interface:
    case ASTDecl::Protocol: {
      TypeDefn * tdef = new TypeDefn(m, static_cast<const ASTTypeDecl *>(ast));
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
      tdef->setTypeValue(new EnumType(tdef, parent));
      return tdef;
    }

    #if 0
    case ASTDecl::Alias:
      break;

#endif

    case ASTDecl::Let:
      return new VariableDefn(Defn::Let, m, static_cast<const ASTDecl *>(ast));

    case ASTDecl::Var:
      return new VariableDefn(Defn::Var, m, static_cast<const ASTDecl *>(ast));

    case ASTDecl::Function:
      return new FunctionDefn(Defn::Function, m, static_cast<const ASTFunctionDecl *>(ast));

    case ASTDecl::Macro:
      return new FunctionDefn(Defn::Macro, m, static_cast<const ASTFunctionDecl *>(ast));

    case ASTDecl::Prop:
      return new PropertyDefn(Defn::Property, m, static_cast<const ASTPropertyDecl *>(ast));

    case ASTDecl::Idx:
      return new IndexerDefn(Defn::Indexer, m, static_cast<const ASTPropertyDecl *>(ast));

    case ASTDecl::Template:
      return createTemplateDefn(parent, m, static_cast<const ASTTemplate *>(ast));

    case ASTDecl::Namespace:
      return new NamespaceDefn(m, ast);

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

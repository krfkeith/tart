/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Defn.h"
#include "tart/Defn/Module.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/CompositeType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/ADT/Twine.h"

namespace tart {

// -------------------------------------------------------------------
// Defn
Defn::Defn(DefnType dtype, Module * m, StringRef nm)
  : defnType_(dtype)
  , loc(SourceLocation())
  , name_(nm)
  , ast_(NULL)
  , md_(NULL)
  , storage_(Storage_Global)
  , module_(m)
  , parentDefn_(NULL)
  , nextInScope_(NULL)
  , tsig_(NULL)
  , tinst_(NULL)
{
}

Defn::Defn(DefnType dtype, Module * m, const ASTDecl * de)
  : defnType_(dtype)
  , loc(de->location())
  , name_(de->name())
  , ast_(de)
  , md_(NULL)
  , modifiers_(de->modifiers())
  , storage_(Storage_Global)
  , module_(m)
  , parentDefn_(NULL)
  , nextInScope_(NULL)
  , tsig_(NULL)
  , tinst_(NULL)
{
}

StringRef Defn::qualifiedName() const {
  if (qname_.empty()) {
    diag.fatal(this) << "Unqualified name " << name_;
  }

  return qname_;
}

void Defn::createQualifiedName(Defn * parent) {
  DASSERT_OBJ(qname_.empty(), this);

  if (parent != NULL) {
    StringRef qualifier =
        (parent->defnType() == Mod)
            ? static_cast<Module *>(parent)->packageName()
            : parent->qualifiedName();
    if (!qualifier.empty()) {

      (Twine(qualifier) + "." + StringRef(name_)).toVector(qname_);
      return;
    }
  }

  qname_ = name_;
}

StringRef Defn::linkageName() const {
  if (lnkName.empty()) {
    if (tinst_ != NULL && tinst_->templateDefn() == Builtins::typeArray.typeDefn()) {
      // Handle arrays specially.
      typeLinkageName(lnkName, (*tinst_->typeArgs())[0]);
      lnkName += "[]";
      return lnkName;
    }

    if (parentDefn_ != NULL && parentDefn_->defnType() != Defn::Mod) {
      lnkName = parentDefn_->linkageName();
      lnkName += ".";
      lnkName += name_;
    } else {
      lnkName = qualifiedName();
    }

    // Template instance parameters.
    if (tinst_ != NULL) {
      lnkName += "[";
      const TupleType * typeArgs = tinst_->typeArgs();
      int index = 0;
      Template * tsig = tinst_->templateDefn()->templateSignature();
      for (TupleType::const_iterator it = typeArgs->begin(); it != typeArgs->end(); ++it, ++index) {
        if (it != typeArgs->begin()) {
          lnkName += ",";
        }

        if (tsig->isVariadicParam(index)) {
          Qualified<TupleType> variadicArgs = it->as<TupleType>();
          for (TupleType::const_iterator t = variadicArgs->begin(); t != variadicArgs->end(); ++t) {
            if (t != variadicArgs->begin()) {
              lnkName += ",";
            }
            typeLinkageName(lnkName, *t);
          }
        } else {
          typeLinkageName(lnkName, *it);
        }
      }

      lnkName += "]";
    } else if (tsig_ != NULL) {
      lnkName += "[";
      typeLinkageName(lnkName, tsig_->typeParams());
      lnkName += "]";
    }
  }

  return lnkName;
}

const SourceLocation & Defn::location() const {
  //static const SourceLocation defaultLocation;
  return loc;
  //return ast_ ? ast_->location() : defaultLocation;
}

Module * Defn::sourceModule() const {
  if (isSynthetic()) {
    for (const Defn * de = this; de != NULL; de = de->parentDefn()) {
      if (de->templateInstance() != NULL) {
        return de->templateInstance()->templateDefn()->module();
      }
    }
  }

  return module_;
}

Defn * Defn::moduleLevelParent() const {
  Defn * de = const_cast<Defn *>(this);
  for (;;) {
    Defn * parent = de->parentDefn();
    DASSERT_OBJ(parent != NULL, this);
    if (parent->defnType() == Defn::Mod) {
      return de;
    }
    de = parent;
  }
}

TypeDefn * Defn::enclosingClassDefn() const {
  Defn * parent = parentDefn();
  if (parent == NULL) {
    return NULL;
  } else if (parent->defnType() == Typedef) {
    return cast<TypeDefn>(parent);
  } else if (LVALUE_DEFS.contains(parent->defnType())) {
    return parent->enclosingClassDefn();
  } else {
    return NULL;
  }
}

bool Defn::hasUnboundTypeParams() const {
  if (tsig_ != NULL) {
    return tsig_->patternVarCount() > 0;
  }

  return false;
}

void Defn::dumpHierarchy(bool full) const {
  const char * kind;
  switch (defnType_) {
    case Defn::Typedef: {
      QualifiedType ty = static_cast<const TypeDefn *>(this)->value();
      switch (ty->typeClass()) {
        case Type::Primitive: kind = "type"; break;
        case Type::Class: kind = "class"; break;
        case Type::Struct: kind = "struct"; break;
        case Type::Interface: kind = "interface"; break;
        case Type::Enum: kind = "enum"; break;
        case Type::Alias: kind = "typealias"; break;
        default:
          kind = "unknown";
          break;
      }

      break;
    }

    case Defn::Namespace: kind = "namespace"; break;
    case Defn::Var: kind = "var"; break;
    case Defn::Let: kind = "let"; break;
    case Defn::Property: kind = "property"; break;
    case Defn::Indexer: kind = "indexer"; break;
    case Defn::Function: kind = "def"; break;
    case Defn::Macro: kind = "macro"; break;
    case Defn::Parameter: kind = "param"; break;
    case Defn::Mod: kind = "module"; break;
    default:
      kind = "unknown";
      break;
  };

  //diag.info() << kind << " " << qualifiedName();

  llvm::SmallString<256> out;
  out += kind;
  if (isTemplate()) {
    out += " <>";
  }
  out += " ";
  out += name();
  if (isTemplateInstance()) {
    out += "<>";
  }
  out += " ";
  //members.getDebugSummary(out);

  diag.writeLnIndent(out);
}

void Defn::trace() const {
  loc.trace();
  safeMark(ast_);
  safeMark(tsig_);
  safeMark(tinst_);
  safeMark(module_);
  //safeMark(parentDefn_);
  markList(attrs_.begin(), attrs_.end());
}

// -------------------------------------------------------------------
// ValueDefn

const CompositeType * ValueDefn::definingClass() const {
  TypeDefn * enclosingType = enclosingClassDefn();
  if (enclosingType != NULL) {
    return dyn_cast<CompositeType>(enclosingType->typePtr());
  }

  return NULL;
}

void ValueDefn::trace() const {
  Defn::trace();
}

/// -------------------------------------------------------------------
/// ExplicitImportDefn
void ExplicitImportDefn::format(FormatStream & out) const {
  out << "[import " << name() << "]";
}

void ExplicitImportDefn::trace() const {
  Defn::trace();
  markList(importDefs_.begin(), importDefs_.end());
}

// -------------------------------------------------------------------
// Utility functions
void formatParameterList(FormatStream & out, const ParameterList & params) {
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    const ParameterDefn * param = *it;
    if (it != params.begin()) {
      out << ", ";
    }

    if (!param->name().empty()) {
      out << param->name();
    }

    if (out.getShowType() && param->type()) {
      out << ":" << param->type();
      if (param->isVariadic()) {
        out << "...";
      }
    }

    if (out.isVerbose() && param->initValue()) {
      out << "=" << param->initValue();
    }
  }
}

} // namespace tart

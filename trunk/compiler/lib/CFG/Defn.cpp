/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/CompositeType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

namespace {
  bool isOverloadable(Defn::DefnType dt) {
    return dt == Defn::Function || dt == Defn::Macro;
  }
}

// -------------------------------------------------------------------
// Defn
Defn::Defn(DefnType dtype, Module * m, const char * nm)
  : defnType_(dtype)
  , loc(SourceLocation())
  , name_(nm)
  , ast_(NULL)
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
  , modifiers_(de->modifiers())
  , module_(m)
  , parentDefn_(NULL)
  , nextInScope_(NULL)
  , tsig_(NULL)
  , tinst_(NULL)
{
  if (modifiers_.flags & tart::ReadOnly) {
    addTrait(ReadOnly);
  }
}

const std::string & Defn::qualifiedName() const {
  if (qname_.empty()) {
    diag.fatal(this) << "Unqualified name " << name_;
  }

  return qname_;
}

std::string & Defn::qualifiedName() {
  if (qname_.empty()) {
    diag.fatal(this) << "Unqualified name " << name_;
  }

  return qname_;
}

void Defn::createQualifiedName(Defn * parent) {
  DASSERT_OBJ(qname_.empty(), this);

  if (parent != NULL) {
    const std::string & qualifier =
        (parent->defnType() == Mod)
            ? static_cast<Module *>(parent)->packageName()
            : parent->qualifiedName();
    if (!qualifier.empty()) {
      qname_ = qualifier + "." + name_;
      return;
    }
  }

  qname_ = name_;
}

const std::string & Defn::linkageName() const {
  if (lnkName.empty()) {
    if (tinst_ != NULL && tinst_->templateDefn() == Builtins::typeArray.typeDefn()) {
      // Handle arrays specially.
      typeLinkageName(lnkName, (*tinst_->typeArgs())[0]);
      lnkName.append("[]");
      return lnkName;
    }

    if (parentDefn_ != NULL && parentDefn_->defnType() != Defn::Mod) {
      lnkName = parentDefn_->linkageName();
      lnkName.append(".");
      lnkName.append(name_);
    } else {
      lnkName.assign(qualifiedName());
    }

    // Template instance parameters.
    if (tinst_ != NULL) {
      lnkName.append("[");
      const TupleType * typeArgs = tinst_->typeArgs();
      int index = 0;
      TemplateSignature * tsig = tinst_->templateDefn()->templateSignature();
      for (TupleType::const_iterator it = typeArgs->begin(); it != typeArgs->end(); ++it, ++index) {
        const TupleType * variadicArgs = NULL;
        if (const TypeVariable * tv = dyn_cast<TypeVariable>(tsig->typeParam(index))) {
          if (tv->isVariadic()) {
            variadicArgs = cast<TupleType>(*it);
            if (variadicArgs->size() == 0) {
              break;
            }
          }
        }

        if (it != typeArgs->begin()) {
          lnkName.append(",");
        }

        // Special formatting for variadic template params
        if (variadicArgs != NULL) {
          for (TupleType::const_iterator t = variadicArgs->begin(); t != variadicArgs->end(); ++t) {
            if (t != variadicArgs->begin()) {
              lnkName.append(",");
            }
            typeLinkageName(lnkName, *t);
          }
        } else {
          typeLinkageName(lnkName, *it);
        }
      }

      lnkName.append("]");
    } else if (tsig_ != NULL) {
      lnkName.append("[");
      typeLinkageName(lnkName, tsig_->typeParams());
      lnkName.append("]");
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

bool Defn::beginPass(DefnPass pass) {
  if (finished_.contains(pass)) {
    return false;
  }

  running_.add(pass);
  return true;
}

void Defn::dumpHierarchy(bool full) const {
  const char * kind;
  switch (defnType_) {
    case Defn::Typedef: {
      const Type * ty = static_cast<const TypeDefn *>(this)->typeValue();
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

  std::string out;
  out.append(kind);
  if (isTemplate()) {
    out.append(" <>");
  }
  out.append(" ");
  out.append(name());
  if (isTemplateInstance()) {
    out.append("<>");
  }
  out.append(" ");
  //members.getDebugSummary(out);

  diag.writeLnIndent(out);
}

void Defn::trace() const {
  loc.trace();
  safeMark(ast_);
  safeMark(tsig_);
  safeMark(tinst_);
  safeMark(module_);
  safeMark(parentDefn_);
}

// -------------------------------------------------------------------
// ValueDefn

const CompositeType * ValueDefn::definingClass() const {
  TypeDefn * enclosingType = enclosingClassDefn();
  if (enclosingType != NULL) {
    return dyn_cast<CompositeType>(enclosingType->typeValue());
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
  markList(importValues_.begin(), importValues_.end());
}

// -------------------------------------------------------------------
// Utility functions
void formatParameterList(FormatStream & out, const ParameterList & params) {
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    const ParameterDefn * param = *it;
    if (it != params.begin()) {
      out << ", ";
    }

    if (param->name() != NULL) {
      out << param->name();
    }

    if (out.getShowType() && param->type() != NULL) {
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

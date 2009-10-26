/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Defn.h"
#include "tart/CFG/Scope.h"
#include "tart/CFG/Type.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Template.h"
#include "tart/AST/ASTDecl.h"
#include "tart/Objects/Builtins.h"
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
  if (modifiers_.flags & tart::Final) {
    addTrait(Final);
  }

  if (modifiers_.flags & tart::Abstract) {
    addTrait(Abstract);
  }

  if (modifiers_.flags & tart::Undef) {
    addTrait(Undefined);
  }

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
      for (Defn * arg = tinst_->firstParamDefn(); arg != NULL; arg = arg->nextInScope()) {
        // Last arg is the defn itself.
        // TODO: Should be a better way to do this.
        if (arg->nextInScope() == NULL) {
          break;
        }

        if (arg != tinst_->firstParamDefn()) {
          lnkName.append(",");
        }

        if (TypeDefn * typeAlias = dyn_cast<TypeDefn>(arg)) {
          typeLinkageName(lnkName, typeAlias->typeValue());
        } else {
          VariableDefn * argDefn = cast<VariableDefn>(arg);
          Expr * argVal = argDefn->initValue();
          if (ConstantExpr * lval = dyn_cast<ConstantExpr>(argVal)) {
            DFAIL("Implement");
          } else {
            DFAIL("Non-constant template argument");
          }
        }
      }

      lnkName.append("]");
    }
  }

  return lnkName;
}

const SourceLocation & Defn::location() const {
  static const SourceLocation defaultLocation;
  return ast_ ? ast_->location() : defaultLocation;
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

  if (running_.contains(pass)) {
    diag.fatal(this) << "Infinite recursion during " << pass << " of " << this;
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
// NamespaceDefn
NamespaceDefn::NamespaceDefn(Module * m, const char * name)
  : Defn(Namespace, m, name)
{
  members.setScopeName(name);
}

NamespaceDefn::NamespaceDefn(Module * m, const ASTDecl * de)
  : Defn(Namespace, m, de)
{
  members.setScopeName(name());
}

void NamespaceDefn::format(FormatStream & out) const {
  out << "namespace " << name_;
}

void NamespaceDefn::trace() const {
  Defn::trace();
  members.trace();
}

// -------------------------------------------------------------------
// ValueDefn
void ValueDefn::trace() const {
  Defn::trace();
}

// -------------------------------------------------------------------
// VariableDefn
void VariableDefn::trace() const {
  ValueDefn::trace();
  type_.trace();
  safeMark(initValue_);
}

void VariableDefn::format(FormatStream & out) const {
  if (out.isVerbose()) {
    switch (defnType()) {
      case Defn::Let: out << "let "; break;
      case Defn::Var: out << "var "; break;
      default:
        break;
    }
  }

  if (out.getShowQualifiedName()) {
    if (out.getShowType() && enclosingClassDefn()) {
      out << enclosingClassDefn() << "." << name_;
    } else {
      out << qname_;
    }
  } else {
    out << name_;
  }

  if (out.getShowType() && type_.isDefined()) {
    out << ":" << type_;
  }

  if (out.getShowInitializer() && initValue_) {
    out << "=" << initValue_;
  }
}

// -------------------------------------------------------------------
// PropertyDefn
void PropertyDefn::trace() const {
  ValueDefn::trace();
  type_.trace();
  safeMark(getter_);
  safeMark(setter_);
}

void PropertyDefn::format(FormatStream & out) const {
  if (out.getShowQualifiedName()) {
    if (out.getShowType() && enclosingClassDefn()) {
      out << enclosingClassDefn() << "." << name_;
    } else {
      out << qname_;
    }
  } else {
    out << name_;
  }
}

// -------------------------------------------------------------------
// IndexerDefn
void IndexerDefn::trace() const {
  PropertyDefn::trace();
}

void IndexerDefn::format(FormatStream & out) const {
  if (out.isVerbose()) {
    out << "def ";
  }

  if (out.getShowQualifiedName() && !qname_.empty()) {
    if (out.getShowType() && parentDefn()) {
      out << parentDefn() << "[]";
    } else {
      out << qname_;
    }
  } else {
    out << "[]";
  }

  FunctionType * ftype = dyn_cast_or_null<FunctionType>(type().type());
  if (out.getShowType() && ftype != NULL) {
    out << "(";
    formatParameterList(out, ftype->params());
    out << ")";
    if (ftype->returnType().isNonVoidType()) {
      out << ":" << ftype->returnType();
    }
  }
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

    if (out.getShowType() && param->type().isDefined()) {
      out << ":" << param->type();
    }

    if (out.isVerbose() && param->initValue()) {
      out << "=" << param->initValue();
    }
  }
}

const char * getPassName(DefnPass pass) {
  switch (pass) {
    case Pass_CreateMembers:
      return "CreateMembers";

    case Pass_ResolveAttributes:
      return "ResolveAttributes";

    case Pass_ResolveImport:
      return "ResolveImport";

    case DefnPassCount:
      DFAIL("Invalid pass");
  }

  DFAIL("Invalid pass");
}

FormatStream & operator<<(FormatStream & out, DefnPass pass) {
  out << getPassName(pass);
  return out;
}

} // namespace tart

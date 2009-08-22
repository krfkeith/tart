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

  // Given a type, append the linkage name of that type to the output buffer.
  void typeLinkageName(std::string & out, Type * ty) {
    ty = dealias(ty);
    if (TypeDefn * td = ty->typeDefn()) {
      out.append(td->getLinkageName());
    } else if (FunctionType * ftype = dyn_cast<FunctionType>(ty)) {
      out.append("fn");
      if (!ftype->params().empty()) {
        out.append("(");
        ParameterList & params = ftype->params();
        for (ParameterList::iterator it = params.begin(); it != params.end(); ++it) {
          if (it != params.begin()) {
            out.append(",");
          }
          
          typeLinkageName(out, (*it)->getType());
        }
        out.append(")");
      }
      
      if (ftype->returnType() != NULL && !ftype->returnType()->isVoidType()) {
        out.append("->");
        typeLinkageName(out, ftype->returnType());
      }
    } else {
      DFAIL("Can't compute linkage name of type");
    }
  }
}
  
// -------------------------------------------------------------------
// Defn
Defn::Defn(DefnType dtype, Module * m, const char * nm)
  : defnType_(dtype)
  , loc(SourceLocation())
  , name_(nm)
  , ast(NULL)
  , module_(m)
  , parentDefn_(NULL)
  , nextInScope_(NULL)
  , tsig_(NULL)
  , tinst_(NULL)
{
}

Defn::Defn(DefnType dtype, Module * m, const ASTDecl * de)
  : defnType_(dtype)
  , loc(de->getLocation())
  , name_(de->name())
  , ast(de)
  , modifiers(de->modifiers())
  , module_(m)
  , parentDefn_(NULL)
  , nextInScope_(NULL)
  , tsig_(NULL)
  , tinst_(NULL)
{
  if (modifiers.flags & tart::Final) {
    addTrait(Final);
  }

  if (modifiers.flags & tart::Abstract) {
    addTrait(Abstract);
  }

  if (modifiers.flags & tart::ReadOnly) {
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

const std::string & Defn::getLinkageName() const {
  if (lnkName.empty()) {
    /*if (isExtern()) {
      // Look for external name.
      // TODO: Do this using the internal evaluator instead of looking at the
      // call.
      const Expr * externAttr = findAttribute("tart.core.Extern");
      DASSERT_OBJ(externAttr != NULL, this);
      const CallExpr * call = dyn_cast<CallExpr>(externAttr);
      const ConstantString * extName = dyn_cast<ConstantString>(call->arg(0));
      DASSERT_OBJ(extName != NULL, this);
      lnkName.assign(extName->value());
      return lnkName;
    }*/

    if (parentDefn_ != NULL && parentDefn_->defnType() != Defn::Mod) {
      lnkName = parentDefn_->getLinkageName();
      lnkName.append(".");
      lnkName.append(name_);
    } else {
      lnkName.assign(qualifiedName());
    }

    // Template instance parameters.
    if (tinst_ != NULL) {
      lnkName.append("[");
      for (Defn * arg = tinst_->getFirstArg(); arg != NULL;
          arg = arg->nextInScope()) {
        // Last arg is the defn itself.
        // TODO: Should be a better way to do this.
        if (arg->nextInScope() == NULL) {
          break;
        }

        if (arg != tinst_->getFirstArg()) {
          lnkName.append(",");
        }

        if (TypeDefn * typeAlias = dyn_cast<TypeDefn>(arg)) {
          typeLinkageName(lnkName, typeAlias->getTypeValue());
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
  
const Expr * Defn::findAttribute(const Type * attrType) const {
  DASSERT(attrType != NULL);
  for (ExprList::const_iterator it = attrs_.begin(); it != attrs_.end(); ++it) {
    Expr * attr = *it;
    if (attr->getType()->isEqual(attrType)) {
      return attr;
    }
  }

  return NULL;
}

const Expr * Defn::findAttribute(const char * attrTypeName) const {
  DASSERT(attrTypeName != NULL);
  for (ExprList::const_iterator it = attrs_.begin(); it != attrs_.end(); ++it) {
    Expr * attr = *it;
    Type * attrType = dealias(attr->getType());
    if (TypeDefn * tdef = attrType->typeDefn()) {
      if (tdef->qualifiedName() == attrTypeName) {
        return attr;
      }
    }
  }

  return NULL;
}

const SourceLocation & Defn::getLocation() const {
  static const SourceLocation defaultLocation;
  return ast ? ast->getLocation() : defaultLocation;
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

bool Defn::beginPass(DefnPass pass) {
  if (finished.contains(pass)) {
    return false;
  }
  
  if (running.contains(pass)) {
    diag.fatal(this) << "Infinite recursion during " << pass << " of " << this;
    return false;
  }
  
  running.add(pass);
  return true;
}

void Defn::dumpHierarchy(bool full) const {
  const char * kind;
  switch (defnType_) {
    case Defn::Typedef: {
      const Type * ty = static_cast<const TypeDefn *>(this)->getTypeValue();
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
  safeMark(ast);
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
// TypeDefn
#if 0
Type * TypeDefn::metaType() const {
  switch (getTypeValue()->typeClass()) {
    case Type::Primitive:
      DASSERT(Builtins::typeType != NULL);
      return Builtins::typeType;
      
    case Type::Class:
      DASSERT(Builtins::typeClass != NULL);
      return Builtins::typeClass;

    case Type::Struct:
      DASSERT(Builtins::typeStruct != NULL);
      return Builtins::typeStruct;

    case Type::Interface:
      DASSERT(Builtins::typeInterface != NULL);
      return Builtins::typeInterface;

    case Type::Enum:
      DASSERT(Builtins::typeEnum != NULL);
      return Builtins::typeEnum;

    case Type::Alias:
      DFAIL("Implement");
      
    default:
      DFAIL("Not a type");
  }
}
#endif

ConstantType * TypeDefn::asExpr() {
  DASSERT(Builtins::typeType != NULL);
  if (expr_ == NULL) {
    expr_ = new ConstantType(ast ? ast->getLocation() : SourceLocation(), this);
  }
  
  return expr_;
}

void TypeDefn::trace() const {
  Defn::trace();
  value->mark();
  safeMark(expr_);
}

void TypeDefn::format(FormatStream & out) const {
  if (out.getShowQualifiedName()) {
    out << qname_;
  } else {
    out << name_;
  }

  if (isTemplate() /*&& out.getShowType()*/) {
    templateSignature()->format(out);
  } else if (templateInstance() != NULL) {
    templateInstance()->format(out);
  }
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
  safeMark(type_);
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
    out << qname_;
  } else {
    out << name_;
  }

  if (out.getShowType() && type_) {
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
  safeMark(type_);
  safeMark(getter_);
  safeMark(setter_);
}

void PropertyDefn::format(FormatStream & out) const {
  if (out.getShowQualifiedName()) {
    out << qname_;
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
  out << "[]";
}

/// -------------------------------------------------------------------
/// ExplicitImportDefn
void ExplicitImportDefn::format(FormatStream & out) const {
  out << "[import " << name() << "]";
}

void ExplicitImportDefn::trace() const {
  Defn::trace();
  markList(importValues.begin(), importValues.end());
}

// -------------------------------------------------------------------
// Utility functions
void formatParameterList(FormatStream & out, const ParameterList & params) {
  for (ParameterList::const_iterator it = params.begin(); it != params.end();
      ++it) {
    if (it != params.begin()) {
      out << ", ";
    }
    
    out << *it;
  }
}

const char * getPassName(DefnPass pass) {
  switch (pass) {
    case Pass_CreateMembers:
      return "CreateMembers";

    case Pass_ResolveBaseTypes:
      return "ResolveBaseTypes";
  
    case Pass_ResolveAttributes:
      return "ResolveAttributes";

    case Pass_AnalyzeConstructors:
      return "AnalyzeConstructors";

    case Pass_AnalyzeFields:
      return "AnalyzeFields";

    case Pass_AnalyzeMethods:
      return "AnalyzeMethods";

    case Pass_ResolveOverloads:
      return "ResolveOverloads";
  
    case Pass_ResolveReturnType:
      return "ResolveReturnType";
  
    case Pass_ResolveParameterTypes:
      return "ResolveParameterTypes";
  
    case Pass_ResolveVarType:
      return "ResolveVarType";
  
    case Pass_ResolveElementType:
      return "ResolveElementType";

    case Pass_CreateCFG:
      return "CreateCFG";

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

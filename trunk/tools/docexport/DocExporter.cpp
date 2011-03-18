/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "DocExporter.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/VariableDefn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/NamespaceDefn.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/EnumType.h"

namespace tart {

using llvm::dyn_cast;

void DocExporter::begin() {
  xml_.writeXmlPrologue();
  xml_.beginElement("module-list");
}

void DocExporter::end() {
  xml_.endElement("module-list");
}

void DocExporter::generate(tart::Module * mod) {
  if (!mod->astMembers().empty()) {
    exportModule(mod);
  }
}

void DocExporter::exportModule(const Module * mod) {
  xml_.beginElement("module");
  xml_.appendAttribute("name", mod->qualifiedName());
  writeModifiers(mod);
  writeMembers(mod);
  xml_.endElement("module");
}

void DocExporter::exportNamespace(const NamespaceDefn * ns) {
  xml_.beginElement("namespace");
  xml_.appendAttribute("name", ns->name());
  writeModifiers(ns);
  writeAttributes(ns);
  writeMembers(&ns->memberScope());
  writeDocComment(ns);
  xml_.endElement("namespace");
}

void DocExporter::exportCompositeType(const CompositeType * ctype) {
  const char * kind = NULL;
  switch (ctype->typeClass()) {
    case Type::Class:
      kind = "class";
      break;
    case Type::Interface:
      kind = "interface";
      break;
    case Type::Struct:
      kind = "struct";
      break;
    case Type::Protocol:
      kind = "protocol";
      break;
    default:
      return;
  }

  TypeDefn * td = ctype->typeDefn();

  xml_.beginElement("typedef");
  xml_.appendAttribute("type", kind);
  xml_.appendAttribute("name", td->name());
  writeModifiers(td);
  writeAttributes(td);
  writeMembers(ctype->memberScope());
  writeDocComment(td);
  xml_.endElement("typedef");
}

void DocExporter::exportEnumType(const EnumType * etype) {
  TypeDefn * td = etype->typeDefn();

  xml_.beginElement("typedef");
  xml_.appendAttribute("type", "enum");
  xml_.appendAttribute("name", td->name());
  writeModifiers(td);
  writeAttributes(td);
  // TODO: Members
  writeDocComment(td);
  xml_.endElement("typedef");
}

void DocExporter::exportMethod(const FunctionDefn * method) {
  const char * elName = NULL;
  if (method->defnType() == Defn::Macro) {
    elName = "macro";
  } else if (method->isUndefined()) {
    elName = "undef";
  } else if (method->isOverride()) {
    elName = "override";
  } else {
    elName = "method";
  }

  // TODO: Process the doc comment before writing out the params so we can
  // associate each param with a comment.

  xml_.beginElement(elName);
  xml_.appendAttribute("name", method->name());
  writeModifiers(method);
  writeAttributes(method);
  if (method->type() != NULL) {
    if (!method->returnType()->isVoidType()) {
      writeTypeExpression("returnType", method->returnType());
    }
    for (ParameterList::const_iterator it = method->params().begin(); it != method->params().end();
        ++it) {
      ParameterDefn * p = *it;
      xml_.beginElement("param");
      xml_.appendAttribute("name", p->name());
      writeAttributes(p);
      writeTypeExpression("type", p->type());
      writeDocComment(p);
      xml_.endElement("param");
    }
  }
  writeDocComment(method);
  xml_.endElement(elName);
}

void DocExporter::exportVariable(const VariableDefn * var) {
  const char * elName = var->defnType() == Defn::Let ? "let" : "var";
  xml_.beginElement(elName);
  xml_.appendAttribute("name", var->name());
  writeModifiers(var);
  writeAttributes(var);
  if (var->type() != NULL) {
    writeTypeExpression("type", var->type());
  }
  writeDocComment(var);
  xml_.endElement(elName);
}

void DocExporter::exportProperty(const PropertyDefn * prop) {
  xml_.beginElement("property");
  xml_.appendAttribute("name", prop->name());
  writeModifiers(prop);
  writeAttributes(prop);
  if (prop->type() != NULL) {
    writeTypeExpression("type", prop->type());
  }
  writeDocComment(prop);
  xml_.endElement("property");
}

void DocExporter::exportIndexer(const IndexerDefn * idx) {
  xml_.beginElement("indexer");
  xml_.appendAttribute("name", idx->name());
  writeModifiers(idx);
  writeAttributes(idx);
  if (idx->type() != NULL) {
    writeTypeExpression("type", idx->type());
  }
  writeDocComment(idx);
  xml_.endElement("indexer");
}

void DocExporter::writeAttributes(const Defn * de) {
  // Only export retained attributes
  ExprList exportableAttributes;
  for (ExprList::const_iterator it = de->attrs().begin(), itEnd = de->attrs().end();
      it != itEnd; ++it) {
    if (const CompositeType * attrType = dyn_cast<CompositeType>((*it)->type())) {
      DASSERT(attrType->isAttribute());
    }
  }
}

void DocExporter::writeMembers(const IterableScope * scope) {
  for (const Defn * de = scope->firstMember(); de != NULL; de = de->nextInScope()) {
    switch (de->defnType()) {
      case Defn::Typedef: {
        const TypeDefn * td = static_cast<const TypeDefn *>(de);
        const Type * type = td->typeValue();
        if (const CompositeType * cty = dyn_cast<CompositeType>(type)) {
          exportCompositeType(cty);
        } else if (const EnumType * ety = dyn_cast<EnumType>(type)) {
          exportEnumType(ety);
        }
        break;
      }

      case Defn::Function:
        exportMethod(static_cast<const FunctionDefn *>(de));
        break;

      case Defn::Var:
      case Defn::Let:
        exportVariable(static_cast<const VariableDefn *>(de));
        break;

      case Defn::Property:
        exportProperty(static_cast<const PropertyDefn *>(de));
        break;

      case Defn::Indexer:
        exportIndexer(static_cast<const IndexerDefn *>(de));
        break;

      case Defn::Namespace:
        exportNamespace(static_cast<const NamespaceDefn *>(de));
        break;

      default:
        break;
    }
  }
}

void DocExporter::writeTypeExpression(llvm::StringRef tagName, const Type * ty) {
  xml_.beginElement(tagName);
  writeTypeRef(ty);
  xml_.endElement(tagName);
}

void DocExporter::writeTypeRef(const Type * ty) {
  switch (ty->typeClass()) {
    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      // TODO: Arrays

      const CompositeType * ctype = static_cast<const CompositeType *>(ty);
      TypeDefn * td = ctype->typeDefn();
      xml_.writeCharacterData(td->qualifiedName());
      // TODO: template instance params.
      break;
    }

    case Type::Enum: {
      const EnumType * etype = static_cast<const EnumType *>(ty);
      TypeDefn * td = etype->typeDefn();
      xml_.beginElement("enum");
      xml_.writeCharacterData(td->qualifiedName());
      xml_.endElement("enum");
      break;
    }

    case Type::Primitive: {
      const PrimitiveType * ptype = static_cast<const PrimitiveType *>(ty);
      TypeDefn * td = ptype->typeDefn();
      break;

    }

    case Type::Function: {
      break;
    }

    case Type::Tuple: {
      break;
    }

    case Type::Union: {
      break;
    }

    case Type::NAddress: {
      break;
    }

    case Type::NArray: {
      break;
    }

    case Type::FlexibleArray: {
      break;
    }

    case Type::Unit: {
      break;
    }

    case Type::TypeLiteral: {
      break;
    }

    case Type::Alias: {
      break;
    }

    default:
      break;
  }
}

void DocExporter::writeDocComment(const Defn * de) {
  if (de->ast() != NULL && !de->ast()->docComment().empty()) {
    xml_.beginElement("doc");
    llvm::SmallString<256> docCommentText;
    de->ast()->docComment().toString(docCommentText);
    xml_.writeCharacterData(docCommentText);
    xml_.endElement("doc");
  }
}

void DocExporter::writeModifiers(const Defn * de) {
  switch (de->visibility()) {
    case Public:
      xml_.appendAttribute("visibility", "public");
      break;
    case Protected:
      xml_.appendAttribute("visibility", "protected");
      break;
    case Private:
      xml_.appendAttribute("visibility", "private");
      break;
    case Internal:
      xml_.appendAttribute("visibility", "internal");
      break;
  }

  if (de->storageClass() == Storage_Static) {
    xml_.appendAttribute("static", "true");
  }
}

void DocExporter::writeElement(llvm::StringRef elName, llvm::StringRef content) {
  xml_.beginElement(elName);
  xml_ << content;
  xml_.endElement(elName);
}

}

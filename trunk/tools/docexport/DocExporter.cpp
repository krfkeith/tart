/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "DocExporter.h"
#include "DocCommentProcessor.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/VariableDefn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/NamespaceDefn.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/UnionType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

namespace tart {

using llvm::dyn_cast;

void DocExporter::begin() {
  xml_.writeXmlPrologue();
  xml_.beginElement("module-list");
  xml_.appendAttribute("xmlns", "http://org.viridia/2011/tart/doc");
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
  writeDocComment(ns);
  writeMembers(&ns->memberScope());
  xml_.endElement("namespace");
}

void DocExporter::exportCompositeType(const CompositeType * ctype) {
  const char * kind = NULL;
  const char * group = NULL;

  switch (ctype->typeClass()) {
    case Type::Class:
      kind = "class";
      if (ctype->isAttribute()) {
        group = "attribute";
      } else if (ctype->isSubclassOf(Builtins::typeThrowable)) {
        group = "exception";
      }
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
  if (group != NULL) {
    xml_.appendAttribute("group", group);
  }
  writeModifiers(td);
  writeAttributes(td);
  for (ClassList::const_iterator it = ctype->bases().begin(); it != ctype->bases().end(); ++it) {
    writeTypeExpression("base-type", *it);
  }
  writeDocComment(td);
  writeMembers(ctype->memberScope());
  xml_.endElement("typedef");
}

void DocExporter::exportEnumType(const EnumType * etype) {
  TypeDefn * td = etype->typeDefn();

  xml_.beginElement("typedef");
  xml_.appendAttribute("type", "enum");
  xml_.appendAttribute("name", td->name());
  writeModifiers(td);
  writeAttributes(td);
  writeDocComment(td);
  // TODO: Members
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
  writeDocComment(method);
  if (method->type() != NULL) {
    if (!method->returnType()->isVoidType()) {
      writeTypeExpression("return-type", method->returnType());
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

      const CompositeType * cty = static_cast<const CompositeType *>(ty);
      TypeDefn * td = cty->typeDefn();

      // Special case for arrays
      if (td->ast() != NULL &&
          td->ast() == Builtins::typeArray->typeDefn()->ast() &&
          td->isTemplateInstance()) {
        xml_.beginElement("array");
        writeTypeRef(cty->typeParam(0));
        xml_.endElement("array");
        break;
      }

      const char * kind = "class";
      switch (ty->typeClass()) {
        case Type::Struct: kind = "struct"; break;
        case Type::Interface: kind = "interface"; break;
        case Type::Protocol: kind = "protocol"; break;
        default: break;
      }

      if (td->isTemplateInstance()) {
        // TODO:
      }

      // TODO: template instance params.
      xml_.beginElement("typename", false);
      xml_.appendAttribute("kind", kind);
      xml_.writeCharacterData(td->qualifiedName());
      xml_.endElement("typename", false);

      if (td->isTemplateInstance()) {
        // TODO:
      }
      break;
    }

    case Type::Enum: {
      const EnumType * ety = static_cast<const EnumType *>(ty);
      TypeDefn * td = ety->typeDefn();
      xml_.beginElement("typename", false);
      xml_.appendAttribute("kind", "enum");
      xml_.writeCharacterData(td->qualifiedName());
      xml_.endElement("typename", false);
      break;
    }

    case Type::Primitive: {
      const PrimitiveType * pty = static_cast<const PrimitiveType *>(ty);
      TypeDefn * td = pty->typeDefn();
      xml_.beginElement("typename", false);
      xml_.appendAttribute("kind", "primitive");
      xml_.writeCharacterData(td->name());
      xml_.endElement("typename", false);
      break;
    }

    case Type::Function: {
      const FunctionType * fty = static_cast<const FunctionType *>(ty);
      xml_.beginElement("fn-type");
      if (!fty->returnType()->isVoidType()) {
        writeTypeExpression("return-type", fty->returnType());
        for (ParameterList::const_iterator it = fty->params().begin(); it != fty->params().end();
            ++it) {
          ParameterDefn * p = *it;
          xml_.beginElement("param");
          if (p->name() != NULL) {
            xml_.appendAttribute("name", p->name());
          }
          writeAttributes(p);
          writeTypeExpression("type", p->type());
          writeDocComment(p);
          xml_.endElement("param");
        }
      }
      xml_.endElement("fn-type");
      break;
    }

    case Type::Tuple: {
      const TupleType * tty = static_cast<const TupleType *>(ty);
      xml_.beginElement("tuple");
      for (TupleType::const_iterator it = tty->begin(); it != tty->end(); ++it) {
        writeTypeRef(*it);
      }
      xml_.endElement("tuple");
      break;
    }

    case Type::Union: {
      const UnionType * uty = static_cast<const UnionType *>(ty);
      xml_.beginElement("union");
      for (TupleType::const_iterator it = uty->members().begin();
          it != uty->members().end(); ++it) {
        writeTypeRef(*it);
      }
      xml_.endElement("union");
      break;
    }

    case Type::NAddress: {
      xml_.beginElement("address");
      writeTypeRef(ty->typeParam(0));
      xml_.endElement("address");
      break;
    }

    case Type::NArray: {
      xml_.beginElement("native-array");
      writeTypeRef(ty->typeParam(0));
      xml_.endElement("native-array");
      break;
    }

    case Type::FlexibleArray: {
      xml_.beginElement("flexible-array");
      writeTypeRef(ty->typeParam(0));
      xml_.endElement("flexible-array");
      break;
    }

    case Type::Unit: {
      break;
    }

    case Type::TypeLiteral: {
      xml_.beginElement("type-literal");
      xml_.endElement("type-literal");
      break;
    }

    case Type::Alias: {
      const TypeAlias * ta = static_cast<const TypeAlias *>(ty);
      if (ty->typeDefn() != NULL) {
        xml_.beginElement("typename", false);
        xml_.appendAttribute("kind", "alias");
        xml_.writeCharacterData(ty->typeDefn()->qualifiedName());
        xml_.endElement("typename", false);
      } else {
        writeTypeRef(ta->value());
      }
      break;
    }

    default:
      break;
  }
}

void DocExporter::writeDocComment(const Defn * de) {
  if (de->ast() != NULL && !de->ast()->docComment().empty()) {
    DocCommentProcessor processor(de->ast()->docComment());
    Doc::Node * node = processor.process();
    if (node != NULL) {
      xml_.beginElement("doc");
      writeDocCommentNodeList(node);
      xml_.endElement("doc");
      delete node;
    }
  }
}

void DocExporter::writeDocCommentNode(const Doc::Node * node) {
  switch (node->type()) {
    case Doc::TEXT: {
      const Doc::TextNode * tn = static_cast<const Doc::TextNode *>(node);
      xml_.writeCharacterData(tn->text());
      break;
    }

    case Doc::PARAGRAPH:
      xml_.beginElement("p");
      writeDocCommentNodeList(node);
      xml_.endElement("p");
      break;

    case Doc::BLOCKQUOTE:
      xml_.beginElement("blockquote");
      writeDocCommentNodeList(node);
      xml_.endElement("blockquote");
      break;

    case Doc::SECTION: {
      const Doc::SectionNode * sn = static_cast<const Doc::SectionNode *>(node);
      switch (sn->sectionType()) {
        case Doc::GENERIC:
          DFAIL("Implement GENERIC");
          break;
        case Doc::DESCRIPTION:
          xml_.beginElement("description");
          writeDocCommentNodeList(node);
          xml_.endElement("description");
          break;
        case Doc::ATTRIBUTES:
          DFAIL("Implement ATTRIBUTES");
          break;
        case Doc::SEE_ALSO:
          DFAIL("Implement SEE_ALSO");
          break;
      }
      break;
    }

    case Doc::STYLE: {
      const Doc::StyleNode * sn = static_cast<const Doc::StyleNode *>(node);
      switch (sn->style()) {
        case Doc::STYLE_STRONG:
          xml_.beginElement("strong", false);
          writeDocCommentNodeList(node);
          xml_.endElement("strong", false);
          break;

        case Doc::STYLE_EMPHATIC:
          xml_.beginElement("em", false);
          writeDocCommentNodeList(node);
          xml_.endElement("em", false);
          break;

        case Doc::STYLE_CODE:
          xml_.beginElement("code", false);
          writeDocCommentNodeList(node);
          xml_.endElement("code", false);
          break;

        case Doc::STYLE_SYMBOL:
          xml_.beginElement("span", false);
          xml_.appendAttribute("class", "symbol");
          writeDocCommentNodeList(node);
          xml_.endElement("span", false);
          break;
      }
      break;
    }

    case Doc::PARAMETER: {
      const Doc::DefinitionNode * nin = static_cast<const Doc::DefinitionNode *>(node);
      xml_.beginElement("parameter");
      xml_.appendAttribute("name", nin->name());
      writeDocCommentNodeList(node);
      xml_.endElement("parameter");
      break;
    }

    case Doc::EXCEPTION: {
      const Doc::DefinitionNode * nin = static_cast<const Doc::DefinitionNode *>(node);
      xml_.beginElement("exception");
      xml_.appendAttribute("name", nin->name());
      writeDocCommentNodeList(node);
      xml_.endElement("exception");
      break;
    }

    case Doc::RETURNS:
      xml_.beginElement("returns");
      writeDocCommentNodeList(node);
      xml_.endElement("returns");
      break;

    case Doc::SECTION_HEADING:
      DFAIL("Implement SECTION_HEADING");
      break;
    case Doc::PROPERTY:
      DFAIL("Implement PROPERTY");
      break;
    case Doc::UNORDERED_LIST:
      DFAIL("Implement UL");
      break;
    case Doc::ORDERED_LIST:
      DFAIL("Implement OL");
      break;
    case Doc::LIST_ITEM:
      DFAIL("Implement LI");
      break;
    case Doc::CODE:
      DFAIL("Implement CODE");
      break;
    default:
      DFAIL("Invalid node type");
  }
}

void DocExporter::writeDocCommentNodeList(const Doc::Node * node) {
  for (Doc::Node::const_iterator it = node->begin(), itEnd = node->end(); it != itEnd; ++it) {
    writeDocCommentNode(*it);
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

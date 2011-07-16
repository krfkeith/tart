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
#include "tart/Defn/Template.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/UnionType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Sema/DefnAnalyzer.h"

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
  // The compiler normally doesn't analyze templates unless they are instantiated.
  // However for this we want everything.
  // Also mark the module resolve members pass as unfinished, so we don't get
  // warnings about adding symbols.
  mod->finished().remove(Pass_ResolveModuleMembers);
  for (Defn * de = mod->firstMember(); de != NULL; de = de->nextInScope()) {
    DefnAnalyzer::analyzeCompletely(de);
  }

  if (!mod->astMembers().empty()) {
    exportModule(mod);
  }
}

void DocExporter::exportModule(const Module * mod) {
  docOptions_.clear();
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
  if (ctype->isAbstract()) {
    xml_.appendAttribute("abstract", "true");
  }
  if (ctype->isFinal()) {
    xml_.appendAttribute("final", "true");
  }
  writeTypeArgs(ctype);
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
  if (etype->isFlags()) {
    xml_.appendAttribute("flags", "true");
  }
  writeModifiers(td);
  writeAttributes(td);
  writeDocComment(td);
  for (Defn * ec = etype->memberScope()->firstMember(); ec != NULL; ec = ec->nextInScope()) {
    if (ec->defnType() == Defn::Let && !ec->isSynthetic()) {
      const VariableDefn * ecvar = cast<VariableDefn>(ec);
      StrFormatStream valueStrm;
      valueStrm << ecvar->initValue();
      valueStrm.flush();
      xml_.beginElement("econst");
      xml_.appendAttribute("name", ec->name());
      xml_.appendAttribute("value", valueStrm.str());
      writeDocComment(ecvar);
      xml_.endElement("econst");
    }
  }
  xml_.endElement("typedef");
}

void DocExporter::exportMethod(const FunctionDefn * method) {
  // Don't document methods that were added by the compiler.
  if (method->ast() == NULL && method->isSynthetic()) {
    return;
  }

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
  if (method->isUnsafe()) {
    xml_.appendAttribute("unsafe", "true");
  }
  if (method->isAbstract()) {
    xml_.appendAttribute("abstract", "true");
  }
  if (method->isExplicitFinal()) {
    xml_.appendAttribute("final", "true");
  }
  writeModifiers(method);
  writeAttributes(method);
  if (method->isTemplateInstance()) {
    const TemplateInstance * ti = method->templateInstance();
    for (TupleType::const_iterator it = ti->typeArgs()->begin();
        it != ti->typeArgs()->end(); ++it) {
      writeTypeExpression("type-arg", *it);
    }
  } else if (method->isTemplate()) {
    const Template * ts = method->templateSignature();
    for (TupleType::const_iterator it = ts->typeParams()->begin();
        it != ts->typeParams()->end(); ++it) {
      writeTypeExpression("type-param", *it);
    }
  }
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
      writeTypeExpression("type", p->type(), p->isVariadic());
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
      // Ones we don't want: Essential, Noinline
      writeExpression("attribute", *it);
    }
  }
}

void DocExporter::writeTypeArgs(const Type * type) {
  size_t numParams = type->numTypeParams();
  for (size_t i = 0; i < numParams; ++i) {
    writeTypeExpression("type-arg", type->typeParam(i));
  }
}

void DocExporter::writeMembers(const IterableScope * scope) {
  for (const Defn * de = scope->firstMember(); de != NULL; de = de->nextInScope()) {
    DocOptions savedOptions = docOptions_;
    DefnAnalyzer::analyzeCompletely(const_cast<Defn *>(de));
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
      case Defn::Macro:
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
    docOptions_ = savedOptions;
  }
}

void DocExporter::writeTypeExpression(llvm::StringRef tagName, const Type * ty, bool variadic) {
  xml_.beginElement(tagName);
  if (variadic) {
    xml_.beginElement("variadic", false);
  }
  writeTypeRef(ty);
  if (variadic) {
    xml_.endElement("variadic", false);
  }
  xml_.endElement(tagName);
}

void DocExporter::writeTypeRef(const Type * ty) {
  ty = dealias(ty);
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
        xml_.beginElement("template-instance");
      }

      xml_.beginElement("typename", td->isTemplateInstance());
      xml_.appendAttribute("type", kind);
      xml_.writeCharacterData(td->qualifiedName());
      xml_.endElement("typename", td->isTemplateInstance());

      if (td->isTemplateInstance()) {
        const TemplateInstance * ti = td->templateInstance();
        for (size_t i = 0; i < ti->typeArgs()->size(); ++i) {
          writeTypeExpression("template-arg", ti->typeArg(i));
        }
        xml_.endElement("template-instance");
      }
      break;
    }

    case Type::Enum: {
      const EnumType * ety = static_cast<const EnumType *>(ty);
      TypeDefn * td = ety->typeDefn();
      xml_.beginElement("typename", false);
      xml_.appendAttribute("type", "enum");
      xml_.writeCharacterData(td->qualifiedName());
      xml_.endElement("typename", false);
      break;
    }

    case Type::Primitive: {
      const PrimitiveType * pty = static_cast<const PrimitiveType *>(ty);
      TypeDefn * td = pty->typeDefn();
      xml_.beginElement("typename", false);
      xml_.appendAttribute("type", "primitive");
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
        xml_.appendAttribute("type", "alias");
        xml_.writeCharacterData(ty->typeDefn()->qualifiedName());
        xml_.endElement("typename", false);
      } else {
        writeTypeRef(ta->value());
      }
      break;
    }

    case Type::TypeVar: {
      const TypeVariable * tv = static_cast<const TypeVariable *>(ty);
      xml_.beginElement("type-variable", false);
      xml_.appendAttribute("name", tv->name());
      if (tv->isVariadic()) {
        xml_.appendAttribute("variadic", "true");
      }
      // TODO: Constraints?
      xml_.endElement("type-variable", false);
      break;
    }

    default:
      diag.error() << "What type is " << ty;
      break;
  }
}

void DocExporter::writeExpression(llvm::StringRef tagName, const Expr * e) {
  xml_.beginElement(tagName);
  writeExpression(e);
  xml_.endElement(tagName);
}

void DocExporter::writeExpression(const Expr * e) {
  switch (e->exprType()) {
    case Expr::ConstObjRef: {
      const ConstantObjectRef * cobj = static_cast<const ConstantObjectRef *>(e);
      xml_.beginElement("const-object", false);
      writeTypeRef(cobj->type());
      xml_.endElement("const-object", false);
      break;
    }

    case Expr::CtorCall: {
      const FnCallExpr * call = static_cast<const FnCallExpr *>(e);
      xml_.beginElement("ctor-call", false);
      writeTypeRef(call->type());
      xml_.endElement("ctor-call", false);
      break;
    }

    default:
      diag.fatal(e) << "Unimplemented expression type: '" << exprTypeName(e->exprType()) << "'";
      DFAIL("Unimplemented");
  }
}

void DocExporter::writeDocComment(const Defn * de) {
  Doc::Node * node = NULL;
  Doc::Node * inherited = NULL;
  if (de->ast() != NULL && !de->ast()->docComment().empty()) {
    DocCommentProcessor processor(de->ast()->docComment());
    node = processor.process();
    if (node != NULL) {
      getDocOptions(node);
    }
  }

  if (docOptions_.inherit) {
    inherited = getInheritedDocComment(de);
  }

  if (node != NULL || inherited != NULL) {
    xml_.beginElement("doc");

    if (node != NULL) {
      writeDocCommentNodeList(node);
      delete node;
    } else if (inherited != NULL) {
      writeDocCommentNodeList(inherited);
      delete inherited;
    }

    xml_.endElement("doc");
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

    case Doc::DEPRECATED:
      xml_.beginElement("deprecated");
      writeDocCommentNodeList(node);
      xml_.endElement("deprecated");
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

    case Doc::INHERIT:
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

Doc::Node * DocExporter::getInheritedDocComment(const Defn * de) {
  if (const FunctionDefn * fn = dyn_cast<FunctionDefn>(de)) {
    for (FunctionDefn::FunctionSet::const_iterator it = fn->overriddenMethods().begin();
        it != fn->overriddenMethods().end(); ++it) {
      const FunctionDefn * overridden = *it;
      if (overridden->ast() != NULL && !overridden->ast()->docComment().empty()) {
        DocCommentProcessor processor(overridden->ast()->docComment());
        Doc::Node * node = processor.process();
        if (node != NULL) {
          return node;
        }
      }
    }
  } else if (const PropertyDefn * prop = dyn_cast<PropertyDefn>(de)) {
    const CompositeType * cls = prop->definingClass();
    ClassSet bases;
    cls->ancestorClasses(bases);
    for (ClassSet::const_iterator it = bases.begin(); it != bases.end(); ++it) {
      DefnList defns;
      (*it)->lookupMember(prop->name(), defns, false);
      if (!defns.empty()) {
        for (DefnList::const_iterator d = defns.begin(); d != defns.end(); ++d) {
          if (const PropertyDefn * baseProp = dyn_cast<PropertyDefn>(*d)) {
            if (baseProp->type() != NULL &&
                baseProp->type()->isEqual(prop->type()) &&
                baseProp->ast() != NULL &&
                !baseProp->ast()->docComment().empty()) {
              DocCommentProcessor processor(baseProp->ast()->docComment());
              Doc::Node * node = processor.process();
              if (node != NULL) {
                return node;
              }
            }
          }
        }
      }
    }
  }

  return NULL;
}

void DocExporter::getDocOptions(const Doc::Node * node) {
  for (Doc::Node::const_iterator it = node->begin(), itEnd = node->end(); it != itEnd; ++it) {
    const Doc::Node * node = *it;
    if (node->type() == Doc::INHERIT) {
      docOptions_.inherit = true;
    } else {
      getDocOptions(node);
    }
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

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/AST/Stmt.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/NamespaceDefn.h"

#include "tart/Expr/Exprs.h"

#include "tart/Meta/Tags.h"
#include "tart/Meta/VarInt.h"
#include "tart/Meta/ASTReader.h"
#include "tart/Meta/MDReader.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/TypeAlias.h"

#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/TemplateParamAnalyzer.h"
#include "tart/Sema/VarAnalyzer.h"

#include "llvm/Metadata.h"
#include "llvm/Constants.h"

#include "config_paths.h"

namespace tart {

using namespace llvm;

// -------------------------------------------------------------------
// NodeRef

unsigned NodeRef::size() const {
  return node_ ? node_->getNumOperands() : 0;
}

llvm::Value * NodeRef::arg(unsigned n) const {
  if (node_->getNumOperands() <= n) {
    diag.error() << "Attempt to access operand # " << n <<
        " from a metadata node with only " << size() << " operands.";
    node_->dump();
    DFAIL("IllegalState");
  }

  return node_->getOperand(n);
}

uint32_t NodeRef::intArg(unsigned n) const {
  const llvm::Value * val = arg(n);
  if (const ConstantInt * ci = dyn_cast_or_null<ConstantInt>(val)) {
    return uint32_t(ci->getValue().getZExtValue());
  }

  diag.error() << "Expected metadata operand # " << n << " to be an integer";
  node_->dump();
  DFAIL("IllegalState");
  return 0;
}

StringRef NodeRef::strArg(unsigned n) const {
  const llvm::Value * val = arg(n);
  if (const MDString * str = dyn_cast_or_null<MDString>(val)) {
    return str->getString();
  }

  diag.error() << "Expected metadata operand # " << n << " to be an MDString";
  node_->dump();
  DFAIL("IllegalState");
  return StringRef();
}

StringRef NodeRef::optStrArg(unsigned n) const {
  const llvm::Value * val = arg(n);
  if (val == NULL) {
    return StringRef();
  } else if (const MDString * str = dyn_cast_or_null<MDString>(val)) {
    return str->getString();
  }

  diag.error() << "Expected metadata operand # " << n << " to be an MDString";
  node_->dump();
  DFAIL("IllegalState");
  return StringRef();
}

NodeRef NodeRef::nodeArg(unsigned n) const {
  const llvm::Value * val = arg(n);
  if (const MDNode * md = dyn_cast_or_null<MDNode>(val)) {
    return md;
  }

  diag.error() << "Expected metadata operand # " << n << " to be an MDNode";
  node_->dump();
  DFAIL("IllegalState");
  return NodeRef(NULL);
}

NodeRef NodeRef::optNodeArg(unsigned n) const {
  const llvm::Value * val = arg(n);
  if (val == NULL) {
    return NULL;
  } else if (const MDNode * md = dyn_cast<MDNode>(val)) {
    return md;
  }

  diag.error() << "Expected metadata operand # " << n << " to be an MDNode";
  node_->dump();
  DFAIL("IllegalState");
  return NodeRef(NULL);
}

// -------------------------------------------------------------------
// MDReader

bool MDReader::read(NamedMDNode * md) {
  if (md->getNumOperands() != 6) {
    diag.fatal() << "Invalid number of MD operands " << md->getNumOperands();
    return false;
  }

  // Get the serialization format version.
  if (ConstantInt * version = dyn_cast<ConstantInt>(
      md->getOperand(FIELD_MODULE_VERSION)->getOperand(0))) {
    version_ = (uint32_t) version->getValue().getZExtValue();
  } else {
    diag.error() << "No version for compiled module " << module_->qualifiedName();
    return NULL;
  }

  // Get the timestamp of the original source file.
  if (MDNode * timestamp = dyn_cast<MDNode>(md->getOperand(FIELD_MODULE_TIMESTAMP))) {
    (void)timestamp;
    ///
  } else {
    diag.error() << "No timestamp for compiled module " << module_->qualifiedName();
    return false;
  }

  // List of imports.
  if (MDNode * imports = dyn_cast<MDNode>(md->getOperand(FIELD_MODULE_IMPORTS))) {
    if (!readModuleImports(imports)) {
      return false;
    }
  }

  // List of exports.
  if (MDNode * exports = dyn_cast<MDNode>(md->getOperand(FIELD_MODULE_EXPORTS))) {
    module_->setMDNode(exports);
    return true;
  }

  diag.error() << "No exports for compiled module " << module_->qualifiedName();
  return false;
}

bool MDReader::readModuleImports(NodeRef node) {
  if (node.size() > 0) {
    ASTReader reader(module_->location(), module_->moduleStrings(), node.strArg(0));
    if (!reader.readAll(module_->imports())) {
      return false;
    }
  }
  return true;
}

bool MDReader::readImports(Defn * de, ASTNodeList & imports) {
  NodeRef node = de->mdNode();
  NodeRef importsNode;
  if (isa<TypeDefn>(de)) {
    importsNode = node.optNodeArg(FIELD_TYPEDEF_IMPORTS);
  } else if (isa<NamespaceDefn>(de)) {
    importsNode = node.optNodeArg(FIELD_NS_IMPORTS);
  } else {
    return false;
  }

  if (importsNode.size() == 0) {
    return true;
  }

  StringRef str = importsNode.strArg(0);
  if (!str.empty()) {
    ASTReader reader(de->location(), module_->moduleStrings(), str);
    if (!reader.readAll(imports)) {
      return false;
    }
  }

  return true;
}

bool MDReader::readMembers(Defn * parentDefn) {
  NodeRef node(parentDefn->mdNode());
  NodeRef members;
  Scope * memberScope;
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(parentDefn)) {
    members = node.nodeArg(FIELD_TYPEDEF_MEMBERS);
    memberScope = tdef->typePtr()->mutableMemberScope();
  } else if (PropertyDefn * prop = dyn_cast<PropertyDefn>(parentDefn)) {
    members = node.nodeArg(FIELD_PROP_ACCESSORS);
    memberScope = &prop->accessorScope();
  } else if (NamespaceDefn * ns = dyn_cast<NamespaceDefn>(parentDefn)) {
    members = node.nodeArg(FIELD_NS_MEMBERS);
    memberScope = &ns->memberScope();
  } else if (Module * m = dyn_cast<Module>(parentDefn)) {
    members = parentDefn->mdNode();
    memberScope = m;
  } else {
    return false;
  }

  return readMembers(members, memberScope, parentDefn);
}

bool MDReader::readMembers(NodeRef members, Scope * scope, Defn * parentDefn) {
  bool success = true;
  unsigned numOperands = members.size();
  StorageClass storage = Storage_Instance;
  if (parentDefn->defnType() == Defn::Mod || parentDefn->defnType() == Defn::Namespace) {
    storage = Storage_Global;
  }
  Defn * savedSubject = setSubject(parentDefn);
  for (unsigned i = 0; i < numOperands; ++i) {
    Defn * member = readMember(members.nodeArg(i), scope, storage);
    if (member != NULL) {
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
      if (parentDefn->isSingular() && !member->isTemplate()) {
        member->addTrait(Defn::Singular);
      }
    } else {
      success = false;
    }
  }

  setSubject(savedSubject);
  return success;
}

Defn * MDReader::readMember(NodeRef node, Scope * parent, StorageClass storage) {
  diag.recovered();

  // Read the definition type
  meta::Defn::Tag tag = meta::Defn::Tag(node.intArg(FIELD_DEFN_TYPE));
  if (tag == meta::Defn::INVALID) {
    return NULL;
  }

  // Read the name
  StringRef name = node.strArg(FIELD_DEFN_NAME);

  SourceLocation location;
  NodeRef loc = node.optNodeArg(FIELD_DEFN_LOCATION);
  if (!loc.isNull()) {
    StringRef file = loc.strArg(0);
    TokenPosition pos;
    location.file = module_->moduleSource();
    pos.beginLine = loc.intArg(1);
    pos.beginCol = loc.intArg(2);
    pos.endLine = loc.intArg(3);
    pos.endCol = loc.intArg(4);

    // TODO: This doesn't take into account definitions which may have
    // come from a different module.
    ArchiveEntry::encodeLocation(pos, location);
  }

  // Read the modifier flags
  uint32_t modifiers = node.intArg(FIELD_DEFN_MODS);
  if (modifiers & meta::DefnFlag::TEMPLATE) {
    if (ASTTemplate * ast = readTemplate(location, node.strArg(FIELD_DEFN_AST), name)) {
      return ScopeBuilder::createTemplateDefn(parent, module_, ast, storage);
    }
    return NULL;
  }

  Visibility visibility = Public;
  if (modifiers & meta::DefnFlag::PRIVATE) {
    visibility = Private;
  } else if (modifiers & meta::DefnFlag::PROTECTED) {
    visibility = Protected;
  } else if (modifiers & meta::DefnFlag::INTERNAL) {
    visibility = Internal;
  }

  if (modifiers & meta::DefnFlag::STATIC) {
    if (storage == Storage_Instance) {
      storage = Storage_Static;
    }
  }

  //diag.debug() << "Reading: " << name;
  switch (tag) {
    case meta::Defn::CLASS:
    case meta::Defn::STRUCT:
    case meta::Defn::INTERFACE:
    case meta::Defn::PROTOCOL: {
      Type::TypeClass tc;
      switch (int(tag)) {
        case meta::Defn::CLASS: tc = Type::Class; break;
        case meta::Defn::STRUCT: tc = Type::Struct; break;
        case meta::Defn::INTERFACE: tc = Type::Interface; break;
        case meta::Defn::PROTOCOL: tc = Type::Protocol; break;
      }

      TypeDefn * tdef = new TypeDefn(module_, module_->internString(name));
      tdef->setLocation(location);
      tdef->setVisibility(visibility);

      CompositeType * ctype = new CompositeType(tc, tdef, parent);
      if (modifiers & meta::DefnFlag::FINAL) {
        ctype->setClassFlag(CompositeType::Final, true);
      }
      if (modifiers & meta::DefnFlag::ABSTRACT) {
        ctype->setClassFlag(CompositeType::Abstract, true);
      }
      if (modifiers & meta::DefnFlag::ATTRIBUTE) {
        ctype->setClassFlag(CompositeType::Attribute, true);
      }
      if (modifiers & meta::DefnFlag::REFLECTED) {
        tdef->addTrait(Defn::Reflect);
      }
      if (modifiers & meta::DefnFlag::UNSAFE) {
        tdef->addTrait(Defn::Unsafe);
      }

      tdef->setValue(ctype);
      tdef->setStorageClass(storage);
      tdef->setMDNode(node.node());
      return tdef;
    }

    case meta::Defn::ENUM: {
      TypeDefn * tdef = new TypeDefn(module_, module_->internString(name));
      tdef->setVisibility(visibility);
      tdef->setLocation(location);

      EnumType * etype = new EnumType(tdef, parent);
      tdef->setValue(etype);
      tdef->setStorageClass(storage);
      tdef->setMDNode(node.node());

      if (modifiers & meta::DefnFlag::FLAGS_ENUM) {
        etype->setIsFlags(true);
      }

      return tdef;
    }

    case meta::Defn::TYPEALIAS: {
      TypeDefn * tdef = new TypeDefn(module_, module_->internString(name));
      tdef->setLocation(location);
      tdef->setVisibility(visibility);
      tdef->setStorageClass(storage);

      const Type * type = readTypeRef(node.strArg(FIELD_TYPEALIAS_VALUE));
      if (type == NULL) {
        return NULL;
      }
      tdef->setValue(new TypeAlias(type, tdef));
      return tdef;
    }

    case meta::Defn::NAMESPACE: {
      NamespaceDefn * ns = new NamespaceDefn(module_, module_->internString(name));
      ns->setLocation(location);
      ns->setVisibility(visibility);
      ns->setMDNode(node.node());
      if (modifiers & meta::DefnFlag::REFLECTED) {
        ns->addTrait(Defn::Reflect);
      }
      return ns;
    }

    case meta::Defn::FUNCTION:
    case meta::Defn::UNDEF:
    case meta::Defn::OVERRIDE:
    case meta::Defn::CONSTRUCTOR:
    case meta::Defn::MACRO: {
      FunctionDefn * fn = new FunctionDefn(
          tag == meta::Defn::MACRO ? Defn::Macro : Defn::Function, module_,
              module_->internString(name));
      fn->setLocation(location);
      fn->setVisibility(visibility);
      fn->setStorageClass(storage);
      fn->setMDNode(node.node());

//      TemplateInstance * tinst_;  // Template arguments
//      int dispatchIndex_;
//      Intrinsic * intrinsic_;
//      FunctionSet overriddenMethods_;

      // Modifiers
      if (modifiers & meta::DefnFlag::FINAL) {
        fn->setFlag(FunctionDefn::Final);
      }
      if (modifiers & meta::DefnFlag::ABSTRACT) {
        fn->setFlag(FunctionDefn::Abstract);
      }
      if (tag == meta::Defn::UNDEF) {
        fn->setFlag(FunctionDefn::Undefined);
      }
      if (tag == meta::Defn::OVERRIDE) {
        fn->setFlag(FunctionDefn::Override);
      }
      if (modifiers & meta::DefnFlag::REFLECTED) {
        fn->addTrait(Defn::Reflect);
      }
      if (modifiers & meta::DefnFlag::UNSAFE) {
        fn->addTrait(Defn::Unsafe);
      }
      if (modifiers & meta::DefnFlag::INTRINSIC) {
        fn->setFlag(FunctionDefn::Intrinsic);
      }
      if (modifiers & meta::DefnFlag::TRACE_METHOD) {
        fn->setFlag(FunctionDefn::TraceMethod);
      }
      if (modifiers & meta::DefnFlag::ENTRY_POINT) {
        module_->setEntryPoint(fn);
      }

      return fn;
    }

    case meta::Defn::PARAM: {
      ParameterDefn * param = new ParameterDefn(module_, module_->internString(name));
      param->setLocation(location);
      const Type * type = readTypeRef(node.strArg(FIELD_PARAM_TYPE));
      if (type == NULL) {
        return NULL;
      }

      // Use VarAnalyzer to set the type so we get internal types as well.
      VarAnalyzer(param, module_, module_, param, NULL).setTargetType(type);

      if (node.size() > FIELD_PARAM_DEFAULT) {
        Expr * defaultValue = readExpression(location, node.nodeArg(FIELD_PARAM_DEFAULT));
        if (defaultValue == NULL) {
          return NULL;
        }
        param->setInitValue(defaultValue);
      }

      if (modifiers & meta::DefnFlag::VARIADIC) {
        param->setFlag(ParameterDefn::Variadic);
      }
      if (modifiers & meta::DefnFlag::KEYWORDONLY) {
        param->setFlag(ParameterDefn::KeywordOnly);
      }

      param->passes().finish(VariableDefn::VariableTypePass);
      param->passes().finish(VariableDefn::InitializerPass);
      param->addTrait(Defn::Singular);
      return param;
    }

    case meta::Defn::PROPERTY: {
      PropertyDefn * prop = new PropertyDefn(Defn::Property, module_, module_->internString(name));
      prop->setLocation(location);
      prop->setVisibility(visibility);
      prop->setStorageClass(storage);
      prop->setMDNode(node.node());
      return prop;
    }

    case meta::Defn::INDEXER: {
      IndexerDefn * idx = new IndexerDefn(Defn::Indexer, module_, module_->internString(name));
      idx->setLocation(location);
      idx->setVisibility(visibility);
      idx->setStorageClass(storage);
      idx->setMDNode(node.node());
      return idx;
    }

    case meta::Defn::VARIABLE:
    case meta::Defn::LET: {
      VariableDefn * var = new VariableDefn(tag == meta::Defn::LET ? Defn::Let : Defn::Var,
          module_, module_->internString(name));
      var->setLocation(location);
      var->setVisibility(visibility);
      var->setStorageClass(storage);
      var->setMDNode(node.node());
      const Type * type = readTypeRef(node.strArg(FIELD_VAR_TYPE));
      if (type == NULL) {
        return NULL;
      }
      var->setType(type);

      if (modifiers & meta::DefnFlag::EXTERN) {
        var->setFlag(VariableDefn::Extern);
      }

      NodeRef init = node.optNodeArg(FIELD_VAR_INIT);
      if (!init.isNull()) {
        Expr * initExpr = readExpression(location, init);
        if (initExpr == NULL) {
          return NULL;
        }
        var->setInitValue(initExpr);
      }

      var->passes().finish(VariableDefn::VariableTypePass);
      var->passes().finish(VariableDefn::InitializerPass);
      var->addTrait(Defn::Singular);
      return var;
    }

    case meta::Defn::IMPORT:
    case meta::Defn::TYPE_PARAM:
    //case meta::MemberDef::ATTRIBUTE:
    default:
      DFAIL("Implement");
      break;
  }

  return NULL;
}

bool MDReader::readCompositeDetails(CompositeType * ty, TypeList & bases) {
  TypeDefn * tdef = ty->typeDefn();
  NodeRef details = NodeRef(tdef->mdNode()).nodeArg(FIELD_TYPEDEF_DETAILS);
  if (details.isNull()) {
    return false;
  }
  unsigned count = details.size();
  for (unsigned i = 0; i < count; ++i) {
    NodeRef el = details.nodeArg(i);
    meta::Detail::Tag tag = meta::Detail::Tag(el.intArg(0));
    switch (tag) {
      case meta::Detail::BASE_TYPE: {
        const Type * type = readTypeRef(el.strArg(1));
        if (type == NULL) {
          return false;
        }
        bases.push_back(const_cast<Type *>(type));
        break;
      }

      case meta::Detail::ATTRIBUTE: {
        AttributeInfo & ai = ty->attributeInfo();
        ai.setTarget(el.intArg(1));
        ai.setRetention(el.intArg(2));
        ai.setPropagation(el.intArg(3));
        break;
      }

      default:
        DFAIL("Invalid detail type");
    }
  }
  return true;
}

const Type * MDReader::readEnumBase(TypeDefn * ety) {
  return readTypeRef(NodeRef(ety->mdNode()).optStrArg(FIELD_TYPEDEF_DETAILS));
}

bool MDReader::readEnumConstants(TypeDefn * tdef) {
  const EnumType * ety = cast<EnumType>(tdef->typePtr());
  NodeRef node(tdef->mdNode());
  NodeRef members = node.nodeArg(FIELD_TYPEDEF_MEMBERS);
  unsigned numOperands = members.size();
  for (unsigned i = 0; i < numOperands; ++i) {
    NodeRef econst = members.nodeArg(i);
    StringRef name = econst.strArg(FIELD_ECONST_NAME);
    if (name.empty()) {
      diag.error() << "Invalid name for enum constant";
      diag.info() << "enum " << Format_QualifiedName << tdef;
      return false;
    }
    Expr * value = readExpression(tdef->location(), econst.nodeArg(FIELD_ECONST_VALUE));
    if (value == NULL) {
      diag.error() << "Missing value for enum constant: " << name;
      diag.info() << "enum " << Format_QualifiedName << tdef;
      return false;
    }

    VariableDefn * var = new VariableDefn(Defn::Let, module_, module_->internString(name), value);
    var->setStorageClass(Storage_Static);
    var->setType(ety);
    var->setLocation(tdef->location());
    var->addTrait(Defn::Singular);
    var->passes().finish(VariableDefn::AttributePass);
    var->passes().finish(VariableDefn::VariableTypePass);
    var->passes().finish(VariableDefn::InitializerPass);
    var->passes().finish(VariableDefn::CompletionPass);
    ety->mutableMemberScope()->addMember(var);
  }

  return true;
}

bool MDReader::readTypeMembers(TypeDefn * tdef, Scope * memberScope) {
  return readMembers(NodeRef(tdef->mdNode()).nodeArg(FIELD_TYPEDEF_MEMBERS), memberScope, tdef);
}

bool MDReader::readFunctionType(FunctionDefn * fn) {
  fn->passes().finish(FunctionDefn::ParameterTypePass);
  fn->passes().finish(FunctionDefn::ReturnTypePass);

  NodeRef node = fn->mdNode();
  const Type * returnType = readTypeRef(node.strArg(FIELD_FUNCTION_RTYPE));
  if (returnType == NULL) {
    return false;
  }

  ParameterList plist;
  NodeRef params = node.nodeArg(FIELD_FUNCTION_PARAMS);
  if (params.isNull()) {
    diag.error() << "Missing parameters node: " << fn->name();
    return false;
  }

  unsigned count = params.size();
  for (unsigned i = 0; i < count; ++i) {
    ParameterDefn * param = cast_or_null<ParameterDefn>(
        readMember(params.nodeArg(i), NULL, Storage_Local));
    if (param == NULL) {
      return false;
    }

    plist.push_back(param);
  }

  fn->setFunctionType(new FunctionType(returnType, plist));
  fn->addTrait(Defn::Singular);
  return true;
}

const Stmt * MDReader::readFunctionBody(FunctionDefn * fn) {
  NodeRef node = fn->mdNode();
  StringRef astStr = node.optStrArg(FIELD_FUNCTION_AST);
  if (astStr.empty()) {
    return NULL;
  }
  //diag.debug() << Format_QualifiedName << "Reading body for: " << fn;
  ASTReader reader(fn->location(), module_->moduleStrings(), astStr);
  return cast_or_null<Stmt>(reader.read());
}

bool MDReader::readPropertyType(PropertyDefn * prop) {
  const Type * type = readTypeRef(NodeRef(prop->mdNode()).strArg(FIELD_PROP_TYPE));
  if (type == NULL) {
    return false;
  }
  prop->setType(type);
  prop->addTrait(Defn::Singular);
  prop->passes().finish(PropertyDefn::PropertyTypePass);
  return true;
}

bool MDReader::readPropertyAccessors(PropertyDefn * prop) {
  prop->passes().finish(PropertyDefn::AccessorCreationPass);
  return readAccessorList(prop, NodeRef(prop->mdNode()).nodeArg(FIELD_PROP_ACCESSORS));
}

bool MDReader::readAttributeList(Defn * de) {
  NodeRef node(de->mdNode());
  return readExpressionList(de->location(), node.nodeArg(FIELD_DEFN_ATTRS), de->attrs());
}

bool MDReader::readAccessorList(PropertyDefn * prop, NodeRef node) {
  unsigned nodeCount = node.size();
  for (unsigned i = 0; i < nodeCount; ++i) {
    Defn * accessor = readMember(node.nodeArg(i), &prop->accessorScope(), prop->storageClass());
    if (accessor == NULL) {
      return false;
    }
    prop->addAccessor(cast<FunctionDefn>(accessor));
  }
  return true;
}

ASTTemplate * MDReader::readTemplate(SourceLocation loc, StringRef source,
    StringRef name) {
  ASTReader reader(loc, module_->moduleStrings(), source);
  ASTNode * ast = reader.read();
  if (ast == NULL) {
    diag.fatal(loc) << "No template AST found for: " << name;
    return NULL;
  }
  if (ast->isInvalid()) {
    return NULL;
  }

  if (ASTTemplate * astt = dyn_cast<ASTTemplate>(ast)) {
    return astt;
  } else {
    diag.fatal(loc) << "No template AST found for: " << name;
    return NULL;
  }
}

bool MDReader::readExpressionList(SourceLocation loc, NodeRef exprs, ExprList & out) {
  unsigned count = exprs.size();
  for (unsigned i = 0; i < count; ++i) {
    Expr * e = readExpression(loc, exprs.nodeArg(i));
    if (e == NULL) {
      return false;
    }
    out.push_back(e);
  }
  return true;
}

Expr * MDReader::readExpression(SourceLocation loc, NodeRef node) {
  if (node.isNull()) {
    return NULL;
  }

  uint16_t tag = uint16_t(node.intArg(0));
  switch (tag) {
    case meta::ExprID::CONST_INT: {
      const Type * ty = readTypeRef(node.strArg(1));
      if (ty == NULL) {
        return NULL;
      }
      ConstantInt * cint = cast<ConstantInt>(node.arg(2));
      return new ConstantInteger(loc, ty, cint);
    }

    case meta::ExprID::CONST_STR: {
      StringRef str = node.strArg(1);
      return new ConstantString(loc, str);
    }

    case meta::ExprID::CONST_OBJ: {
      const Type * objType = readTypeRef(node.strArg(1));
      if (objType == NULL) {
        return NULL;
      }
      if (const CompositeType * ctype = dyn_cast<CompositeType>(objType)) {
        if (!AnalyzerBase::analyzeType(ctype, Task_PrepConstruction)) {
          return NULL;
        }
        ConstantObjectRef * cobj = new ConstantObjectRef(loc, ctype);
        unsigned count = node.size() - 2;
        DASSERT(count == (unsigned) ctype->instanceFieldCountRecursive());
        for (unsigned i = 0; i < count; ++i) {
          Value * v = node.arg(i + 2);
          if (v == NULL) {
            cobj->members()[i] = NULL;
          } else {
            Expr * e = readExpression(loc, cast<MDNode>(v));
            if (e == NULL) {
              return &Expr::ErrorVal;
            }
            cobj->members()[i] = e;
          }
        }

        return cobj;
      } else {
        diag.fatal(module_) << "Invalid type for constant object: " << objType;
        return NULL;
      }
    }

    case meta::ExprID::CONST_LVAL: {
      StringRef qname = node.strArg(1);
      DASSERT(false) << "Implement deserialization of variable reference: " << qname;
      break;
    }

//    case meta::ExprID::CONST_NULL: {
//    }

    case meta::ExprID::UPCAST: {
      const Type * ty = readTypeRef(node.strArg(1));
      Expr * arg = readExpression(loc, node.nodeArg(2));
      return new CastExpr(Expr::UpCast, loc, ty, arg);
    }

    default:
      diag.recovered();
      diag.fatal() << "Unhandled serialization code: " << tag;
      DFAIL("Implement");
      break;
  }

  return NULL;
}

const Type * MDReader::readTypeRef(StringRef str) {
  if (str.empty()) {
    return NULL;
  }

  ASTReader reader(module_->location(), module_->moduleStrings(), str);
  ASTNode * ast = reader.read();
  TypeAnalyzer ta(module_, module_);
  ta.setSubject(subject_);
  return ta.typeFromAST(ast);
}

Defn * MDReader::lookupSymbol(StringRef name) {
  DefnList defs;
  if (!lookupName(name, defs)) {
    return NULL;
  }

  if (defs.size() > 1) {
    diag.error() << "Error reading compiled module " << module_->qualifiedName() <<
       ": ambiguous definition for " << name;
    return NULL;
  }

  return defs.front();
}

bool MDReader::lookupName(StringRef qname, DefnList & defs) {
  if (module_->import(qname, defs, true)) {
    return true;
  } else {
    size_t dot = qname.rfind('.');
    if (dot == qname.npos) {
      return false;
    }

    StringRef baseName = qname.substr(0, dot);
    if (Defn * baseDefn = lookupSymbol(baseName)) {
      (void)baseDefn;
      StringRef name = qname.substr(dot + 1, qname.npos);
      DFAIL("Implement");
    }

    return false;
  }
}

} // namespace tart

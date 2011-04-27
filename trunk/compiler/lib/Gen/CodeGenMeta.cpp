/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Module.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/NamespaceDefn.h"
#include "tart/Defn/PropertyDefn.h"

#include "tart/Gen/CodeGenerator.h"

#include "tart/Type/TypeAlias.h"

#include "tart/Meta/Tags.h"

#include "llvm/Module.h"

namespace tart {

using namespace llvm;

void CodeGenerator::genModuleMetadata() {
/*  ProgramSource * source = module_->moduleSource();
  SmallString<128> mdName("tart.xdef.");
  mdName += module_->qualifiedName();
  NamedMDNode * md = irModule_->getOrInsertNamedMetadata(mdName);
  // TODO: Add compiler version.
  md->addOperand(getModuleDeps());
  md->addOperand(getModuleTimestamp());
  md->addOperand(getMemberExports(module_)); */
}

MDNode * CodeGenerator::getModuleDeps() {
  ValueList deps;
  const ModuleSet & modules = module_->importModules();
  for (ModuleSet::const_iterator it = modules.begin(); it != modules.end(); ++it) {
    Module * m = *it;
    if (!m->moduleSource()->getFilePath().empty()) {
      Value * modAttrs[2];
      modAttrs[0] = MDString::get(context_, m->moduleSource()->getFilePath());
      modAttrs[1] = MDString::get(context_, m->qualifiedName());
      deps.push_back(MDNode::get(context_, modAttrs));
    }
  }

  return MDNode::get(context_, deps);
}

MDNode * CodeGenerator::getModuleTimestamp() {
  Value * modTimestamp[2];
  modTimestamp[0] = ConstantInt::get(builder_.getInt64Ty(), module_->timestamp().seconds());
  modTimestamp[1] = ConstantInt::get(builder_.getInt32Ty(), module_->timestamp().nanoseconds());
  return MDNode::get(context_, modTimestamp);
}

MDNode * CodeGenerator::getMemberExports(const IterableScope * scope) {
  ValueList exports;
  for (Defn * de = scope->firstMember(); de != NULL; de = de->nextInScope()) {
    switch (de->defnType()) {
      case Defn::Typedef:
        exports.push_back(exportType(static_cast<TypeDefn *>(de)));
        break;

      case Defn::Namespace:
        exports.push_back(exportNamespace(static_cast<NamespaceDefn *>(de)));
        break;

      case Defn::Var:
      case Defn::Let:
        exports.push_back(exportVar(static_cast<VariableDefn *>(de)));
        break;

      case Defn::Property:
        exports.push_back(exportProperty(static_cast<PropertyDefn *>(de)));
        break;

      case Defn::Indexer:
        exports.push_back(exportIndexer(static_cast<IndexerDefn *>(de)));
        break;

      case Defn::Function:
      case Defn::Macro:
        exports.push_back(exportFunction(static_cast<FunctionDefn *>(de)));
        break;

      case Defn::ExplicitImport:
        break;

      default:
        DASSERT_OBJ(false && "Invalid export", de);
        break;
    }
  }

  return MDNode::get(context_, exports);
}

MDNode * CodeGenerator::exportType(const TypeDefn * td) {
  const Type * type = td->typeValue();
  ValueList args;
  switch (type->typeClass()) {
    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      MemberTag tag;
      switch (type->typeClass()) {
        case Type::Class: tag = TAG_DEF_CLASS; break;
        case Type::Struct: tag = TAG_DEF_STRUCT; break;
        case Type::Interface: tag = TAG_DEF_INTERFACE; break;
        case Type::Protocol: tag = TAG_DEF_PROTOCOL; break;
        default: break;
      }
      args.push_back(ConstantInt::get(builder_.getInt8Ty(), tag));
      args.push_back(MDString::get(context_, td->name()));
      args.push_back(exportModifiers(td));
      args.push_back(getMemberExports(type->memberScope()));
      break;
    }

    case Type::Enum: {
      args.push_back(ConstantInt::get(builder_.getInt8Ty(), TAG_DEF_ENUM));
      args.push_back(MDString::get(context_, td->name()));
      args.push_back(getMemberExports(type->memberScope()));
      break;
    }

    case Type::Alias: {
      const TypeAlias * typeAlias = static_cast<const TypeAlias *>(type);
      args.push_back(ConstantInt::get(builder_.getInt8Ty(), TAG_DEF_TYPEALIAS));
      args.push_back(MDString::get(context_, td->name()));
      args.push_back(exportTypeRef(typeAlias->value()));
      break;
    }

    default:
      break;
  }

  args.push_back(MDString::get(context_, td->name()));
  return MDNode::get(context_, args);
}

MDNode * CodeGenerator::exportNamespace(const NamespaceDefn * ns) {
  ValueList args;
  args.push_back(ConstantInt::get(builder_.getInt8Ty(), TAG_DEF_NAMESPACE));
  args.push_back(MDString::get(context_, ns->name()));
  return MDNode::get(context_, args);
}

MDNode * CodeGenerator::exportVar(const VariableDefn * var) {
  ValueList args;
  args.push_back(ConstantInt::get(builder_.getInt8Ty(),
      var->defnType() == Defn::Let ? TAG_DEF_LET : TAG_DEF_VARIABLE));
  args.push_back(MDString::get(context_, var->name()));
  args.push_back(exportModifiers(var));
  args.push_back(exportTypeRef(var->type()));
  return MDNode::get(context_, args);
}

MDNode * CodeGenerator::exportProperty(const PropertyDefn * prop) {
  ValueList args;
  args.push_back(ConstantInt::get(builder_.getInt8Ty(), TAG_DEF_PROPERTY));
  args.push_back(MDString::get(context_, prop->name()));
  args.push_back(exportModifiers(prop));
  args.push_back(exportTypeRef(prop->type()));
  return MDNode::get(context_, args);
}

MDNode * CodeGenerator::exportIndexer(const IndexerDefn * idx) {
  ValueList args;
  args.push_back(ConstantInt::get(builder_.getInt8Ty(), TAG_DEF_INDEXER));
  args.push_back(MDString::get(context_, idx->name()));
  args.push_back(exportModifiers(idx));
  args.push_back(exportTypeRef(idx->type()));
  return MDNode::get(context_, args);
}

MDNode * CodeGenerator::exportFunction(const FunctionDefn * fn) {
  ValueList args;
  MemberTag tag;
  if (fn->defnType() == Defn::Macro) {
    tag = TAG_DEF_MACRO;
  } else if (fn->isUndefined()) {
    tag = TAG_DEF_UNDEF;
  } else if (fn->isOverride()) {
    tag = TAG_DEF_OVERRIDE;
  } else {
    tag = TAG_DEF_FUNCTION;
  }

  args.push_back(ConstantInt::get(builder_.getInt8Ty(), tag));
  args.push_back(MDString::get(context_, fn->name()));
  args.push_back(exportModifiers(fn));

  if (fn->type() != NULL) {
    ValueList params;
    params.push_back(exportTypeRef(fn->returnType()));
    for (ParameterList::const_iterator it = fn->params().begin(); it != fn->params().end(); ++it) {
      // Do params and param types.
    }
    args.push_back(MDNode::get(context_, params));
  } else {
  }

  // Template stuff?

  if (!fn->isUndefined() &&
      !fn->isAbstract() &&
      !fn->isIntrinsic() &&
      !fn->isInterfaceMethod() &&
      (fn->ast() != NULL || fn->isExtern()) &&
      fn->defnType() != Defn::Macro &&
      fn->isSingular()) {
    args.push_back(genFunctionValue(fn));
  } else if (fn->ast() != NULL) {
    // TODO: AST
  } else {
    // Nothing
  }

  return MDNode::get(context_, args);
}

MDString * CodeGenerator::exportTypeRef(const Type * type) {
  llvm::SmallString<128> str;
  llvm::raw_svector_ostream typestrm(str);
  typeLinkageName(typestrm, type);
  return MDString::get(context_, typestrm.str());
}

Value * CodeGenerator::exportModifiers(const Defn * de) {
  llvm::SmallString<16> str;
  uint32_t mods = 0;
  if (de->storageClass() == Storage_Static) {
    mods |= DEFNFLAG_STATIC;
  }

  switch (de->visibility()) {
    case Internal: mods |= DEFNFLAG_INTERNAL; break;
    case Private: mods |= DEFNFLAG_PRIVATE; break;
    case Protected: mods |= DEFNFLAG_PROTECTED; break;
    default: break;
  }

  if (de->hasTrait(Defn::ReadOnly)) {
    mods |= DEFNFLAG_READONLY;
  }

  if (const FunctionDefn * fn = dyn_cast<FunctionDefn>(de)) {
    if (fn->isAbstract()) {
      mods |= DEFNFLAG_ABSTRACT;
    }
    if (fn->isFinal()) {
      mods |= DEFNFLAG_FINAL;
    }
    if (fn->isUnsafe()) {
      mods |= DEFNFLAG_UNSAFE;
    }
    if (fn->isExtern()) {
      mods |= DEFNFLAG_EXTERN;
    }
  }

  if (const ParameterDefn * p = dyn_cast<ParameterDefn>(de)) {
    if (p->isVariadic()) {
      mods |= DEFNFLAG_VARIADIC;
    }
    if (p->isKeywordOnly()) {
      mods |= DEFNFLAG_KEYWORDONLY;
    }
  }

  return ConstantInt::get(builder_.getInt32Ty(), mods);
}

} // namespace tart

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Module.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/NamespaceDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/Template.h"

#include "tart/Expr/Constant.h"

#include "tart/Meta/ASTWriter.h"
#include "tart/Meta/MDNodeBuilder.h"
#include "tart/Meta/MDWriter.h"
#include "tart/Meta/VarInt.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"
#include "tart/Objects/Intrinsics.h"

#include "tart/Type/TypeAlias.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/TupleType.h"

#include "tart/AST/Stmt.h"

#include "tart/Gen/CodeGenerator.h"

#include "llvm/Module.h"

namespace tart {

using namespace llvm;

// -------------------------------------------------------------------
// MDWriter

MDWriter::MDWriter(CodeGenerator & cg)
  : cg_(cg)
  , builder_(cg.builder())
  , context_(cg.context())
  , needImports_(false)
{}

MDNode * MDWriter::scopeMembers(const IterableScope * scope) {
  MDNodeBuilder builder(context_);
  for (Defn * de = scope->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->isSynthetic()) {
      continue;
    }
    switch (de->defnType()) {
      case Defn::Typedef:
        builder.put(typeDefn(static_cast<TypeDefn *>(de)));
        break;

      case Defn::Namespace:
        builder.put(namespaceDefn(static_cast<NamespaceDefn *>(de)));
        break;

      case Defn::Var:
      case Defn::Let:
        builder.put(variableDefn(static_cast<VariableDefn *>(de)));
        break;

      case Defn::Property:
        builder.put(propertyDefn(static_cast<PropertyDefn *>(de)));
        break;

      case Defn::Indexer:
        builder.put(indexerDefn(static_cast<IndexerDefn *>(de)));
        break;

      case Defn::Function:
      case Defn::Macro:
        builder.put(functionDefn(static_cast<FunctionDefn *>(de)));
        break;

      case Defn::ExplicitImport:
        break;

      default:
        DASSERT_OBJ(false && "Invalid export", de);
        break;
    }
  }

  return builder.build();
}

MDNode * MDWriter::typeDefn(const TypeDefn * td) {
  const Type * type = td->value().type();
  MDNodeBuilder builder(context_);
  switch (type->typeClass()) {
    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol: {
      meta::Defn::Tag tag;
      switch (type->typeClass()) {
        case Type::Class: tag = meta::Defn::CLASS; break;
        case Type::Struct: tag = meta::Defn::STRUCT; break;
        case Type::Interface: tag = meta::Defn::INTERFACE; break;
        case Type::Protocol: tag = meta::Defn::PROTOCOL; break;
        default: break;
      }
      putDefnHeader(builder, td, tag);
      if (td->isTemplate()) {
        needImports_ = true;
        builder.put(serializeAst(td->templateSignature()->ast()));
      } else {
        const CompositeType * ctype = static_cast<const CompositeType *>(type);
        builder.put(imports(td->ast()->imports()));
        builder.put(scopeMembers(type->memberScope()));

        // Class details - bases, attribute details, etc.
        MDNodeBuilder details(context_);
        const ClassList & bases = ctype->bases();
        for (ClassList::const_iterator it = bases.begin(); it != bases.end(); ++it) {
          const CompositeType * base = *it;
          if (base != Builtins::typeObject.get()) {
            MDNodeBuilder baseDetails(context_);
            baseDetails.put(tagValue(meta::Detail::BASE_TYPE));
            baseDetails.put(serializeType(base));
            details.put(baseDetails.build());
          }
        }

        // TODO - extern, linkage name, etc.
        if (ctype->isAttribute()) {
          MDNodeBuilder attrDetails(context_);
          attrDetails.put(tagValue(meta::Detail::ATTRIBUTE));
          attrDetails.put(tagValue(ctype->attributeInfo().target()));
          attrDetails.put(tagValue(ctype->attributeInfo().retention()));
          attrDetails.put(tagValue(ctype->attributeInfo().propagation()));
          details.put(attrDetails.build());
        }

        builder.put(details.build());
      }
      break;
    }

    case Type::Enum: {
      const EnumType * etype = static_cast<const EnumType *>(type);
      putDefnHeader(builder, td, meta::Defn::ENUM);
      builder.put(NULL);

      // Enum constants
      MDNodeBuilder members(context_);
      for (Defn * ec = etype->firstMember(); ec != NULL; ec = ec->nextInScope()) {
        if (!ec->isSynthetic()) {
          VariableDefn * var = cast<VariableDefn>(ec);
          MDNodeBuilder econst(context_);
          econst.put(var->name());
          econst.put(expression(var->initValue()));
          members.put(econst.build());
        }
      }
      builder.put(members.build());

      // Enum base type
      if (etype->baseType() == &Int32Type::instance) {
        builder.put(NULL);
      } else {
        builder.put(serializeType(etype->baseType()));
      }
      break;
    }

    case Type::Alias: {
      const TypeAlias * typeAlias = static_cast<const TypeAlias *>(type);
      putDefnHeader(builder, td, meta::Defn::TYPEALIAS);
      builder.put(serializeType(typeAlias->value()));
      break;
    }

    default:
      break;
  }

  builder.put(td->name());
  return builder.build();
}

MDNode * MDWriter::namespaceDefn(const NamespaceDefn * ns) {
  MDNodeBuilder builder(context_);
  putDefnHeader(builder, ns, meta::Defn::NAMESPACE);
  builder.put(imports(ns->ast()->imports()));
  builder.put(scopeMembers(&ns->memberScope()));
  return builder.build();
}

MDNode * MDWriter::variableDefn(const VariableDefn * var) {
  MDNodeBuilder builder(context_);
  putDefnHeader(builder, var,
      var->defnType() == Defn::Let ? meta::Defn::LET : meta::Defn::VARIABLE);
  builder.put(serializeType(var->type()));
  if (var->initValue() != NULL && var->defnType() == Defn::Let) {
    builder.put(expression(var->initValue()));
  } else {
    builder.put(NULL);
  }
  return builder.build();
}

MDNode * MDWriter::propertyDefn(const PropertyDefn * prop) {
  MDNodeBuilder builder(context_);
  putDefnHeader(builder, prop, meta::Defn::PROPERTY);
  builder.put(serializeType(prop->type()));
  //builder.put(NULL); // Indexer params?
  builder.put(scopeMembers(&prop->accessorScope()));
  return builder.build();
}

MDNode * MDWriter::indexerDefn(const IndexerDefn * idx) {
  MDNodeBuilder builder(context_);
  putDefnHeader(builder, idx, meta::Defn::INDEXER);
  builder.put(serializeType(idx->type()));
  //builder.put(parameterList(idx->params())); // Indexer params?
  builder.put(scopeMembers(&idx->accessorScope()));
  return builder.build();
}

MDNode * MDWriter::functionDefn(const FunctionDefn * fn) {
  MDNodeBuilder builder(context_);
  meta::Defn::Tag tag;
  if (fn->defnType() == Defn::Macro) {
    tag = meta::Defn::MACRO;
  } else if (fn->isUndefined()) {
    tag = meta::Defn::UNDEF;
  } else if (fn->isOverride()) {
    tag = meta::Defn::OVERRIDE;
  } else {
    tag = meta::Defn::FUNCTION;
  }

  putDefnHeader(builder, fn, tag);
  if (fn->isTemplate()) {
    needImports_ = true;
    builder.put(serializeAst(fn->templateSignature()->ast()));
  } else {
    if (fn->type()) {
      builder.put(serializeType(fn->returnType()));
      builder.put(parameterList(fn->params()));

      DASSERT_OBJ(!fn->hasUnboundTypeParams(), fn);
      DASSERT_OBJ(fn->isSingular(), fn);

      // Definition of function body.
      if (!fn->isUndefined() &&
          !fn->isAbstract() &&
          !fn->isIntrinsic() &&
          !fn->isInterfaceMethod() &&
          !fn->isSynthetic()) {
        ASTWriter writer;
        writer.write(fn->functionDecl()->body());
        builder.put(MDString::get(context_, writer.str()));
        if (fn->defnType() == Defn::Macro) {
          builder.put(NULL);
        } else {
          builder.put(cg_.genFunctionValue(fn));
        }
      } else {
        // Function with no body
        builder.put(NULL);
        builder.put(NULL);
      }
    } else {
      diag.fatal(fn) << "No type or AST: " << fn;
    }
  }

  return builder.build();
}

MDNode * MDWriter::parameterList(const ParameterList & params) {
  MDNodeBuilder builder(context_);
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    builder.put(parameterDefn(*it));
  }
  return builder.build();
}

MDNode * MDWriter::parameterDefn(const ParameterDefn * param) {
  MDNodeBuilder builder(context_);
  putDefnHeader(builder, param, meta::Defn::PARAM);
  builder.put(serializeType(param->type()));
  if (param->initValue() != NULL) {
    builder.put(expression(param->initValue()));
  }
  return builder.build();
}

llvm::MDNode * MDWriter::moduleImports() {
  MDNodeBuilder builder(context_);
  if (needImports_) {
    ASTWriter writer;
    writer.writeNodeList(cg_.module()->imports());
    builder.put(MDString::get(context_, writer.str()));
  }
  return builder.build();
}

llvm::MDNode * MDWriter::imports(const ASTNodeList & imports) {
  if (imports.empty()) {
    return NULL;
  } else {
    MDNodeBuilder builder(context_);
    ASTWriter writer;
    writer.writeNodeList(imports);
    builder.put(MDString::get(context_, writer.str()));
    return builder.build();
  }
}

void MDWriter::putDefnHeader(MDNodeBuilder & builder, const Defn * de, meta::Defn::Tag tag) {
  builder.put(tagValue(tag));
  builder.put(de->name().empty() ? "" : de->name());
  builder.put(location(de));
  builder.put(modifiers(de));
  builder.put(attributeList(de));
}

Value * MDWriter::templateSignature(const Defn * de) {
  const Template * tm = de->templateSignature();
  const TupleType * typeParams = tm->typeParams();
  const TypeList & typeParamDefaults = tm->typeParamDefaults();
  size_t numTypeParams = typeParams->size();
  ASTWriter writer;
  for (size_t i = 0; i < numTypeParams; ++i) {
    QualifiedType param = typeParams->member(i);
    const Type * paramDefault = typeParamDefaults[i];
    if (paramDefault != NULL) {
      writer.write(meta::AST::ASSIGN);
      writer.write(param);
      writer.write(paramDefault);
    } else {
      writer.write(param);
    }
  }
  return MDString::get(context_, writer.str());
}

MDNode * MDWriter::location(const Defn * de) {
  if (de->location().file != NULL) {
    TokenPosition pos = de->location().file->tokenPosition(de->location());
    MDNodeBuilder builder(context_);
    builder.put(de->location().file->filePath());
    builder.put(tagValue(pos.beginLine));
    builder.put(tagValue(pos.beginCol));
    builder.put(tagValue(pos.endLine));
    builder.put(tagValue(pos.endCol));
    return builder.build();
  }

  return NULL;
}

MDNode * MDWriter::attributeList(const Defn * de) {
  MDNodeBuilder builder(context_);
  for (ExprList::const_iterator it = de->attrs().begin(); it != de->attrs().end(); ++it) {
    Expr * attrExpr = *it;
    if (const CompositeType * attrClass = dyn_cast<CompositeType>(attrExpr->type())) {
      if (!attrNeedsSerialization(attrClass)) {
        continue;
      }
    }
    builder.put(expression(attrExpr));
  }
  return builder.build();
}

bool MDWriter::attrNeedsSerialization(const CompositeType * attrClass) {
  // No need to mark intrinsics or attributes - there's a modifier bit for those.
  if (attrClass == Builtins::typeIntrinsicAttribute.get() ||
      attrClass == Builtins::typeAttribute.get()) {
    return false;
  } else if (attrClass->isAttribute()) {
    // Many of the built-in attributes do not need serialization,
    // since their effects are serialized instead.
    FunctionDefn * applyFn = dyn_cast_or_null<FunctionDefn>(
        attrClass->lookupSingleMember("apply", true));
    if (applyFn != NULL) {
      const Intrinsic * intrinsic = applyFn->intrinsic();
      // TODO: Extern, LinkageName, ThreadLocal, TargetProperty, Coalesce, possibly Essential
      if (intrinsic == &FlagsApplyIntrinsic::instance ||
          intrinsic == &EntryPointApplyIntrinsic::instance ||
          intrinsic == &ReflectApplyIntrinsic::instance ||
          intrinsic == &TraceMethodApplyIntrinsic::instance ||
          intrinsic == &UnsafeApplyIntrinsic::instance) {
        return false;
      }
    }
  }

  return true;
}

Value * MDWriter::modifiers(const Defn * de) {
  uint32_t mods = 0;
  if (de->isTemplate()) {
    mods |= meta::DefnFlag::TEMPLATE;
  }

  if (de->storageClass() == Storage_Static) {
    mods |= meta::DefnFlag::STATIC;
  }

  switch (de->visibility()) {
    case Internal: mods |= meta::DefnFlag::INTERNAL; break;
    case Private: mods |= meta::DefnFlag::PRIVATE; break;
    case Protected: mods |= meta::DefnFlag::PROTECTED; break;
    default: break;
  }

  if (de->isReadOnly()) {
    mods |= meta::DefnFlag::READONLY;
  }
  if (de->isUnsafe()) {
    mods |= meta::DefnFlag::UNSAFE;
  }
  if (de->isReflected()) {
    mods |= meta::DefnFlag::REFLECTED;
  }

  if (const FunctionDefn * fn = dyn_cast<FunctionDefn>(de)) {
    if (fn->isAbstract()) {
      mods |= meta::DefnFlag::ABSTRACT;
    }
    if (fn->isFinal()) {
      mods |= meta::DefnFlag::FINAL;
    }
    if (fn->isExtern()) {
      mods |= meta::DefnFlag::EXTERN;
    }
    if (fn->isIntrinsic()) {
      mods |= meta::DefnFlag::INTRINSIC;
    }
    if (fn->isTraceMethod()) {
      mods |= meta::DefnFlag::TRACE_METHOD;
    }
    if (fn == fn->module()->entryPoint()) {
      mods |= meta::DefnFlag::ENTRY_POINT;
    }
  }

  if (const ParameterDefn * p = dyn_cast<ParameterDefn>(de)) {
    if (p->isVariadic()) {
      mods |= meta::DefnFlag::VARIADIC;
    }
    if (p->isKeywordOnly()) {
      mods |= meta::DefnFlag::KEYWORDONLY;
    }
  }

  if (const TypeDefn * td = dyn_cast<TypeDefn>(de)) {
    if (const CompositeType * cty = dyn_cast<CompositeType>(td->typePtr())) {
      if (cty->isAbstract()) {
        mods |= meta::DefnFlag::ABSTRACT;
      }
      if (cty->isFinal()) {
        mods |= meta::DefnFlag::FINAL;
      }
      if (cty->isAttribute()) {
        mods |= meta::DefnFlag::ATTRIBUTE;
      }
    } else if (const EnumType * ety = dyn_cast<EnumType>(td->typePtr())) {
      if (ety->isFlags()) {
        mods |= meta::DefnFlag::FLAGS_ENUM;
      }
    }
  }

  return ConstantInt::get(builder_.getInt32Ty(), mods);
}

Value * MDWriter::expression(const Expr * in) {
  using namespace tart::meta;

  switch (in->exprType()) {
    case Expr::ConstInt: {
      const ConstantInteger * cint = static_cast<const ConstantInteger *>(in);
      MDNodeBuilder builder(context_);
      builder.put(tagValue(ExprID::CONST_INT));
      builder.put(serializeType(cint->type()));
      builder.put(cint->value());
      return builder.build();
    }

    case Expr::ConstString: {
      const ConstantString * cstr = static_cast<const ConstantString *>(in);
      MDNodeBuilder builder(context_);
      builder.put(tagValue(ExprID::CONST_STR));
      builder.put(cstr->value());
      return builder.build();
    }

    case Expr::ConstObjRef: {
      const ConstantObjectRef * obj = static_cast<const ConstantObjectRef *>(in);
      MDNodeBuilder builder(context_);
      builder.put(tagValue(ExprID::CONST_OBJ));
      builder.put(serializeType(obj->type()));
      for (ExprList::const_iterator it = obj->members().begin(); it != obj->members().end(); ++it) {
        if (*it == NULL) {
          builder.put(NULL);
        } else {
          builder.put(expression(*it));
        }
      }
      return builder.build();
    }

    case Expr::ConstNArray: {
      // TODO: Implement
      return NULL;
    }

    case Expr::UpCast: {
      const CastExpr * cast = static_cast<const CastExpr *>(in);
      MDNodeBuilder builder(context_);
      builder.put(tagValue(ExprID::UPCAST));
      builder.put(serializeType(in->type()));
      builder.put(expression(cast->arg()));
      return builder.build();
    }

    case Expr::LValue: {
      const LValueExpr * lval = static_cast<const LValueExpr *>(in);
      DASSERT(lval->base() == NULL) << "Can't serialize lvalue with base: " << in;
      const ValueDefn * val = lval->value();
      DASSERT(val->storageClass() == Storage_Global || val->storageClass() == Storage_Global) <<
          "Can't serialize non-static variable: " << val;
      MDNodeBuilder builder(context_);
      builder.put(tagValue(ExprID::CONST_LVAL));
      builder.put(val->qualifiedName());
      return builder.build();
    }

    default:
      diag.error(in) << "Implement expr serialization for: " << exprTypeName(in->exprType());
      return NULL;
  }
}

Value * MDWriter::serializeType(QualifiedType ty) {
  ASTWriter writer;
  writer.write(ty.type());
  return MDString::get(context_, writer.str());
}

Value * MDWriter::serializeType(const Type * ty) {
  ASTWriter writer;
  writer.write(ty);
  return MDString::get(context_, writer.str());
}

Value * MDWriter::serializeAst(const ASTNode * ast) {
  ASTWriter writer;
  writer.write(ast);
  return MDString::get(context_, writer.str());
}

ConstantInt * MDWriter::tagValue(unsigned tag) {
  return ConstantInt::get(builder_.getInt32Ty(), tag);
}

Value * MDWriter::nullValue() {
  return llvm::ConstantPointerNull::get(builder_.getInt8PtrTy());
}

} // namespace tart

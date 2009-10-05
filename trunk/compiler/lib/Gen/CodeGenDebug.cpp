/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/EnumType.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Support/Dwarf.h"

namespace tart {

using namespace llvm;

typedef llvm::SmallVector<llvm::DIDescriptor, 16> DIDescriptorArray;

static unsigned typeEncoding(TypeId id) {
  if (id == TypeId_Bool) {
    return dwarf::DW_ATE_boolean;
  } else if (id == TypeId_Char) {
    return dwarf::DW_ATE_unsigned_char;
  } else if (isFloatingType(id)) {
    return dwarf::DW_ATE_float;
  } else if (isSignedIntegerType(id)) {
    return dwarf::DW_ATE_signed;
  } else if (isUnsignedIntegerType(id)) {
    return dwarf::DW_ATE_unsigned;
  }

  return 0;
}

llvm::DIType CodeGenerator::genDIType(const TypeRef & ref) {
  return genDIType(ref.type());
}

DIType CodeGenerator::genDIType(const Type * type) {
  DITypeMap::iterator it = dbgTypeMap_.find(type);
  if (it != dbgTypeMap_.end()) {
    return it->second;
  }

  DIType result;
  switch (type->typeClass()) {
    case Type::Primitive:
      result = genDIPrimitiveType(static_cast<const PrimitiveType *>(type));
      break;

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
      result = genDICompositeType(static_cast<const CompositeType *>(type));
      break;

    case Type::Enum:
      result = genDIEnumType(static_cast<const EnumType *>(type));
      break;

    case Type::NativeArray:
      result = genDINativeArrayType(static_cast<const NativeArrayType *>(type));
      break;

    case Type::NativePointer:
      result = genDINativePointerType(static_cast<const NativePointerType *>(type));
      break;

    case Type::Address:
      result = genDIAddressType(static_cast<const AddressType *>(type));
      break;

    case Type::Union:
      result = genDIUnionType(static_cast<const UnionType *>(type));
      break;

    case Type::Function:
      result = genDIFunctionType(static_cast<const FunctionType *>(type));
      break;

    case Type::Alias: {
      TypeAlias * alias = static_cast<const TypeAlias *>(type);
      return genDIType(alias->value());
    }

    default:
      diag.debug() << type;
      DFAIL("Invalid type defn");
      break;
  }

  dbgTypeMap_[type] = result;
  return result;
}

DIBasicType CodeGenerator::genDIPrimitiveType(const PrimitiveType * type) {
  /// CreateBasicType - Create a basic type like int, float, etc.
  const llvm::Type * irType = type->irType();
  return dbgFactory_.CreateBasicType(
      dbgCompileUnit_,
      type->typeDefn()->qualifiedName(),
      dbgCompileUnit_, 0,
      llvm::ConstantExpr::getSizeOf(irType),
      llvm::ConstantExpr::getAlignOf(irType),
      getInt64Val(0), // Offset
      0, // Flags
      typeEncoding(type->typeId()));
}

DIType CodeGenerator::genDIEmbeddedType(const TypeRef & type) {
  DIType di = genDIType(type);
  if (type.typeClass() == Type::Class) {
    di = dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_pointer_type,
        dbgCompileUnit_,
        "",
        dbgCompileUnit_,
        0,
        llvm::ConstantExpr::getSizeOf(type.irEmbeddedType()),
        llvm::ConstantExpr::getAlignOf(type.irEmbeddedType()),
        getInt64Val(0), 0,
        di);
  }

  di.Verify();
  return di;
}

DIType CodeGenerator::genDIParameterType(const TypeRef & type) {
  DIType di = genDIType(type);
  if (type.typeClass() == Type::Class) {
    di = dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_pointer_type,
        dbgCompileUnit_,
        "",
        dbgCompileUnit_,
        0,
        llvm::ConstantExpr::getSizeOf(type.irEmbeddedType()),
        llvm::ConstantExpr::getAlignOf(type.irEmbeddedType()),
        getInt64Val(0), 0,
        di);
  }

  di.Verify();
  return di;
}

DIDerivedType CodeGenerator::genDITypeBase(const CompositeType * type) {
  return dbgFactory_.CreateDerivedType(
      dwarf::DW_TAG_inheritance,
      dbgCompileUnit_,
      type->typeDefn()->name(),
      dbgCompileUnit_,
      getSourceLineNumber(type->typeDefn()->location()),
      llvm::ConstantExpr::getSizeOf(type->irType()),
      llvm::ConstantExpr::getAlignOf(type->irType()),
      getInt64Val(0), 0,
      genDIType(type));
}

DIDerivedType CodeGenerator::genDITypeMember(const VariableDefn * var, Constant * offset) {
  return dbgFactory_.CreateDerivedType(
      dwarf::DW_TAG_member,
      dbgCompileUnit_,
      var->name(),
      dbgCompileUnit_,
      getSourceLineNumber(var->location()),
      llvm::ConstantExpr::getSizeOf(var->type().irEmbeddedType()),
      llvm::ConstantExpr::getAlignOf(var->type().irEmbeddedType()),
      offset, 0,
      genDIEmbeddedType(var->type()));
}

DICompositeType CodeGenerator::genDICompositeType(const CompositeType * type) {
  DICompositeType placeHolder;
  dbgTypeMap_[type] = placeHolder;

  const DefnList & fields = type->instanceFields();
  DIDescriptorArray members;

  if (type->typeClass() == Type::Class && type->super() != NULL) {
    members.push_back(genDITypeBase(type->super()));
  }

  for (DefnList::const_iterator it = fields.begin(); it != fields.end(); ++it) {
    if (*it) {
      const VariableDefn * var = cast<VariableDefn>(*it);
      Constant * offset = llvm::ConstantExpr::getOffsetOf(
          cast<StructType>(type->irType()), var->memberIndex());
      members.push_back(genDITypeMember(var, offset));
    }
  }

  DICompositeType di = dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      type->typeDefn()->linkageName(),
      dbgCompileUnit_,
      getSourceLineNumber(type->typeDefn()->location()),
      llvm::ConstantExpr::getSizeOf(type->irType()),
      llvm::ConstantExpr::getAlignOf(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(members.data(), members.size()));

  dbgTypeMap_[type] = di;
  placeHolder.replaceAllUsesWith(di);
  di.Verify();
  return di;
}

DIType CodeGenerator::genDIEnumType(const EnumType * type) {
  DIDescriptorArray members;
  return dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_enumeration_type,
      dbgCompileUnit_,
      type->typeDefn()->name(),
      dbgCompileUnit_,
      getSourceLineNumber(type->typeDefn()->location()),
      llvm::ConstantExpr::getSizeOf(type->irType()),
      llvm::ConstantExpr::getAlignOf(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(members.data(), members.size()));
}

DICompositeType CodeGenerator::genDINativeArrayType(const NativeArrayType * type) {
  DIDescriptor subrange = dbgFactory_.GetOrCreateSubrange(0, type->size());
  return dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_enumeration_type,
      dbgCompileUnit_,
      "",
      dbgCompileUnit_,
      0,
      llvm::ConstantExpr::getSizeOf(type->irType()),
      llvm::ConstantExpr::getAlignOf(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(&subrange, 1));
}

DIDerivedType CodeGenerator::genDINativePointerType(const NativePointerType * type) {
  return dbgFactory_.CreateDerivedType(
      dwarf::DW_TAG_pointer_type,
      dbgCompileUnit_,
      "",
      dbgCompileUnit_,
      0,
      llvm::ConstantExpr::getSizeOf(type->irType()),
      llvm::ConstantExpr::getAlignOf(type->irType()),
      getInt64Val(0), 0,
      genDIType(type->typeParam(0)));
}

DIDerivedType CodeGenerator::genDIAddressType(const AddressType * type) {
  return dbgFactory_.CreateDerivedType(
      dwarf::DW_TAG_pointer_type,
      dbgCompileUnit_,
      "",
      dbgCompileUnit_,
      0,
      llvm::ConstantExpr::getSizeOf(type->irType()),
      llvm::ConstantExpr::getAlignOf(type->irType()),
      getInt64Val(0), 0,
      genDIType(type->typeParam(0)));
}

DICompositeType CodeGenerator::genDIUnionType(const UnionType * type) {
  const llvm::Type * irType = type->irType();
  DIDescriptorArray unionMembers;

  // Collect union members
  int memberIndex = 0;
  for (TypeRefList::const_iterator it = type->members().begin(); it != type->members().end(); ++it) {
    TypeRef memberType = *it;
    if (memberType.isNonVoidType()) {
      char name[16];
      snprintf(name, 16, "t%d", memberIndex);
      DIType memberDbgType = genDIEmbeddedType(memberType);
      unionMembers.push_back(dbgFactory_.CreateDerivedType(
          dwarf::DW_TAG_member,
          dbgCompileUnit_,
          name,
          dbgCompileUnit_,
          0,
          llvm::ConstantExpr::getSizeOf(memberType.irEmbeddedType()),
          llvm::ConstantExpr::getAlignOf(memberType.irEmbeddedType()),
          getInt64Val(0), 0,
          memberDbgType));
    }

    ++memberIndex;
  }

  // Create the non-discriminated union.
  DICompositeType unionType = dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_union_type,
      dbgCompileUnit_,
      "",
      dbgCompileUnit_,
      0,
      llvm::ConstantExpr::getSizeOf(type->irType()),
      llvm::ConstantExpr::getAlignOf(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(unionMembers.data(), unionMembers.size()));

  // If there's a discriminator field
  if (irType->getNumContainedTypes() > 1) {
    const llvm::Type * discType = irType->getContainedType(0);
    DIDescriptorArray structMembers;
    DIType discDbgType = dbgFactory_.CreateBasicType(
        dbgCompileUnit_,
        "",
        dbgCompileUnit_, 0,
        llvm::ConstantExpr::getSizeOf(discType),
        llvm::ConstantExpr::getAlignOf(discType),
        getInt64Val(0), // Offset
        0, // Flags
        dwarf::DW_ATE_unsigned);

    structMembers.push_back(dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_member,
        dbgCompileUnit_,
        "vindex",
        dbgCompileUnit_,
        0,
        llvm::ConstantExpr::getSizeOf(discType),
        llvm::ConstantExpr::getAlignOf(discType),
        llvm::ConstantExpr::getOffsetOf(cast<StructType>(type->irType()), 0),
        0,
        discDbgType));

    structMembers.push_back(dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_member,
        dbgCompileUnit_,
        ".value",
        dbgCompileUnit_,
        0,
        llvm::ConstantExpr::getSizeOf(irType->getContainedType(1)),
        llvm::ConstantExpr::getAlignOf(irType->getContainedType(1)),
        llvm::ConstantExpr::getOffsetOf(cast<StructType>(type->irType()), 1),
        0,
        unionType));

    unionType = dbgFactory_.CreateCompositeType(
        dwarf::DW_TAG_structure_type,
        dbgCompileUnit_,
        "",
        dbgCompileUnit_,
        0,
        llvm::ConstantExpr::getSizeOf(type->irType()),
        llvm::ConstantExpr::getAlignOf(type->irType()),
        getInt64Val(0), 0,
        DIType(),
        dbgFactory_.GetOrCreateArray(structMembers.data(), structMembers.size()));
  }

  return unionType;
}

DICompositeType CodeGenerator::genDIFunctionType(const FunctionType * type) {
  DIDescriptorArray args;
  args.push_back(genDIType(type->returnType()));

  if (type->selfParam() != NULL) {
    const ParameterDefn * param = type->selfParam();
    DIType ptype = genDIParameterType(param->type());
    ptype = dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_formal_parameter,
        dbgCompileUnit_,
        "self",
        dbgCompileUnit_,
        getSourceLineNumber(param->location()),
        llvm::ConstantExpr::getSizeOf(param->type().irParameterType()),
        getInt64Val(0),
        getInt64Val(0), 0,
        ptype);
    args.push_back(ptype);
  }

  const ParameterList & params = type->params();
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    const ParameterDefn * param = *it;
    DIType ptype = genDIParameterType(param->type());
    ptype = dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_formal_parameter,
        dbgCompileUnit_,
        param->name() != NULL ? param->name() : "",
        dbgCompileUnit_,
        getSourceLineNumber(param->location()),
        llvm::ConstantExpr::getSizeOf(param->internalType().irParameterType()),
        getInt64Val(0),
        getInt64Val(0), 0,
        ptype);
    args.push_back(ptype);
  }

  return dbgFactory_.CreateCompositeType(
      llvm::dwarf::DW_TAG_subroutine_type,
      dbgCompileUnit_,
      "",
      dbgCompileUnit_,
      0, // Source line
      uint64_t(0), // Size
      0, // Align
      0, // Offset
      0, // Flags
      DIType(),
      dbgFactory_.GetOrCreateArray(args.data(), args.size()));
}

} // namespace tart

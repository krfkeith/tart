/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "config.h"

#include "tart/Gen/CodeGenerator.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/CFG/FunctionRegion.h"
#include "tart/CFG/LexicalBlockRegion.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/EnumType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/TargetSelection.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Support/Dwarf.h"

namespace tart {

using namespace llvm;

typedef SmallVector<DIDescriptor, 16> DIDescriptorArray;

static unsigned typeEncoding(TypeId id) {
  if (id == TypeId_Bool) {
    return dwarf::DW_ATE_boolean;
  } else if (id == TypeId_Char) {
    return dwarf::DW_ATE_unsigned_char;
  } else if (isFloatingTypeId(id)) {
    return dwarf::DW_ATE_float;
  } else if (isSignedIntegerTypeId(id)) {
    return dwarf::DW_ATE_signed;
  } else if (isUnsignedIntegerTypeId(id)) {
    return dwarf::DW_ATE_unsigned;
  } else if (id == TypeId_Void) {
    return dwarf::DW_ATE_boolean;
  } else if (id == TypeId_Null) {
    return dwarf::DW_ATE_address;
  }

  return 0;
}

void CodeGenerator::setDebugLocation(const SourceLocation & loc) {
  if (debug_ && loc != dbgLocation_) {
    dbgLocation_ = loc;
    if (loc.region == NULL) {
      builder_.SetCurrentDebugLocation(llvm::DebugLoc());
    } else {
      TokenPosition pos = tokenPosition(loc);
      DASSERT(pos.beginLine);

      // TODO: Avoid creating the debug loc each time.
//      MDNode * inlinedAt = NULL;
//      SourceLocation inLoc = loc.region->inlinedAt();
//      if (inLoc.region != NULL) {
//        TokenPosition inPos = tokenPosition(inLoc);
//        inlinedAt = DebugLoc::get(
//            inPos.beginLine, inPos.endLine, genRegionScope(inLoc.region)).getAsMDNode(context_);
//      }

      builder_.SetCurrentDebugLocation(
          DebugLoc::get(pos.beginLine, pos.beginCol, genRegionScope(loc.region) /*, inlinedAt */));
    }
  }
}

void CodeGenerator::clearDebugLocation() {
  builder_.SetCurrentDebugLocation(llvm::DebugLoc());
}

DICompileUnit CodeGenerator::genDICompileUnit(const ProgramSource * source) {
  DICompileUnit compileUnit;
  if (source != NULL) {
    sys::Path srcPath(source->getFilePath());
    if (!srcPath.empty()) {
      DASSERT(srcPath.isAbsolute());
      compileUnit = dbgFactory_.CreateCompileUnit(
        llvm::dwarf::DW_LANG_lo_user,
        //0xABBA, // Take a chance on me...
        srcPath.getLast(),
        srcPath.getDirname(),
        "0.1 tartc",
        module_->entryPoint() != NULL);
    }
  }

  return compileUnit;
}

DIFile CodeGenerator::genDIFile(const SourceRegion * source) {
  DIFile & file = dbgFiles_[source->getFilePath()];
  if ((MDNode *)file == NULL) {
    if (source != NULL) {
      sys::Path srcPath(source->getFilePath());
      if (!srcPath.empty()) {
        DASSERT(srcPath.isAbsolute());
        DASSERT(dbgCompileUnit_.Verify());
        file = dbgFactory_.CreateFile(
            srcPath.getLast(),
            srcPath.getDirname(),
            dbgCompileUnit_);
      } else {
        file = dbgFile_;
      }
    } else {
      DFAIL("No source?");
    }
  }

  DASSERT(file.Verify());
  return file;
}

DIFile CodeGenerator::genDIFile(const Defn * defn) {
  if (defn == NULL || defn->location().region == NULL) {
    return dbgFile_;
  }

  return genDIFile(defn->location().region);
}

DIScope CodeGenerator::genRegionScope(SourceRegion * region) {
  if (region->dbgScope().isScope()) {
    return region->dbgScope();
  }

  if (const LexicalBlockRegion * bregion = dyn_cast<LexicalBlockRegion>(region)) {
    TokenPosition pos = tokenPosition(bregion->location());
    DASSERT(pos.beginLine);
    region->dbgScope() = dbgFactory_.CreateLexicalBlock(
        genRegionScope(region->parentRegion()),
        genDIFile(region),
        pos.beginLine,
        pos.beginCol);
  } else if (const FunctionRegion * fregion = dyn_cast<FunctionRegion>(region)) {
    if (fregion->function()->defnType() != Defn::Macro) {
      region->dbgScope() = genDISubprogram(fregion->function());
    } else {
      region->dbgScope() = genRegionScope(region->parentRegion());
    }
  } else if (const ProgramSource * source = dyn_cast<ProgramSource>(region)) {
    return dbgCompileUnit_;
  } else {
    diag.fatal() << "Unsupported region type";
    return DIScope();
  }

  return region->dbgScope();
}

DISubprogram CodeGenerator::genDISubprogram(const FunctionDefn * fn) {
  DASSERT(fn != NULL);
  DISubprogram & sp = dbgSubprograms_[fn];
  if ((MDNode *)sp == NULL) {
    DICompositeType dbgFuncType = genDIFunctionType(fn->functionType());
    DASSERT(dbgCompileUnit_.Verify());
    DASSERT_OBJ(fn->hasBody(), fn);
    sp = dbgFactory_.CreateSubprogram(
        dbgCompileUnit_,
        fn->name(),
        fn->qualifiedName(), // fn->qualifiedName(),
        fn->linkageName(),
        genDIFile(fn),
        getSourceLineNumber(fn->location()),
        dbgFuncType,
        fn->isSynthetic() /* isLocalToUnit */,
        fn->hasBody() /* isDefinition */,
        0 /* VK */,
        0 /* VIndex */,
        DIType() /* DIType */,
        0 /* Flags */,
        false /* isOptimized */,
        genFunctionValue(fn->mergeTo() ? fn->mergeTo() : fn));
    if (!sp.Verify()) {
      sp.Verify();
      DFAIL("Bad DBG");
    }
  }

  return sp;
}

void CodeGenerator::genDISubprogramStart(const FunctionDefn * fn) {
  // Generate debugging information (this has to be done after local variable allocas.)
  if (debug_ && (MDNode *)dbgContext_ != NULL) {
    const FunctionType * ftype = fn->functionType();
    if (ftype->selfParam() != NULL) {
      genDIParameter(ftype->selfParam());
    }

    const ParameterList & params = ftype->params();
    for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
      genDIParameter(*it);
    }

    const LocalScopeList & lsl = fn->localScopes();
    for (LocalScopeList::const_iterator it = lsl.begin(); it != lsl.end(); ++it) {
      LocalScope * lscope = *it;
      for (const Defn * de = lscope->firstMember(); de != NULL; de = de->nextInScope()) {
        if (const VariableDefn * var = dyn_cast<VariableDefn>(de)) {
          /// CreateVariable - Create a new descriptor for the specified variable.
          DIVariable dbgVar = dbgFactory_.CreateVariable(
              dwarf::DW_TAG_auto_variable, dbgContext_,
              var->name(), dbgFile_, getSourceLineNumber(var->location()),
              genDIEmbeddedType(var->type()));
          if (var->hasStorage()) {
            setDebugLocation(var->location());
            dbgFactory_.InsertDeclare(var->irValue(), dbgVar, builder_.GetInsertBlock());
          } else {
//            dbgFactory_.InsertDbgValueIntrinsic(var->irValue(), 0,
//                dbgVar, builder_.GetInsertBlock());
          }
        }
      }
    }
  }
}

void CodeGenerator::genDIParameter(const ParameterDefn * param) {
  // TODO: Need to take 'shape' into account, esp for return type.
  DIVariable dbgVar = dbgFactory_.CreateVariable(
      dwarf::DW_TAG_arg_variable, dbgContext_,
      param->name(), dbgFile_, getSourceLineNumber(param->location()),
      genDIParameterType(param->type()));
  if (param->isLValue()) {
    dbgFactory_.InsertDeclare(param->irValue(), dbgVar, builder_.GetInsertBlock());
  } else {
    dbgFactory_.InsertDbgValueIntrinsic(param->irValue(), 0, dbgVar, builder_.GetInsertBlock());
  }
}

void CodeGenerator::genDIGlobalVariable(const VariableDefn * var, GlobalVariable * gv) {
  DASSERT(var != NULL);
  DIType varType = genDIEmbeddedType(var->type());
  dbgFactory_.CreateGlobalVariable(
      dbgCompileUnit_,
      var->name(),
      var->qualifiedName(),
      var->linkageName(),
      genDIFile(var),
      getSourceLineNumber(var->location()),
      varType,
      var->storageClass() == Storage_Static,
      true,
      gv);
}

void CodeGenerator::genDILocalVariable(const VariableDefn * var, Value * value) {
  if (debug_ && var->location().region != NULL) {
    /// CreateVariable - Create a new descriptor for the specified variable.
    DIVariable dbgVar = dbgFactory_.CreateVariable(
        dwarf::DW_TAG_auto_variable, genRegionScope(var->location().region),
        var->name(), dbgFile_, getSourceLineNumber(var->location()),
        genDIEmbeddedType(var->type()));
    if (var->hasStorage()) {
      //dbgFactory_.InsertDeclare(var->irValue(), dbgVar, builder_.GetInsertBlock());
    } else {
      setDebugLocation(var->location());
      dbgFactory_.InsertDbgValueIntrinsic(value, 0, dbgVar, builder_.GetInsertBlock());
    }
  }
}

DIType CodeGenerator::genDIType(const Type * type) {
  DASSERT(type != NULL);
  DIType result = dbgTypeMap_[type];
  if ((MDNode *)result != NULL) {
    return result;
  }

  switch (type->typeClass()) {
    case Type::Primitive:
      result = genDIPrimitiveType(static_cast<const PrimitiveType *>(type));
      break;

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
      return genDICompositeType(static_cast<const CompositeType *>(type));
      break;

    case Type::Enum:
      result = genDIEnumType(static_cast<const EnumType *>(type));
      break;

    case Type::NArray:
      result = genDINativeArrayType(static_cast<const NativeArrayType *>(type));
      break;

    case Type::FlexibleArray:
      result = genDIFlexibleArrayType(static_cast<const FlexibleArrayType *>(type));
      break;

    case Type::NAddress:
      result = genDIAddressType(static_cast<const AddressType *>(type));
      break;

    case Type::Union:
      result = genDIUnionType(static_cast<const UnionType *>(type));
      break;

    case Type::Tuple:
      result = genDITupleType(static_cast<const TupleType *>(type));
      break;

    case Type::Function:
      result = genDIFunctionType(static_cast<const FunctionType *>(type));
      break;

    case Type::BoundMethod:
      result = genDIBoundMethodType(static_cast<const BoundMethodType *>(type));
      break;

    case Type::Alias: {
      const TypeAlias * alias = static_cast<const TypeAlias *>(type);
      result = genDIType(alias->value());
    }

    case Type::TypeLiteral:
      result = genDICompositeType(static_cast<const CompositeType *>(Builtins::typeType));
      break;

    default:
      diag.debug() << type;
      DFAIL("Invalid type defn");
      break;
  }

  DASSERT(result.Verify());
  dbgTypeMap_[type] = result;
  return result;
}

DIBasicType CodeGenerator::genDIPrimitiveType(const PrimitiveType * type) {
  /// CreateBasicType - Create a basic type like int, float, etc.
  const llvm::Type * irType = type->irType();
  DASSERT(dbgCompileUnit_.Verify());
  DASSERT(dbgFile_.Verify());
  DIBasicType di = dbgFactory_.CreateBasicType(
      dbgCompileUnit_,
      type->typeDefn()->qualifiedName().c_str(),
      dbgFile_, 0,
      getSizeOfInBits(irType),
      getAlignOfInBits(irType),
      0, // Offset
      0, // Flags
      typeEncoding(type->typeId()));
  DASSERT(di.Verify());
  return di;
}

DIType CodeGenerator::genDIEmbeddedType(const Type * type) {
  DIType di = genDIType(type);
  //DASSERT(di.Verify());
  if (type->typeClass() == Type::Class || type->typeClass() == Type::Interface) {
    di = dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_pointer_type,
        dbgCompileUnit_,
        "",
        genDIFile(type->typeDefn()),
        0,
        getSizeOfInBits(type->irEmbeddedType()),
        getAlignOfInBits(type->irEmbeddedType()),
        0, 0,
        di);
  }

  DASSERT(di.Verify());
  return di;
}

DIType CodeGenerator::genDIParameterType(const Type * type) {
  // TODO: Need to take 'shape' into account.
  DIType di = genDIType(type);
  if (type->typeClass() == Type::Class || type->typeClass() == Type::TypeLiteral) {
    di = dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_pointer_type,
        dbgCompileUnit_,
        "",
        genDIFile(type->typeDefn()),
        0,
        getSizeOfInBits(type->irParameterType()),
        getAlignOfInBits(type->irParameterType()),
        0, 0,
        di);
  }

  DASSERT(di.Verify());
  return di;
}

DIDerivedType CodeGenerator::genDITypeMember(const Type * type, StringRef name, uint64_t & offset) {
  return genDITypeMember(type->irEmbeddedType(), genDIEmbeddedType(type), name, offset);
}

DIDerivedType CodeGenerator::genDITypeMember(const llvm::Type * type, llvm::DIType derivedFrom,
    llvm::StringRef name, uint64_t & offset) {
  uint64_t memberSize = getSizeOfInBits(type);
  uint64_t memberAlign = getAlignOfInBits(type);

  offset = align(offset, memberAlign);
  DIDerivedType result = dbgFactory_.CreateDerivedType(
      dwarf::DW_TAG_member,
      dbgCompileUnit_,
      name.str().c_str(),
      dbgFile_,
      0,
      memberSize,
      memberAlign,
      offset, 0,
      derivedFrom);

  offset += memberSize;
  return result;
}

DICompositeType CodeGenerator::genDICompositeType(const CompositeType * type) {
  DIType placeHolder = dbgFactory_.CreateTemporaryType();
  dbgTypeMap_[type] = placeHolder;

  const DefnList & fields = type->instanceFields();
  DIDescriptorArray members;

  uint64_t memberOffset = 0;
  if (type->typeClass() == Type::Class && type->super() != NULL) {
    const Type * super = type->super();
    members.push_back(dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_inheritance,
        dbgCompileUnit_,
        StringRef(),
        genDIFile(type->typeDefn()),
        getSourceLineNumber(type->typeDefn()->location()),
        getSizeOfInBits(super->irType()),
        getAlignOfInBits(super->irType()),
        0, 0,
        genDIType(super)));
    memberOffset += getSizeOfInBits(super->irType());
    DASSERT(memberOffset > 0);
  }

  for (DefnList::const_iterator it = fields.begin(); it != fields.end(); ++it) {
    if (*it) {
      const VariableDefn * var = cast<VariableDefn>(*it);
      members.push_back(genDITypeMember(var->type(), var->name(), memberOffset));
    }
  }

  DICompositeType di = dbgFactory_.CreateCompositeType(
      type->typeClass() == Type::Class ? dwarf::DW_TAG_class_type : dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      type->typeDefn()->linkageName().c_str(),
      genDIFile(type->typeDefn()),
      getSourceLineNumber(type->typeDefn()->location()),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(members.data(), members.size()));

  dbgTypeMap_[type] = di;
  placeHolder.replaceAllUsesWith(di);
  DASSERT(di.Verify());
  return di;
}

DIType CodeGenerator::genDIEnumType(const EnumType * type) {
  DIDescriptorArray members;
  for (const Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
    if (const VariableDefn * enumConstant = dyn_cast<VariableDefn>(member)) {
      if (const ConstantInteger * enumVal = dyn_cast<ConstantInteger>(enumConstant->initValue())) {
        members.push_back(dbgFactory_.CreateEnumerator(
            enumConstant->name(), enumVal->intValue().getSExtValue()));
      }
    }
  }

  return dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_enumeration_type,
      dbgCompileUnit_,
      type->typeDefn()->name(),
      genDIFile(type->typeDefn()),
      getSourceLineNumber(type->typeDefn()->location()),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(members.data(), members.size()));
}

DICompositeType CodeGenerator::genDINativeArrayType(const NativeArrayType * type) {
  DIDescriptor subrange = dbgFactory_.GetOrCreateSubrange(0, type->size());
  return dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_array_type,
      dbgCompileUnit_,
      "",
      dbgFile_,
      0,
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      0, 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(&subrange, 1));
}

DICompositeType CodeGenerator::genDIFlexibleArrayType(const FlexibleArrayType * type) {
  DIDescriptor subrange = dbgFactory_.GetOrCreateSubrange(0, 0);
  return dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_array_type,
      dbgCompileUnit_,
      "",
      dbgFile_,
      0,
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      0, 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(&subrange, 1));
}

DIDerivedType CodeGenerator::genDIAddressType(const AddressType * type) {
  return dbgFactory_.CreateDerivedType(
      dwarf::DW_TAG_pointer_type,
      dbgCompileUnit_,
      "",
      dbgFile_,
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, 0,
      genDIType(type->typeParam(0)));
}

DICompositeType CodeGenerator::genDIUnionType(const UnionType * type) {
  const llvm::Type * irType = type->irType();
  DIDescriptorArray unionMembers;

  // Collect union members
  int memberIndex = 0;
  for (TupleType::const_iterator it = type->members().begin(); it != type->members().end(); ++it) {
    const Type * memberType = *it;
    if (!memberType->isVoidType()) {
      char name[16];
      snprintf(name, 16, "t%d", memberIndex);
      DIType memberDbgType = genDIEmbeddedType(memberType);
      unionMembers.push_back(dbgFactory_.CreateDerivedType(
          dwarf::DW_TAG_member,
          dbgCompileUnit_,
          name,
          dbgFile_,
          0,
          getSizeOfInBits(memberType->irEmbeddedType()),
          getAlignOfInBits(memberType->irEmbeddedType()),
          0, 0,
          memberDbgType));
    }

    ++memberIndex;
  }

  // Create the non-discriminated union.
  DICompositeType unionType = dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_union_type,
      dbgCompileUnit_,
      "",
      dbgFile_,
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(unionMembers.data(), unionMembers.size()));

  // If there's a discriminator field
  if (irType->getNumContainedTypes() > 1) {
    const llvm::Type * discType = irType->getContainedType(0);
    DIDescriptorArray structMembers;
    DIType discDbgType = dbgFactory_.CreateBasicType(
        dbgCompileUnit_,
        "",
        dbgFile_, 0,
        getSizeOfInBits(discType),
        getAlignOfInBits(discType),
        0, // Offset
        0, // Flags
        dwarf::DW_ATE_unsigned);

    uint64_t offset = 0;
    structMembers.push_back(genDITypeMember(discType, discDbgType, "vindex", offset));
    structMembers.push_back(genDITypeMember(
        irType->getContainedType(1), unionType, ".value", offset));

    unionType = dbgFactory_.CreateCompositeType(
        dwarf::DW_TAG_structure_type,
        dbgCompileUnit_,
        "",
        dbgFile_,
        0,
        getSizeOfInBits(type->irType()),
        getAlignOfInBits(type->irType()),
        0, 0,
        DIType(),
        dbgFactory_.GetOrCreateArray(structMembers.data(), structMembers.size()));
  }

  DASSERT(unionType.Verify());
  return unionType;
}

DICompositeType CodeGenerator::genDITupleType(const TupleType * type) {
  DIDescriptorArray members;
  int32_t index = 0;
  char memberName[16];
  uint64_t memberOffset = 0;
  for (TupleType::const_iterator it = type->begin(); it != type->end(); ++it) {
    const Type * memberType = *it;
    uint64_t memberSize = getSizeOfInBits(memberType->irEmbeddedType());
    uint64_t memberAlign = getAlignOfInBits(memberType->irEmbeddedType());
    memberOffset = align(memberOffset, memberAlign);
    sprintf(memberName, "_%d", index);
    members.push_back(dbgFactory_.CreateDerivedType(
        dwarf::DW_TAG_member,
        dbgCompileUnit_,
        memberName,
        dbgFile_,
        0,
        memberSize, memberAlign, memberOffset, 0,
        genDIEmbeddedType(memberType)));
    memberOffset += memberSize;
  }

  std::string tupleName;
  typeLinkageName(tupleName, type);
  return dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      tupleName,
      dbgFile_,
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(members.data(), members.size()));
}

DICompositeType CodeGenerator::genDIFunctionType(const FunctionType * type) {
  DIDescriptorArray args;
  // TODO: Need to take 'shape' into account.
  args.push_back(genDIType(type->returnType()));

  if (type->selfParam() != NULL) {
    const ParameterDefn * param = type->selfParam();
    DIType ptype = genDIParameterType(param->type());
    DASSERT(ptype.Verify());
    args.push_back(ptype);
  }

  const ParameterList & params = type->params();
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    const ParameterDefn * param = *it;
    DIType ptype = genDIParameterType(param->type());
    DASSERT(ptype.Verify());
    args.push_back(ptype);
  }

  DICompositeType fnType = dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_subroutine_type,
      dbgCompileUnit_,
      "",
      dbgFile_,
      0, // Source line
      0, // Size
      0, // Align
      0, // Offset
      0, // Flags
      DIType(),
      dbgFactory_.GetOrCreateArray(args.data(), args.size()));

  DASSERT(fnType.Verify());
  return fnType;
}

DICompositeType CodeGenerator::genDIBoundMethodType(const BoundMethodType * type) {
  const StructType * stype = cast<StructType>(type->irType());
  const FunctionType * fnType = type->fnType();
  std::string typeName(".fnref.");
  typeLinkageName(typeName, fnType);

  DIType placeHolder = dbgFactory_.CreateTemporaryType();
  dbgTypeMap_[type] = placeHolder;

  //const DefnList & fields = type->instanceFields();
  DIDescriptorArray members;
  uint64_t offset = 0;
  members.push_back(genDITypeMember(fnType, "method", offset));
  members.push_back(genDITypeMember(Builtins::typeObject, "self", offset));

  DICompositeType di = dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      typeName.c_str(),
      dbgFile_,
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(members.data(), members.size()));

  dbgTypeMap_[type] = di;
  placeHolder.replaceAllUsesWith(di);
  DASSERT(di.Verify());
  return di;
}

unsigned CodeGenerator::getSourceLineNumber(const SourceLocation & loc) {
  TokenPosition pos = tokenPosition(loc);
  return pos.beginLine;
}

uint64_t CodeGenerator::getSizeOfInBits(const llvm::Type * ty) {
  return TargetSelection::instance.targetData()->getTypeSizeInBits(ty);
}

uint64_t CodeGenerator::getAlignOfInBits(const llvm::Type * ty) {
  return TargetSelection::instance.targetData()->getABITypeAlignment(ty) * 8;
}

uint64_t CodeGenerator::align(uint64_t offset, uint64_t align) {
  return TargetData::RoundUpAlignment(offset, align);
}

} // namespace tart

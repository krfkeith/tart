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
#include "tart/Objects/SystemDefs.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Support/Dwarf.h"

namespace tart {

using namespace llvm;

typedef SmallVector<Value *, 16> ValueArray;

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

void CodeGenerator::genDICompileUnit() {
  const ProgramSource * source = module_->moduleSource();
  if (source != NULL) {
    sys::Path srcPath(source->getFilePath());
    if (!srcPath.empty()) {
      DASSERT(srcPath.isAbsolute());
      diBuilder_.CreateCompileUnit(
          llvm::dwarf::DW_LANG_C,
          //0xABBA, // Take a chance on me...
          srcPath.getLast(),
          srcPath.getDirname(),
          "0.1 tartc",
          false,
          "", // flags
          1); // revision
    }
  }
}

llvm::DIScope CodeGenerator::compileUnit() {
  return DIScope(diBuilder_.getCU());
}

DIFile CodeGenerator::genDIFile(const SourceRegion * source) {
  DIFile & file = dbgFiles_[source->getFilePath()];
  if ((MDNode *)file == NULL) {
    if (source != NULL) {
      sys::Path srcPath(source->getFilePath());
      if (!srcPath.empty()) {
        DASSERT(srcPath.isAbsolute());
        file = diBuilder_.CreateFile(
            srcPath.getLast(),
            srcPath.getDirname());
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
    region->dbgScope() = diBuilder_.CreateLexicalBlock(
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
    return compileUnit();
  } else {
    diag.fatal() << "Unsupported region type";
    return DIScope();
  }

  return region->dbgScope();
}

DIDescriptor CodeGenerator::genDefnScope(const Defn * de) {
  TypeDefn * definingClass = de->enclosingClassDefn();
  if (definingClass != NULL) {
    return genDIType(definingClass->typeValue());
  }

  // TODO: Namespace

  return compileUnit();
}

DISubprogram CodeGenerator::genDISubprogram(const FunctionDefn * fn) {
  DASSERT(fn != NULL);
  DISubprogram & sp = dbgSubprograms_[fn];
  if ((MDNode *)sp == NULL) {
    DIType diFuncType = genDIFunctionType(fn->functionType());
    DASSERT_OBJ(fn->hasBody(), fn);
    Function * fval = genFunctionValue(fn->mergeTo() ? fn->mergeTo() : fn);
    if (fn->storageClass() == Storage_Instance) {
      sp = diBuilder_.CreateMethod(
          genDefnScope(fn),
          fn->name(),
          fn->linkageName(),
          genDIFile(fn),
          getSourceLineNumber(fn->location()),
          diFuncType,
          fn->isSynthetic() /* isLocalToUnit */,
          fn->hasBody() /* isDefinition */,
          0, 0, NULL,
          0 /* Flags */,
          fval);
    } else {
      sp = diBuilder_.CreateFunction(
          genDefnScope(fn),
          fn->name(),
          fn->linkageName(),
          genDIFile(fn),
          getSourceLineNumber(fn->location()),
          diFuncType,
          fn->isSynthetic() /* isLocalToUnit */,
          fn->hasBody() /* isDefinition */,
          0 /* Flags */,
          fval);
    }
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
          // If variable does not have storage, then we generate debug info on first assignment
          // rather than on alloca.
          if (var->hasStorage()) {
            /// CreateVariable - Create a new descriptor for the specified variable.
            DIVariable dbgVar = diBuilder_.CreateLocalVariable(
                dwarf::DW_TAG_auto_variable, dbgContext_,
                var->name(), dbgFile_, getSourceLineNumber(var->location()),
                genDIEmbeddedType(var->type()));
            setDebugLocation(var->location());
            Instruction * declareInst = diBuilder_.InsertDeclare(
                var->irValue(), dbgVar, builder_.GetInsertBlock());
            declareInst->setDebugLoc(builder_.getCurrentDebugLocation());
          }
        }
      }
    }
  }
}

void CodeGenerator::genDIParameter(const ParameterDefn * param) {
  // TODO: Need to take 'shape' into account, esp for return type.
  DIVariable dbgVar = diBuilder_.CreateLocalVariable(
      dwarf::DW_TAG_arg_variable, dbgContext_,
      param->name(), dbgFile_, getSourceLineNumber(param->location()),
      genDIParameterType(param->type()));
  if (param->isLValue()) {
    diBuilder_.InsertDeclare(param->irValue(), dbgVar, builder_.GetInsertBlock());
  } else {
    diBuilder_.InsertDbgValueIntrinsic(param->irValue(), 0, dbgVar, builder_.GetInsertBlock());
  }
}

void CodeGenerator::genDIGlobalVariable(const VariableDefn * var, GlobalVariable * gv) {
  DASSERT(var != NULL);
  DIType varType = genDIEmbeddedType(var->type());
  if (var->storageClass() == Storage_Static) {
    TypeDefn * definingClass = var->enclosingClassDefn();
    DASSERT(definingClass != NULL);
    DIScope dbgContext = genDIType(definingClass->typeValue());
    diBuilder_.CreateStaticVariable(
        dbgContext,
        var->name(),
        var->linkageName(),
        genDIFile(var),
        getSourceLineNumber(var->location()),
        varType,
        var->visibility() != Public,
        gv);
  } else {
    diBuilder_.CreateGlobalVariable(
        var->name(),
        genDIFile(var),
        getSourceLineNumber(var->location()),
        varType,
        var->visibility() != Public,
        gv);
  }
}

void CodeGenerator::genDILocalVariable(const VariableDefn * var, Value * value) {
  if (debug_ && var->location().region != NULL) {
    // If var does have storage, we generate the debug info on the alloca rather than on
    // first assignment.
    if (!var->hasStorage()) {
      /// CreateVariable - Create a new descriptor for the specified variable.
      DIVariable dbgVar = diBuilder_.CreateLocalVariable(
          dwarf::DW_TAG_auto_variable, genRegionScope(var->location().region),
          var->name(), dbgFile_, getSourceLineNumber(var->location()),
          genDIEmbeddedType(var->type()));
      setDebugLocation(var->location());
      Instruction * valueInst = diBuilder_.InsertDbgValueIntrinsic(
          value, 0, dbgVar, builder_.GetInsertBlock());
      valueInst->setDebugLoc(builder_.getCurrentDebugLocation());
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

DIType CodeGenerator::genDIPrimitiveType(const PrimitiveType * type) {
  /// CreateBasicType - Create a basic type like int, float, etc.
  const llvm::Type * irType = type->irType();
  DASSERT(dbgFile_.Verify());
  DIType di = diBuilder_.CreateBasicType(
      type->typeDefn()->qualifiedName().c_str(),
      getSizeOfInBits(irType),
      getAlignOfInBits(irType),
      typeEncoding(type->typeId()));
  DASSERT(di.Verify());
  return di;
}

DIType CodeGenerator::genDIEmbeddedType(const Type * type) {
  DIType di = genDIType(type);
  if (type->typeClass() == Type::Class || type->typeClass() == Type::Interface) {
    di = diBuilder_.CreatePointerType(di,
        getSizeOfInBits(type->irEmbeddedType()),
        getAlignOfInBits(type->irEmbeddedType()));
  }

  DASSERT(di.Verify());
  return di;
}

DIType CodeGenerator::genDIParameterType(const Type * type) {
  // TODO: Need to take 'shape' into account.
  DIType di = genDIType(type);
  if (type->typeClass() == Type::Class || type->typeClass() == Type::TypeLiteral) {
    di = diBuilder_.CreatePointerType(di,
        getSizeOfInBits(type->irEmbeddedType()),
        getAlignOfInBits(type->irEmbeddedType()));
  }

  DASSERT(di.Verify());
  return di;
}

DIType CodeGenerator::genDITypeMember(const VariableDefn * var, uint64_t & offset) {
  return genDITypeMember(
      var->type()->irEmbeddedType(),
      genDIEmbeddedType(var->type()),
      var->name(),
      getSourceLineNumber(var->location()),
      offset);
}

DIType CodeGenerator::genDITypeMember(const llvm::Type * type, llvm::DIType memberType,
    llvm::StringRef name, unsigned sourceLine, uint64_t & offset) {
  uint64_t memberSize = getSizeOfInBits(type);
  uint64_t memberAlign = getAlignOfInBits(type);

  offset = align(offset, memberAlign);
  DIType result = diBuilder_.CreateMemberType(
      name.str().c_str(),
      dbgFile_,
      sourceLine,
      memberSize, memberAlign, offset, 0,
      memberType);

  offset += memberSize;
  return result;
}

DIType CodeGenerator::genDICompositeType(const CompositeType * type) {
  DIType placeHolder = diBuilder_.CreateTemporaryType();
  dbgTypeMap_[type] = placeHolder;
  TypeDefn * td = type->typeDefn();

  const DefnList & fields = type->instanceFields();
  ValueArray members;

  uint64_t memberOffset = 0;
  if (type->typeClass() == Type::Class && type->super() != NULL) {
    const Type * super = type->super();
    members.push_back(diBuilder_.CreateInheritance(
        placeHolder, genDIType(super), 0, 0));
    memberOffset += getSizeOfInBits(super->irType());
    DASSERT(memberOffset > 0);
  }

  for (DefnList::const_iterator it = fields.begin(); it != fields.end(); ++it) {
    if (*it) {
      const VariableDefn * var = cast<VariableDefn>(*it);
      members.push_back(genDITypeMember(var, memberOffset));
    }
  }

  DIType di;
  if (type->typeClass() == Type::Class) {
    di = diBuilder_.CreateClassType(
        genDefnScope(td),
        td->linkageName(),
        genDIFile(td),
        getSourceLineNumber(td->location()),
        getSizeOfInBits(type->irType()),
        getAlignOfInBits(type->irType()),
        0, // Offset
        getDefnFlags(td),
        DIType(),
        diBuilder_.GetOrCreateArray(members.data(), members.size()));
  } else {
    di = diBuilder_.CreateStructType(
        genDefnScope(td),
        td->linkageName(),
        genDIFile(td),
        getSourceLineNumber(td->location()),
        getSizeOfInBits(type->irType()),
        getAlignOfInBits(type->irType()),
        getDefnFlags(td),
        diBuilder_.GetOrCreateArray(members.data(), members.size()));
  }

  dbgTypeMap_[type] = di;
  placeHolder.replaceAllUsesWith(di);
  DASSERT(di.Verify());
  return di;
}

DIType CodeGenerator::genDIEnumType(const EnumType * type) {
  ValueArray members;
  for (const Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
    if (const VariableDefn * enumConstant = dyn_cast<VariableDefn>(member)) {
      if (const ConstantInteger * enumVal = dyn_cast<ConstantInteger>(enumConstant->initValue())) {
        members.push_back(diBuilder_.CreateEnumerator(
            enumConstant->name(), enumVal->intValue().getSExtValue()));
      }
    }
  }

  return diBuilder_.CreateEnumerationType(
      genDefnScope(type->typeDefn()),
      type->typeDefn()->name(),
      genDIFile(type->typeDefn()),
      getSourceLineNumber(type->typeDefn()->location()),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      diBuilder_.GetOrCreateArray(&members[0], members.size()));
}

#if 0
DIType CodeGenerator::genDINativeArrayType(const NativeArrayType * type) {
  Value * subrange = diBuilder_.GetOrCreateSubrange(0, type->size());
  return diBuilder_.CreateArrayType(
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      genDIEmbeddedType(type->typeParam(0)),
      diBuilder_.GetOrCreateArray(&subrange, 1));
}
#else
DIType CodeGenerator::genDINativeArrayType(const NativeArrayType * type) {
  DIFactory dbgFactory(*irModule_);
  DIDescriptor subrange = dbgFactory.GetOrCreateSubrange(0, type->size());
  return dbgFactory.CreateCompositeType(
      dwarf::DW_TAG_array_type,
      compileUnit(),
      "",
      dbgFile_,
      0,
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      0, 0,
      DIType(),
      dbgFactory.GetOrCreateArray(&subrange, 1));
}

#endif

#if 0
DIType CodeGenerator::genDIFlexibleArrayType(const FlexibleArrayType * type) {
  Value * subrange = diBuilder_.GetOrCreateSubrange(0, 0);
  return diBuilder_.CreateArrayType(
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      genDIEmbeddedType(type->typeParam(0)),
      diBuilder_.GetOrCreateArray(&subrange, 1));
}
#else
DIType CodeGenerator::genDIFlexibleArrayType(const FlexibleArrayType * type) {
  DIFactory dbgFactory(*irModule_);
  DIDescriptor subrange = dbgFactory.GetOrCreateSubrange(0, 0);
  return dbgFactory.CreateCompositeType(
      dwarf::DW_TAG_array_type,
      compileUnit(),
      "",
      dbgFile_,
      0,
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      0, 0,
      DIType(),
      dbgFactory.GetOrCreateArray(&subrange, 1));
}
#endif

DIType CodeGenerator::genDIAddressType(const AddressType * type) {
  return diBuilder_.CreatePointerType(
      genDIType(type->typeParam(0)),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()));
}

DIType CodeGenerator::genDIUnionType(const UnionType * type) {
  const llvm::Type * irType = type->irType();
  ValueArray unionMembers;

  // Collect union members
  int memberIndex = 0;
  for (TupleType::const_iterator it = type->members().begin(); it != type->members().end(); ++it) {
    const Type * memberType = *it;
    if (!memberType->isVoidType()) {
      char name[16];
      snprintf(name, 16, "t%d", memberIndex);
      DIType memberDbgType = genDIEmbeddedType(memberType);
      unionMembers.push_back(diBuilder_.CreateMemberType(
          name,
          dbgFile_,
          0, // Source line
          getSizeOfInBits(memberType->irEmbeddedType()),
          getAlignOfInBits(memberType->irEmbeddedType()),
          0, // Offset
          0, // Flags
          memberDbgType));
    }

    ++memberIndex;
  }

  // Create the non-discriminated union.
  DIType unionType = diBuilder_.CreateUnionType(
      compileUnit(),
      "value",
      dbgFile_,
      0, // Source line
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, // Flags
      diBuilder_.GetOrCreateArray(&unionMembers[0], unionMembers.size()));

  // If there's a discriminator field
  if (irType->getNumContainedTypes() > 1) {
    const llvm::Type * discType = irType->getContainedType(0);
    ValueArray structMembers;
    DIType discDbgType = diBuilder_.CreateBasicType(
        "disc",
        getSizeOfInBits(discType),
        getAlignOfInBits(discType),
        dwarf::DW_ATE_unsigned);

    uint64_t offset = 0;
    structMembers.push_back(genDITypeMember(discType, discDbgType, "vindex", 0, offset));
    structMembers.push_back(genDITypeMember(
        irType->getContainedType(1), unionType, ".value", 0, offset));

    unionType = diBuilder_.CreateStructType(
        compileUnit(),
        "",
        dbgFile_,
        0, // Source line
        getSizeOfInBits(type->irType()),
        getAlignOfInBits(type->irType()),
        0, // Flags
        diBuilder_.GetOrCreateArray(&structMembers[0], structMembers.size()));
  }

  DASSERT(unionType.Verify());
  return unionType;
}

DIType CodeGenerator::genDITupleType(const TupleType * type) {
  ValueArray members;
  int32_t index = 0;
  char memberName[16];
  uint64_t memberOffset = 0;
  for (TupleType::const_iterator it = type->begin(); it != type->end(); ++it) {
    const Type * memberType = *it;
    uint64_t memberSize = getSizeOfInBits(memberType->irEmbeddedType());
    uint64_t memberAlign = getAlignOfInBits(memberType->irEmbeddedType());
    memberOffset = align(memberOffset, memberAlign);
    sprintf(memberName, "_%d", index);
    members.push_back(diBuilder_.CreateMemberType(
        memberName,
        dbgFile_,
        0, // Source line
        memberSize, memberAlign, memberOffset, 0,
        genDIEmbeddedType(memberType)));
    memberOffset += memberSize;
  }

  std::string tupleName;
  typeLinkageName(tupleName, type);
  return diBuilder_.CreateStructType(
      compileUnit(),
      tupleName,
      dbgFile_,
      0, // Source line
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, // Flags
      diBuilder_.GetOrCreateArray(members.data(), members.size()));
}

DIType CodeGenerator::genDIFunctionType(const FunctionType * type) {
  ValueArray args;
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

  DIType fnType = diBuilder_.CreateSubroutineType(
      dbgFile_,
      diBuilder_.GetOrCreateArray(&args[0], args.size()));

  DASSERT(fnType.Verify());
  return fnType;
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

unsigned CodeGenerator::getDefnFlags(const Defn * de) {
  unsigned result = 0;
  if (de->visibility() == Private) {
    result |= DIDescriptor::FlagPrivate;
  } else if (de->visibility() == Protected) {
    result |= DIDescriptor::FlagProtected;
  }

//  FlagFwdDecl          = 1 << 2,
//  FlagVirtual          = 1 << 5,
//  FlagArtificial       = 1 << 6,
//  FlagPrototyped       = 1 << 8

  return result;
}

} // namespace tart

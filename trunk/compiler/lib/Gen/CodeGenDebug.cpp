/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "config.h"

#include "tart/Gen/CodeGenerator.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "tart/Defn/Module.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/EnumType.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/TargetSelection.h"
#include "tart/Objects/SystemDefs.h"

#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Support/Dwarf.h"

namespace tart {

using namespace llvm;
using namespace llvm::sys;

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
    if (loc.file == NULL) {
      builder_.SetCurrentDebugLocation(llvm::DebugLoc());
    } else {
      TokenPosition pos = tokenPosition(loc);
      DASSERT(pos.beginLine);
      builder_.SetCurrentDebugLocation(
          DebugLoc::get(pos.beginLine, pos.beginCol, dbgContext_, dbgInlineContext_));
    }
  }
}

void CodeGenerator::clearDebugLocation() {
  builder_.SetCurrentDebugLocation(llvm::DebugLoc());
}

void CodeGenerator::genDICompileUnit() {
  const ProgramSource * source = module_->moduleSource();
  if (source != NULL) {
    llvm::StringRef srcPath(source->filePath());
    if (!srcPath.empty()) {
      DASSERT(path::is_absolute(srcPath));
      diBuilder_.createCompileUnit(
          llvm::dwarf::DW_LANG_C,
          //0xABBA, // Take a chance on me...
          path::filename(srcPath),
          path::parent_path(srcPath),
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

DIFile CodeGenerator::genDIFile(const ProgramSource * source) {
  DASSERT(source != NULL);
  if (source->filePath().empty()) {
    DASSERT(dbgFile_.Verify());
    return dbgFile_;
  }
  llvm::StringRef srcPath(source->filePath());
  DIFile & file = dbgFiles_[srcPath];
  if ((MDNode *)file == NULL) {
    DASSERT(path::is_absolute(srcPath));
    file = diBuilder_.createFile(path::filename(srcPath), path::parent_path(srcPath));
  }

  DASSERT(file.Verify());
  return file;
}

DIFile CodeGenerator::genDIFile(const Defn * defn) {
  if (defn == NULL || defn->location().file == NULL) {
    return dbgFile_;
  }

  return genDIFile(defn->location().file);
}

DILexicalBlock CodeGenerator::genLexicalBlock(const SourceLocation & loc) {
  TokenPosition pos = tokenPosition(loc);
//  DASSERT(pos.beginLine);
  return diBuilder_.createLexicalBlock(
      dbgContext_,
      genDIFile(loc.file),
      pos.beginLine,
      pos.beginCol);
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
      sp = diBuilder_.createMethod(
          compileUnit(), // genDefnScope(fn),
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
      sp = diBuilder_.createFunction(
          compileUnit(), // genDefnScope(fn),
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
            /// createVariable - create a new descriptor for the specified variable.
            DIVariable dbgVar = diBuilder_.createLocalVariable(
                dwarf::DW_TAG_auto_variable, dbgContext_,
                var->name(), genDIFile(fn), getSourceLineNumber(var->location()),
                genDIEmbeddedType(var->type()));
            setDebugLocation(var->location());
            Instruction * declareInst = diBuilder_.insertDeclare(
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
  DIVariable dbgVar = diBuilder_.createLocalVariable(
      dwarf::DW_TAG_arg_variable, dbgContext_,
      param->name(), genDIFile(param), getSourceLineNumber(param->location()),
      genDIParameterType(param->type()));
  if (param->isLValue()) {
    diBuilder_.insertDeclare(param->irValue(), dbgVar, builder_.GetInsertBlock());
  } else {
    diBuilder_.insertDbgValueIntrinsic(param->irValue(), 0, dbgVar, builder_.GetInsertBlock());
  }
}

void CodeGenerator::genDIGlobalVariable(const VariableDefn * var, GlobalVariable * gv) {
  DASSERT(var != NULL);
  DIType varType = genDIEmbeddedType(var->type());
  if (var->storageClass() == Storage_Static) {
    TypeDefn * definingClass = var->enclosingClassDefn();
    DASSERT(definingClass != NULL);
    DIScope dbgContext = genDIType(definingClass->typeValue());
    diBuilder_.createStaticVariable(
        dbgContext,
        var->name(),
        var->linkageName(),
        genDIFile(var),
        getSourceLineNumber(var->location()),
        varType,
        var->visibility() != Public,
        gv);
  } else {
    diBuilder_.createGlobalVariable(
        var->name(),
        genDIFile(var),
        getSourceLineNumber(var->location()),
        varType,
        var->visibility() != Public,
        gv);
  }
}

void CodeGenerator::genDILocalVariable(const VariableDefn * var, Value * value) {
  if (debug_ && var->location().file != NULL) {
    // If var does have storage, we generate the debug info on the alloca rather than on
    // first assignment.
    if (!var->hasStorage()) {
      /// createVariable - create a new descriptor for the specified variable.
      DIVariable dbgVar = diBuilder_.createLocalVariable(
          dwarf::DW_TAG_auto_variable, dbgContext_,
          var->name(), genDIFile(var), getSourceLineNumber(var->location()),
          genDIEmbeddedType(var->type()));
      setDebugLocation(var->location());
      Instruction * valueInst = diBuilder_.insertDbgValueIntrinsic(
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

  type->irType(); // Force eager evaluation of irType.

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
  /// createBasicType - create a basic type like int, float, etc.
  const llvm::Type * irType = type->irType();
  if (type->isVoidType()) {
    DIType di = diBuilder_.createBasicType(
        type->typeDefn()->qualifiedName(),
        0,
        0,
        typeEncoding(type->typeId()));
    DASSERT(di.Verify());
    return di;
  } else {
    DIType di = diBuilder_.createBasicType(
        type->typeDefn()->qualifiedName(),
        getSizeOfInBits(irType),
        getAlignOfInBits(irType),
        typeEncoding(type->typeId()));
    DASSERT(di.Verify());
    return di;
  }
}

DIType CodeGenerator::genDIEmbeddedType(const Type * type) {
  DIType di = genDIType(type);
  if (type->typeClass() == Type::Class || type->typeClass() == Type::Interface) {
    di = diBuilder_.createPointerType(di,
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
    di = diBuilder_.createPointerType(di,
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
  DASSERT(dbgFile_.Verify());
  DIType result = diBuilder_.createMemberType(
      name.str().c_str(),
      dbgFile_,
      sourceLine,
      memberSize, memberAlign, offset, 0,
      memberType);

  offset += memberSize;
  return result;
}

#if 0
DISubprogram CodeGenerator::genDITypeMemberFunction(const FunctionDefn * fn) {
  DASSERT(fn != NULL);
  DISubprogram & sp = dbgSubprograms_[fn];
  if ((MDNode *)sp == NULL) {
    DIType diFuncType = genDIFunctionType(fn->functionType());
    DASSERT_OBJ(fn->hasBody(), fn);
    Function * fval = genFunctionValue(fn->mergeTo() ? fn->mergeTo() : fn);
    sp = diBuilder_.createMethod(
        compileUnit(), // genDefnScope(fn),
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
    if (!sp.Verify()) {
      sp.Verify();
      DFAIL("Bad DBG");
    }
  }

  return sp;
}
#endif

DIType CodeGenerator::genDICompositeType(const CompositeType * type) {
  DIType placeHolder = diBuilder_.createTemporaryType();
  dbgTypeMap_[type] = placeHolder;
  TypeDefn * td = type->typeDefn();

  const DefnList & fields = type->instanceFields();
  const MethodList & methods = type->instanceMethods();
  ValueArray members;

  uint64_t memberOffset = 0;
  if (type->typeClass() == Type::Class && type->super() != NULL) {
    const Type * super = type->super();
    members.push_back(diBuilder_.createInheritance(
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

  for (MethodList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
  }

  DIType di;
  if (type->typeClass() == Type::Class) {
    di = diBuilder_.createClassType(
        genDefnScope(td),
        td->linkageName(),
        genDIFile(td),
        getSourceLineNumber(td->location()),
        getSizeOfInBits(type->irType()),
        getAlignOfInBits(type->irType()),
        0, // Offset
        getDefnFlags(td),
        DIType(),
        diBuilder_.getOrCreateArray(members));
  } else {
    di = diBuilder_.createStructType(
        genDefnScope(td),
        td->linkageName(),
        genDIFile(td),
        getSourceLineNumber(td->location()),
        getSizeOfInBits(type->irType()),
        getAlignOfInBits(type->irType()),
        getDefnFlags(td),
        diBuilder_.getOrCreateArray(members));
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
        members.push_back(diBuilder_.createEnumerator(
            enumConstant->name(), enumVal->intValue().getSExtValue()));
      }
    }
  }

  return diBuilder_.createEnumerationType(
      genDefnScope(type->typeDefn()),
      type->typeDefn()->name(),
      genDIFile(type->typeDefn()),
      getSourceLineNumber(type->typeDefn()->location()),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      diBuilder_.getOrCreateArray(members));
}

DIType CodeGenerator::genDINativeArrayType(const NativeArrayType * type) {
  Value * subrange = diBuilder_.getOrCreateSubrange(0, type->size());
  return diBuilder_.createArrayType(
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      genDIEmbeddedType(type->typeParam(0)),
      diBuilder_.getOrCreateArray(subrange));
}

DIType CodeGenerator::genDIFlexibleArrayType(const FlexibleArrayType * type) {
  Value * subrange = diBuilder_.getOrCreateSubrange(0, 0);
  return diBuilder_.createArrayType(
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      genDIEmbeddedType(type->typeParam(0)),
      diBuilder_.getOrCreateArray(subrange));
}

DIType CodeGenerator::genDIAddressType(const AddressType * type) {
  return diBuilder_.createPointerType(
      genDIType(type->typeParam(0)),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()));
}

DIType CodeGenerator::genDIUnionType(const UnionType * type) {
  const llvm::Type * irType = type->irType();
  ValueArray unionMembers;
  DASSERT(dbgFile_.Verify());
  DASSERT(!type->irType()->isAbstract());

  // Collect union members
  int memberIndex = 0;
  for (TupleType::const_iterator it = type->members().begin(); it != type->members().end(); ++it) {
    const Type * memberType = *it;
    if (!memberType->isVoidType()) {
      char name[16];
      snprintf(name, 16, "t%d", memberIndex);
      DIType memberDbgType = genDIEmbeddedType(memberType);
      unionMembers.push_back(diBuilder_.createMemberType(
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

  // create the non-discriminated union.
  DIType unionType = diBuilder_.createUnionType(
      compileUnit(),
      "value",
      dbgFile_,
      0, // Source line
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, // Flags
      diBuilder_.getOrCreateArray(unionMembers));

  // If there's a discriminator field
  if (irType->getNumContainedTypes() > 1) {
    const llvm::Type * discType = irType->getContainedType(0);
    ValueArray structMembers;
    DIType discDbgType = diBuilder_.createBasicType(
        "disc",
        getSizeOfInBits(discType),
        getAlignOfInBits(discType),
        dwarf::DW_ATE_unsigned);

    uint64_t offset = 0;
    structMembers.push_back(genDITypeMember(discType, discDbgType, "vindex", 0, offset));
    structMembers.push_back(genDITypeMember(
        irType->getContainedType(1), unionType, ".value", 0, offset));

    unionType = diBuilder_.createStructType(
        compileUnit(),
        "",
        dbgFile_,
        0, // Source line
        getSizeOfInBits(type->irType()),
        getAlignOfInBits(type->irType()),
        0, // Flags
        diBuilder_.getOrCreateArray(structMembers));
  }

  DASSERT(unionType.Verify());
  return unionType;
}

DIType CodeGenerator::genDITupleType(const TupleType * type) {
  ValueArray members;
  int32_t index = 0;
  char memberName[16];
  uint64_t memberOffset = 0;
  DASSERT(dbgFile_.Verify());
  for (TupleType::const_iterator it = type->begin(); it != type->end(); ++it) {
    const Type * memberType = *it;
    uint64_t memberSize = getSizeOfInBits(memberType->irEmbeddedType());
    uint64_t memberAlign = getAlignOfInBits(memberType->irEmbeddedType());
    memberOffset = align(memberOffset, memberAlign);
    sprintf(memberName, "_%d", index);
    members.push_back(diBuilder_.createMemberType(
        memberName,
        dbgFile_,
        0, // Source line
        memberSize, memberAlign, memberOffset, 0,
        genDIEmbeddedType(memberType)));
    memberOffset += memberSize;
  }

  std::string tupleName;
  typeLinkageName(tupleName, type);
  return diBuilder_.createStructType(
      compileUnit(),
      tupleName,
      dbgFile_,
      0, // Source line
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      0, // Flags
      diBuilder_.getOrCreateArray(members));
}

DIType CodeGenerator::genDIFunctionType(const FunctionType * type) {
  ValueArray args;
  // TODO: Need to take 'shape' into account.
  args.push_back(genDIType(type->returnType()));
  DASSERT(dbgFile_.Verify());

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

  DIType fnType = diBuilder_.createSubroutineType(
      dbgFile_,
      diBuilder_.getOrCreateArray(args));

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

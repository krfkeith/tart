/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "config.h"
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
#include "tart/CFG/TupleType.h"
#include "tart/CFG/EnumType.h"
#include "tart/Objects/Builtins.h"

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
  }

  return 0;
}

DICompileUnit CodeGenerator::genDICompileUnit(const ProgramSource * source) {
  using namespace llvm;
  DICompileUnit & compileUnit = dbgCompileUnits_[source];
  if (compileUnit.isNull()) {
    if (source != NULL) {
      sys::Path srcPath(source->getFilePath());
      if (!srcPath.empty()) {
        DASSERT(srcPath.isAbsolute());
        compileUnit = dbgFactory_.CreateCompileUnit(
          0xABBA, // Take a chance on me...
          srcPath.getLast(),
          srcPath.getDirname(),
          "0.1 tartc",
          module_->entryPoint() != NULL);
      }
    }
  }

  return compileUnit;
}

DICompileUnit CodeGenerator::genDICompileUnit(const Defn * defn) {
  if (defn == NULL || defn->location().file == NULL) {
    return dbgCompileUnit_;
  }

  return genDICompileUnit(defn->location().file);
}

DISubprogram CodeGenerator::genDISubprogram(const FunctionDefn * fn) {
  DASSERT(fn != NULL);
  DISubprogram & sp = dbgSubprograms_[fn];
  if (sp.isNull()) {
    DICompileUnit compileUnit = genDICompileUnit(fn);
    if (!compileUnit.isNull()) {
      DICompositeType dbgFuncType = genDIFunctionType(fn->functionType());
      sp = dbgFactory_.CreateSubprogram(
          compileUnit, // TODO: Replace for functions within a scope.
          fn->name(),
          fn->qualifiedName().c_str(),
          fn->linkageName().c_str(),
          compileUnit,
          getSourceLineNumber(fn->location()),
          dbgFuncType,
          fn->isSynthetic() /* isLocalToUnit */,
          true /* isDefinition */);
      if (!sp.Verify()) {
        sp.Verify();
        DFAIL("Bad DBG");
      }
    }
  }

  return sp;
}

void CodeGenerator::genDISubprogramStart(const FunctionDefn * fn) {
  // Generate debugging information (this has to be done after local variable allocas.)
  if (debug_ && !dbgContext_.isNull()) {
    setDebugLocation(fn->location());

#if 0
    // TODO: Need to take 'shape' into account, esp for return type.
    const FunctionType * ftype = fn->functionType();
    if (ftype->selfParam() != NULL) {
      /// CreateVariable - Create a new descriptor for the specified variable.
      const ParameterDefn * p = ftype->selfParam();
      DIVariable selfVar = dbgFactory_.CreateVariable(
          dwarf::DW_TAG_arg_variable, dbgContext_,
          p->name(), dbgCompileUnit_,
          getSourceLineNumber(p->location()),
          genDIType(p->type()));
//      dbgFactory_.InsertDbgValueIntrinsic(
//          p->irValue(), llvm::Value *Offset,
//          argVar, builder_.GetInsertBlock());
      dbgFactory_.InsertDeclare(p->irValue(), selfVar, builder_.GetInsertBlock());
    }
#endif

#if 0
    const ParameterList & params = ftype->params();
    for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
      const ParameterDefn * p = *it;
      if (p->isLValue()) {
        // TODO: Handle this case later.
      } else {
        /// CreateVariable - Create a new descriptor for the specified variable.
        DIVariable argVar = dbgFactory_.CreateVariable(
            dwarf::DW_TAG_arg_variable, dbgContext_,
            p->name(), dbgCompileUnit_, getSourceLineNumber(p->location()),
            genDIParameterType(p->type()));
        dbgFactory_.InsertDbgValueIntrinsic(
            p->irValue(), llvm::Value *Offset,
            argVar, builder_.GetInsertBlock());
        //dbgFactory_.InsertDeclare(p->irValue(), argVar, builder_.GetInsertBlock());
      }
    }
#endif

    const LocalScopeList & lsl = fn->localScopes();
    for (LocalScopeList::const_iterator it = lsl.begin(); it != lsl.end(); ++it) {
      LocalScope * lscope = *it;
      for (const Defn * de = lscope->firstMember(); de != NULL; de = de->nextInScope()) {
        if (de->defnType() == Defn::Var) {
          /// CreateVariable - Create a new descriptor for the specified variable.
          const VariableDefn * var = static_cast<const VariableDefn *>(de);
          DIVariable dbgVar = dbgFactory_.CreateVariable(dwarf::DW_TAG_auto_variable, dbgContext_,
              var->name(), dbgCompileUnit_, getSourceLineNumber(var->location()),
              genDIEmbeddedType(var->type()));
          setDebugLocation(var->location());
          dbgFactory_.InsertDeclare(var->irValue(), dbgVar, builder_.GetInsertBlock());
        }
      }
    }
  }
}

DIType CodeGenerator::genDIType(const Type * type) {
  DASSERT(type != NULL);
  DIType result = dbgTypeMap_[type];
  if (!result.isNull()) {
    return result;
  }

  if (dbgCompileUnit_.isNull()) {
    dbgCompileUnit_ = genDICompileUnit(module_);
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
      // TODO: Implement this.
      result = DIType();
      break;

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
  return dbgFactory_.CreateBasicTypeEx(
      dbgCompileUnit_,
      type->typeDefn()->qualifiedName().c_str(),
      DICompileUnit(), 0,
      getSizeOfInBits(irType),
      getAlignOfInBits(irType),
      getInt64Val(0), // Offset
      0, // Flags
      typeEncoding(type->typeId()));
}

DIType CodeGenerator::genDIEmbeddedType(const Type * type) {
  DIType di = genDIType(type);
  if (type->typeClass() == Type::Class) {
    di = dbgFactory_.CreateDerivedTypeEx(
        dwarf::DW_TAG_pointer_type,
        dbgCompileUnit_,
        "",
        genDICompileUnit(type->typeDefn()),
        0,
        getSizeOfInBits(type->irEmbeddedType()),
        getAlignOfInBits(type->irEmbeddedType()),
        getInt64Val(0), 0,
        di);
  }

  di.Verify();
  return di;
}

DIType CodeGenerator::genDIParameterType(const Type * type) {
  // TODO: Need to take 'shape' into account.
  DIType di = genDIType(type);
  if (type->typeClass() == Type::Class) {
    di = dbgFactory_.CreateDerivedTypeEx(
        dwarf::DW_TAG_pointer_type,
        dbgCompileUnit_,
        "",
        genDICompileUnit(type->typeDefn()),
        0,
        getSizeOfInBits(type->irParameterType()),
        getAlignOfInBits(type->irParameterType()),
        getInt64Val(0), 0,
        di);
  }

  di.Verify();
  return di;
}

DIDerivedType CodeGenerator::genDITypeBase(const CompositeType * type) {
  return dbgFactory_.CreateDerivedTypeEx(
      dwarf::DW_TAG_inheritance,
      dbgCompileUnit_,
      type->typeDefn()->name(),
      genDICompileUnit(type->typeDefn()),
      getSourceLineNumber(type->typeDefn()->location()),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      getInt64Val(0), 0,
      genDIType(type));
}

DIDerivedType CodeGenerator::genDITypeMember(const VariableDefn * var, Constant * offset) {
  return dbgFactory_.CreateDerivedTypeEx(
      dwarf::DW_TAG_member,
      dbgCompileUnit_,
      var->name(),
      genDICompileUnit(var),
      getSourceLineNumber(var->location()),
      getSizeOfInBits(var->type()->irEmbeddedType()),
      getAlignOfInBits(var->type()->irEmbeddedType()),
      offset, 0,
      genDIEmbeddedType(var->type()));
}

DIDerivedType CodeGenerator::genDITypeMember(const Type * type, const StructType * containingType,
    StringRef name, int index) {
  const llvm::Type * memberType = containingType->getContainedType(0);
  return dbgFactory_.CreateDerivedTypeEx(
      dwarf::DW_TAG_member,
      dbgCompileUnit_,
      name.str().c_str(),
      dbgCompileUnit_,
      0,
      getSizeOfInBits(memberType),
      getAlignOfInBits(memberType),
      getOffsetOfInBits(containingType, index), 0,
      genDIEmbeddedType(type));
}

DICompositeType CodeGenerator::genDICompositeType(const CompositeType * type) {
  DICompositeType placeHolder = dbgFactory_.CreateCompositeTypeEx(
      dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      type->typeDefn()->linkageName().c_str(),
      genDICompileUnit(type->typeDefn()),
      getSourceLineNumber(type->typeDefn()->location()),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      DIArray());

  dbgTypeMap_[type] = placeHolder;

  const DefnList & fields = type->instanceFields();
  DIDescriptorArray members;

  if (type->typeClass() == Type::Class && type->super() != NULL) {
    members.push_back(genDITypeBase(type->super()));
  }

  for (DefnList::const_iterator it = fields.begin(); it != fields.end(); ++it) {
    if (*it) {
      const VariableDefn * var = cast<VariableDefn>(*it);
      Constant * offset = getOffsetOfInBits(
          cast<StructType>(type->irType()), var->memberIndex());
      members.push_back(genDITypeMember(var, offset));
    }
  }

  DICompositeType di = dbgFactory_.CreateCompositeTypeEx(
      dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      type->typeDefn()->linkageName().c_str(),
      genDICompileUnit(type->typeDefn()),
      getSourceLineNumber(type->typeDefn()->location()),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
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
  return dbgFactory_.CreateCompositeTypeEx(
      dwarf::DW_TAG_enumeration_type,
      dbgCompileUnit_,
      type->typeDefn()->name(),
      genDICompileUnit(type->typeDefn()),
      getSourceLineNumber(type->typeDefn()->location()),
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(members.data(), members.size()));
}

DICompositeType CodeGenerator::genDINativeArrayType(const NativeArrayType * type) {
  DIDescriptor subrange = dbgFactory_.GetOrCreateSubrange(0, type->size());
  return dbgFactory_.CreateCompositeTypeEx(
      dwarf::DW_TAG_array_type,
      dbgCompileUnit_,
      "",
      dbgCompileUnit_,
      0,
      getSizeOfInBits(type->irEmbeddedType()),
      getAlignOfInBits(type->irEmbeddedType()),
      getInt64Val(0), 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(&subrange, 1));
}

DIDerivedType CodeGenerator::genDIAddressType(const AddressType * type) {
  return dbgFactory_.CreateDerivedTypeEx(
      dwarf::DW_TAG_pointer_type,
      dbgCompileUnit_,
      "",
      dbgCompileUnit_,
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      getInt64Val(0), 0,
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
      unionMembers.push_back(dbgFactory_.CreateDerivedTypeEx(
          dwarf::DW_TAG_member,
          dbgCompileUnit_,
          name,
          DICompileUnit(),
          0,
          getSizeOfInBits(memberType->irEmbeddedType()),
          getAlignOfInBits(memberType->irEmbeddedType()),
          getInt64Val(0), 0,
          memberDbgType));
    }

    ++memberIndex;
  }

  // Create the non-discriminated union.
  DICompositeType unionType = dbgFactory_.CreateCompositeTypeEx(
      dwarf::DW_TAG_union_type,
      dbgCompileUnit_,
      "",
      DICompileUnit(),
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(unionMembers.data(), unionMembers.size()));

  // If there's a discriminator field
  if (irType->getNumContainedTypes() > 1) {
    const llvm::Type * discType = irType->getContainedType(0);
    DIDescriptorArray structMembers;
    DIType discDbgType = dbgFactory_.CreateBasicTypeEx(
        dbgCompileUnit_,
        "",
        DICompileUnit(), 0,
        getSizeOfInBits(discType),
        getAlignOfInBits(discType),
        getInt64Val(0), // Offset
        0, // Flags
        dwarf::DW_ATE_unsigned);

    structMembers.push_back(dbgFactory_.CreateDerivedTypeEx(
        dwarf::DW_TAG_member,
        dbgCompileUnit_,
        "vindex",
        DICompileUnit(),
        0,
        getSizeOfInBits(discType),
        getAlignOfInBits(discType),
        getOffsetOfInBits(cast<StructType>(type->irType()), 0),
        0,
        discDbgType));

    structMembers.push_back(dbgFactory_.CreateDerivedTypeEx(
        dwarf::DW_TAG_member,
        dbgCompileUnit_,
        ".value",
        DICompileUnit(),
        0,
        getSizeOfInBits(irType->getContainedType(1)),
        getAlignOfInBits(irType->getContainedType(1)),
        getOffsetOfInBits(cast<StructType>(type->irType()), 1),
        0,
        unionType));

    unionType = dbgFactory_.CreateCompositeTypeEx(
        dwarf::DW_TAG_structure_type,
        dbgCompileUnit_,
        "",
        DICompileUnit(),
        0,
        getSizeOfInBits(type->irType()),
        getAlignOfInBits(type->irType()),
        getInt64Val(0), 0,
        DIType(),
        dbgFactory_.GetOrCreateArray(structMembers.data(), structMembers.size()));
  }

  return unionType;
}

DICompositeType CodeGenerator::genDITupleType(const TupleType * type) {
  DIDescriptorArray members;
  int32_t index = 0;
  char memberName[16];
  for (TupleType::const_iterator it = type->begin(); it != type->end(); ++it) {
    const Type * memberType = *it;
    Constant * offset = getOffsetOfInBits(cast<StructType>(type->irType()), index++);
    sprintf(memberName, "_%d", index);
    members.push_back(dbgFactory_.CreateDerivedTypeEx(
        dwarf::DW_TAG_member,
        dbgCompileUnit_,
        memberName,
        DICompileUnit(),
        0,
        getSizeOfInBits(memberType->irEmbeddedType()),
        getAlignOfInBits(memberType->irEmbeddedType()),
        offset, 0,
        genDIEmbeddedType(memberType)));
  }

  std::string tupleName;
  typeLinkageName(tupleName, type);
  return dbgFactory_.CreateCompositeTypeEx(
      dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      tupleName,
      DICompileUnit(),
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      getInt64Val(0), 0,
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
    ptype = dbgFactory_.CreateDerivedTypeEx(
        dwarf::DW_TAG_formal_parameter,
        dbgCompileUnit_,
        "self",
        genDICompileUnit(param),
        getSourceLineNumber(param->location()),
        getSizeOfInBits(llvm::PointerType::get(param->type()->irType(), 0)),
        getInt64Val(0),
        getInt64Val(0), 0,
        ptype);
    args.push_back(ptype);
  }

  const ParameterList & params = type->params();
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    const ParameterDefn * param = *it;
    DIType ptype = genDIParameterType(param->type());
    ptype = dbgFactory_.CreateDerivedTypeEx(
        dwarf::DW_TAG_formal_parameter,
        dbgCompileUnit_,
        param->name() != NULL ? param->name() : "",
        genDICompileUnit(param),
        getSourceLineNumber(param->location()),
        getSizeOfInBits(param->internalType()->irParameterType()),
        getInt64Val(0),
        getInt64Val(0), 0,
        ptype);
    args.push_back(ptype);
  }

  return dbgFactory_.CreateCompositeType(
      dwarf::DW_TAG_subroutine_type,
      dbgCompileUnit_,
      "",
      dbgCompileUnit_,
      0, // Source line
      0, // Size
      0, // Align
      0, // Offset
      0, // Flags
      DIType(),
      dbgFactory_.GetOrCreateArray(args.data(), args.size()));
}

DICompositeType CodeGenerator::genDIBoundMethodType(const BoundMethodType * type) {
  const StructType * stype = cast<StructType>(type->irType());
  const FunctionType * fnType = type->fnType();
  std::string typeName(".fnref.");
  typeLinkageName(typeName, fnType);

  DICompositeType placeHolder = dbgFactory_.CreateCompositeTypeEx(
      dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      typeName.c_str(),
      DICompileUnit(),
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      DIArray());

  dbgTypeMap_[type] = placeHolder;

  //const DefnList & fields = type->instanceFields();
  DIDescriptorArray members;
  members.push_back(genDITypeMember(fnType, stype, "method", 0));
  members.push_back(genDITypeMember(Builtins::typeObject, stype, "self", 1));

  DICompositeType di = dbgFactory_.CreateCompositeTypeEx(
      dwarf::DW_TAG_structure_type,
      dbgCompileUnit_,
      typeName.c_str(),
      DICompileUnit(),
      0,
      getSizeOfInBits(type->irType()),
      getAlignOfInBits(type->irType()),
      getInt64Val(0), 0,
      DIType(),
      dbgFactory_.GetOrCreateArray(members.data(), members.size()));

  dbgTypeMap_[type] = di;
  placeHolder.replaceAllUsesWith(di);
  di.Verify();
  return di;
}

unsigned CodeGenerator::getSourceLineNumber(const SourceLocation & loc) {
  TokenPosition pos = tokenPosition(loc);
  return pos.beginLine;
}

Constant * CodeGenerator::getSizeOfInBits(const llvm::Type * ty) {
  Constant * c = llvm::ConstantExpr::getSizeOf(ty);
  return llvm::ConstantExpr::getMul(c, llvm::ConstantInt::get(c->getType(), 8));
}

Constant * CodeGenerator::getAlignOfInBits(const llvm::Type * ty) {
  Constant * c = llvm::ConstantExpr::getAlignOf(ty);
  return llvm::ConstantExpr::getMul(c, llvm::ConstantInt::get(c->getType(), 8));
}

Constant * CodeGenerator::getOffsetOfInBits(const StructType * st, unsigned fieldIndex) {
  Constant * c = llvm::ConstantExpr::getOffsetOf(st, fieldIndex);
  return llvm::ConstantExpr::getMul(c, llvm::ConstantInt::get(c->getType(), 8));
}

} // namespace tart

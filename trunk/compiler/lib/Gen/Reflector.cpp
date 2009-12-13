/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Gen/StructBuilder.h"

#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"

#include "tart/Objects/Builtins.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "llvm/Module.h"

namespace tart {

using namespace llvm;

/// -------------------------------------------------------------------
/// Contains a lazy reference to an enumeration constant.

// Members of tart.core.reflect.Type.
BuiltinMemberRef<VariableDefn> type_typeKind(Builtins::typeType, "_typeKind");

// Members of tart.core.reflect.SimpleType.
BuiltinMemberRef<VariableDefn> simpleType_subtype(Builtins::typeSimpleType, "_subtype");
BuiltinMemberRef<VariableDefn> simpleType_size(Builtins::typeSimpleType, "_size");

// Members of tart.core.reflect.DerivedType.
BuiltinMemberRef<VariableDefn> derivedType_typeParams(Builtins::typeDerivedType, "_typeParams");

// Members of tart.core.reflect.ComplexType.
BuiltinMemberRef<VariableDefn> complexType_tib(Builtins::typeComplexType, "_typeInfo");
BuiltinMemberRef<VariableDefn> complexType_superType(Builtins::typeComplexType, "_superType");
BuiltinMemberRef<VariableDefn> complexType_interfaces(Builtins::typeComplexType, "_interfaces");
BuiltinMemberRef<VariableDefn> complexType_typeParams(Builtins::typeComplexType, "_typeParams");
BuiltinMemberRef<VariableDefn> complexType_attributes(Builtins::typeComplexType, "_attributes");
BuiltinMemberRef<VariableDefn> complexType_fields(Builtins::typeComplexType, "_fields");
BuiltinMemberRef<VariableDefn> complexType_properties(Builtins::typeComplexType, "_properties");
BuiltinMemberRef<VariableDefn> complexType_constructors(Builtins::typeComplexType, "_constructors");
BuiltinMemberRef<VariableDefn> complexType_methods(Builtins::typeComplexType, "_methods");

// Members of tart.core.reflect.EnumType.
BuiltinMemberRef<VariableDefn> enumType_superType(Builtins::typeEnumType, "_superType");
BuiltinMemberRef<VariableDefn> enumType_values(Builtins::typeEnumType, "_values");

// Members of tart.core.reflect.FunctionType.
BuiltinMemberRef<VariableDefn> functionType_returnType(Builtins::typeFunctionType, "_returnType");
BuiltinMemberRef<VariableDefn> functionType_selfType(Builtins::typeFunctionType, "_selfType");
BuiltinMemberRef<VariableDefn> functionType_paramTypes(Builtins::typeFunctionType, "_paramTypes");
BuiltinMemberRef<VariableDefn> functionType_invoke(Builtins::typeFunctionType, "_invoke");
BuiltinMemberRef<FunctionDefn> functionType_invokeFn(Builtins::typeFunctionType, "invoke");

// Members of tart.core.reflect.TypeRef.
BuiltinMemberRef<VariableDefn> typeRef_type(Builtins::typeTypeRef, "type");
BuiltinMemberRef<VariableDefn> typeRef_modifiers(Builtins::typeTypeRef, "modifiers");
BuiltinMemberRef<TypeDefn> typeRef_enumModifiers(Builtins::typeTypeRef, "Modifiers");

// Members of tart.core.reflect.TypeDescriptor.
BuiltinMemberRef<VariableDefn> typeDescriptor_typeInfo(Builtins::typeTypeDescriptor, "_typeInfo");
BuiltinMemberRef<VariableDefn> typeDescriptor_typeKind(Builtins::typeTypeDescriptor, "_typeKind");

// Members of tart.core.reflect.Member.
BuiltinMemberRef<VariableDefn> member_name(Builtins::typeMember, "_name");
BuiltinMemberRef<VariableDefn> member_kind(Builtins::typeMember, "_kind");
BuiltinMemberRef<VariableDefn> member_access(Builtins::typeMember, "_access");
BuiltinMemberRef<VariableDefn> member_traits(Builtins::typeMember, "_traits");
BuiltinMemberRef<VariableDefn> member_type(Builtins::typeMember, "_type");
BuiltinMemberRef<VariableDefn> member_attributes(Builtins::typeMember, "_attributes");

// Members of tart.core.reflect.Method.
BuiltinMemberRef<VariableDefn> method_typeParams(Builtins::typeMethod, "_typeParams");
//BuiltinMemberRef<VariableDefn> method_functionType(Builtins::typeMethod, "_functionType");
BuiltinMemberRef<VariableDefn> method_params(Builtins::typeMethod, "_params");
BuiltinMemberRef<VariableDefn> method_methodPointer(Builtins::typeMethod, "_methodPointer");
BuiltinMemberRef<FunctionDefn> method_checkArgs(Builtins::typeMethod, "checkArgCount");

// Members of tart.core.reflect.Module.
BuiltinMemberRef<VariableDefn> module_name(Builtins::typeModule, "_name");
BuiltinMemberRef<VariableDefn> module_types(Builtins::typeModule, "_types");
BuiltinMemberRef<VariableDefn> module_methods(Builtins::typeModule, "_methods");

Reflector::Reflector(CodeGenerator & cg)
    : cg_(cg)
    , context_(cg.context())
    , builder_(cg.builder())
    , irModule_(cg.irModule())
    , moduleTable_(NULL)
    , syntheticIndex_(0)
{
}

GlobalVariable * Reflector::getModulePtr(Module * module) {
  std::string moduleSymbol(".module." + module->linkageName());
  GlobalVarMap::iterator it = globals_.find(moduleSymbol);
  if (it != globals_.end()) {
    return it->second;
  }

  irModule_->addTypeName("tart.reflect.Module", Builtins::typeModule->irType());
  GlobalVariable * rfModule = new GlobalVariable(*irModule_, Builtins::typeModule->irType(), true,
      GlobalValue::ExternalLinkage, NULL, moduleSymbol);
  globals_[moduleSymbol] = rfModule;
  return rfModule;
}

llvm::Constant * Reflector::internSymbol(const llvm::StringRef &Key) {
  return cg_.genStringLiteral(Key, Key);
}

void Reflector::emitModule(Module * module) {
  if (Builtins::typeModule != NULL) {
    GlobalVariable * modulePtr = getModulePtr(module);
    if (!modulePtr->hasInitializer()) {
      ReflectedMembers rfMembers;

      // First visit members which are explicitly declared in this module.
      for (Defn * member = module->firstMember(); member != NULL; member = member->nextInScope()) {
        visitMember(rfMembers, member);
      }

      StructBuilder sb(cg_);
      sb.createObjectHeader(Builtins::typeModule);
      sb.addField(internSymbol(module->qualifiedName()));
      sb.addField(emitArray("tart.reflect.Module.", module_types.get(), rfMembers.types));
      sb.addField(emitArray("tart.reflect.Module.", module_methods.get(), rfMembers.methods));
      modulePtr->setInitializer(sb.build());
    }

    // If this module is the "Type" module, then also do the built-in types.
    if (module == Builtins::typeType->typeDefn()->module()) {
      ReflectedMembers rfBuiltins;
      visitMember(rfBuiltins, &VoidType::typedefn);
      visitMember(rfBuiltins, &BoolType::typedefn);
      visitMember(rfBuiltins, &CharType::typedefn);
      visitMember(rfBuiltins, &ByteType::typedefn);
      visitMember(rfBuiltins, &ShortType::typedefn);
      visitMember(rfBuiltins, &IntType::typedefn);
      visitMember(rfBuiltins, &LongType::typedefn);
      visitMember(rfBuiltins, &UByteType::typedefn);
      visitMember(rfBuiltins, &UShortType::typedefn);
      visitMember(rfBuiltins, &UIntType::typedefn);
      visitMember(rfBuiltins, &ULongType::typedefn);
      visitMember(rfBuiltins, &FloatType::typedefn);
      visitMember(rfBuiltins, &DoubleType::typedefn);
    }

    // Reflect any template instantiations that were generated from this module.
    ReflectedMembers rfSynthetics;
    while (syntheticIndex_ < synthetics_.size()) {
      Defn * member = synthetics_[syntheticIndex_++];
      visitMember(rfSynthetics, member);
    }
  }
}

GlobalVariable * Reflector::getTypePtr(const Type * type) {
  // Generate a unique string which identifies this type.
  std::string typeString(".type.");
  typeLinkageName(typeString, type);
  GlobalVarMap::iterator it = globals_.find(typeString);
  if (it != globals_.end()) {
    return it->second;
  }

  GlobalVariable * rfType;
  if (type->typeDefn() != NULL) {
    // Types which have unique names
    TypeDefn * td = type->typeDefn();
    GlobalValue::LinkageTypes linkageType = GlobalValue::ExternalLinkage;
    if (td->isSynthetic() && cg_.module()->exportDefs().count(td) > 0) {
      linkageType = GlobalValue::LinkOnceODRLinkage;
      synthetics_.insert(td);
    }

    rfType = new GlobalVariable(*irModule_, reflectedTypeOf(type), true,
        linkageType, NULL, typeString);
  } else {
    llvm::Constant * typeValue = emitType(type);
    rfType = new GlobalVariable(*irModule_, typeValue->getType(), true,
        GlobalValue::LinkOnceODRLinkage, typeValue, typeString);
  }

  globals_[typeString] = rfType;
  return rfType;
}

bool Reflector::visitMembers(ReflectedMembers & rm, const IterableScope * scope) {
  for (const Defn * member = scope->firstMember(); member != NULL; member = member->nextInScope()) {
    if (!visitMember(rm, member)) {
      return false;
    }
  }

  return true;
}

bool Reflector::visitMember(ReflectedMembers & rm, const Defn * member) {
  switch (member->defnType()) {
    case Defn::Typedef: {
      const TypeDefn * td = static_cast<const TypeDefn *>(member);
      GlobalVariable * rfType = emitTypeDefn(td);
      if (rfType != NULL) {
        rm.types.push_back(
            llvm::ConstantExpr::getPointerCast(rfType, Builtins::typeType->irEmbeddedType()));
      }

      break;
    }

    case Defn::Namespace:
      break;

    case Defn::Var:
    case Defn::Let:
      break;

    case Defn::Property:
      break;

    case Defn::Indexer:
      break;

    case Defn::Function: {
      const FunctionDefn * fn = static_cast<const FunctionDefn *>(member);
      if (!fn->isIntrinsic() && fn->isSingular()) {
        Constant * method = emitMethod(fn);
        if (method != NULL) {
          GlobalVariable * rMethod = new GlobalVariable(*irModule_, method->getType(), true,
              GlobalValue::InternalLinkage, method, ".method." + fn->qualifiedName());
          rm.methods.push_back(rMethod);
        }
      }
      break;
    }

    case Defn::Macro:
    case Defn::Parameter:
    case Defn::Mod:
    case Defn::ExplicitImport:
      break;
  }

  return true;
}

llvm::GlobalVariable * Reflector::emitTypeDefn(const TypeDefn * td) {
  if (td->typeValue() != NULL && td->isSingular()) {
    const Type * type = td->typeValue();
    GlobalVariable * rfType = getTypePtr(type);
    if (rfType != NULL && !rfType->hasInitializer()) {
      rfType->setInitializer(emitType(type));
    }

    return rfType;
  }

  return NULL;
}

llvm::Constant * Reflector::emitArray(
    const std::string & baseName, const VariableDefn * var, const ConstantList & values)
{
  const CompositeType * arrayType = cast<CompositeType>(var->type());
  const Type * elementType = arrayType->typeParam(0);
  irModule_->addTypeName(arrayType->typeDefn()->linkageName(), arrayType->irType());
  DASSERT_OBJ(arrayType->passes().isFinished(CompositeType::FieldTypePass), var);

  if (values.empty()) {
    // TODO: point to shared empty array.
  }

  StructBuilder sb(cg_);
  sb.createObjectHeader(arrayType);
  sb.addField(cg_.getInt32Val(values.size()));
  sb.addArrayField(elementType, values);

  llvm::Constant * arrayStruct = sb.build();
  GlobalVariable * array = new GlobalVariable(*irModule_,
      arrayStruct->getType(), true, GlobalValue::InternalLinkage, arrayStruct,
      ".data." + baseName + var->name());
  return llvm::ConstantExpr::getPointerCast(array, arrayType->irEmbeddedType());
}

llvm::Constant * Reflector::emitMethod(const FunctionDefn * func) {
  StructBuilder sb(cg_);
  sb.addField(emitMember(cast<CompositeType>(Builtins::typeMethod), func));
  sb.addNullField(method_typeParams.type());
  //sb.addField(emitFunctionType(func->functionType()));
  sb.addNullField(method_params.type());
  sb.addNullField(method_methodPointer.type());
  return sb.build(Builtins::typeMethod->irType());
}

llvm::Constant * Reflector::emitMember(const CompositeType * structType, const ValueDefn * def) {
  TypeDefn * parent = def->enclosingClassDefn();
  Module * module = def->module();
  StructBuilder sb(cg_);
  sb.createObjectHeader(structType);
  sb.addField(internSymbol(def->name()));
  sb.addField(internSymbol(def->qualifiedName()));
  sb.addIntegerField(member_kind, memberKind(def));
  sb.addIntegerField(member_access, memberAccess(def));
  sb.addIntegerField(member_traits, memberTraits(def));
  sb.addField(emitTypeReference(def->type()));
  sb.addField(emitArray(
      module->qualifiedName(),
      member_attributes.get(),
      ConstantList()));
  return sb.build(Builtins::typeMember->irType());
}

llvm::Constant * Reflector::emitTypeReference(const TypeRef & type) {
  return llvm::ConstantExpr::getPointerCast(
      getTypePtr(type.type()), Builtins::typeType->irEmbeddedType());
}

const llvm::Type * Reflector::reflectedTypeOf(const Type * type) {
  switch (type->typeClass()) {
    case Type::Primitive:
      return Builtins::typeSimpleType->irType();

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
      if (type->typeDefn()->hasTrait(Defn::Nonreflective)) {
        return Builtins::typeSimpleType->irType();
      }

      return Builtins::typeComplexType->irType();

    case Type::Enum:
      return Builtins::typeEnumType->irType();

    case Type::Function:
      return Builtins::typeFunctionType->irType();

    case Type::Tuple:
    case Type::Union:
    case Type::NAddress:
    case Type::NPointer:
    case Type::NArray:
      return Builtins::typeDerivedType->irType();

    default:
      DFAIL("Invalid type");
  }
}

llvm::Constant * Reflector::emitType(const Type * type) {
  switch (type->typeClass()) {
    case Type::Primitive:
      return emitSimpleType(Builtins::typeSimpleType, static_cast<const PrimitiveType *>(type));

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
      if (type->typeDefn()->hasTrait(Defn::Nonreflective)) {
        return emitOpaqueType(type);
      } else {
        return emitCompositeType(static_cast<const CompositeType *>(type));
      }

    case Type::Enum:
      return emitEnumType(static_cast<const EnumType *>(type));

    case Type::Function:
      return emitFunctionType(static_cast<const FunctionType *>(type));

    case Type::Tuple:
    case Type::Union:
    case Type::NAddress:
    case Type::NPointer:
    case Type::NArray:
      return emitDerivedType(type);

    default:
      DFAIL("Invalid type");
  }
}

llvm::Constant * Reflector::emitCompositeType(const CompositeType * type) {
  // Don't reflect non-retained attributes.
  if (type->isAttribute() && !type->attributeInfo().isRetained()) {
    return NULL;
  }

  //ReflectedMembers rfMembers;
  //visitMembers(rfMembers, type->memberScope());

  StructBuilder sb(cg_);
  sb.addField(emitSimpleType(Builtins::typeComplexType, type));
  sb.addNullField(complexType_tib.type());
  sb.addNullField(complexType_superType.type());
  sb.addNullField(complexType_interfaces.type());
  sb.addNullField(complexType_typeParams.type());
  sb.addNullField(complexType_attributes.type());
  sb.addNullField(complexType_fields.type());
  sb.addNullField(complexType_properties.type());
  sb.addNullField(complexType_constructors.type());
  sb.addNullField(complexType_methods.type());
  return sb.build(Builtins::typeComplexType->irType());
}

llvm::Constant * Reflector::emitEnumType(const EnumType * type) {
  StructBuilder sb(cg_);
  sb.addField(emitSimpleType(Builtins::typeEnumType, type));
  sb.addNullField(enumType_superType.type());
  sb.addNullField(enumType_values.type());
  return sb.build(Builtins::typeEnumType->irType());
}

llvm::Constant * Reflector::emitFunctionType(const FunctionType * type) {
  // TODO: Merge with same type. (unless its already doing that...)
  StructBuilder sb(cg_);
  sb.addField(emitTypeBase(Builtins::typeFunctionType, FUNCTION));
  sb.addField(emitTypeReference(type->returnType()));
  if (type->selfParam() != NULL) {
    sb.addField(getTypePtr(type->selfParam()->type()));
  } else {
    sb.addNullField(functionType_selfType.type());
  }
  sb.addField(emitTupleType(type->paramTypes()));

  if (type->selfParam() != NULL) {
    const Type * selfType = type->selfParam()->type();
    if (selfType->typeClass() == Type::Class || selfType->typeClass() == Type::Interface) {
      // For now, we only support reflection of classes.
      sb.addNullField(functionType_invoke.type());
      //sb.addField(cg_.genInvokeFn(type));
    } else {
      sb.addNullField(functionType_invoke.type());
    }
  } else {
    //sb.addField(cg_.genInvokeFn(type));
    sb.addNullField(functionType_invoke.type());
  }

  return sb.build(Builtins::typeFunctionType->irType());
}

llvm::Constant * Reflector::emitDerivedType(const Type * type) {
  ConstantList typeParams;
  for (size_t i = 0; i < type->numTypeParams(); ++i) {
    typeParams.push_back(emitTypeReference(type->typeParam(i)));
  }

  TypeKind kind;
  switch (type->typeClass()) {
    case Type::Union: kind = UNION; break;
    case Type::NAddress: kind = ADDRESS; break;
    case Type::NPointer: kind = POINTER; break;
    case Type::NArray: kind = NATIVE_ARRAY; break;
    case Type::Tuple: kind = TUPLE; break;
    default:
      DFAIL("Invalid subtype");
  }

  StructBuilder sb(cg_);
  sb.addField(emitTypeBase(Builtins::typeDerivedType, UNION));
  sb.addField(emitArray("tart.reflect.DerivedType.", derivedType_typeParams.get(), typeParams));
  return sb.build(Builtins::typeDerivedType->irType());
}

llvm::Constant * Reflector::emitOpaqueType(const Type * type) {
  StructBuilder sb(cg_);
  DASSERT_OBJ(type->typeDefn() != NULL, type);
  sb.addField(emitTypeBase(Builtins::typeSimpleType, OPAQUE));
  sb.addIntegerField(type_typeKind.get(), NONE);
  sb.addField(internSymbol(type->typeDefn()->qualifiedName()));
  sb.addField(llvm::ConstantExpr::getTrunc(
      llvm::ConstantExpr::getSizeOf(type->irType()), builder_.getInt32Ty()));
  return sb.build(Builtins::typeSimpleType->irType());
}

llvm::Constant * Reflector::emitSimpleType(const Type * reflectType, const Type * type) {
  SubtypeId subtype = NONE;
  TypeKind kind;
  switch (type->typeClass()) {
    case Type::Primitive: {
      kind = PRIMITIVE;
      const PrimitiveType * ptype = static_cast<const PrimitiveType *>(type);
      switch  (ptype->typeId()) {
        case TypeId_Void: subtype = VOID; break;
        case TypeId_Bool: subtype = BOOL; break;
        case TypeId_Char: subtype = CHAR; break;
        case TypeId_SInt8: subtype = BYTE; break;
        case TypeId_SInt16: subtype = SHORT; break;
        case TypeId_SInt32: subtype = INT; break;
        case TypeId_SInt64: subtype = LONG; break;
        case TypeId_SIntPtr: subtype = INTPTR; break;
        case TypeId_UInt8: subtype = UBYTE; break;
        case TypeId_UInt16: subtype = USHORT; break;
        case TypeId_UInt32: subtype = UINT; break;
        case TypeId_UInt64: subtype = ULONG; break;
        case TypeId_UIntPtr: subtype = UINTPTR; break;
        case TypeId_Float: subtype = FLOAT; break;
        case TypeId_Double: subtype = DOUBLE; break;
        //case TypeId_LongDouble: subtype = VOID; break;
        default:
          DFAIL("Invalid subtype");
      }

      break;
    }

    case Type::Class: kind = CLASS; break;
    case Type::Struct: kind = STRUCT; break;
    case Type::Interface: kind = INTERFACE; break;
    case Type::Protocol: kind = PROTOCOL; break;
    case Type::Enum: kind = ENUM; break;

    default:
      DFAIL("Invalid type");
  }

  StructBuilder sb(cg_);
  DASSERT_OBJ(type->typeDefn() != NULL, type);
  sb.addField(emitTypeBase(reflectType, kind));
  sb.addIntegerField(type_typeKind.get(), subtype);
  sb.addField(internSymbol(type->typeDefn()->qualifiedName()));
  sb.addField(llvm::ConstantExpr::getTrunc(
      llvm::ConstantExpr::getSizeOf(type->irType()), builder_.getInt32Ty()));
  return sb.build(Builtins::typeSimpleType->irType());
}

llvm::Constant * Reflector::emitTypeBase(const Type * reflectType, TypeKind kind) {
  StructBuilder sb(cg_);
  sb.createObjectHeader(reflectType);
  sb.addIntegerField(type_typeKind.get(), kind);
  return sb.build(Builtins::typeType->irType());
}

llvm::Constant * Reflector::emitTupleType(const TupleType * types) {
  // Get cached version if already generated.
  std::string typeTupleName(".TupleType(");
  typeLinkageName(typeTupleName, types);
  typeTupleName.append(")");
  GlobalVarMap::iterator it = globals_.find(typeTupleName);
  if (it != globals_.end()) {
    return it->second;
  }

  // Generate the list of values.
  ConstantList values;
  for (TupleType::const_iterator it = types->begin(); it != types->end(); ++it) {
    values.push_back(emitTypeReference(*it));
  }

  const CompositeType * arrayType = cast<CompositeType>(derivedType_typeParams->type());
  const Type * elementType = arrayType->typeParam(0);
  DASSERT_OBJ(arrayType->passes().isFinished(CompositeType::FieldTypePass), derivedType_typeParams);

  if (values.empty()) {
    // TODO: point to shared empty array.
  }

  StructBuilder sb(cg_);
  sb.createObjectHeader(arrayType);
  sb.addField(cg_.getInt32Val(values.size()));
  sb.addArrayField(elementType, values);

  llvm::Constant * arrayStruct = sb.build();
  GlobalVariable * array = new GlobalVariable(*irModule_, arrayStruct->getType(),
      true, GlobalValue::LinkOnceODRLinkage, arrayStruct, typeTupleName);
  return llvm::ConstantExpr::getPointerCast(array, arrayType->irEmbeddedType());
}

Reflector::Access Reflector::memberAccess(const Defn * member) {
  switch (member->visibility()) {
    case Public: return PUBLIC;
    case Protected: return PROTECTED;
    case Private: return PRIVATE;
    default:
      DFAIL("Illegal state");
  }
}

Reflector::MemberKind Reflector::memberKind(const Defn * member) {
  switch (member->defnType()) {
    case Defn::Let:
    case Defn::Var:
      return FIELD;
      break;

    case Defn::Property:
      return PROPERTY;
      break;

    case Defn::Function:
      return static_cast<const FunctionDefn *>(member)->isCtor() ? CONSTRUCTOR : METHOD;
      break;

    default:
      DFAIL("Invalid member defn");
  }
}

Reflector::Traits Reflector::memberTraits(const Defn * member) {
  int memberTraits = 0;
  if (member->isFinal()) {
    memberTraits |= FINAL;
  }

  if (member->isAbstract()) {
    memberTraits |= ABSTRACT;
  }

  if (member->storageClass() == Storage_Static || member->storageClass() == Storage_Global) {
    memberTraits |= STATIC;
  }

  return Traits(memberTraits);
}

} // namespace tart

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

void Reflector::emitModule(Module * module) {
  if (Builtins::typeModule != NULL) {
    GlobalVariable * modulePtr = getModulePtr(module);
    if (!modulePtr->hasInitializer()) {
      ReflectedMembers rfMembers;
      for (DefnSet::const_iterator it = module->exportDefs().begin();
          it != module->exportDefs().end(); ++it) {
        visitMember(rfMembers, *it);
      }

      StructBuilder sb(cg_);
      sb.createObjectHeader(Builtins::typeModule);
      sb.addStringField(module->qualifiedName());
      sb.addField(emitArray("tart.reflect.Module.", module_types.get(), rfMembers.types));
      sb.addField(emitArray("tart.reflect.Module.", module_methods.get(), rfMembers.methods));
      modulePtr->setInitializer(sb.build());
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
      if (td->typeValue() != NULL && td->isSingular()) {
        const Type * type = td->typeValue();
        GlobalVariable * rfType = getTypePtr(type);
        if (rfType != NULL) {
          if (!rfType->hasInitializer()) {
            rfType->setInitializer(emitType(type));
          }

          // We don't count synthetic types as being 'exported' by the module.
          if (!td->isSynthetic()) {
            rm.types.push_back(
                llvm::ConstantExpr::getPointerCast(rfType, Builtins::typeType->irEmbeddedType()));
          }
        }
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
          //rm.methods.push_back(method);
        }
      }
      break;
    }

    case Defn::Macro:
    case Defn::Parameter:
    case Defn::TemplateParam:
    case Defn::Mod:
    case Defn::ExplicitImport:
      break;
  }

  return true;
}

llvm::Constant * Reflector::emitArray(
    const std::string & baseName, const VariableDefn * var, const ConstantList & values)
{
  const CompositeType * arrayType = cast<CompositeType>(var->type().type());
  const Type * elementType = arrayType->typeParam(0);
  irModule_->addTypeName(arrayType->typeDefn()->linkageName(), arrayType->irType());
  DASSERT_OBJ(arrayType->typeDefn()->isPassFinished(Pass_ResolveOverloads), var);

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
  return sb.build();
}

llvm::Constant * Reflector::emitMember(const CompositeType * structType, const ValueDefn * def) {
  TypeDefn * parent = def->enclosingClassDefn();
  Module * module = def->module();

  StructBuilder sb(cg_);
  sb.createObjectHeader(structType);
  sb.addStringField(def->qualifiedName());
  sb.addField(emitTypeReference(def->type()));
  sb.addIntegerField(member_kind, memberKind(def));
  sb.addIntegerField(member_access, memberAccess(def));
  sb.addIntegerField(member_traits, memberTraits(def));
  sb.addField(emitArray(
      module->qualifiedName(),
      member_attributes.get(),
      ConstantList()));

  return sb.build();
}

llvm::Constant * Reflector::emitTypeReference(const TypeRef & type) {
  StructBuilder sb(cg_);
  sb.addField(getTypePtr(type.type()));
  sb.addIntegerField(typeRef_modifiers, 0);
  return sb.build();
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
    case Type::Address:
    case Type::NativePointer:
    case Type::NativeArray:
      return Builtins::typeDerivedType->irType();

    default:
      DFAIL("Invalid type");
  }
}

llvm::Constant * Reflector::emitType(const Type * type) {
  switch (type->typeClass()) {
    case Type::Primitive:
      return emitPrimitiveType(static_cast<const PrimitiveType *>(type));

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
    case Type::Address:
    case Type::NativePointer:
    case Type::NativeArray:
      return emitDerivedType(type);

    default:
      DFAIL("Invalid type");
  }
}

llvm::Constant * Reflector::emitPrimitiveType(const PrimitiveType * type) {
  DFAIL("Implement");
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
  llvm::Constant * result = sb.build();
  DASSERT(result->getType() == Builtins::typeComplexType->irType());
  return result;
}

llvm::Constant * Reflector::emitEnumType(const EnumType * type) {
  StructBuilder sb(cg_);
  sb.addField(emitSimpleType(Builtins::typeEnumType, type));
  sb.addNullField(enumType_superType.type());
  sb.addNullField(enumType_values.type());
  llvm::Constant * result = sb.build();
  DASSERT(result->getType() == Builtins::typeEnumType->irType());
  return result;
}

llvm::Constant * Reflector::emitFunctionType(const FunctionType * type) {
  StructBuilder sb(cg_);
  sb.addField(emitTypeBase(Builtins::typeFunctionType, type));
  llvm::Constant * result = sb.build();
  DASSERT(result->getType() == Builtins::typeFunctionType->irType());
  return result;
}

llvm::Constant * Reflector::emitDerivedType(const Type * type) {
  DFAIL("Implement");
}

llvm::Constant * Reflector::emitOpaqueType(const Type * type) {
  StructBuilder sb(cg_);
  DASSERT_OBJ(type->typeDefn() != NULL, type);
  sb.addField(emitTypeBase(Builtins::typeSimpleType, type));
  sb.addIntegerField(type_typeKind.get(), NONE);
  sb.addStringField(type->typeDefn()->qualifiedName());
  sb.addField(llvm::ConstantExpr::getTrunc(
      llvm::ConstantExpr::getSizeOf(type->irType()), builder_.getInt32Ty()));
  llvm::Constant * result = sb.build();
  DASSERT(result->getType() == Builtins::typeSimpleType->irType());
  return result;
}

llvm::Constant * Reflector::emitSimpleType(const Type * reflectType, const Type * type) {
  SubtypeId subtype;
  switch (type->typeClass()) {
    case Type::Primitive: {
      break;
    }

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
    case Type::Enum:
      subtype = NONE;
      break;


    case Type::Function:
    case Type::Tuple:
    case Type::Union:
    case Type::Address:
    case Type::NativePointer:
    case Type::NativeArray:
    default:
      DFAIL("Invalid type");
  }

  StructBuilder sb(cg_);
  DASSERT_OBJ(type->typeDefn() != NULL, type);
  sb.addField(emitTypeBase(reflectType, type));
  sb.addIntegerField(type_typeKind.get(), subtype);
  sb.addStringField(type->typeDefn()->qualifiedName());
  sb.addField(llvm::ConstantExpr::getTrunc(
      llvm::ConstantExpr::getSizeOf(type->irType()), builder_.getInt32Ty()));
  llvm::Constant * result = sb.build();
  DASSERT(result->getType() == Builtins::typeSimpleType->irType());
  return result;
}

llvm::Constant * Reflector::emitTypeBase(const Type * reflectType, const Type * type) {
  TypeKind kind;
  switch (type->typeClass()) {
    case Type::Primitive: kind = PRIMITIVE; break;
    case Type::Class: kind = CLASS; break;
    case Type::Struct: kind = STRUCT; break;
    case Type::Interface: kind = INTERFACE; break;
    case Type::Protocol: kind = PROTOCOL; break;
    case Type::Enum: kind = ENUM; break;
    case Type::Function: kind = FUNCTION; break;
    case Type::Tuple: kind = TUPLE; break;
    case Type::Union: kind = UNION; break;
    case Type::Address: kind = ADDRESS; break;
    case Type::NativePointer: kind = POINTER; break;
    case Type::NativeArray: kind = NATIVE_ARRAY; break;

    default:
      DFAIL("Invalid type");
  }

  if (type->typeDefn() != NULL && type->typeDefn()->hasTrait(Defn::Nonreflective)) {
    kind = OPAQUE;
  }

  StructBuilder sb(cg_);
  sb.createObjectHeader(reflectType);
  sb.addIntegerField(type_typeKind.get(), kind);
  llvm::Constant * result = sb.build();
  DASSERT(result->getType() == Builtins::typeType->irType());
  return result;
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
      return member->isCtor() ? CONSTRUCTOR : METHOD;
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

#if 0
class EnumConstantRef {
public:
  EnumConstantRef(BuiltinMemberRef<TypeDefn> & enumType, const char * name)
    : enumType_(enumType)
    , name_(name)
    , value_(NULL)
    {}

  ConstantInteger * get() const {
    if (value_ == NULL) {
      TypeDefn * tdef = enumType_.get();
      EnumType * etype = cast<EnumType>(tdef->typeValue());
      VariableDefn * letDef = Builtins::getMember<VariableDefn>(etype, name_);
      value_ = cast<ConstantInteger>(letDef->initValue());
    }

    return value_;
  }

  ConstantInteger * operator->() const {
    return get();
  }

  operator ConstantInteger *() const {
    return get();
  }

private:
  BuiltinMemberRef<TypeDefn> & enumType_;
  const char * name_;
  mutable ConstantInteger * value_;
};
#endif

//EnumConstantRef typeRef_enumModifiers_CONST(typeRef_enumModifiers, "CONST");
//EnumConstantRef typeRef_enumModifiers_VOLATILE(typeRef_enumModifiers, "VOLATILE");
//EnumConstantRef member_enumAccess_PUBLIC(member_enumAccess, "PUBLIC");
//EnumConstantRef member_enumAccess_PROTECTED(member_enumAccess, "PROTECTED");
//EnumConstantRef member_enumAccess_PRIVATE(member_enumAccess, "PRIVATE");
//EnumConstantRef member_enumKind_FIELD(member_enumMemberKind, "FIELD");
//EnumConstantRef member_enumKind_PROPERTY(member_enumMemberKind, "PROPERTY");
//EnumConstantRef member_enumKind_METHOD(member_enumMemberKind, "METHOD");
//EnumConstantRef member_enumKind_CONSTRUCTOR(member_enumMemberKind, "CONSTRUCTOR");
//EnumConstantRef member_enumTraits_FINAL(member_enumTraits, "FINAL");
//EnumConstantRef member_enumTraits_ABSTRACT(member_enumTraits, "ABSTRACT");
//EnumConstantRef member_enumTraits_STATIC(member_enumTraits, "STATIC");
//EnumConstantRef member_enumTraits_UNSAFE(member_enumTraits, "UNSAFE");
//BuiltinMemberRef<TypeDefn> member_enumAccess(Builtins::typeMember, "Access");
//BuiltinMemberRef<TypeDefn> member_enumMemberKind(Builtins::typeMember, "MemberKind");
//BuiltinMemberRef<TypeDefn> member_enumTraits(Builtins::typeMember, "Traits");
//BuiltinMemberRef<TypeDefn> type_enumTypeKind(Builtins::typeTypeRef, "TypeKind");

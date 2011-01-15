/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Gen/StructBuilder.h"

#include "tart/Meta/NameTable.h"
#include "tart/Meta/Tags.h"
#include "tart/Meta/VarInt.h"

#include "tart/CFG/Module.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/NamespaceDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/PropertyDefn.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/TypeLiteral.h"
#include "tart/CFG/TypeOrdering.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/UnitType.h"

#include "tart/Objects/Builtins.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"

#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringExtras.h"

#define DEBUG_VERBOSE 0

namespace tart {

using namespace llvm;

/// -------------------------------------------------------------------

// Members of tart.reflect.Module.
SystemClassMember<VariableDefn> module_names(Builtins::typeModule, "_names");
SystemClassMember<VariableDefn> module_nameIndex(Builtins::typeModule, "_nameIndex");
SystemClassMember<VariableDefn> module_strmData(Builtins::typeModule, "_strmData");
SystemClassMember<VariableDefn> module_typeRefs(Builtins::typeModule, "_typeRefs");
SystemClassMember<VariableDefn> module_globalRefs(Builtins::typeModule, "_globalRefs");
SystemClassMember<VariableDefn> module_methodRefs(Builtins::typeModule, "_methodRefs");
SystemClassMember<VariableDefn> module_memberTypes(Builtins::typeModule, "_memberTypes");
SystemClassMember<VariableDefn> module_methods(Builtins::typeModule, "_methods");

// Members of tart.reflect.CompositeType
SystemClassMember<VariableDefn> type_typeKind(Builtins::typeType, "_typeKind");

// Members of tart.reflect.CompositeType
SystemClassMember<VariableDefn> compositeType_names(
    Builtins::typeCompositeType, "_names");
SystemClassMember<VariableDefn> compositeType_nameIndex(
    Builtins::typeCompositeType, "_nameIndex");
SystemClassMember<VariableDefn> compositeType_size(
    Builtins::typeCompositeType, "_size");
SystemClassMember<VariableDefn> compositeType_typeInfo(
    Builtins::typeCompositeType, "_typeInfo");
SystemClassMember<VariableDefn> compositeType_strmData(
    Builtins::typeCompositeType, "_strmData");
SystemClassMember<VariableDefn> compositeType_typeRefs(
    Builtins::typeCompositeType, "_typeRefs");
SystemClassMember<VariableDefn> compositeType_globalRefs(
    Builtins::typeCompositeType, "_globalRefs");
SystemClassMember<VariableDefn> compositeType_methodRefs(
    Builtins::typeCompositeType, "_methodRefs");
SystemClassMember<VariableDefn> compositeType_supertype(
    Builtins::typeCompositeType, "_supertype");
SystemClassMember<VariableDefn> compositeType_interfaces(
    Builtins::typeCompositeType, "_interfaces");
SystemClassMember<VariableDefn> compositeType_typeParams(
    Builtins::typeCompositeType, "_typeParams");
SystemClassMember<VariableDefn> compositeType_attributes(
    Builtins::typeCompositeType, "_attributes");
SystemClassMember<VariableDefn> compositeType_fields(
    Builtins::typeCompositeType, "_fields");
SystemClassMember<VariableDefn> compositeType_properties(
    Builtins::typeCompositeType, "_properties");
SystemClassMember<VariableDefn> compositeType_constructors(
    Builtins::typeCompositeType, "_constructors");
SystemClassMember<VariableDefn> compositeType_methods(
    Builtins::typeCompositeType, "_methods");
SystemClassMember<VariableDefn> compositeType_memberTypes(
    Builtins::typeCompositeType, "_memberTypes");
SystemClassMember<VariableDefn> compositeType_alloc(
    Builtins::typeCompositeType, "_alloc");
SystemClassMember<VariableDefn> compositeType_noArgCtor(
    Builtins::typeCompositeType, "_noArgCtor");
SystemClassMember<VariableDefn> compositeType_methodBaseIndex(
    Builtins::typeCompositeType, "_methodBaseIndex");

// Members of tart.reflect.EnumType
SystemClassMember<VariableDefn> enumType_names(Builtins::typeEnumType, "_names");
SystemClassMember<VariableDefn> enumType_nameIndex(Builtins::typeEnumType, "_nameIndex");
SystemClassMember<VariableDefn> enumType_supertype(Builtins::typeEnumType, "_supertype");
SystemClassMember<VariableDefn> enumType_values(Builtins::typeEnumType, "_values");

// Members of tart.reflect.DerivedType
SystemClassMember<VariableDefn> derivedType_typeParams(Builtins::typeDerivedType, "_typeParams");

// Members of tart.reflect.NameTable.
SystemClassMember<VariableDefn> nameTable_nameStrmSimple(
    Builtins::typeNameTable, "_nameStrmSimple");
SystemClassMember<VariableDefn> nameTable_simpleNames(Builtins::typeNameTable, "_simpleNames");
SystemClassMember<VariableDefn> nameTable_compoundNames(Builtins::typeNameTable, "_compoundNames");
SystemClassMember<VariableDefn> nameTable_compoundNameStrings(
    Builtins::typeNameTable, "_compoundNameStrings");

// Members of tart.reflect.FunctionType
SystemClassMember<TypeDefn> functionType_CallAdapterFnType(
    Builtins::typeFunctionType, "CallAdapterFn");
SystemClassMember<VariableDefn> functionType_returnType(Builtins::typeFunctionType, "_returnType");
SystemClassMember<VariableDefn> functionType_selfType(Builtins::typeFunctionType, "_selfType");
SystemClassMember<VariableDefn> functionType_paramTypes(Builtins::typeFunctionType, "_paramTypes");
SystemClassMember<VariableDefn> functionType_invoke(Builtins::typeFunctionType, "_invoke");

// Members of tart.reflect.StaticTypeList
SystemClassMember<VariableDefn> staticTypeList_size(Builtins::typeStaticTypeList, "_size");

// Members of tart.reflect.Package.
SystemClassMember<VariableDefn> package_name(Builtins::typePackage, "_name");
SystemClassMember<VariableDefn> package_modules(Builtins::typePackage, "_modules");
SystemClassMember<VariableDefn> package_subpackages(Builtins::typePackage, "_subpackages");

namespace {

/// -------------------------------------------------------------------
/// Comparator for names by use count.

struct DefnOrder {
  bool operator()(const Defn * d0, const Defn * d1) {
    return StringRef(d0->name()).compare(d1->name()) < 0;
  }
};

}

struct TypeOrder {
  bool operator()(
      const ReflectionMetadata::TypeArrayElement & t0,
      const ReflectionMetadata::TypeArrayElement & t1) {
    if (t0.second.useCount > t1.second.useCount) return true;
    if (t0.second.useCount < t1.second.useCount) return false;
    return LexicalTypeOrdering::compare(t0.first, t1.first) > 0;
  }
};

/// -------------------------------------------------------------------
/// Reflector

Reflector::Reflector(CodeGenerator & cg)
  : cg_(cg)
  , context_(cg.context())
  , builder_(cg.builder())
  , irModule_(cg.irModule())
  , nameTableVar_(NULL)
  , outputPhase_(false)
{
}

Reflector::~Reflector() {
  // TODO: Delete reflection metadata pointers.
}

llvm::Constant * Reflector::internSymbol(const llvm::StringRef &Key) {
  return cg_.genStringLiteral(Key, Key);
}

GlobalVariable * Reflector::getModulePtr(Module * module) {
  DASSERT(enabled_);

  std::string moduleSymbol(".module." + module->linkageName());
  GlobalVarMap::iterator it = globals_.find(moduleSymbol);
  if (it != globals_.end()) {
    return it->second;
  }

  irModule_->addTypeName("tart.reflect.Module", Builtins::typeModule->irType());
  irModule_->addTypeName("tart.reflect.NameTable", Builtins::typeNameTable->irType());
  GlobalVariable * rfModule = new GlobalVariable(*irModule_, Builtins::typeModule->irType(), false,
      GlobalValue::ExternalLinkage, NULL, moduleSymbol);
  globals_[moduleSymbol] = rfModule;
  return rfModule;
}

GlobalVariable * Reflector::getNameTablePtr(Module * module) {
  DASSERT(enabled_);

  if (nameTableVar_ == NULL) {
    std::string nameTableSymbol(".names." + module->linkageName());
    nameTableVar_ = new GlobalVariable(*irModule_, Builtins::typeNameTable->irType(),
        false, GlobalValue::ExternalLinkage, NULL, nameTableSymbol);
  }

  return nameTableVar_;
}

llvm::GlobalVariable * Reflector::getPackagePtr(Module * module) {
  std::string packageSymbol(".package." + module->packageName());
  GlobalVarMap::iterator it = globals_.find(packageSymbol);
  if (it != globals_.end()) {
    return it->second;
  }

  irModule_->addTypeName("tart.reflect.Package", Builtins::typePackage->irType());
  StructBuilder sb(cg_);
  sb.createObjectHeader(Builtins::typePackage);
  sb.addField(internSymbol(module->packageName()));
  sb.addField(emitArray("tart.reflect.Package.", package_modules.get(), ConstantList()));
  sb.addField(emitArray("tart.reflect.Package.", package_subpackages.get(), ConstantList()));

  GlobalVariable * rfPackage = new GlobalVariable(*irModule_, Builtins::typePackage->irType(), true,
      GlobalValue::LinkOnceAnyLinkage, sb.build(), packageSymbol);
  globals_[packageSymbol] = rfPackage;
  return rfPackage;
}

llvm::Constant * Reflector::getTypePtr(const Type * type) {
  switch (type->typeClass()) {
    case Type::Primitive:
      return cg_.getPrimitiveTypeObjectPtr(static_cast<const PrimitiveType *>(type));

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
      return getCompositeTypePtr(static_cast<const CompositeType *>(type));

    case Type::Enum:
      return getEnumTypePtr(static_cast<const EnumType *>(type));

    case Type::Tuple:
    case Type::Union:
    case Type::NAddress:
      return getDerivedTypePtr(type);

    case Type::Function:
      return getFunctionTypePtr(static_cast<const FunctionType *>(type));

    case Type::BoundMethod:
      return getFunctionTypePtr(static_cast<const BoundMethodType *>(type)->fnType());

    case Type::NArray:
      diag.fatal() << "Attempt to get type pointer for unimplemented type: " << type;
      DFAIL("Implement");

    default:
      diag.fatal() << "Attempt to get type pointer for non-supported type: " << type;
  }

  return NULL;
}

llvm::GlobalVariable * Reflector::getCompositeTypePtr(const CompositeType * type) {
  std::string typeSymbol(".compositeType." + type->typeDefn()->linkageName());
  GlobalVariable * var = irModule_->getGlobalVariable(typeSymbol, true);
  if (var != NULL) {
    return var;
  }

  if (!isExport(type->typeDefn())) {
    return new GlobalVariable(*irModule_, Builtins::typeCompositeType->irType(), true,
        GlobalValue::ExternalLinkage, NULL, typeSymbol);
  }

  if (outputPhase_) {
    diag.fatal() << "Attempting to add a type export during output phase: " << type;
  }
  typeExports_.insert(type);
  return new GlobalVariable(*irModule_, Builtins::typeCompositeType->irType(), false,
      type->typeDefn()->isSynthetic()
          ? GlobalValue::LinkOnceAnyLinkage
          : GlobalValue::ExternalLinkage,
      NULL, typeSymbol);
}

llvm::GlobalVariable * Reflector::getEnumTypePtr(const EnumType * type) {
  std::string typeSymbol(".enumType." + type->typeDefn()->linkageName());
  GlobalVariable * var = irModule_->getGlobalVariable(typeSymbol, true);
  if (var != NULL) {
    return var;
  }

  if (!isExport(type->typeDefn())) {
    return new GlobalVariable(*irModule_, Builtins::typeEnumType->irType(), true,
        GlobalValue::ExternalLinkage, NULL, typeSymbol);
  }

  if (outputPhase_) {
    diag.fatal() << "Attempting to add a type export during output phase: " << type;
  }
  typeExports_.insert(type);
  return new GlobalVariable(*irModule_, Builtins::typeEnumType->irType(), false,
      type->typeDefn()->isSynthetic()
          ? GlobalValue::LinkOnceAnyLinkage
          : GlobalValue::ExternalLinkage,
      NULL, typeSymbol);
}

llvm::GlobalVariable * Reflector::getDerivedTypePtr(const Type * type) {
  std::string typeSymbol(".type.");
  typeLinkageName(typeSymbol, type);
  GlobalVariable * var = irModule_->getGlobalVariable(typeSymbol, true);
  if (var != NULL) {
    return var;
  }

  if (outputPhase_) {
    diag.fatal() << "Attempting to add a type export during output phase: " << type;
  }
  typeExports_.insert(type);

  for (size_t i = 0; i < type->numTypeParams(); ++i) {
    getTypePtr(type->typeParam(i));
  }

  return new GlobalVariable(*irModule_, Builtins::typeDerivedType->irType(), false,
      GlobalValue::LinkOnceODRLinkage,
      NULL, typeSymbol);
}

llvm::GlobalVariable * Reflector::getFunctionTypePtr(const FunctionType * type) {
  std::string typeSymbol(".type.");
  typeLinkageName(typeSymbol, type);
  GlobalVariable * var = irModule_->getGlobalVariable(typeSymbol, true);
  if (var != NULL) {
    return var;
  }

  if (outputPhase_) {
    diag.fatal() << "Attempting to add a type export during output phase: " << type;
  }

  getTypePtr(type->returnType());
  for (ParameterList::const_iterator it = type->params().begin();
      it != type->params().end(); ++it) {
    getTypePtr((*it)->type());
  }

  typeExports_.insert(type);
  return new GlobalVariable(*irModule_, Builtins::typeFunctionType->irType(), false,
      GlobalValue::LinkOnceODRLinkage,
      NULL, typeSymbol);
}

void Reflector::emitModule(Module * module) {
  DASSERT(enabled_);

  NameTable & nameTable = cg_.nameTable();
  const DefnSet & reflectedDefs = module->reflectedDefs();

  // See if there are any reflected defns.
  bool hasReflectedDefns = false;
  if (!reflectedDefs.empty() /*|| !mmd_.defnsToExport().empty()*/) {
    hasReflectedDefns = true;
  }

  if (hasReflectedDefns) {
    GlobalVariable * modulePtr = getModulePtr(module);
    if (!modulePtr->hasInitializer()) {

      // First visit members which are explicitly declared in this module.
      ReflectionMetadata * rmd = getOrCreateReflectionMetadata(module);

      for (DefnSet::const_iterator it = reflectedDefs.begin(); it != reflectedDefs.end(); ++it) {
        createMetadata(*it);
      }

      createMetadataForMembers(module, rmd);

      GlobalVariable * nameTableVar = getNameTablePtr(cg_.module());
      NameTable::Name * qualifiedName = nameTable.addQualifiedName(module->qualifiedName());
      DASSERT(qualifiedName != NULL);

      nameTable.assignIndices();
      rmd->assignIndices();

      // Encode any methods in the module.
      raw_ostream & strm = rmd->strm();
      rmd->setMethodBaseIndex(0);
      emitReflectedMembers(rmd, module);
      strm << char(0);
      strm.flush();

      // Generate the Module struct
      StructBuilder sb(cg_);
      sb.createObjectHeader(Builtins::typeModule);
      sb.addField(nameTableVar);
      sb.addIntegerField(module_nameIndex, qualifiedName->encodedIndex());

      // Module._strmData
      Constant * defnStrm = ConstantArray::get(context_, rmd->strmData(), false);
      GlobalVariable * defnStrmVar = new GlobalVariable(*irModule_, defnStrm->getType(),
          true, GlobalValue::InternalLinkage, defnStrm, ".module_members");
      sb.addField(llvm::ConstantExpr::getPointerCast(defnStrmVar, builder_.getInt8PtrTy()));

      // Module._typeRefs
      if (!rmd->typeRefs().empty()) {
        sb.addPointerField(module_typeRefs, emitTypeRefArray(rmd, "module"));
      } else {
        sb.addNullField(module_typeRefs.type());
      }

      // Module._globalRefs
      if (!rmd->globalRefs().empty()) {
        sb.addPointerField(module_globalRefs, emitGlobalRefArray(rmd, "module"));
      } else {
        sb.addNullField(module_globalRefs.type());
      }

      // Module._methodRefs
      if (!rmd->methodRefs().empty()) {
        sb.addPointerField(module_methodRefs, emitMethodRefArray(rmd, "module"));
      } else {
        sb.addNullField(module_methodRefs.type());
      }

      sb.addPointerField(module_memberTypes, emitMemberTypes(module));
      sb.addNullField(module_methods.type());
      modulePtr->setInitializer(sb.build());
    }
  } else {
    nameTable.assignIndices();
  }

  outputPhase_ = true;

  emitNameTable(module);
  for (TypeSet::iterator it = typeExports_.begin(); it != typeExports_.end(); ++it) {
    emitType(*it);
  }
}

void Reflector::emitNameTable(Module * module) {
  GlobalVariable * nameTablePtr = getNameTablePtr(module);
  if (!nameTablePtr->hasInitializer()) {
    NameTable & nameTable = cg_.nameTable();

    // Generate the table of encoded strings.
    std::string encodedStringData;
    raw_string_ostream encodedStringStream(encodedStringData);
    nameTable.writeStringTable(encodedStringStream);

    // Generate the table of encoded compound names.
    std::string encodedNamesData;
    raw_string_ostream encodedNamesStream(encodedNamesData);
    nameTable.writeCompoundNameTable(encodedNamesStream);

    // Generate the module constants structure
    StructBuilder sb(cg_);
    sb.createObjectHeader(Builtins::typeNameTable);

    // Write out encoded string data stream
    if (!encodedStringData.empty()) {
      std::string encodedStringsSymbol(".names_simple." + module->linkageName());
      Constant * encodedStrings = ConstantArray::get(context_, encodedStringData, false);
      GlobalVariable * encodedStringsVar = new GlobalVariable(*irModule_, encodedStrings->getType(),
          true, GlobalValue::InternalLinkage, encodedStrings, encodedStringsSymbol);
      sb.addField(llvm::ConstantExpr::getPointerCast(encodedStringsVar, builder_.getInt8PtrTy()));
    } else {
      sb.addNullField(nameTable_nameStrmSimple.type());
    }

    // Write out encoded compound name stream
    if (!encodedNamesData.empty()) {
      std::string encodedNamesSymbol(".names_compound." + module->linkageName());
      Constant * encodedNames = ConstantArray::get(context_, encodedNamesData, false);
      GlobalVariable * encodedNamesVar = new GlobalVariable(*irModule_, encodedNames->getType(),
          true, GlobalValue::InternalLinkage, encodedNames, encodedNamesSymbol);

      sb.addField(llvm::ConstantExpr::getPointerCast(encodedNamesVar, builder_.getInt8PtrTy()));
    } else {
      sb.addNullField(nameTable_nameStrmSimple.type());
    }

    sb.addNullField(nameTable_simpleNames.type());
    sb.addNullField(nameTable_compoundNames.type());
    sb.addNullField(nameTable_compoundNameStrings.type());

    nameTablePtr->setInitializer(sb.build(Builtins::typeNameTable->irType()));
  }
}

void Reflector::createMetadata(const Defn * def) {
  DASSERT(isExport(def));

#if 0
  diag.debug() << "Adding metadata for def " << def;
#endif

  cg_.nameTable().addQualifiedName(def->qualifiedName())->use();

  // Add all of the members of this definition
  switch (def->defnType()) {
    case Defn::Typedef: {
      const TypeDefn * td = static_cast<const TypeDefn *>(def);
      const Type * type = td->typeValue();
      getTypePtr(type);
      if (const CompositeType * ctype = dyn_cast<CompositeType>(type)) {
        ReflectionMetadata * rmd = getOrCreateReflectionMetadata(def);
        for (ClassList::const_iterator it = ctype->bases().begin();
            it != ctype->bases().end(); ++it) {
          getTypePtr(*it);
        }

        for (size_t i = 0; i < ctype->numTypeParams(); ++i) {
          getTypePtr(ctype->typeParam(i));
        }

        createMetadataForMembers(ctype->memberScope(), rmd);
      }

      switch (type->typeClass()) {
        case Type::Enum: {
          //const EnumType * etype = static_cast<const EnumType *>(type);
#if DEBUG_VERBOSE
          diag.debug() << "Emitting metadata for enum " << etype;
          diag.indent();
#endif
          //getEnumTypePtr(etype);
          //createMetadataForMembers(etype->memberScope(), rmd);
#if DEBUG_VERBOSE
          diag.unindent();
#endif
          break;
        }

        case Type::Alias: {
          DASSERT_OBJ(false, def);
          break;
        }

        default: {
          break;
        }
      }
      break;
    }

    case Defn::Namespace: {
      ReflectionMetadata * rmd = getOrCreateReflectionMetadata(def);
      const NamespaceDefn * ns = static_cast<const NamespaceDefn *>(def);
#if DEBUG_VERBOSE
      diag.debug() << "Emitting metadata for namespace " << def;
      diag.indent();
#endif
      createMetadataForMembers(&ns->memberScope(), rmd);
#if DEBUG_VERBOSE
      diag.unindent();
#endif
      break;
    }

    default: {
      break;
    }
  }
}

void Reflector::createMetadataForMembers(const IterableScope * scope, ReflectionMetadata * rmd) {
  for (const Defn * m = scope->firstMember(); m != NULL; m = m->nextInScope()) {
    if (m->isReflected()) {
      createMetadataForMember(m, rmd);
    }
  }
}

void Reflector::createMetadataForMember(const Defn * def, ReflectionMetadata * rmd) {
  NameTable & names = cg_.nameTable();

  // Add all of the members of this definition
  switch (def->defnType()) {
    case Defn::Typedef: {
      const TypeDefn * td = static_cast<const TypeDefn *>(def);
      const Type * type = td->typeValue();
      if (td->isSingular()) {
        getTypePtr(type);
      }
      break;
    }

    case Defn::Namespace: {
      break;
    }

    case Defn::Var:
    case Defn::Let: {
      const VariableDefn * v = static_cast<const VariableDefn *>(def);
      if (v->type()->typeClass() == Type::NArray ||
          v->type()->typeClass() == Type::FlexibleArray ||
          v->type()->typeClass() == Type::Function) {
        break;
      }
      cg_.nameTable().addName(v->name())->use();
      rmd->addTypeRef(v->type());
      getTypePtr(v->type());
#if DEBUG_VERBOSE
      diag.debug() << "Emitting metadata for var " << def;
#endif
      break;
    }

    case Defn::Property:
    case Defn::Indexer: {
      const PropertyDefn * prop = static_cast<const PropertyDefn *>(def);
      cg_.nameTable().addName(prop->name())->use();
      rmd->addTypeRef(prop->type());
      getTypePtr(prop->type());
#if DEBUG_VERBOSE
      diag.debug() << "Emitting metadata for property " << def;
#endif
      break;
    }

    case Defn::Function: {
      // Note: Macros are not reflected.
      const FunctionDefn * fn = static_cast<const FunctionDefn *>(def);
      const FunctionType * fnType = fn->functionType();
      if (!fn->isIntrinsic() && fn->isReflected() &&
          fn->isSingular() && fn->functionType() != NULL) {
        cg_.nameTable().addName(fn->name())->use();
        getTypePtr(fn->functionType());
        rmd->addTypeRef(fn->functionType());
        for (ParameterList::const_iterator it = fnType->params().begin();
            it != fnType->params().end(); ++it) {
          cg_.nameTable().addName((*it)->name())->use();
        }

#if DEBUG_VERBOSE
        diag.debug() << "Emitting metadata for method " << def;
#endif
      }
      break;
    }

    case Defn::ExplicitImport: {
      break;
    }

    default: {
      break;
    }
  }
}

ReflectionMetadata * Reflector::getReflectionMetadata(const Defn * def) {
  DASSERT(def != NULL);
  DASSERT(enabled_);
  ReflectedSymbolMap::const_iterator it = rmdMap_.find(def);
  if (it != rmdMap_.end()) {
    DASSERT(it->second != NULL);
    return it->second;
  }

  return NULL;
}

ReflectionMetadata * Reflector::getOrCreateReflectionMetadata(const Defn * def) {
  ReflectionMetadata * rsym = getReflectionMetadata(def);
  if (rsym != NULL) {
    return rsym;
  }

  rsym = new ReflectionMetadata(def);
  rmdMap_[def] = rsym;
  return rsym;
}

void Reflector::emitType(const Type * type) {
  switch (type->typeClass()) {
    case Type::Primitive:
      diag.fatal() << "Attempt to emit type pointer for primtitive type: " << type;
      break;

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
      emitCompositeType(static_cast<const CompositeType *>(type));
      break;

    case Type::Enum:
      emitEnumType(static_cast<const EnumType *>(type));
      break;

    case Type::Tuple:
    case Type::Union:
    case Type::NAddress:
      emitDerivedType(type);
      break;

    case Type::Function:
      emitFunctionType(static_cast<const FunctionType *>(type));
      break;

    case Type::NArray:
      DFAIL("Implement");

    default:
      diag.fatal() << "Attempt to emit type pointer for unsupported type: " << type;
      break;
  }
}

void Reflector::emitCompositeType(const CompositeType * type) {
  GlobalVariable * var = getCompositeTypePtr(type);
  if (var->hasInitializer()) {
    return;
  }

  TypeDefn * td = type->typeDefn();
  GlobalVariable * nameTableVar = getNameTablePtr(cg_.module());
  ReflectionMetadata * rmd = getReflectionMetadata(td);
  DASSERT_OBJ(rmd != NULL, type);

  // Generate the CompositeType instance
  StructBuilder sb(cg_);
  sb.createObjectHeader(Builtins::typeCompositeType);
  sb.addIntegerField(type_typeKind.type(), typeKind(type->typeClass()));
  sb.combine();

  // CompositeType._names
  sb.addField(nameTableVar);
  NameTable::Name * qname = cg_.nameTable().getQualifiedName(td->qualifiedName());
  sb.addIntegerField(compositeType_nameIndex, qname ? qname->encodedIndex() : 0);

  // CompositeType._size
  if (type->isSingular()) {
    sb.addField(llvm::ConstantExpr::getSizeOf(type->irType()));
  } else {
    sb.addIntegerField(compositeType_size.type(), 0);
  }

  // CompositeType._typeInfo
  sb.addField(cg_.getTypeInfoBlockPtr(type));

  // Generate the stream of definition data.
  rmd->assignIndices();
  raw_ostream & strm = rmd->strm();
  rmd->setMethodBaseIndex(type->instanceMethodCount());
  emitAttributeSection(rmd, td->attrs());
  emitReflectedMembers(rmd, type->memberScope());
  strm << char(0);
  strm.flush();

  // Definition stream
  if (!rmd->strmData().empty()) {
    std::string defnStrmSymbol(".members." + td->linkageName());
    Constant * defnStrm = ConstantArray::get(context_, rmd->strmData(), false);
    GlobalVariable * defnStrmVar = new GlobalVariable(*irModule_, defnStrm->getType(),
        true, GlobalValue::InternalLinkage, defnStrm, defnStrmSymbol);
    sb.addField(llvm::ConstantExpr::getPointerCast(defnStrmVar, builder_.getInt8PtrTy()));
  } else {
    sb.addNullField(compositeType_strmData.type());
  }

  // CompositeType._typeRefs
  if (!rmd->typeRefs().empty()) {
    sb.addPointerField(compositeType_typeRefs, emitTypeRefArray(rmd, td->linkageName()));
  } else {
    sb.addNullField(compositeType_typeRefs.type());
  }

  // CompositeType._globalRefs
  if (!rmd->globalRefs().empty()) {
    sb.addPointerField(compositeType_globalRefs, emitGlobalRefArray(rmd, td->linkageName()));
  } else {
    sb.addNullField(compositeType_globalRefs.type());
  }

  // CompositeType._methodRefs
  if (!rmd->methodRefs().empty()) {
    sb.addPointerField(compositeType_methodRefs, emitMethodRefArray(rmd, td->linkageName()));
  } else {
    sb.addNullField(compositeType_methodRefs.type());
  }

  // CompositeType._supertype
  if (type->super() != NULL) {
    sb.addField(getCompositeTypePtr(type->super()));
  } else {
    sb.addNullField(compositeType_supertype.type());
  }

  // CompositeType._interfaces
  ConstTypeList interfaceList;
  for (ClassList::const_iterator it = type->bases().begin(); it != type->bases().end(); ++it) {
    if ((*it)->typeClass() == Type::Interface) {
      interfaceList.push_back(*it);
    }
  }
  sb.addPointerField(compositeType_interfaces, emitStaticTypeList(interfaceList));

  // CompositeType._typeParams
  ConstTypeList typeParamList;
  if (td->isReflected()) {
    type->getTypeParams(typeParamList);
  }
  sb.addPointerField(compositeType_typeParams, emitStaticTypeList(typeParamList));

  // Lazily-initialized fields.
  sb.addNullField(compositeType_attributes.type());
  sb.addNullField(compositeType_fields.type());
  sb.addNullField(compositeType_properties.type());
  sb.addNullField(compositeType_constructors.type());
  sb.addNullField(compositeType_methods.type());

  if (td->isReflected()) {
    sb.addPointerField(compositeType_memberTypes, emitMemberTypes(type->memberScope()));
  } else {
    sb.addNullField(compositeType_memberTypes.type());
  }

  if (type->isSingular()) {
    Function * alloc = cg_.getTypeAllocator(type);
    if (alloc != NULL) {
      sb.addPointerField(compositeType_alloc, alloc);
    } else {
      sb.addNullField(compositeType_alloc.type());
    }
  } else {
    sb.addNullField(compositeType_alloc.type());
  }

  FunctionDefn * noArgCtor = type->noArgConstructor();
  if (noArgCtor != NULL && noArgCtor->hasBody() && !noArgCtor->isUndefined()) {
    sb.addPointerField(compositeType_noArgCtor, cg_.genFunctionValue(noArgCtor));
  } else {
    sb.addNullField(compositeType_noArgCtor.type());
  }

  sb.addIntegerField(compositeType_methodBaseIndex, type->instanceMethodCount());

  var->setInitializer(sb.build(Builtins::typeCompositeType->irType()));
}

void Reflector::emitEnumType(const EnumType * type) {
  GlobalVariable * var = getEnumTypePtr(type);
  if (var->hasInitializer()) {
    return;
  }

  //diag.debug() << "Emitting composite type: " << Format_Verbose << type;

  TypeDefn * td = type->typeDefn();
  GlobalVariable * nameTableVar = getNameTablePtr(cg_.module());

  // Generate the EnumType instance
  StructBuilder sb(cg_);
  sb.createObjectHeader(Builtins::typeEnumType);
  sb.addIntegerField(type_typeKind.type(), ENUM);
  sb.combine();

  // EnumType._names
  sb.addField(nameTableVar);
  NameTable::Name * qname = cg_.nameTable().getQualifiedName(td->qualifiedName());
  sb.addIntegerField(enumType_nameIndex, qname ? qname->encodedIndex() : 0);

  // EnumType._supertype
  sb.addNullField(enumType_supertype.type());
  sb.addNullField(enumType_values.type());

  var->setInitializer(sb.build(Builtins::typeEnumType->irType()));
}

void Reflector::emitDerivedType(const Type * type) {
  GlobalVariable * var = getDerivedTypePtr(type);
  if (var->hasInitializer()) {
    return;
  }

  // Generate the DerivedType instance
  StructBuilder sb(cg_);
  sb.createObjectHeader(Builtins::typeDerivedType);
  sb.addIntegerField(type_typeKind.type(), typeKind(type->typeClass()));
  sb.combine();

  ConstTypeList typeParamList;
  type->getTypeParams(typeParamList);
  sb.addPointerField(derivedType_typeParams, emitStaticTypeList(typeParamList));

  var->setInitializer(sb.build(Builtins::typeDerivedType->irType()));
}

void Reflector::emitFunctionType(const FunctionType * type) {
  GlobalVariable * var = getFunctionTypePtr(type);
  if (var->hasInitializer()) {
    return;
  }

  // Union return values temporarily disabled for the moment.
  bool canInvoke = false;
  const Type * selfType = type->selfParam() != NULL ? type->selfParam()->type() : NULL;
  const Type * returnType = type->returnType();
  if (type->isInvocable() &&
      returnType->typeClass() != Type::Union &&
      (selfType == NULL ||
          selfType->typeClass() == Type::Class ||
          selfType->typeClass() == Type::Interface)) {
    canInvoke = true;
  }

  // Generate the FunctionType instance
  StructBuilder sb(cg_);
  sb.createObjectHeader(Builtins::typeFunctionType);
  sb.addIntegerField(type_typeKind.type(), typeKind(type->typeClass()));
  sb.combine();

  // FunctionType._returnType
  sb.addPointerField(functionType_returnType, getTypePtr(type->returnType()));

  // FunctionType._selfType
  if (type->selfParam() != NULL) {
    sb.addPointerField(functionType_selfType, getTypePtr(type->selfParam()->type()));
  } else {
    sb.addNullField(functionType_selfType.type());
  }

  // FunctionType._paramTypes
  sb.addPointerField(functionType_paramTypes, emitStaticTypeList(type->paramTypes()->members()));

  // FunctionType._invoke
  if (canInvoke) {
    sb.addField(cg_.genCallAdapterFn(type));
  } else {
    sb.addNullField(functionType_invoke.type());
  }

  var->setInitializer(sb.build(Builtins::typeFunctionType->irType()));
}

llvm::Constant * Reflector::emitMemberTypes(const IterableScope * scope) {
  ConstTypeList memberTypes;

  for (Defn * de = scope->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->isReflected() && isExport(de) && de->isSingular() && de->defnType() == Defn::Typedef) {
      memberTypes.push_back(static_cast<const TypeDefn *>(de)->typeValue());
    }
  }

  return emitStaticTypeList(memberTypes);
}

void Reflector::emitReflectedMembers(ReflectionMetadata * rmd, const IterableScope * scope) {
  DefnList namespaces;
  DefnList fields;
  DefnList properties;
  MethodList methods;

  for (Defn * de = scope->firstMember(); de != NULL; de = de->nextInScope()) {
    if (!de->isReflected() || !isExport(de)) {
      continue;
    }

    switch (de->defnType()) {
      case Defn::Var:
      case Defn::Let:
        fields.push_back(de);
        break;

      case Defn::Function: {
        FunctionDefn * fn = static_cast<FunctionDefn *>(de);
        if (!fn->isIntrinsic()) {
          methods.push_back(fn);
        }
        break;
      }

      case Defn::Namespace:
        break;

      case Defn::Property:
      case Defn::Indexer:
        break;

      default:
        break;
    }
  }

  if (!namespaces.empty()) {
    std::sort(namespaces.begin(), namespaces.end(), DefnOrder());
    std::string nsData;
    raw_string_ostream nsStrm(nsData);

    for (DefnList::const_iterator it = namespaces.begin(); it != namespaces.end(); ++it) {
      emitNamespaceDefn(rmd, static_cast<const NamespaceDefn *>(*it), nsStrm);
    }

    nsStrm.flush();
    if (!nsData.empty()) {
      rmd->strm() << char(TAG_SECTION_NAMESPACES) << VarInt(nsData.size()) << nsData;
    }
  }

  if (!fields.empty()) {
    std::sort(fields.begin(), fields.end(), DefnOrder());
    std::string fieldData;
    raw_string_ostream fieldStrm(fieldData);

    for (DefnList::const_iterator it = fields.begin(); it != fields.end(); ++it) {
      emitFieldDefn(rmd, static_cast<const VariableDefn *>(*it), fieldStrm);
    }

    fieldStrm.flush();
    if (!fieldData.empty()) {
      rmd->strm() << char(TAG_SECTION_FIELDS) << VarInt(fieldData.size()) << fieldData;
    }
  }

  if (!properties.empty()) {
    std::sort(properties.begin(), properties.end(), DefnOrder());
    std::string propData;
    raw_string_ostream propStrm(propData);

    for (DefnList::const_iterator it = properties.begin(); it != properties.end(); ++it) {
      emitPropertyDefn(rmd, static_cast<const PropertyDefn *>(*it), propStrm);
    }

    propStrm.flush();
    if (!propData.empty()) {
      rmd->strm() << char(TAG_SECTION_PROPERTIES) << VarInt(propData.size()) << propData;
    }
  }

  if (!methods.empty()) {
    std::string methodData;
    raw_string_ostream methodStrm(methodData);

#if DEBUG_VERBOSE
    diag.debug() << methods.size() << " reflected methods";
#endif

    for (MethodList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
      const FunctionDefn * fn = *it;
      emitMethodDefn(rmd, fn, methodStrm);
    }

    // TODO

    methodStrm.flush();
    if (!methodData.empty()) {
      rmd->strm() << char(TAG_SECTION_METHODS) << VarInt(methodData.size()) << methodData;
    }
  }
}

void Reflector::emitAttributeSection(ReflectionMetadata * rmd, const ExprList & attrs) {
  if (!attrs.empty()) {
    std::string attrData;
    raw_string_ostream attrStrm(attrData);

    for (ExprList::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
      llvm::Constant * retainedAttr = getRetainedAttr(*it);
      if (retainedAttr != NULL) {
        size_t attrIndex = rmd->addGlobalRef(retainedAttr);
        attrStrm << VarInt(attrIndex);
      }
    }

    attrStrm.flush();
    if (!attrData.empty()) {
      rmd->strm() << char(TAG_SECTION_ATTRIBUTES) << VarInt(attrData.size()) << attrData;
    }
  }
}

void Reflector::emitTemplateParamsSection(ReflectionMetadata * rmd, const Defn * def) {
  const TemplateSignature * ts = def->templateSignature();
  if (ts != NULL) {
    std::string paramData;
    raw_string_ostream paramStrm(paramData);

    paramStrm.flush();
    if (!paramData.empty()) {
      rmd->strm() << char(TAG_SECTION_TEMPLATE_PARAMS) << VarInt(paramData.size()) << paramData;
    }
  }
}

void Reflector::emitTemplateSection(ReflectionMetadata * rmd, const Defn * def) {
  const TemplateInstance * ti = def->templateInstance();
  if (ti != NULL) {
    std::string templateData;
    raw_string_ostream templateStrm(templateData);
    rmd->encodeTypeRef(cast<TypeDefn>(ti->templateDefn())->typeValue(), templateStrm);

    templateStrm.flush();
    if (!templateData.empty()) {
      rmd->strm() << char(TAG_SECTION_BASE_TEMPLATE) << VarInt(templateData.size()) << templateData;
    }
  }
}

void Reflector::emitNamespaceDefn(ReflectionMetadata * rmd, const NamespaceDefn * def,
    raw_ostream & out) {
}

void Reflector::emitFieldDefn(ReflectionMetadata * rmd, const VariableDefn * def, raw_ostream & out) {
}

void Reflector::emitMethodDefn(ReflectionMetadata * rmd, const FunctionDefn * fn, raw_ostream & out) {
  if (!fn->isSingular() || !fn->isReflected()) {
    // TODO: Implement.
    return;
  }

  char tag = TAG_DEF_METHOD;
  if (fn->defnType() == Defn::Macro) {
    tag = TAG_DEF_MACRO;
  } else if (fn->isOverride()) {
    tag = TAG_DEF_OVERRIDE;
  } else if (fn->isUndefined()) {
    tag = TAG_DEF_UNDEF;
  } else if (fn->isCtor()) {
    tag = TAG_DEF_CONSTRUCTOR;
  }

  char flags = 0;
  if (fn->visibility() == Private) {
    flags |= DEFNFLAG_PRIVATE;
  } else if (fn->visibility() == Protected) {
    flags |= DEFNFLAG_PROTECTED;
  }

  // Definition modifiers
  if (fn->isFinal()) {
    flags |= DEFNFLAG_FINAL;
  }

  if (fn->isAbstract()) {
    flags |= DEFNFLAG_ABSTRACT;
  }

  if (fn->isUnsafe()) {
    flags |= DEFNFLAG_UNSAFE;
  }

  if (fn->storageClass() == Storage_Static) {
    flags |= DEFNFLAG_STATIC;
  }

  NameTable::Name * name = cg_.nameTable().getName(fn->name());
  DASSERT_OBJ(name != NULL, fn);

  size_t methodIndex;
  if (fn->isAbstract() || fn->isUndefined() || fn->isIntrinsic() || fn->isInterfaceMethod()) {
    methodIndex = 0;
  } else if (fn->dispatchIndex() != -1) {
    methodIndex = fn->dispatchIndex();
  } else {
    const FunctionDefn * coalescedFn = fn->mergeTo() != NULL ? fn->mergeTo() : fn;
    const llvm::Type * methodType = cg_.getMethodPointerType();
    llvm::Constant * methodVal = llvm::ConstantExpr::getBitCast(
        cg_.genFunctionValue(coalescedFn), methodType);
    methodIndex = rmd->methodBaseIndex() + rmd->methodRefs().size();
    rmd->methodRefs().push_back(methodVal);
  }

  // Definition tag and name
  out << tag << flags << VarInt(name->encodedIndex());
  rmd->encodeTypeRef(fn->functionType(), out);
  out << VarInt(uint32_t(methodIndex));

  // Declare parameters
  const ParameterList & params = fn->functionType()->params();
  for (ParameterList::const_iterator it = params.begin(); it != params.end(); ++it) {
    const ParameterDefn * p = *it;

    // Deal with any parameter attributes
    for (ExprList::const_iterator attr = p->attrs().begin(); attr != p->attrs().end(); ++it) {
      //var << TAG_DEFMOD_ATTRIBUTE << VarInt(attribute-index);
    }

    if (p->isVariadic()) {
      out << char(TAG_DEFMOD_VARIADIC);
    }
    if (p->isKeywordOnly()) {
      out << char(TAG_DEFMOD_KEYWORD_ONLY);
    }

    NameTable::Name * paramName = cg_.nameTable().getName(p->name());
    DASSERT_OBJ(paramName != NULL, fn);
    out << char(TAG_DEF_PARAM) << VarInt(paramName->encodedIndex());
    //rmd->encodeTypeRef(p->type(), out);
  }

  // Method attributes
  for (ExprList::const_iterator it = fn->attrs().begin(); it != fn->attrs().end(); ++it) {
    llvm::Constant * retainedAttr = getRetainedAttr(*it);
    if (retainedAttr != NULL) {
      size_t attrIndex = rmd->addGlobalRef(retainedAttr);
      out << char(TAG_DEF_ATTRIBUTE) << VarInt(attrIndex);
    }
  }

  out << char(TAG_DEF_SCOPE_END);
}

void Reflector::emitPropertyDefn(ReflectionMetadata * rmd, const PropertyDefn * def,
    raw_ostream & out) {
}

llvm::Constant * Reflector::emitStaticTypeList(const ConstTypeList & types) {
  // Generate the symbolic name of the type list.
  std::string sym = ".typelist.(";
  for (TupleType::const_iterator it = types.begin(); it != types.end(); ++it) {
    if (it != types.begin()) {
      sym.append(",");
    }
    typeLinkageName(sym, *it);
  }
  sym.append(")");

  GlobalVariable * var = irModule_->getGlobalVariable(sym, true);
  if (var != NULL) {
    return var;
  }

  const llvm::Type * typePtrType = Builtins::typeType->irEmbeddedType();
  ConstantList typePtrList;
  for (TupleType::const_iterator it = types.begin(); it != types.end(); ++it) {
    llvm::Constant * ptr = getTypePtr(*it);
    typePtrList.push_back(llvm::ConstantExpr::getPointerCast(ptr, typePtrType));
  }

  Constant * typePtrArray = ConstantArray::get(
      ArrayType::get(typePtrType, typePtrList.size()),
      typePtrList);

  StructBuilder sb(cg_);
  sb.createObjectHeader(Builtins::typeStaticTypeList);
  sb.addIntegerField(staticTypeList_size, typePtrList.size());
  sb.addField(typePtrArray);
  Constant * typeListStruct = sb.build();

  var = new GlobalVariable(*irModule_, typeListStruct->getType(), true,
      GlobalValue::LinkOnceAnyLinkage, typeListStruct, sym);
  return var;
}

llvm::Constant * Reflector::emitTypeRefArray(const ReflectionMetadata * rmd,
    llvm::StringRef baseName) {
  ConstantList typeRefList;
  const llvm::Type * typePointerType = Builtins::typeType.irEmbeddedType();
  const ReflectionMetadata::TypeArray & typeRefs = rmd->typeRefs();
  for (ReflectionMetadata::TypeArray::const_iterator it = typeRefs.begin();
      it != typeRefs.end(); ++it) {
    typeRefList.push_back(
        llvm::ConstantExpr::getPointerCast(getTypePtr(it->first), typePointerType));
  }

  // Create constant array
  Constant * typeArray = ConstantArray::get(
      ArrayType::get(typePointerType, typeRefList.size()),
      typeRefList);

  // Declare global
  return new GlobalVariable(*irModule_,
      typeArray->getType(), true, GlobalValue::InternalLinkage,
      typeArray, ".typeRefs." + baseName);
}

llvm::Constant * Reflector::emitGlobalRefArray(const ReflectionMetadata * rmd,
    llvm::StringRef baseName) {
  ConstantList globalRefList;
  const llvm::Type * globalPointerType = Builtins::typeObject.irEmbeddedType();
  const ReflectionMetadata::ConstantArray & globalRefs = rmd->globalRefs();
  for (ReflectionMetadata::ConstantArray::const_iterator it = globalRefs.begin();
      it != globalRefs.end(); ++it) {
    globalRefList.push_back(llvm::ConstantExpr::getPointerCast(*it, globalPointerType));
  }

  // Create constant array
  Constant * globalArray = ConstantArray::get(
      ArrayType::get(globalPointerType, globalRefList.size()),
      globalRefList);

  // Declare global
  return new GlobalVariable(*irModule_,
      globalArray->getType(), true, GlobalValue::InternalLinkage,
      globalArray, ".globalRefs." + baseName);
}

llvm::Constant * Reflector::emitMethodRefArray(const ReflectionMetadata * rmd,
    llvm::StringRef baseName) {
  ConstantList methodRefList;
  const llvm::Type * methodPointerType = cg_.getMethodPointerType();
  const ReflectionMetadata::ConstantArray & methodRefs = rmd->methodRefs();
  for (ReflectionMetadata::ConstantArray::const_iterator it = methodRefs.begin();
      it != methodRefs.end(); ++it) {
    methodRefList.push_back(llvm::ConstantExpr::getPointerCast(*it, methodPointerType));
  }

  // Create constant array
  Constant * methodArray = ConstantArray::get(
      ArrayType::get(methodPointerType, methodRefList.size()),
      methodRefList);

  // Declare global var
  return new GlobalVariable(*irModule_,
      methodArray->getType(), true, GlobalValue::InternalLinkage,
      methodArray, ".methodRefs." + baseName);
}

// TODO - remove this and replace all arrays with immutable lists.
llvm::Constant * Reflector::emitArray(
    const std::string & baseName, const VariableDefn * var, const ConstantList & values)
{
  const Type * varType = var->type();
  if (const UnionType * utype = dyn_cast<UnionType>(varType)) {
    DASSERT_OBJ(utype->isSingleOptionalType(), utype);
    varType = utype->getFirstNonVoidType();
  }
  const CompositeType * arrayType = cast<CompositeType>(varType);
  const Type * elementType = arrayType->typeParam(0);
  irModule_->addTypeName(arrayType->typeDefn()->linkageName(), arrayType->irType());
  DASSERT_OBJ(arrayType->passes().isFinished(CompositeType::RecursiveFieldTypePass), var);

  if (values.empty()) {
    //DASSERT_OBJ(arrayType->passes().isFinished(CompositeType::CompletionPass), arrayType);
    VariableDefn * emptyArray = cast_or_null<VariableDefn>(
        arrayType->memberScope()->lookupSingleMember("emptyArray"));
    if (emptyArray != NULL) {
      return cast<llvm::Constant>(cg_.genLetValue(emptyArray));
    }
  }

  StructBuilder sb(cg_);
  sb.createObjectHeader(arrayType);
  sb.addField(cg_.getIntVal(values.size()));
  sb.addArrayField(elementType, values);

  llvm::Constant * arrayStruct = sb.build();
  GlobalVariable * array = new GlobalVariable(*irModule_,
      arrayStruct->getType(), true, GlobalValue::InternalLinkage, arrayStruct,
      ".data." + baseName + var->name());
  return llvm::ConstantExpr::getPointerCast(array, arrayType->irEmbeddedType());
}

Module * Reflector::module() { return cg_.module(); }

llvm::Constant * Reflector::getRetainedAttr(const Expr * attrExpr) {
  const CompositeType * ctype = cast<CompositeType>(attrExpr->type());
  if (ctype->attributeInfo().isRetained()) {
    if (const ConstantObjectRef * cobj = dyn_cast<ConstantObjectRef>(attrExpr)) {
      llvm::Constant * attr = cg_.genConstRef(attrExpr, "", false);
      return llvm::ConstantExpr::getPointerCast(attr, Builtins::typeObject->irEmbeddedType());
    } else {
      diag.error(attrExpr) << "Non-constant attribute (not implemented).";
      return NULL;
    }
  } else {
    return NULL;
  }
}

bool Reflector::isExport(const Defn * de) {
  return de->module() == module() || de->isSynthetic();
}

int Reflector::typeKind(Type::TypeClass cls) {
  switch (cls) {
    case Type::Primitive: return PRIMITIVE;
    case Type::Class: return CLASS;
    case Type::Struct: return STRUCT;
    case Type::Interface: return INTERFACE;
    case Type::Protocol: return PROTOCOL;
    case Type::Enum: return ENUM;
    case Type::Function: return FUNCTION;
    case Type::Tuple: return TUPLE;
    case Type::Union: return UNION;
    case Type::NAddress: return ADDRESS;
    case Type::NArray: return NATIVE_ARRAY;
    default:
      return OPAQUE;
  }
}

} // namespace tart

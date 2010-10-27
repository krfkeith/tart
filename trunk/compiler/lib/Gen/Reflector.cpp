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
/// Contains a lazy reference to an enumeration constant.

// Members of tart.reflect.Type.
SystemClassMember<VariableDefn> type_typeKind(Builtins::typeType, "_typeKind");

// Members of tart.reflect.SimpleType.
SystemClassMember<VariableDefn> primitiveType_subtype(Builtins::typePrimitiveType, "_subtype");
SystemClassMember<VariableDefn> primitiveType_size(Builtins::typePrimitiveType, "_size");

// Members of tart.reflect.DerivedType.
SystemClassMember<VariableDefn> derivedType_typeParams(Builtins::typeDerivedType, "_typeParams");

// Members of tart.reflect.CompositeType.
SystemClassMember<VariableDefn> complexType_tib(Builtins::typeCompositeType, "_typeInfo");
SystemClassMember<VariableDefn> complexType_superType(Builtins::typeCompositeType, "_supertype");
SystemClassMember<VariableDefn> complexType_interfaces(Builtins::typeCompositeType, "_interfaces");
SystemClassMember<VariableDefn> complexType_typeParams(Builtins::typeCompositeType, "_typeParams");
SystemClassMember<VariableDefn> complexType_attributes(Builtins::typeCompositeType, "_attributes");
SystemClassMember<VariableDefn> complexType_fields(Builtins::typeCompositeType, "_fields");
SystemClassMember<VariableDefn> complexType_properties(Builtins::typeCompositeType, "_properties");
SystemClassMember<VariableDefn> complexType_ctors(Builtins::typeCompositeType, "_constructors");
SystemClassMember<VariableDefn> complexType_methods(Builtins::typeCompositeType, "_methods");
SystemClassMember<VariableDefn> complexType_innerTypes(Builtins::typeCompositeType, "_innerTypes");
SystemClassMember<VariableDefn> complexType_alloc(Builtins::typeCompositeType, "_alloc");
SystemClassMember<VariableDefn> complexType_noArgCtor(Builtins::typeCompositeType, "_noArgCtor");

// Members of tart.reflect.EnumType.
SystemClassMember<VariableDefn> enumType_meta(Builtins::typeEnumType, "_meta");
SystemClassMember<VariableDefn> enumType_nameIndex(Builtins::typeEnumType, "_nameIndex");
SystemClassMember<VariableDefn> enumType_superType(Builtins::typeEnumType, "_supertype");
SystemClassMember<VariableDefn> enumType_values(Builtins::typeEnumType, "_values");

// Members of tart.reflect.FunctionType.
SystemClassMember<VariableDefn> functionType_returnType(Builtins::typeFunctionType, "_returnType");
SystemClassMember<VariableDefn> functionType_selfType(Builtins::typeFunctionType, "_selfType");
SystemClassMember<VariableDefn> functionType_paramTypes(Builtins::typeFunctionType, "_paramTypes");
SystemClassMember<VariableDefn> functionType_invoke(Builtins::typeFunctionType, "_invoke");
SystemClassMember<FunctionDefn> functionType_invokeFn(Builtins::typeFunctionType, "invoke");
SystemClassMember<FunctionDefn> functionType_checkArgs(Builtins::typeFunctionType, "checkArgCount");
SystemClassMember<FunctionDefn> functionType_ignoreObj(Builtins::typeFunctionType, "ignoreObjectPtr");

// Members of tart.reflect.Member.
SystemClassMember<VariableDefn> member_name(Builtins::typeMember, "_name");
SystemClassMember<VariableDefn> member_kind(Builtins::typeMember, "_kind");
SystemClassMember<VariableDefn> member_visibility(Builtins::typeMember, "_visibility");
SystemClassMember<VariableDefn> member_traits(Builtins::typeMember, "_traits");
SystemClassMember<VariableDefn> member_attributes(Builtins::typeMember, "_attributes");

// Members of tart.reflect.Method.
SystemClassMember<VariableDefn> method_typeParams(Builtins::typeMethod, "_typeParams");
SystemClassMember<VariableDefn> method_params(Builtins::typeMethod, "_params");
SystemClassMember<VariableDefn> method_methodPointer(Builtins::typeMethod, "_methodPointer");

// Members of tart.reflect.Module.
SystemClassMember<VariableDefn> module_constants(Builtins::typeModule, "_constants");
SystemClassMember<VariableDefn> module_nameIndex(Builtins::typeModule, "_nameIndex");
//SystemClassMember<VariableDefn> module_name(Builtins::typeModule, "_name");
SystemClassMember<VariableDefn> module_types(Builtins::typeModule, "_types");
SystemClassMember<VariableDefn> module_methods(Builtins::typeModule, "_methods");

// Members of tart.reflect.NameTable.
SystemClassMember<VariableDefn> nameTable_nameStrmSimple(
    Builtins::typeNameTable, "_nameStrmSimple");
SystemClassMember<VariableDefn> nameTable_simpleNames(
    Builtins::typeNameTable, "_simpleNames");
SystemClassMember<VariableDefn> nameTable_compoundNames(
    Builtins::typeNameTable, "_compoundNames");
SystemClassMember<VariableDefn> nameTable_compoundNameStrings(
    Builtins::typeNameTable, "_compoundNameStrings");

SystemClassMember<VariableDefn> rmd_value(
    Builtins::typeReflectionMetadata, "_value");
SystemClassMember<VariableDefn> rmd_names(
    Builtins::typeReflectionMetadata, "_names");
SystemClassMember<VariableDefn> rmd_strmTypeRefs(
    Builtins::typeReflectionMetadata, "_strmTypeRefs");
SystemClassMember<VariableDefn> rmd_strmDefns(
    Builtins::typeReflectionMetadata, "_strmDefns");
SystemClassMember<VariableDefn> rmd_nameIndex(
    Builtins::typeReflectionMetadata, "_nameIndex");
SystemClassMember<VariableDefn> rmd_defnType(
    Builtins::typeReflectionMetadata, "_defnType");
SystemClassMember<VariableDefn> rmd_traits(
    Builtins::typeReflectionMetadata, "_traits");
SystemClassMember<VariableDefn> rmd_derivedTypes(
    Builtins::typeReflectionMetadata, "_derivedTypes");
SystemClassMember<VariableDefn> rmd_compositeTypes(
    Builtins::typeReflectionMetadata, "_compositeTypes");
SystemClassMember<VariableDefn> rmd_enumTypes(
    Builtins::typeReflectionMetadata, "_enumTypes");
SystemClassMember<VariableDefn> rmd_invokeFns(
    Builtins::typeReflectionMetadata, "_invokeFns");
SystemClassMember<VariableDefn> rmd_methods(
    Builtins::typeReflectionMetadata, "_methods");

SystemClassMember<TypeDefn> rmd_CallAdapterFnType(
    Builtins::typeReflectionMetadata, "CallAdapterFn");
SystemClassMember<TypeDefn> rmd_MethodPtrType(
    Builtins::typeReflectionMetadata, "MethodPtr");

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

#if 0
struct InvokeTypeOrder {
  bool operator()(const InvokeArrayElement & k0, const InvokeArrayElement & k1) {
    if (k0.second.useCount != k1.second.useCount) {
      return k0.second.useCount > k1.second.useCount;
    }

    int cmp = StringRef(k0.first.name()).compare(k1.first.name());
    if (cmp != 0) {
      return cmp < 0;
    }

    return LexicalTypeOrdering::compare(k0.first.type(), k1.first.type());
  }
};
#endif

}

#if 0
/// -------------------------------------------------------------------
/// UniqueMethodKey

UniqueMethodKey::UniqueMethodKey(const char * name, const FunctionType * fnType)
  : name_(name)
  , type_(fnType)
{}

const UniqueMethodKey UniqueMethodKey::KeyInfo::getEmptyKey() {
  return UniqueMethodKey("#empty", NULL);
}

const UniqueMethodKey UniqueMethodKey::KeyInfo::getTombstoneKey() {
  return UniqueMethodKey("#tombstone", NULL);
}

unsigned UniqueMethodKey::KeyInfo::getHashValue(const UniqueMethodKey & key) {
  unsigned result = llvm::HashString(StringRef(key.name()));

  if (key.type_ != NULL) {
    result *= 0x5bd1e995;
    result ^= result >> 24;
    result ^= Type::KeyInfo::getHashValue(key.type_);
  }
  return result;
}

bool UniqueMethodKey::KeyInfo::isEqual(const UniqueMethodKey & lhs, const UniqueMethodKey & rhs) {
  if (lhs.type_ == NULL) {
    return rhs.type_ == NULL;
  } else if (rhs.type_ == NULL) {
    return false;
  }

  return llvm::StringRef(lhs.name_).equals(rhs.name_) && lhs.type_->isEqual(rhs.type_);
}
#endif

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
  , invokeFnTableVar_(NULL)
  , mmd_(cg_.nameTable())
{
}

Reflector::~Reflector() {
  // TODO: Delete reflection metadata pointers.
}

GlobalVariable * Reflector::getModulePtr(Module * module) {
  std::string moduleSymbol(".module." + module->linkageName());
  GlobalVarMap::iterator it = globals_.find(moduleSymbol);
  if (it != globals_.end()) {
    return it->second;
  }

  irModule_->addTypeName("tart.reflect.Module", Builtins::typeModule->irType());
  irModule_->addTypeName("tart.reflect.NameTable", Builtins::typeNameTable->irType());
  irModule_->addTypeName("tart.reflect.Method", Builtins::typeMethod->irType());
  irModule_->addTypeName("tart.reflect.Member", Builtins::typeMember->irType());
  GlobalVariable * rfModule = new GlobalVariable(*irModule_, Builtins::typeModule->irType(), true,
      GlobalValue::ExternalLinkage, NULL, moduleSymbol);
  globals_[moduleSymbol] = rfModule;
  return rfModule;
}

GlobalVariable * Reflector::getNameTablePtr(Module * module) {
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

llvm::Constant * Reflector::internSymbol(const llvm::StringRef &Key) {
  return cg_.genStringLiteral(Key, Key);
}

void Reflector::emitModule(Module * module) {
  NameTable & nameTable = cg_.nameTable();

  // See if there are any reflected defns.
  bool hasReflectedDefns = false;
  for (DefnSet::iterator it = module->reflectedDefs().begin();
      it != module->reflectedDefs().end(); ++it) {
    if (!(*it)->isNonreflective()) {
      hasReflectedDefns = true;
      break;
    }
  }

  if (!rmdMap_.empty()) {
    hasReflectedDefns = true;
  }

  if (hasReflectedDefns) {
    GlobalVariable * modulePtr = getModulePtr(module);
    if (!modulePtr->hasInitializer()) {
      ReflectedMembers rfMembers;

      // First visit members which are explicitly declared in this module.
      ReflectionMetadata * rmdForModule = getReflectionMetadata(module);
      //addMembers(module, rmdForModule);

      visitMembers(rfMembers, module);

      while (!mmd_.defnsToExport().empty()) {
        addDefn(mmd_.defnsToExport().next());
      }

      NameTable::Name * qualifiedName = nameTable.addQualifiedName(module->qualifiedName());
      DASSERT(qualifiedName != NULL);

      nameTable.assignIndices();

      StructBuilder sb(cg_);
      sb.createObjectHeader(Builtins::typeModule);
      sb.addField(rmdForModule->var());
      sb.addIntegerField(module_nameIndex, qualifiedName->encodedIndex());
      //sb.addField(emitArray("tart.reflect.Module.", module_types.get(), rfMembers.types));
      sb.addField(emitArray("tart.reflect.Module.", module_methods.get(), rfMembers.methods));
      sb.addNullField(module_methods.type());
      modulePtr->setInitializer(sb.build());
    }

    // If this module is the "Type" module, then also do the built-in types.
    if (module == Builtins::typeObject->typeDefn()->module()) {
      ReflectedMembers rfBuiltins;
      visitMember(rfBuiltins, &VoidType::typedefn);
      visitMember(rfBuiltins, &BoolType::typedefn);
      visitMember(rfBuiltins, &CharType::typedefn);
      visitMember(rfBuiltins, &Int8Type::typedefn);
      visitMember(rfBuiltins, &Int16Type::typedefn);
      visitMember(rfBuiltins, &Int32Type::typedefn);
      visitMember(rfBuiltins, &Int64Type::typedefn);
      visitMember(rfBuiltins, &UInt8Type::typedefn);
      visitMember(rfBuiltins, &UInt16Type::typedefn);
      visitMember(rfBuiltins, &UInt32Type::typedefn);
      visitMember(rfBuiltins, &UInt64Type::typedefn);
      visitMember(rfBuiltins, &FloatType::typedefn);
      visitMember(rfBuiltins, &DoubleType::typedefn);
      visitMember(rfBuiltins, &NullType::typedefn);
    }
  } else {
    nameTable.assignIndices();
  }

  if (!mmd_.invokeMap().empty()) {
    for (TypeMap::iterator it = mmd_.invokeMap().begin(); it != mmd_.invokeMap().end(); ++it) {
      invokeRefs_.push_back(*it);
    }

    std::sort(invokeRefs_.begin(), invokeRefs_.end(), TypeOrder());
    for (unsigned i = 0; i < invokeRefs_.size(); ++i) {
      invokeRefs_[i].second.index = i + 1;
      mmd_.invokeMap()[invokeRefs_[i].first].index = i + 1;
    }

#if 0
    diag.debug() << invokeMap_.size() << " unique invoke map entries added";
    for (TypeArray::iterator it = invokeRefs_.begin(); it != invokeRefs_.end(); ++it) {
      diag.debug() << Format_Type << "   " << it->second.useCount << " " << it->first <<
          (cast<FunctionType>(it->first)->isStatic() ? " [static]" : "");
    }
#endif
  }

  for (DefnSet::iterator it = module->reflectedDefs().begin();
      it != module->reflectedDefs().end(); ++it) {
    if (const TypeDefn * td = dyn_cast<TypeDefn>(*it)) {
      emitTypeDefn(td);
    }
  }

  emitNameTable(module);
  emitCallAdapterFnTable(module);

  for (ReflectedSymbolMap::const_iterator it = rmdMap_.begin(); it != rmdMap_.end(); ++it) {
    emitReflectedDefn(it->second, it->first);
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

void Reflector::emitCallAdapterFnTable(Module * module) {
  std::string invokeTableSymbol;
  if (invokeRefs_.empty()) {
    invokeTableSymbol = ".invoke_fns.empty";
  } else {
    invokeTableSymbol = ".invoke_fns." + module->linkageName();
  }

  const llvm::Type * invokeFnType = cg_.getCallAdapterFnType();
  const llvm::PointerType * invokeFnPtrType =
      cg_.getCallAdapterFnType()->getPointerTo();

  ConstantList invokeFnList;
  invokeFnList.push_back(llvm::ConstantPointerNull::get(invokeFnPtrType));
  for (TypeArray::const_iterator it = invokeRefs_.begin(); it != invokeRefs_.end(); ++it) {
    const FunctionType * ft = cast<FunctionType>(it->first);
    // TODO: Temporarily disable union types
    if (ft->returnType()->typeClass() != Type::Union) {
      invokeFnList.push_back(cg_.genCallAdapterFn(ft));
    }
  }

  Constant * invokeFnArray = ConstantArray::get(
      ArrayType::get(invokeFnPtrType, invokeFnList.size()),
      invokeFnList);
  GlobalVariable * invokeFnListPtr = new GlobalVariable(*irModule_,
      invokeFnArray->getType(), true, GlobalValue::LinkOnceODRLinkage,
      invokeFnArray, invokeTableSymbol);
  llvm::Constant * gepIndices[2];
  gepIndices[0] = gepIndices[1] = cg_.getInt32Val(0);
  invokeFnTableVar_ = llvm::ConstantExpr::getInBoundsGetElementPtr(
      invokeFnListPtr, &gepIndices[0], 2);
}

void Reflector::addDefn(const Defn * def) {
  if (def->isNonreflective() || !isExport(def)) {
    return;
  }

#if 0
  diag.debug() << "Adding metadata for def " << def;
#endif

  // Add all of the members of this definition
  switch (def->defnType()) {
    case Defn::Typedef: {
      const TypeDefn * td = static_cast<const TypeDefn *>(def);
      const Type * type = td->typeValue();
      switch (type->typeClass()) {
        case Type::Class:
        case Type::Struct:
        case Type::Interface:
        case Type::Protocol: {
          ReflectionMetadata * rmd = getReflectionMetadata(def);
          diag.indent();
          cg_.nameTable().addQualifiedName(def->qualifiedName())->use();
          if (def->isTemplateInstance()) {
            rmd->addTypeRef(cast<TypeDefn>(def->templateInstance()->templateDefn())->typeValue());
            rmd->addTypeRef(def->templateInstance()->typeArgs());
          } else {
            const CompositeType * ctype = static_cast<const CompositeType *>(type);
            if (def->isTemplate()) {
              const TemplateSignature * tsig = def->templateSignature();
              for (const Defn * param = tsig->paramScope().firstMember(); param != NULL;
                  param = param->nextInScope()) {
                cg_.nameTable().addName(param->name())->use();
              }

              for (ClassList::const_iterator it = ctype->bases().begin();
                  it != ctype->bases().end(); ++it) {
                rmd->addTypeRef(*it);
              }
              //addASTDecl(def->ast());
            } else {
              for (ClassList::const_iterator it = ctype->bases().begin();
                  it != ctype->bases().end(); ++it) {
                rmd->addTypeRef(*it);
              }

              addMembers(ctype->memberScope(), rmd);
            }
          }
          diag.unindent();
          break;
        }

        case Type::Enum: {
          cg_.nameTable().addName(def->name())->use();
          const EnumType * etype = static_cast<const EnumType *>(type);
#if DEBUG_VERBOSE
          diag.debug() << "Emitting metadata for enum " << etype;
          diag.indent();
#endif
          //addMembers(etype->memberScope(), rmd);
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
      ReflectionMetadata * rmd = getReflectionMetadata(def);
      cg_.nameTable().addQualifiedName(def->qualifiedName())->use();
      const NamespaceDefn * ns = static_cast<const NamespaceDefn *>(def);
#if DEBUG_VERBOSE
      diag.debug() << "Emitting metadata for namespace " << def;
      diag.indent();
#endif
      addMembers(&ns->memberScope(), rmd);
#if DEBUG_VERBOSE
      diag.unindent();
#endif
      break;
    }

    case Defn::Mod: {
      ReflectionMetadata * rmd = getReflectionMetadata(def);
      const Module * mod = static_cast<const Module *>(def);
#if DEBUG_VERBOSE
      diag.debug() << "Emitting metadata for module " << def;
      diag.indent();
#endif
      addMembers(mod, rmd);
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

void Reflector::addMembers(const IterableScope * scope, ReflectionMetadata * rmd) {
  for (const Defn * m = scope->firstMember(); m != NULL; m = m->nextInScope()) {
    if (!m->isNonreflective()) {
      addMember(m, rmd);
    }
  }
}

void Reflector::addMember(const Defn * def, ReflectionMetadata * rmd) {
  NameTable & names = cg_.nameTable();

  // Add all of the members of this definition
  switch (def->defnType()) {
    case Defn::Typedef: {
      const TypeDefn * td = static_cast<const TypeDefn *>(def);
      const Type * type = td->typeValue();
      rmd->addTypeRef(type);
      break;
    }

    case Defn::Namespace: {
      mmd_.defnsToExport().append(def);
      break;
    }

    case Defn::Var:
    case Defn::Let: {
      const VariableDefn * v = static_cast<const VariableDefn *>(def);
      cg_.nameTable().addName(v->name())->use();
      rmd->addTypeRef(v->type());
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
#if DEBUG_VERBOSE
      diag.debug() << "Emitting metadata for property " << def;
#endif
      break;
    }

    case Defn::Function:
    /*case Defn::Macro:*/ {
      const FunctionDefn * fn = static_cast<const FunctionDefn *>(def);
      const FunctionType * fnType = fn->functionType();
      if (!fn->isIntrinsic() && !fn->isNonreflective() && fn->isSingular()) {
        cg_.nameTable().addName(fn->name())->use();
        DASSERT(fn->functionType());
        rmd->addTypeRef(fnType);
        for (ParameterList::const_iterator it = fn->functionType()->params().begin();
            it != fn->functionType()->params().end(); ++it) {
          const ParameterDefn * p = *it;
          cg_.nameTable().addName(p->name())->use();
        }

        const Type * selfType = fnType->selfParam() != NULL ? fnType->selfParam()->type() : NULL;
        // For now, we only support reflected invocation of global, class, or interface methods.
        if (/*fn->isSingular() &&*/ !fn->isUnsafe() &&
            (selfType == NULL ||
                selfType->typeClass() == Type::Class ||
                selfType->typeClass() == Type::Interface)) {
          TagInfo & info = mmd_.invokeMap()[fnType];
          info.useCount++;
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
  DASSERT_OBJ(!def->isNonreflective(), def);
  ReflectedSymbolMap::const_iterator it = rmdMap_.find(def);
  if (it != rmdMap_.end()) {
    return it->second;
  }

  if (isExport(def)) {
    mmd_.defnsToExport().append(def);
  }

  ReflectionMetadata * rsym = new ReflectionMetadata(def, mmd_);
  std::string metaVarName(".meta." + def->linkageName());
  if (def->defnType() == Defn::Mod) {
    metaVarName = ".meta.module." + def->linkageName();
  }
  rsym->setVar(new GlobalVariable(*irModule_, Builtins::typeReflectionMetadata->irType(),
      false,
      def->isSynthetic() ? GlobalValue::LinkOnceAnyLinkage : GlobalValue::ExternalLinkage,
      NULL, metaVarName));
  rmdMap_[def] = rsym;
  return rsym;
}

void Reflector::emitReflectedDefn(ReflectionMetadata * rmd, const Defn * def) {
  GlobalVariable * nameTableVar = getNameTablePtr(cg_.module());

  if (!isExport(def)) {
    return;
  }

  rmd->assignIndices();

  // Generate the module constants structure
  StructBuilder sb(cg_);
  sb.createObjectHeader(Builtins::typeReflectionMetadata);
  sb.addNullField(rmd_value.type());
  sb.addField(nameTableVar);

  // Generate the stream of encoded derived types.
  std::string derivedTypesData;
  raw_string_ostream derivedTypesStream(derivedTypesData);
  rmd->encodeTypesTable(derivedTypesStream);

  // Generate the stream of definition data.
  raw_ostream & strm = rmd->strm();
  char defnTypeId = 0;
  NameTable::Name * qname = NULL;
  if (def->isTemplateInstance()) {
    defnTypeId = TAG_DEF_TEMPLATE_INST;
    emitTemplateSection(rmd, def);
    emitTypeParamsSection(rmd, def);
    qname = cg_.nameTable().getQualifiedName(def->qualifiedName());
    DASSERT_OBJ(qname != NULL, def);
    //qname = NULL;
    strm << char(0);
    // TODO: Encode type args.
  } else if (def->isSynthetic()) {
    qname = cg_.nameTable().getQualifiedName(def->qualifiedName());
    strm << char(0);
    // It's a subtype of a template - how to deal?
  } else {
    qname = cg_.nameTable().getQualifiedName(def->qualifiedName());
    DASSERT_OBJ(qname != NULL, def);
    if (const Module * mod = dyn_cast<Module>(def)) {
      defnTypeId = TAG_DEF_MODULE;
      emitAttributeSection(rmd, mod->attrs());
      emitReflectedMembers(rmd, mod);
    } else if (const NamespaceDefn * ns = dyn_cast<NamespaceDefn>(def)) {
      defnTypeId = TAG_DEF_NAMESPACE;
      emitAttributeSection(rmd, ns->attrs());
      emitReflectedMembers(rmd, &ns->memberScope());
    } else if (const TypeDefn * td = dyn_cast<TypeDefn>(def)) {
      const Type * type = td->typeValue();
      if (const CompositeType * ctype = dyn_cast<CompositeType>(type)) {
        switch (ctype->typeClass()) {
          case Type::Class: defnTypeId = TAG_DEF_CLASS; break;
          case Type::Struct: defnTypeId = TAG_DEF_STRUCT; break;
          case Type::Interface: defnTypeId = TAG_DEF_INTERFACE; break;
          case Type::Protocol: defnTypeId = TAG_DEF_PROTOCOL; break;
          default:
            DFAIL("Illegal state");
            break;
        }
        emitAttributeSection(rmd, td->attrs());
        if (td->isTemplate()) {
          emitTemplateParamsSection(rmd, def);
        }
        emitBaseClassSection(rmd, ctype);
        emitReflectedMembers(rmd, ctype->memberScope());
      } else if (const EnumType * etype = dyn_cast<EnumType>(type)) {
        defnTypeId = TAG_DEF_ENUM;
        strm << char(TAG_DEF_ENUM) << VarInt(qname->encodedIndex());
        emitAttributeSection(rmd, td->attrs());
      }
    }

    strm << char(0);
  }

#if DEBUG_VERBOSE
  rmd->dump();
  if (!derivedTypesData.empty()) {
    diag.debug() << "  " << derivedTypesData.size() << " bytes of derived type information added.";
  }
#endif

  // Generate the table of TypeInfoBlocks referred to by this module.
  ConstantList classRefList;
  const ReflectionMetadata::TypeArray & compositeTypes = rmd->compositeTypeRefs();
  for (ReflectionMetadata::TypeArray::const_iterator it = compositeTypes.begin();
      it != compositeTypes.end(); ++it) {
    classRefList.push_back(cg_.getTypeInfoBlockPtr(cast<CompositeType>(it->first)));
  }

  // Generate the table of TypeInfoBlocks referred to by this module.
  ConstantList enumRefList;
  const ReflectionMetadata::TypeArray & enumTypes = rmd->enumTypeRefs();
  for (ReflectionMetadata::TypeArray::const_iterator it = enumTypes.begin();
      it != enumTypes.end(); ++it) {
    enumRefList.push_back(cg_.getEnumInfoBlock(cast<EnumType>(it->first)));
  }

  // Write out encoded derived type stream
  if (!derivedTypesData.empty()) {
    std::string derivedTypesSymbol(".derived_typerefs." + def->linkageName());
    Constant * derivedTypes = ConstantArray::get(context_, derivedTypesData, false);
    GlobalVariable * derivedTypesVar = new GlobalVariable(*irModule_, derivedTypes->getType(),
        true, GlobalValue::InternalLinkage, derivedTypes, derivedTypesSymbol);

    sb.addField(llvm::ConstantExpr::getPointerCast(derivedTypesVar, builder_.getInt8PtrTy()));
  } else {
    sb.addNullField(rmd_strmTypeRefs.type());
  }

  // Definition stream
  strm.flush();
  if (rmd->strmData().empty()) {
    sb.addNullField(rmd_strmDefns.type());
  } else {
    std::string defnStrmSymbol(".meta_defn." + def->linkageName());
    Constant * defnStrm = ConstantArray::get(context_, rmd->strmData(), false);
    GlobalVariable * defnStrmVar = new GlobalVariable(*irModule_, defnStrm->getType(),
        true, GlobalValue::InternalLinkage, defnStrm, defnStrmSymbol);
    sb.addField(llvm::ConstantExpr::getPointerCast(defnStrmVar, builder_.getInt8PtrTy()));
  }

  // Definition info
  sb.addIntegerField(rmd_nameIndex, qname ? qname->encodedIndex() : 0);
  sb.addIntegerField(rmd_defnType, defnTypeId);
  sb.addIntegerField(rmd_traits, memberTraits(def));

  // Derived type array is initially null.
  sb.addNullField(rmd_derivedTypes.type());

  // Write out the list of TIB references
  if (!classRefList.empty()) {
    const llvm::PointerType * tibPointerType =
        Builtins::typeTypeInfoBlock.irType()->getPointerTo();
    Constant * compositeTypeArray = ConstantArray::get(
        ArrayType::get(tibPointerType, classRefList.size()),
        classRefList);
    std::string encodedCompositesSymbol(".composite_typerefs." + def->linkageName());
    GlobalVariable * classRefListPtr = new GlobalVariable(*irModule_,
        compositeTypeArray->getType(), true, GlobalValue::InternalLinkage,
        compositeTypeArray, encodedCompositesSymbol);

    sb.addField(llvm::ConstantExpr::getPointerCast(
        classRefListPtr, tibPointerType->getPointerTo()));
  } else {
    sb.addNullField(rmd_compositeTypes.type());
  }

  // Write out the list of EIB references
  if (!enumRefList.empty()) {
    const llvm::PointerType * eibPointerType =
        Builtins::typeEnumInfoBlock.irType()->getPointerTo();
    Constant * enumTypeArray = ConstantArray::get(
        ArrayType::get(eibPointerType, enumRefList.size()),
        enumRefList);
    std::string encodedEnumSymbol(".enum_typerefs." + def->linkageName());
    GlobalVariable * enumRefListPtr = new GlobalVariable(*irModule_,
        enumTypeArray->getType(), true, GlobalValue::InternalLinkage,
        enumTypeArray, encodedEnumSymbol);

    sb.addField(llvm::ConstantExpr::getPointerCast(
        enumRefListPtr, eibPointerType->getPointerTo()));
  } else {
    sb.addNullField(rmd_enumTypes.type());
  }

  sb.addField(invokeFnTableVar_);

  if (!rmd->methodTable().empty()) {
    Constant * methodTable = ConstantArray::get(
        ArrayType::get(rmd_MethodPtrType.get()->typeValue()->irType(), rmd->methodTable().size()),
        rmd->methodTable());
    std::string methodTableSymbol(".methods." + def->linkageName());
    GlobalVariable * methodTableVar = new GlobalVariable(*irModule_, methodTable->getType(), true,
        GlobalValue::InternalLinkage, methodTable, methodTableSymbol);
    sb.addField(llvm::ConstantExpr::getPointerCast(
        methodTableVar, cast<llvm::PointerType>(rmd_methods.type()->irType())));
  } else {
    sb.addNullField(rmd_methods.type());
  }

  rmd->var()->setInitializer(sb.build(Builtins::typeReflectionMetadata->irType()));
}

void Reflector::emitReflectedMembers(ReflectionMetadata * rmd, const IterableScope * scope) {
  DefnList namespaces;
  TypeList innerTypes;
  DefnList fields;
  DefnList properties;
  MethodList methods;

  for (Defn * de = scope->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->isNonreflective()) {
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

      case Defn::Macro:
        break;

      case Defn::Namespace:
        break;

      case Defn::Typedef:
        innerTypes.push_back(static_cast< TypeDefn * const>(de)->typeValue());
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

  if (!innerTypes.empty()) {
    std::sort(innerTypes.begin(), innerTypes.end(), LexicalTypeOrdering());
    std::string typeData;
    raw_string_ostream typeStrm(typeData);

    for (TypeList::const_iterator it = innerTypes.begin(); it != innerTypes.end(); ++it) {
      // TODO Implement
    }

    typeStrm.flush();
    if (!typeData.empty()) {
      rmd->strm() << char(TAG_SECTION_INNER_TYPES) << VarInt(typeData.size()) << typeData;
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
    std::sort(methods.begin(), methods.end(), DefnOrder());
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

    }

    attrStrm.flush();
    if (!attrData.empty()) {
      rmd->strm() << char(TAG_SECTION_ATTRIBUTES) << VarInt(attrData.size()) << attrData;
    }
  }
}

void Reflector::emitTypeParamsSection(ReflectionMetadata * rmd, const Defn * def) {
  const TemplateInstance * ts = def->templateInstance();
  if (ts != NULL) {
    std::string paramData;
    raw_string_ostream paramStrm(paramData);

    for (TupleType::const_iterator it = ts->typeArgs()->begin(); it != ts->typeArgs()->end();
        ++it) {
      rmd->encodeTypeRef(*it, paramStrm);
    }

    paramStrm.flush();
    if (!paramData.empty()) {
      rmd->strm() << char(TAG_SECTION_TYPE_PARAMS) << VarInt(paramData.size()) << paramData;
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

void Reflector::emitBaseClassSection(ReflectionMetadata * rmd, const CompositeType * type) {
  if (type->super() != NULL) {
    std::string baseClassData;
    raw_string_ostream baseClassStrm(baseClassData);
    rmd->encodeTypeRef(type->super(), baseClassStrm);
    baseClassStrm.flush();
    rmd->strm() << char(TAG_SECTION_BASE_CLASS) << VarInt(baseClassData.size()) << baseClassData;
  }
}

void Reflector::emitInterfacesSection(ReflectionMetadata * rmd, const CompositeType * type) {
  if (!type->bases().empty()) {
    std::string ifaceData;
    raw_string_ostream ifaceStrm(ifaceData);
    for (ClassList::const_iterator it = type->bases().begin(); it != type->bases().end(); ++it) {
      if (*it != type->super()) {
        rmd->encodeTypeRef(*it, ifaceStrm);
      }
    }
    ifaceStrm.flush();
    if (!ifaceData.empty()) {
      rmd->strm() << char(TAG_SECTION_INTERFACES) << VarInt(ifaceData.size()) << ifaceData;
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

//void Reflector::emitInnerTypeDefn(ReflectionMetadata * rmd, const Defn * def) {
//}

void Reflector::emitNamespaceDefn(ReflectionMetadata * rmd, const NamespaceDefn * def,
    raw_ostream & out) {
}

void Reflector::emitFieldDefn(ReflectionMetadata * rmd, const VariableDefn * def, raw_ostream & out) {

}

void Reflector::emitConstructorDefn(ReflectionMetadata * rmd, const FunctionDefn * def,
    raw_ostream & out) {

}

void Reflector::emitMethodDefn(ReflectionMetadata * rmd, const FunctionDefn * fn, raw_ostream & out) {
  if (!fn->isSingular() || fn->isNonreflective()) {
    // TODO: Implement.
    return;
  }

  // Add the method to the method table.
  if (fn->isAbstract() || fn->isUndefined() || fn->isIntrinsic() ||
      fn->isInterfaceMethod() || !fn->isSingular()) {
    rmd->methodTable().push_back(
        ConstantPointerNull::get(
            cast<llvm::PointerType>(rmd_MethodPtrType.get()->typeValue()->irType())));
  } else  {
    llvm::Constant * fnVal = cg_.genFunctionValue(fn);
    rmd->methodTable().push_back(
        llvm::ConstantExpr::getBitCast(fnVal, rmd_MethodPtrType.get()->typeValue()->irType()));
  }

  char tag = TAG_DEF_METHOD;
  if (fn->defnType() == Defn::Macro) {
    tag = TAG_DEF_MACRO;
  } else if (fn->isOverride()) {
    tag = TAG_DEF_OVERRIDE;
  } else if (fn->isUndefined()) {
    tag = TAG_DEF_UNDEF;
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

  // Definition tag and name
  out << tag << flags << VarInt(name->encodedIndex());

  // Return type and parameter types.
  rmd->encodeTypeRef(fn->functionType(), out);

  if (fn->dispatchIndex() != -1) {
  }

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
  }

  // method tag
  // Name index
  // Function type tuple index
  // method code index (NO)
  // invoke function index.

  out << char(TAG_DEF_SCOPE_END);
}

void Reflector::emitPropertyDefn(ReflectionMetadata * rmd, const PropertyDefn * def, raw_ostream & out) {
}

GlobalVariable * Reflector::getTypePtr(const Type * type) {
//  if (type->typeDefn() != NULL && type->typeClass() != Type::Primitive &&
//      module()->reflectedDefs().count(type->typeDefn()) == 0) {
//    diag.fatal() << Format_Verbose << "Attempting to reference reflection type " << type <<
//        " but it has not been listed as a reflected type.";
//  }

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

    if (type->typeClass() != Type::Primitive &&
        module()->exportDefs().count(td) == 0 &&
        module()->importDefs().count(td) == 0) {
      diag.fatal() << Format_Verbose << "Attempting to reference type " << type <<
          " but it has not been imported into this module.";
    }

    if ((td->isSynthetic() || td->isNonreflective()) &&
        module()->exportDefs().count(td) > 0) {
      linkageType = GlobalValue::LinkOnceODRLinkage;
      if (module()->reflectedDefs().count(type->typeDefn()) == 0) {
        diag.fatal() << Format_Verbose << "Attempting to reflect synthetic type " << td <<
            " but it has not been listed as a reflected type.";
      }
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
  for (const Defn * m = scope->firstMember(); m != NULL; m = m->nextInScope()) {
    if (module()->reflectedDefs().count(const_cast<Defn *>(m)) && !visitMember(rm, m)) {
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
      if (member->isNonreflective()) {
        return true;
      }

      const FunctionDefn * fn = static_cast<const FunctionDefn *>(member);
      if (!fn->isIntrinsic() && fn->isSingular()) {
        Constant * method = emitMethod(fn);
        if (method != NULL) {
          GlobalVariable * rMethod = new GlobalVariable(*irModule_, method->getType(), true,
              GlobalValue::InternalLinkage, method, ".method." + fn->linkageName());
          if (fn->isCtor()) {
            rm.constructors.push_back(rMethod);
          } else {
            rm.methods.push_back(rMethod);
          }
        }
      }
      break;
    }

    case Defn::Macro:
    case Defn::Parameter:
    case Defn::Mod:
    case Defn::ExplicitImport:
    default:
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
    VariableDefn * emptyArray = cast_or_null<VariableDefn>(
        arrayType->memberScope()->lookupSingleMember("emptyArray"));
    if (emptyArray != NULL) {
      return cast<llvm::Constant>(cg_.genLetValue(emptyArray));
      //GlobalVariable * eaVar = cast<GlobalVariable>(cg_.genVarValue(emptyArray));
      //return eaVar->getInitializer();
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

llvm::Constant * Reflector::emitMethod(const FunctionDefn * func) {
  StructBuilder sb(cg_);
  sb.addField(emitMember(Builtins::typeMethod.get(), func));
  sb.addNullField(method_typeParams.type());
  sb.addField(getTypePtr(func->functionType()));
  sb.addNullField(method_params.type());

  if (func->isAbstract() || func->isUndefined() || func->isIntrinsic() ||
      func->isInterfaceMethod()) {
    sb.addNullField(method_methodPointer.type());
  } else  {
    llvm::Constant * fnVal = cg_.genFunctionValue(func);
    sb.addField(llvm::ConstantExpr::getBitCast(fnVal, method_methodPointer.type()->irType()));
  }
  return sb.build(Builtins::typeMethod->irType());
}

llvm::Constant * Reflector::emitMember(const CompositeType * structType, const ValueDefn * def) {
  TypeDefn * parent = def->enclosingClassDefn();
  Module * module = def->module();
  StructBuilder sb(cg_);
  sb.createObjectHeader(structType);
  sb.addField(internSymbol(def->name()));
  sb.addField(internSymbol(def->linkageName()));
  sb.addIntegerField(member_kind, memberKind(def));
  sb.addIntegerField(member_visibility, memberVisibility(def));
  sb.addIntegerField(member_traits, memberTraits(def));
  sb.addField(emitAttributeArray(def->linkageName(), def->attrs()));
  return sb.build(Builtins::typeMember->irType());
}

llvm::Constant * Reflector::emitTypeReference(const Type * type) {
  return llvm::ConstantExpr::getPointerCast(
      getTypePtr(type), Builtins::typeType->irEmbeddedType());
}

const llvm::Type * Reflector::reflectedTypeOf(const Type * type) {
  switch (type->typeClass()) {
    case Type::Primitive:
      return Builtins::typePrimitiveType->irType();

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
      if (type->typeDefn()->isNonreflective()) {
        return Builtins::typePrimitiveType->irType();
      } else {
        if (type->typeDefn()->isSynthetic() &&
            module()->exportDefs().count(type->typeDefn()) == 0) {
          diag.fatal() << "Attempting to use reflected type of synthetic type " << type <<
              " but it has not been imported into the module.";
        }

        return Builtins::typeCompositeType->irType();
      }

    case Type::Enum:
      return Builtins::typeEnumType->irType();

    case Type::Function:
      return Builtins::typeFunctionType->irType();

    case Type::Tuple:
    case Type::Union:
    case Type::NAddress:
    case Type::NArray:
    case Type::FlexibleArray:
      return Builtins::typeDerivedType->irType();

    default:
      DFAIL("Invalid type");
  }
}

llvm::Constant * Reflector::emitType(const Type * type) {
  switch (type->typeClass()) {
    case Type::Primitive:
      return emitSimpleType(Builtins::typePrimitiveType, static_cast<const PrimitiveType *>(type));

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
      if (type->typeDefn()->isNonreflective()) {
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
    case Type::NArray:
    case Type::FlexibleArray:
      return emitDerivedType(type);

    default:
      DFAIL("Invalid type");
  }
}

llvm::Constant * Reflector::emitCompositeType(const CompositeType * type) {
  DASSERT_OBJ(!type->typeDefn()->isNonreflective(), type);
  if (type->typeDefn()->isSynthetic() &&
      module()->exportDefs().count(type->typeDefn()) == 0) {
    diag.fatal() << "Attempting to reflect synthetic type " << type <<
        " but it has not been imported into the module.";
  }

  // Don't reflect non-retained attributes.
  if (type->isAttribute() && !type->attributeInfo().isRetained()) {
    return NULL;
  }

  std::string qname = type->typeDefn()->linkageName();
  ReflectedMembers rfMembers;

  // First visit members which are explicitly declared in this module.
  visitMembers(rfMembers, type->memberScope());

  TypeKind kind;
  switch (type->typeClass()) {
    case Type::Class: kind = CLASS; break;
    case Type::Struct: kind = STRUCT; break;
    case Type::Interface: kind = INTERFACE; break;
    case Type::Protocol: kind = PROTOCOL; break;
    default:
      DFAIL("Invalid type");
  }

  // Type base
  StructBuilder sb(cg_);
  sb.addField(emitTypeBase(Builtins::typeCompositeType, kind));
  sb.addField(getReflectionMetadata(type->typeDefn())->var());
  sb.addField(llvm::ConstantExpr::getTrunc(
      llvm::ConstantExpr::getSizeOf(type->irType()), builder_.getInt32Ty()));

  // Pointer to TIB for this type.
  sb.addField(cg_.getTypeInfoBlockPtr(type));

  // Superclass
  sb.addNullField(complexType_superType.type());
//  if (type->super() != NULL) {
//    sb.addField(getTypePtr(type->super()));
//  } else {
//    sb.addNullField(complexType_superType.type());
//  }

  // Interface list
  ConstantList interfaces;
  for (ClassList::const_iterator it = type->bases().begin(); it != type->bases().end(); ++it) {
    const CompositeType * base = *it;
    if (base->typeClass() == Type::Interface) {
      interfaces.push_back(getTypePtr(base));
    }
  }

  sb.addField(emitArray(qname, complexType_interfaces.get(), interfaces));
  sb.addNullField(complexType_typeParams.type());
  sb.addField(emitAttributeArray(qname, type->typeDefn()->attrs()));
  sb.addField(emitArray(qname, complexType_fields.get(), rfMembers.fields));
  sb.addField(emitArray(qname, complexType_properties.get(), rfMembers.properties));
  sb.addField(emitArray(qname, complexType_ctors.get(), rfMembers.constructors));
  sb.addField(emitArray(qname, complexType_methods.get(), rfMembers.methods));
  sb.addField(emitArray(qname, complexType_innerTypes.get(), rfMembers.types));

  /** The allocator function for this type. */
  if (type->typeClass() == Type::Class) {
    llvm::Function * alloc = cg_.getTypeAllocator(type);
    if (alloc != NULL) {
      sb.addField(llvm::ConstantExpr::getPointerCast(alloc,
          complexType_alloc.type()->irEmbeddedType()));
    } else {
      sb.addNullField(complexType_alloc.type());
    }
  } else {
    sb.addNullField(complexType_alloc.type());
  }

  if (type->typeClass() == Type::Class) {
    const FunctionDefn * ctor = type->noArgConstructor();
    if (ctor != NULL) {
      llvm::Function * fn = cg_.genFunctionValue(ctor);
      sb.addField(llvm::ConstantExpr::getPointerCast(fn,
          complexType_noArgCtor.type()->irEmbeddedType()));
    } else {
      sb.addNullField(complexType_noArgCtor.type());
    }
  } else {
    // TODO: Structs can be constructed, but must be boxed afterwards.
    sb.addNullField(complexType_noArgCtor.type());
  }

  return sb.build(Builtins::typeCompositeType->irType());
}

llvm::Constant * Reflector::emitEnumType(const EnumType * type) {
  StructBuilder sb(cg_);
  sb.addField(emitTypeBase(Builtins::typeEnumType, ENUM));
  sb.addNullField(enumType_meta.type());
  sb.addIntegerField(enumType_nameIndex, 0);
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
    sb.addField(emitTypeReference(type->selfParam()->type()));
  } else {
    sb.addNullField(functionType_selfType.type());
  }
  sb.addField(emitTupleType(type->paramTypes()));

  if (type->selfParam() != NULL) {
    const Type * selfType = type->selfParam()->type();
    // For now, we only support reflection of classes and interfaces.
    if (selfType->typeClass() == Type::Class || selfType->typeClass() == Type::Interface) {
      sb.addField(cg_.genCallAdapterFn(type));
    } else {
      sb.addNullField(functionType_invoke.type());
    }
  } else {
    sb.addField(cg_.genCallAdapterFn(type));
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
  sb.addField(emitTypeBase(Builtins::typePrimitiveType, OPAQUE));
  sb.addIntegerField(type_typeKind.get(), NONE);
  sb.addField(internSymbol(type->typeDefn()->linkageName()));
  sb.addField(llvm::ConstantExpr::getTrunc(
      llvm::ConstantExpr::getSizeOf(type->irType()), builder_.getInt32Ty()));
  return sb.build(Builtins::typePrimitiveType->irType());
}

llvm::Constant * Reflector::emitSimpleType(const Type * simpleType, const Type * type) {
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
        case TypeId_UInt8: subtype = UBYTE; break;
        case TypeId_UInt16: subtype = USHORT; break;
        case TypeId_UInt32: subtype = UINT; break;
        case TypeId_UInt64: subtype = ULONG; break;
        case TypeId_Float: subtype = FLOAT; break;
        case TypeId_Double: subtype = DOUBLE; break;
        case TypeId_Null: subtype = NULLTYPE; break;
        //case TypeId_LongDouble: subtype = VOID; break;
        default:
          DFAIL("Invalid subtype");
      }

      break;
    }

    case Type::Enum: kind = ENUM; break;

    default:
      DFAIL("Invalid type");
  }

  StructBuilder sb(cg_);
  DASSERT_OBJ(type->typeDefn() != NULL, type);
  sb.addField(emitTypeBase(simpleType, kind));
  sb.addIntegerField(type_typeKind.get(), subtype);
  sb.addField(internSymbol(type->typeDefn()->linkageName()));
  sb.addField(llvm::ConstantExpr::getTrunc(
      llvm::ConstantExpr::getSizeOf(type->irType()), builder_.getInt32Ty()));
  return sb.build(Builtins::typePrimitiveType->irType());
}

llvm::Constant * Reflector::emitTypeBase(const Type * typeBase, TypeKind kind) {
  StructBuilder sb(cg_);
  sb.createObjectHeader(typeBase);
  sb.addIntegerField(type_typeKind.get(), kind);
  return sb.build(Builtins::typeType->irType());
}

llvm::Constant * Reflector::emitTupleType(const TupleType * types) {
  // Get cached version if already generated.
  std::string typeTupleName(".tuple");
  typeLinkageName(typeTupleName, types);
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
  DASSERT_OBJ(arrayType->passes().isFinished(CompositeType::RecursiveFieldTypePass), derivedType_typeParams);

  if (values.empty()) {
    // TODO: point to shared empty array.
  }

  StructBuilder sb(cg_);
  sb.createObjectHeader(arrayType);
  sb.addField(cg_.getIntVal(values.size()));
  sb.addArrayField(elementType, values);

  llvm::Constant * arrayStruct = sb.build();
  GlobalVariable * array = new GlobalVariable(*irModule_, arrayStruct->getType(),
      true, GlobalValue::LinkOnceODRLinkage, arrayStruct, typeTupleName);
  return llvm::ConstantExpr::getPointerCast(array, arrayType->irEmbeddedType());
}

llvm::Constant * Reflector::emitAttributeArray(
    const std::string & baseName, const ExprList & attrs) {
  ConstantList attrInstances;
  std::string attrArrayName(".attrs.");
  attrArrayName.append(baseName);

  for (ExprList::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
    const Expr * e = *it;
    const CompositeType * ctype = cast<CompositeType>(e->type());
    DASSERT(ctype->isAttribute());
    if (ctype->attributeInfo().isRetained()) {
      if (const ConstantObjectRef * cobj = dyn_cast<ConstantObjectRef>(e)) {
        llvm::Constant * attr = cg_.genConstRef(e, "", false);
        attr = llvm::ConstantExpr::getPointerCast(attr, Builtins::typeObject->irEmbeddedType());
        attrInstances.push_back(attr);
      } else {
        diag.error(e) << "Non-constant attribute (not implemented).";
      }
    }
  }

  return emitArray(attrArrayName, member_attributes.get(), attrInstances);
}

Reflector::Visibility Reflector::memberVisibility(const Defn * member) {
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
  //if (member->isFinal()) {
  //  memberTraits |= FINAL;
  //}

  //if (member->isAbstract()) {
  //  memberTraits |= ABSTRACT;
  //}

  if (member->storageClass() == Storage_Static || member->storageClass() == Storage_Global) {
    memberTraits |= STATIC;
  }

  return Traits(memberTraits);
}

Module * Reflector::module() { return cg_.module(); }

bool Reflector::isExport(const Defn * de) {
  return de->module() == module() || de->isSynthetic();
}

} // namespace tart

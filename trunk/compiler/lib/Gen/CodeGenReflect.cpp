/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/CodeGenerator.h"
#include "tart/Gen/StructBuilder.h"

#include "tart/CFG/Module.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/CompositeType.h"

#include "tart/Objects/Builtins.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/Module.h"

namespace tart {

using namespace llvm;

namespace {
  // Keep these enums in sync with Member.tart
  enum Access {
    PUBLIC,
    PRIVATE,
    PROTECTED,
  };

  enum MemberKind {
    FIELD,
    PROPERTY,
    METHOD,
    CONSTRUCTOR,
  };

  enum Traits {
    FINAL     = (1 << 0),
    ABSTRACT  = (1 << 1),
    STATIC    = (1 << 2),
  };

  Access memberAccess(const Defn * member) {
    switch (member->visibility()) {
      case Public: return PUBLIC;
      case Protected: return PROTECTED;
      case Private: return PRIVATE;
      default:
        DFAIL("Illegal state");
    }
  }

  MemberKind memberKind(const Defn * member) {
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

  Traits memberTraits(const Defn * member) {
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
}

GlobalVariable * CodeGenerator::createModuleObjectPtr() {
  if (moduleObject_ == NULL) {
    irModule_->addTypeName("tart.reflect.Module", Builtins::typeModule->irType());
    moduleObject_ = new GlobalVariable(*irModule_, Builtins::typeModule->irType(), true,
        GlobalValue::ExternalLinkage, NULL, module_->linkageName() + ".module");
  }

  return moduleObject_;
}

void CodeGenerator::createModuleObject() {
  createModuleObjectPtr();
  if (!moduleObject_->hasInitializer()) {
    StructBuilder sb(*this);
    sb.createObjectHeader(Builtins::typeModule);
    sb.addStringField(module_->qualifiedName());
    sb.addNullField(Builtins::rfModule.memberTypes->type().type());
    //sb.addNullField(Builtins::rfModule.memberMethods->type());

    {
      StructBuilder ss(*this);
      StructBuilder ss2(*this);
      const llvm::Type * methodListType = Builtins::rfModule.memberMethods->type().irType();
      ss.addField(ConstantPointerNull::get(cast<PointerType>(methodListType->getContainedType(0)->getContainedType(0))));
      ss2.addField(ss.build());
      sb.addField(ss2.build());
    }

    llvm::Constant * moduleStruct = sb.build();
    moduleObject_->setInitializer(moduleStruct);
  }
}

void CodeGenerator::createModuleTable() {
  if (moduleTable_ == NULL) {
    Constant * mod = createModuleObjectPtr();
    Constant * moduleArray = ConstantArray::get(ArrayType::get(mod->getType(), 1), &mod, 1);
    moduleTable_ = new GlobalVariable(*irModule_, moduleArray->getType(), true,
        GlobalValue::AppendingLinkage, moduleArray, "tart.reflect.Module.moduleList");
  }
}

void CodeGenerator::createModuleCount() {
  createModuleTable();

  // TODO: Move this.
  Constant * indices[2];
  indices[0] = indices[1] = getInt32Val(0);

  Constant * moduleArraySize = llvm::ConstantExpr::getSizeOf(moduleTable_->getType());
  //Constant * moduleArraySize = llvm::ConstantExpr::getSizeOf(
  //    llvm::ConstantExpr::getInBoundsGetElementPtr(moduleTable_, indices, 1)->getType());
  moduleArraySize = llvm::ConstantExpr::getTruncOrBitCast(moduleArraySize, builder_.getInt32Ty());
  //Constant * moduleElementSize = llvm::ConstantExpr::getSizeOf(
  //    llvm::ConstantExpr::getInBoundsGetElementPtr(moduleTable_, indices, 2)->getType());
  //Constant * moduleCount64 =
  //    llvm::ConstantExpr::getUDiv(moduleArraySize, moduleElementSize);
  //Constant * moduleCount =
  //    llvm::ConstantExpr::getTruncOrBitCast(moduleCount64, builder_.getInt32Ty());

  new GlobalVariable(*irModule_, builder_.getInt32Ty(), true,
      GlobalValue::ExternalLinkage, moduleArraySize, "tart.reflect.Module.moduleCount");

  indices[0] = getInt32Val(1);
  indices[1] = getInt32Val(1);
  Constant * moduleArrayEnd = llvm::ConstantExpr::getGetElementPtr(moduleTable_, indices, 2);
  new GlobalVariable(*irModule_, moduleArrayEnd->getType(), true,
      GlobalValue::ExternalLinkage, moduleArrayEnd, "tart.reflect.Module.moduleListEnd");
}

llvm::Constant * CodeGenerator::genReflectionDataArray(
    const std::string & baseName, const VariableDefn * var, const ConstantList & values)
{
  const CompositeType * arrayType = cast<CompositeType>(var->type().type());
  irModule_->addTypeName(arrayType->typeDefn()->linkageName(), arrayType->irType());
  DASSERT_OBJ(arrayType->typeDefn()->isPassFinished(Pass_ResolveOverloads), var);

  StructBuilder sb(*this);
  sb.createObjectHeader(arrayType);
  sb.addField(getInt32Val(values.size()));
  sb.addArrayField(var, values);

  llvm::Constant * arrayStruct = sb.build();
  GlobalVariable * array = new GlobalVariable(*irModule_,
      arrayStruct->getType(), true, GlobalValue::InternalLinkage, arrayStruct,
      baseName + ".type." + var->name());
  return llvm::ConstantExpr::getPointerCast(array, arrayType->irEmbeddedType());
}

llvm::Constant * CodeGenerator::reflectMethod(const FunctionDefn * func) {
  StructBuilder sb(*this);
  sb.addField(reflectMember(cast<CompositeType>(Builtins::typeMethod), func));
  return sb.build();
}

#if 0
llvm::Constant * CodeGenerator::reflectProperty(const PropertyDefn * func) {

}

llvm::Constant * CodeGenerator::reflectField(const VariableDefn * func) {

}
#endif

llvm::Constant * CodeGenerator::reflectMember(const CompositeType * structType, const ValueDefn * def) {
  TypeDefn * parent = def->enclosingClassDefn();

  StructBuilder sb(*this);
  sb.createObjectHeader(structType);
  sb.addStringField(def->qualifiedName());
  sb.addTypeReference(def->type().type());
  sb.addIntegerField(Builtins::rfMember.memberKind, memberKind(def));
  sb.addTypeReference(parent != NULL ? parent->typeValue() : NULL);
  sb.addIntegerField(Builtins::rfMember.memberAccess, memberAccess(def));
  sb.addIntegerField(Builtins::rfMember.memberTraits, memberTraits(def));
  sb.addField(genReflectionDataArray(
      module_->qualifiedName(),
      Builtins::rfMember.memberAttributes,
      ConstantList()));

  return sb.build();
}

} // namespace tart

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

void CodeGenerator::createModuleObject() {
  createModuleObjectPtr();
  if (!moduleObject_->hasInitializer()) {
    StructBuilder sb(*this);
    sb.createObjectHeader(Builtins::typeModule);
    sb.addStringField(module_->qualifiedName());
    sb.addNullField(Builtins::rfModule.memberTypes->type());
    sb.addNullField(Builtins::rfModule.memberMethods->type());
    llvm::Constant * moduleStruct = sb.build();
    moduleObject_->setInitializer(moduleStruct);
  }
}

llvm::Constant * CodeGenerator::genReflectionDataArray(
    const std::string & baseName, const VariableDefn * var, const ConstantList & values)
{
  const CompositeType * arrayType = cast<CompositeType>(var->type());
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
  sb.addTypeReference(def->type());
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

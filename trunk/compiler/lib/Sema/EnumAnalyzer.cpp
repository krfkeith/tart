/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Sema/EnumAnalyzer.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Module.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"

#include <llvm/Instructions.h>

namespace tart {

EnumAnalyzer::EnumAnalyzer(TypeDefn * de)
  : DefnAnalyzer(de->module(), de->definingScope())
  , target_(de)
  , prevValue_(NULL)
  , minValue_(NULL)
  , maxValue_(NULL)
  , intValueType_(NULL)
{
  DASSERT(de != NULL);
}

bool EnumAnalyzer::analyze() {
  if (!analyzeEnum()) {
    return false;
  }
}

bool EnumAnalyzer::analyzeEnum() {
  if (target_->isPassRunning(Pass_ResolveBaseTypes)) {
    diag.fatal(target_) << "Circular inheritance not allowed";
    return false;
  }

  if (!target_->beginPass(Pass_CreateMembers)) {
    return true;
  }
  
  if (!resolveAttributes(target_)) {
    return false;
  }
  
  EnumType * enumType = cast<EnumType>(target_->getTypeValue());
  bool isFlags = target_->findAttribute("tart.core.FlagsAttribute") != NULL;
  enumType->setIsFlags(isFlags);

  const ASTTypeDecl * ast = cast<const ASTTypeDecl>(target_->getAST());
  DASSERT_OBJ(enumType->isSingular(), enumType);
  DASSERT_OBJ(enumType->baseType() == NULL, enumType);

  // Analyze the base type of the enum.
  const PrimitiveType * intValueType_ = &IntType::instance;
  //const Type * superType = ast->getSuper();
  if (!ast->bases().empty()) {
    // For the moment, we require enums to be derived from integer types only.
    DASSERT(ast->bases().size() == 1);
    TypeAnalyzer ta(module, activeScope);
    Type * baseType = ta.typeFromAST(ast->bases().front());
    if (baseType != NULL) {
      intValueType_ = cast<PrimitiveType>(baseType);
      if (intValueType_ == NULL || !isIntegerType(intValueType_->getTypeId())) {
        diag.fatal(ast) << "Enumerations can only derive from integer types.";
        return false;
      }
    }
  }

  enumType->setBaseType(intValueType_);

  // Define any custom operators for this enumerated type.
  defineOperators();

  // Mark as finished so that we don't recurse when referring to members.
  target_->finishPass(Pass_CreateMembers);

  Scope * savedScope = setActiveScope(enumType->memberScope());
  const ASTDeclList & members = ast->members();
  for (ASTDeclList::const_iterator it = members.begin(); it != members.end(); ++it) {
    if (!createEnumConstant(cast<ASTVarDecl>(*it))) {
      return false;
    }
  }

  if (!isFlags && minValue_ != NULL) {
    VariableDefn * minDef = new VariableDefn(Defn::Let, module, "minVal", minValue_);
    minDef->setType(enumType);
    minDef->setLocation(target_->location());
    minDef->finishPass(Pass_CreateMembers);
    minDef->finishPass(Pass_ResolveVarType);
    enumType->memberScope()->addMember(minDef);

    VariableDefn * maxDef = new VariableDefn(Defn::Let, module, "maxVal", maxValue_);
    maxDef->setType(enumType);
    maxDef->setLocation(target_->location());
    maxDef->finishPass(Pass_CreateMembers);
    maxDef->finishPass(Pass_ResolveVarType);
    enumType->memberScope()->addMember(maxDef);
  }

  setActiveScope(savedScope);
  return true;
}

bool EnumAnalyzer::createEnumConstant(const ASTVarDecl * ast) {
  DefnList dlist;
  if (activeScope->lookupMember(ast->getName(), dlist, false)) {
    diag.error(ast) << "Definition of '" << ast << "' conflicts with earlier definition";
    diag.info(dlist.front()) << "defined here.";
    return false;
  }

  EnumType * enumType = cast<EnumType>(target_->getTypeValue());
  bool isFlags = enumType->isFlags();
  VariableDefn * ec = new VariableDefn(Defn::Let, module, ast);
  ConstantInteger * value = NULL;
  if (ast->getValue() != NULL) {
    // The constant has an explicit value.
    ExprAnalyzer ea(module, activeScope);
    ConstantExpr * enumValue = ea.reduceConstantExpr(ast->getValue(), intValueType_);
    if (isErrorResult(enumValue)) {
      return false;
    }
    
    if (!isa<ConstantInteger>(enumValue)) {
      diag.fatal(ast) << "Not an integer constant " << enumValue;
    }
    
    value = static_cast<ConstantInteger *>(enumValue);
    assert(value->value() != NULL);
  } else {
    // No explicit value, use the previous value.
    llvm::ConstantInt * irVal;
    if (prevValue_ == NULL) {
      value = ConstantInteger::get(ec->location(), enumType, isFlags ? 1 : 0);
      irVal = value->value();
    } else {
      // Calculate the successor value.
      llvm::ConstantInt * irVal = prevValue_->value();
      const llvm::Type * irType = irVal->getType();
      if (isFlags) {
        // TODO: Also want to mask out any low-order bits.
        if (irVal->isNullValue()) {
          irVal = llvm::ConstantInt::get(irType, 1);
        } else {
          irVal = cast<llvm::ConstantInt>(
              llvm::ConstantExpr::getShl(irVal, llvm::ConstantInt::get(irType, 1)));
        }
      } else {
        irVal = cast<llvm::ConstantInt>(
            llvm::ConstantExpr::getAdd(irVal, llvm::ConstantInt::get(irType, 1)));
      }

      value = ConstantInteger::get(ast->location(), enumType, irVal);
    }
  }

  ec->setLocation(ast->location());
  ec->addTrait(Defn::Singular);
  ec->setInitValue(value);
  ec->setType(enumType);
  ec->finishPass(Pass_CreateMembers);
  ec->finishPass(Pass_ResolveVarType);
  enumType->memberScope()->addMember(ec);

  prevValue_ = value;

  // Compute min and max values.
  if (!isFlags) {
    if (minValue_ == NULL) {
      minValue_ = maxValue_ = value;
    } else {
      llvm::Constant * cval = value->value();
      llvm::ConstantInt * cmin = minValue_->value();
      llvm::ConstantInt * cmax = maxValue_->value();
      if (llvm::ConstantExpr::getCompare(llvm::ICmpInst::ICMP_SLT, cval, cmin)->isNullValue()) {
        minValue_ = value;
      }
      if (llvm::ConstantExpr::getCompare(llvm::ICmpInst::ICMP_SLT, cval, cmax)->isNullValue()) {
        maxValue_ = value;
      }
    }
  }
  
  return true;
}

void EnumAnalyzer::defineOperators() {
  EnumType * type = cast<EnumType>(target_->getTypeValue());
  if (type->isFlags()) {
    //Builtins::addOperator(new EnumOpFunction(this, "bitAnd", llvm::Instruction::And));
    //Builtins::addOperator(new EnumOpFunction(this, "bitOr", llvm::Instruction::Or));
    //Builtins::addOperator(new EnumOpFunction(this, "bitXor", llvm::Instruction::Xor));
  } else {
  }
}

}

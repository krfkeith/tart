/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/EnumAnalyzer.h"
#include "tart/CFG/EnumType.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/Module.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Objects/Builtins.h"

#include "llvm/Instructions.h"

namespace tart {

class EnumContainsFunction : public FunctionDefn {
public:
  EnumContainsFunction(Module * m, EnumType * type)
    : FunctionDefn(m, ASTIdent::operatorContains.value(), createFunctionType(m, type))
    , type_(type)
  {
    addTrait(Defn::Synthetic);
    addTrait(Defn::Final);
    addTrait(Defn::Singular);
    addTrait(Defn::Nonreflective);
    setStorageClass(Storage_Global);
    createQualifiedName(m);
  }

  static FunctionType * createFunctionType(Module * m, EnumType * type) {
    ParameterList params;
    params.push_back(new ParameterDefn(m, "e1", type, 0));
    params.push_back(new ParameterDefn(m, "e2", type, 0));
    return new FunctionType(&BoolType::instance, params);
  }

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    assert(args.size() == 2);
    Expr * arg0 = args[0];
    Expr * arg1 = args[1];
    Expr * result;
    Type * baseType = const_cast<Type *>(type_->baseType());

    if (arg0->exprType() == Expr::ConstInt && arg1->exprType() == Expr::ConstInt) {
      ConstantInteger * c0 = static_cast<ConstantInteger *>(arg0);
      ConstantInteger * c1 = static_cast<ConstantInteger *>(arg1);
      DASSERT(c0->type() == c1->type());
      result = new ConstantInteger(
            c0->location() | c1->location(),
            baseType,
            cast<llvm::ConstantInt>(llvm::ConstantExpr::getAnd(c0->value(), c1->value())));
    } else {
      result = new BinaryOpcodeExpr(llvm::Instruction::And, loc, baseType, arg0, arg1);
    }

    return BoolType::instance.explicitCast(loc, result);
  }

private:
  EnumType * type_;
};

class EnumBinaryFunction : public FunctionDefn {
public:
  EnumBinaryFunction(Module * m, EnumType * type, ASTIdent * id,
      llvm::Instruction::BinaryOps opCode)
    : FunctionDefn(m, id->value(), createFunctionType(m, type))
    , type_(type)
    , opCode_(opCode)
  {
    addTrait(Defn::Synthetic);
    addTrait(Defn::Final);
    addTrait(Defn::Singular);
    addTrait(Defn::Nonreflective);
    setStorageClass(Storage_Global);
    createQualifiedName(m);
  }

  static FunctionType * createFunctionType(Module * m, EnumType * type) {
    ParameterList params;
    params.push_back(new ParameterDefn(m, "e1", type, 0));
    params.push_back(new ParameterDefn(m, "e2", type, 0));
    return new FunctionType(type, params);
  }

  Expr * eval(const SourceLocation & loc, Expr * self, const ExprList & args) const {
    assert(args.size() == 2);
    Expr * arg0 = args[0];
    Expr * arg1 = args[1];
    if (arg0->exprType() == Expr::ConstInt && arg1->exprType() == Expr::ConstInt) {
      ConstantInteger * c0 = static_cast<ConstantInteger *>(arg0);
      ConstantInteger * c1 = static_cast<ConstantInteger *>(arg1);
      DASSERT(c0->type() == c1->type());
      return new ConstantInteger(
            c0->location() | c1->location(),
            type_,
            cast<llvm::ConstantInt>(llvm::ConstantExpr::get(opCode_, c0->value(), c1->value())));
    } else {
      return new BinaryOpcodeExpr(opCode_, loc, type_, arg0, arg1);
    }
  }

private:
  EnumType * type_;
  llvm::Instruction::BinaryOps opCode_;
};

EnumAnalyzer::EnumAnalyzer(TypeDefn * de)
  : DefnAnalyzer(de->module(), de->definingScope(), de)
  , target_(de)
  , prevValue_(NULL)
  , minValue_(NULL)
  , maxValue_(NULL)
  , intValueType_(NULL)
{
  DASSERT(de != NULL);
}

bool EnumAnalyzer::analyze() {
  return analyzeEnum();
}

bool EnumAnalyzer::analyzeEnum() {
  if (!target_->beginPass(Pass_CreateMembers)) {
    return true;
  }

  EnumType * enumType = cast<EnumType>(target_->typeValue());
  if (target_->parentDefn() == Builtins::typeAttribute->typeDefn()) {
    // Don't evaluate the attributes if the enclosing class is Attribute, because that creates
    // a circular dependency. For now, assume that any Enum defined within Attribute that has
    // any attributes at all is a Flags enum.
    if (!target_->ast()->attributes().empty()) {
      enumType->setIsFlags(true);
    }
    target_->finishPass(Pass_ResolveAttributes);
  } else if (!resolveAttributes(target_)) {
    return false;
  }

  bool isFlags = enumType->isFlags();

  const ASTTypeDecl * ast = cast<const ASTTypeDecl>(target_->ast());
  DASSERT_OBJ(enumType->isSingular(), enumType);
  DASSERT_OBJ(enumType->baseType() == NULL, enumType);

  // Analyze the base type of the enum.
  const PrimitiveType * intValueType = &IntType::instance;
  //const Type * superType = ast->super();
  if (!ast->bases().empty()) {
    // For the moment, we require enums to be derived from integer types only.
    DASSERT(ast->bases().size() == 1);
    TypeAnalyzer ta(module, activeScope);
    Type * baseType = ta.typeFromAST(ast->bases().front());
    if (baseType != NULL) {
      intValueType = cast<PrimitiveType>(baseType);
      if (intValueType == NULL || !isIntegerType(intValueType->typeId())) {
        diag.fatal(ast) << "Enumerations can only derive from integer types.";
        return false;
      }
    }
  }

  enumType->setBaseType(intValueType);

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
    minDef->passes().finish(VariableDefn::AttributePass);
    minDef->passes().finish(VariableDefn::VariableTypePass);
    minDef->passes().finish(VariableDefn::InitializerPass);
    minDef->passes().finish(VariableDefn::CompletionPass);
    enumType->memberScope()->addMember(minDef);

    VariableDefn * maxDef = new VariableDefn(Defn::Let, module, "maxVal", maxValue_);
    maxDef->setType(enumType);
    maxDef->setLocation(target_->location());
    maxDef->passes().finish(VariableDefn::AttributePass);
    maxDef->passes().finish(VariableDefn::VariableTypePass);
    maxDef->passes().finish(VariableDefn::InitializerPass);
    maxDef->passes().finish(VariableDefn::CompletionPass);
    enumType->memberScope()->addMember(maxDef);
  }

  setActiveScope(savedScope);
  return true;
}

bool EnumAnalyzer::createEnumConstant(const ASTVarDecl * ast) {
  DefnList dlist;
  if (activeScope->lookupMember(ast->name(), dlist, false)) {
    diag.error(ast) << "Definition of '" << ast << "' conflicts with earlier definition";
    diag.info(dlist.front()) << "defined here.";
    return false;
  }

  EnumType * enumType = cast<EnumType>(target_->typeValue());
  bool isFlags = enumType->isFlags();
  VariableDefn * ec = new VariableDefn(Defn::Let, module, ast);
  ConstantInteger * value = NULL;
  if (ast->value() != NULL) {
    // The constant has an explicit value.
    ExprAnalyzer ea(module, activeScope, subject());
    ConstantExpr * enumValue = ea.reduceConstantExpr(ast->value(), intValueType_);
    if (isErrorResult(enumValue)) {
      return false;
    }

    if (!isa<ConstantInteger>(enumValue)) {
      diag.fatal(ast) << "Not an integer constant " << enumValue;
    }

    value = ConstantInteger::get(ast->location(), enumType,
        static_cast<ConstantInteger *>(enumValue)->value());
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
          irVal = llvm::ConstantInt::get(cast<llvm::IntegerType>(irType), 1, true);
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
  ec->passes().finish(VariableDefn::AttributePass);
  ec->passes().finish(VariableDefn::VariableTypePass);
  ec->passes().finish(VariableDefn::InitializerPass);
  ec->passes().finish(VariableDefn::CompletionPass);
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
  EnumType * type = cast<EnumType>(target_->typeValue());
  Scope * parentScope = target_->definingScope();
  DASSERT(parentScope != NULL);

  if (type->isFlags()) {
    Module * m = target_->module();
    parentScope->addMember(new EnumContainsFunction(m, type));
    parentScope->addMember(
        new EnumBinaryFunction(m, type, &ASTIdent::operatorBitAnd, llvm::Instruction::And));
    parentScope->addMember(
        new EnumBinaryFunction(m, type, &ASTIdent::operatorBitOr, llvm::Instruction::Or));
    parentScope->addMember(
        new EnumBinaryFunction(m, type, &ASTIdent::operatorBitXor, llvm::Instruction::Xor));
  } else {
  }
}

}

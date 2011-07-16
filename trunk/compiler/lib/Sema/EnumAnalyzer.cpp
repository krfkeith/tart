/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"

#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Module.h"

#include "tart/Type/EnumType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/FunctionType.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"

#include "tart/Sema/EnumAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Meta/MDReader.h"

#include "llvm/Instructions.h"

namespace tart {

class EnumConstructor : public FunctionDefn {
public:
  EnumConstructor(Module * m, EnumType * type)
    : FunctionDefn(m, istrings.idCreate, createFunctionType(m, type))
    , type_(type)
  {
    addTrait(Defn::Synthetic);
    addTrait(Defn::Singular);
    setFlag(Final);
    setStorageClass(Storage_Static);
    setParentDefn(type->typeDefn());
    createQualifiedName(type->typeDefn());
  }

  static FunctionType * createFunctionType(Module * m, EnumType * type) {
    ParameterList params;
    params.push_back(new ParameterDefn(m, "value", type->baseType(), 0));
    FunctionType * ft = new FunctionType(type, params);
    return ft;
  }

  Expr * eval(const SourceLocation & loc, Module * callingModules, Expr * self,
      const ExprList & args) const {
    DASSERT(args.size() == 1);
    return type_->explicitCast(loc, args[0], Conversion::Coerce);
  }

private:
  EnumType * type_;
};

class EnumContainsFunction : public FunctionDefn {
public:
  EnumContainsFunction(Module * m, EnumType * type)
    : FunctionDefn(m, ASTIdent::operatorContains.value(), createFunctionType(m, type))
    , type_(type)
  {
    addTrait(Defn::Synthetic);
    addTrait(Defn::Singular);
    setFlag(Final);
    setStorageClass(Storage_Instance);
    setParentDefn(type->typeDefn());
    createQualifiedName(type->typeDefn());
  }

  static FunctionType * createFunctionType(Module * m, EnumType * type) {
    ParameterList params;
    params.push_back(new ParameterDefn(m, "eval", type, 0));
    FunctionType * ft = new FunctionType(&BoolType::instance, params);
    ft->setSelfParam(new ParameterDefn(m, "self", type, ParameterDefn::Reference));
    return ft;
  }

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 1);
    Expr * arg0 = args[0];
    Expr * result;
    const Type * baseType = type_->baseType();

    if (arg0->exprType() == Expr::ConstInt && self->exprType() == Expr::ConstInt) {
      ConstantInteger * c0 = static_cast<ConstantInteger *>(arg0);
      ConstantInteger * c1 = static_cast<ConstantInteger *>(self);
      DASSERT(c0->type() == c1->type());
      result = new ConstantInteger(
            c0->location() | c1->location(),
            baseType,
            cast<llvm::ConstantInt>(llvm::ConstantExpr::getAnd(c0->value(), c1->value())));
    } else {
      result = new BinaryOpcodeExpr(llvm::Instruction::And, loc, baseType, arg0, self);
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
    addTrait(Defn::Singular);
    setFlag(Final);
    setStorageClass(Storage_Global);
    setParentDefn(type->typeDefn());
    createQualifiedName(m);
  }

  static FunctionType * createFunctionType(Module * m, EnumType * type) {
    ParameterList params;
    params.push_back(new ParameterDefn(m, "e1", type, 0));
    params.push_back(new ParameterDefn(m, "e2", type, 0));
    return new FunctionType(type, params);
  }

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
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

class EnumToStringMethod : public FunctionDefn {
public:
  EnumToStringMethod(Module * m, EnumType * type)
    : FunctionDefn(m, "toString", createFunctionType(m, type))
    , type_(type)
  {
    addTrait(Defn::Synthetic);
    addTrait(Defn::Singular);
    setFlag(Final);
    setStorageClass(Storage_Instance);
    setParentDefn(type->typeDefn());
    createQualifiedName(type->typeDefn());
  }

  static FunctionType * createFunctionType(Module * m, EnumType * type) {
    FunctionType * ft = new FunctionType(Builtins::typeString, NULL, 0);
    ft->setSelfParam(new ParameterDefn(m, "self", type, ParameterDefn::Reference));
    return ft;
  }

  Expr * eval(const SourceLocation & loc, Module * callingModule, Expr * self,
      const ExprList & args) const {
    assert(args.size() == 0);
    if (self->exprType() == Expr::ConstInt) {
      ConstantInteger * ci = static_cast<ConstantInteger *>(self);
      for (Defn * de = type_->memberScope()->firstMember(); de != NULL; de = de->nextInScope()) {
        if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
          if (ConstantInteger * cEnumVal = dyn_cast_or_null<ConstantInteger>(var->initValue())) {
            if (cEnumVal->isEqual(ci)) {
              return new ConstantString(var->location(), var->name());
            }
          }
        }
      }

      diag.error(loc) << "Invalid enumeration constant: " << self;
      return NULL;
    } else {
      return NULL;
    }
  }

private:
  EnumType * type_;
};

EnumAnalyzer::EnumAnalyzer(TypeDefn * de)
  : DefnAnalyzer(de->module(), de->definingScope(), de, NULL)
  , target_(de)
  , prevValue_(NULL)
  , intValueType_(NULL)
{
  DASSERT(de != NULL);
}

static const EnumType::PassSet PASS_SET_RESOLVETYPE = EnumType::PassSet::of(
  EnumType::AttributePass,
  EnumType::BaseTypePass,
  EnumType::ScopeCreationPass
);

static const EnumType::PassSet PASS_SET_COMPLETE = EnumType::PassSet::of(
  EnumType::AttributePass,
  EnumType::BaseTypePass,
  EnumType::ScopeCreationPass,
  EnumType::OperatorCreationPass
);

bool EnumAnalyzer::analyze(AnalysisTask task) {
  switch (task) {
    case Task_PrepTypeComparison:
    case Task_PrepConversion:
      return runPasses(PASS_SET_RESOLVETYPE);

    default:
      return runPasses(PASS_SET_COMPLETE);
  }
  return runPasses(EnumType::PassSet::of(EnumType::AttributePass, EnumType::ScopeCreationPass));
}

bool EnumAnalyzer::runPasses(EnumType::PassSet passesToRun) {
  // Work out what passes need to be run.
  EnumType * type = cast<EnumType>(target_->typeValue());
  passesToRun.removeAll(type->passes().finished());
  if (passesToRun.empty()) {
    return true;
  }

  if (passesToRun.contains(EnumType::AttributePass)) {
    if (target_->parentDefn() == Builtins::typeAttribute->typeDefn()) {
      // Don't evaluate the attributes if the enclosing class is Attribute, because that creates
      // a circular dependency. For now, assume that any Enum defined within Attribute that has
      // any attributes at all is a Flags enum.
      if (target_->mdNode() == NULL && !target_->ast()->attributes().empty()) {
        type->setIsFlags(true);
      }
    } else if (type->passes().begin(EnumType::AttributePass)) {
      if (!resolveAttributes(target_)) {
        return false;
      }
      type->passes().finish(EnumType::AttributePass);
    }
  }

  if (passesToRun.contains(EnumType::BaseTypePass) &&
      type->passes().begin(EnumType::BaseTypePass)) {
    if (!analyzeBase()) {
      return false;
    }
  }

  if (passesToRun.contains(EnumType::ScopeCreationPass) &&
      type->passes().begin(EnumType::ScopeCreationPass)) {
    if (!createMembers()) {
      return false;
    }
  }

  if (passesToRun.contains(EnumType::OperatorCreationPass) &&
      type->passes().begin(EnumType::OperatorCreationPass)) {
    createOperators();
  }

  return true;
}

bool EnumAnalyzer::analyzeBase() {
  EnumType * enumType = cast<EnumType>(target_->typeValue());
  enumType->passes().finish(EnumType::BaseTypePass);

  DASSERT_OBJ(enumType->isSingular(), enumType);
  DASSERT_OBJ(enumType->baseType() == NULL, enumType);

  // Analyze the base type of the enum.
  intValueType_ = &Int32Type::instance;

  if (target_->mdNode() != NULL) {
    // Read the base type from the metadata
    const Type * base = MDReader(module_, target_).readEnumBase(target_);
    if (base != NULL) {
      intValueType_ = cast<PrimitiveType>(base);
    }
  } else {
    const ASTTypeDecl * ast = cast<const ASTTypeDecl>(target_->ast());
    if (!ast->bases().empty()) {
      // For the moment, we require enums to be derived from integer types only.
      DASSERT(ast->bases().size() == 1);
      TypeAnalyzer ta(module(), activeScope());
      Type * baseType = ta.typeFromAST(ast->bases().front());
      if (baseType != NULL) {
        intValueType_ = dyn_cast<PrimitiveType>(baseType);
        if (intValueType_ == NULL || !intValueType_->isIntType()) {
          diag.fatal(ast) << "Enumerations can only derive from integer types.";
          return false;
        }
      }
    }
  }

  enumType->setBaseType(intValueType_);
  return true;
}

bool EnumAnalyzer::createMembers() {
  EnumType * enumType = cast<EnumType>(target_->typeValue());
  bool success = true;

  // Mark as finished so that we don't recurse when referring to members.
  enumType->passes().finish(EnumType::ScopeCreationPass);

  if (target_->mdNode() != NULL) {
    if (!MDReader(module_, target_).readEnumConstants(target_)) {
      success = false;
    }
  } else {
    Scope * savedScope = setActiveScope(enumType->memberScope());
    const ASTTypeDecl * ast = cast<const ASTTypeDecl>(target_->ast());
    const ASTDeclList & members = ast->members();
    bool success = true;
    for (ASTDeclList::const_iterator it = members.begin(); it != members.end(); ++it) {
      if (!createEnumConstant(cast<ASTVarDecl>(*it))) {
        success = false;
      }
    }
    setActiveScope(savedScope);
  }

  // Compute min and max values.
  if (!enumType->isFlags()) {
    ConstantInteger * minValue = NULL;
    ConstantInteger * maxValue = NULL;

    for (Defn * de = enumType->memberScope()->firstMember(); de != NULL; de = de->nextInScope()) {
      if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
        ConstantInteger * value = cast<ConstantInteger>(var->initValue());
        if (minValue == NULL) {
          minValue = maxValue = value;
        } else {
          llvm::Constant * cval = value->value();
          llvm::ConstantInt * cmin = minValue->value();
          llvm::ConstantInt * cmax = maxValue->value();
          if (llvm::ConstantExpr::getCompare(llvm::ICmpInst::ICMP_SLT, cmin, cval)->isNullValue()) {
            minValue = value;
          }
          if (llvm::ConstantExpr::getCompare(llvm::ICmpInst::ICMP_SLT, cval, cmax)->isNullValue()) {
            maxValue = value;
          }
        }
      } else if (!de->isSynthetic()) {
        diag.fatal(de) << "Invalid member for enum: " << de;
      }
    }

    if (minValue != NULL) {
      VariableDefn * minDef = new VariableDefn(Defn::Let, module(), "minVal", minValue);
      minDef->setType(enumType);
      minDef->addTrait(Defn::Synthetic);
      minDef->setLocation(target_->location());
      minDef->setStorageClass(Storage_Static);
      minDef->passes().finish(VariableDefn::AttributePass);
      minDef->passes().finish(VariableDefn::VariableTypePass);
      minDef->passes().finish(VariableDefn::InitializerPass);
      minDef->passes().finish(VariableDefn::CompletionPass);
      enumType->memberScope()->addMember(minDef);

      VariableDefn * maxDef = new VariableDefn(Defn::Let, module(), "maxVal", maxValue);
      maxDef->setType(enumType);
      maxDef->addTrait(Defn::Synthetic);
      maxDef->setLocation(target_->location());
      maxDef->setStorageClass(Storage_Static);
      maxDef->passes().finish(VariableDefn::AttributePass);
      maxDef->passes().finish(VariableDefn::VariableTypePass);
      maxDef->passes().finish(VariableDefn::InitializerPass);
      maxDef->passes().finish(VariableDefn::CompletionPass);
      enumType->memberScope()->addMember(maxDef);
    }
  }

  return success;
}

bool EnumAnalyzer::createEnumConstant(const ASTVarDecl * ast) {
  DefnList dlist;
  if (activeScope()->lookupMember(ast->name(), dlist, false)) {
    diag.error(ast) << "Definition of '" << ast << "' conflicts with earlier definition";
    diag.info(dlist.front()) << "defined here.";
    return false;
  }

  EnumType * enumType = cast<EnumType>(target_->typeValue());
  const llvm::Type * irType = enumType->irType();
  bool isFlags = enumType->isFlags();
  bool isSigned = !enumType->baseType()->isUnsignedType();
  VariableDefn * ec = new VariableDefn(Defn::Let, module(), ast);
  ConstantInteger * value = NULL;
  if (ast->value() != NULL) {
    // The constant has an explicit value.
    ExprAnalyzer ea(this, NULL);
    Expr * enumValue = ea.reduceConstantExpr(ast->value(), NULL /*intValueType_*/);
    if (!isErrorResult(enumValue)) {
      if (!isa<ConstantInteger>(enumValue)) {
        diag.fatal(ast) << "Not an integer constant " << enumValue;
      } else {
        llvm::ConstantInt * enumIntVal = static_cast<ConstantInteger *>(enumValue)->value();
        enumIntVal = cast<llvm::ConstantInt>(
            llvm::ConstantExpr::getIntegerCast(enumIntVal, irType, isSigned));
        value = ConstantInteger::get(ast->location(), enumType, enumIntVal);
        assert(value->value() != NULL);
      }
    }
  } else {
    // No explicit value, use the previous value.
    llvm::ConstantInt * irVal;
    if (prevValue_ == NULL) {
      value = ConstantInteger::get(ec->location(), enumType, isFlags ? 1 : 0);
      irVal = value->value();
    } else {
      // Calculate the successor value.
      llvm::ConstantInt * irVal = prevValue_->value();
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

  ec->setStorageClass(Storage_Static);
  ec->setLocation(ast->location());
  ec->addTrait(Defn::Singular);
  ec->setInitValue(value);
  ec->setType(enumType);
  ec->passes().finish(VariableDefn::AttributePass);
  ec->passes().finish(VariableDefn::VariableTypePass);
  ec->passes().finish(VariableDefn::InitializerPass);
  ec->passes().finish(VariableDefn::CompletionPass);
  enumType->memberScope()->addMember(ec);

  if (value == NULL) {
    return false;
  }

  prevValue_ = value;
  return true;
}

void EnumAnalyzer::createOperators() {
  Module * m = target_->module();
  EnumType * type = cast<EnumType>(target_->typeValue());
  Scope * parentScope = target_->definingScope();
  DASSERT(parentScope != NULL);

  type->passes().finish(EnumType::OperatorCreationPass);

  type->memberScope()->addMember(new EnumConstructor(m, type));
  if (type->isFlags()) {
    type->memberScope()->addMember(new EnumContainsFunction(m, type));
    parentScope->addMember(
        new EnumBinaryFunction(m, type, &ASTIdent::operatorBitAnd, llvm::Instruction::And));
    parentScope->addMember(
        new EnumBinaryFunction(m, type, &ASTIdent::operatorBitOr, llvm::Instruction::Or));
    parentScope->addMember(
        new EnumBinaryFunction(m, type, &ASTIdent::operatorBitXor, llvm::Instruction::Xor));
  } else {
    type->memberScope()->addMember(new EnumToStringMethod(m, type));
  }
}

}

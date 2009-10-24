/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Module.h"

#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/EvalPass.h"

#include "tart/Objects/Builtins.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

static const VariableDefn::PassSet PASS_SET_RESOLVETYPE = VariableDefn::PassSet::of(
  VariableDefn::AttributePass,
  VariableDefn::VariableTypePass
);

static const VariableDefn::PassSet PASS_SET_CONSTRUCT = VariableDefn::PassSet::of(
  VariableDefn::AttributePass,
  VariableDefn::VariableTypePass,
  VariableDefn::InitializerPass
);

static const VariableDefn::PassSet PASS_SET_COMPLETE = VariableDefn::PassSet::of(
  VariableDefn::AttributePass,
  VariableDefn::VariableTypePass,
  VariableDefn::InitializerPass,
  VariableDefn::CompletionPass
);

VarAnalyzer::VarAnalyzer(VariableDefn * var)
  : DefnAnalyzer(var->module(), var->definingScope(), var)
  , target(var)
{
  DASSERT(var != NULL);
}

VarAnalyzer::VarAnalyzer(VariableDefn * var, Module * module, Defn * subject)
  : DefnAnalyzer(module, var->definingScope(), subject)
  , target(var)
{
  DASSERT(var != NULL);
}

bool VarAnalyzer::analyze(AnalysisTask task) {
  if (target->isTemplate()) {
    return true;
  }

  switch (task) {
    default:
      return runPasses(PASS_SET_RESOLVETYPE);

    case Task_PrepConversion:
    case Task_PrepConstruction:
    case Task_PrepEvaluation:
    case Task_PrepTypeGeneration:
      return runPasses(PASS_SET_CONSTRUCT);

    case Task_PrepCodeGeneration:
      return runPasses(PASS_SET_COMPLETE);
  }
}

bool VarAnalyzer::runPasses(VariableDefn::PassSet passesToRun) {
  passesToRun.removeAll(target->passes().finished());
  if (passesToRun.empty()) {
    if (!target->type().isDefined()) {
      return false;
    }

    return true;
  }

  if (passesToRun.contains(VariableDefn::AttributePass) &&
      target->passes().begin(VariableDefn::AttributePass)) {
    if (!resolveAttributes(target)) {
      return false;
    }

    target->passes().finish(VariableDefn::AttributePass);
  }

  if (passesToRun.contains(VariableDefn::VariableTypePass) && !resolveVarType()) {
    return false;
  }

  if (passesToRun.contains(VariableDefn::InitializerPass) && !resolveInitializers()) {
    return false;
  }

  if (passesToRun.contains(VariableDefn::CompletionPass) &&
      !analyzeType(target->type(), Task_PrepTypeGeneration)) {
    return false;
  }

  return true;
}

bool VarAnalyzer::resolveVarType() {
  if (target->passes().begin(VariableDefn::VariableTypePass)) {
    const ASTVarDecl * ast = cast_or_null<ASTVarDecl>(target->ast());

    // Evaluate the explicitly declared type, if any
    if (!target->type().isDefined()) {
      DASSERT(ast != NULL);
      if (ast->type() != NULL) {
        TypeAnalyzer ta(module, target->definingScope());
        Type * varType = ta.typeFromAST(ast->type());
        if (varType == NULL) {
          target->passes().finish(VariableDefn::VariableTypePass);
          return false;
        }

        if (varType->isEqual(&VoidType::instance)) {
          diag.error(target) << "Variable type cannot be void";
        } else if (FunctionType * fnType = dyn_cast<FunctionType>(varType)) {
          if (!fnType->isStatic()) {
            varType = new BoundMethodType(fnType);
          }
        }

        //diag.info(target) << "Analyzing type of var '" << target << "' : " << varType;
        setTargetType(varType);
      }
    }

    analyzeType(target->type(), Task_PrepTypeComparison);

    // Evaluate the initializer expression, if any
    if (ast != NULL && ast->value() != NULL) {
      Scope * savedScope = activeScope;
      if (target->type().isDefined()) {
        if (target->type().typeClass() == Type::Enum) {
          // If the initializer is an enumerated type, then add that type's member scope
          // to the list of scopes.
          DelegatingScope * enumScope =
              new DelegatingScope(target->type().type()->memberScope(), activeScope);
          savedScope = setActiveScope(enumScope);
        }
      }

      ExprAnalyzer ea(module, activeScope, subject());
      Expr * initExpr = ea.analyze(ast->value(), target->type().type());
      setActiveScope(savedScope);
      if (isErrorResult(initExpr)) {
        target->passes().finish(VariableDefn::VariableTypePass);
        return false;
      } else if (!initExpr->isSingular()) {
        diag.fatal(initExpr) << "Non-singular expression: " << initExpr;
        DASSERT_OBJ(initExpr->isSingular(), initExpr);
        target->passes().finish(VariableDefn::VariableTypePass);
        return false;
      } else if (initExpr->type()->isEqual(&VoidType::instance)) {
        diag.error(initExpr) << "Attempt to assign void expression '" << initExpr <<
            "' to variable '" << target << "'";
        target->passes().finish(VariableDefn::VariableTypePass);
        return false;
      } else {
        Type * initType = initExpr->type();
        DASSERT_OBJ(initType != NULL, target);

        if (initType->isEqual(&UnsizedIntType::instance)) {
          // TODO: Only if this is a var, not a let
          initType = &IntType::instance;
        }

        if (!target->type().isDefined()) {
          setTargetType(initType);
          analyzeType(initType, Task_PrepTypeComparison);
        }

        // TODO: Fold this into inferTypes.
        initExpr = target->type().implicitCast(initExpr->location(), initExpr);
        target->setInitValue(initExpr);
      }
    }

    DASSERT(target->type().isDefined());
    if (target->type().isSingular()) {
      target->addTrait(Defn::Singular);
    }

    target->passes().finish(VariableDefn::VariableTypePass);
  }


  DASSERT_OBJ(target->type().isNonVoidType(), target);
  return true;
}

void VarAnalyzer::setTargetType(Type * type) {
  target->setType(type);
  if (ParameterDefn * pdef = dyn_cast<ParameterDefn>(target)) {
    pdef->setType(type);
    if (pdef->getFlag(ParameterDefn::Variadic)) {
      pdef->setInternalType(getArrayTypeForElement(type));
    } else {
      pdef->setInternalType(type);
    }
  }
}

bool VarAnalyzer::resolveInitializers() {
  if (target->passes().begin(VariableDefn::InitializerPass)) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(target)) {
      switch (var->storageClass()) {
        case Storage_Static:
        case Storage_Global: {
          Expr * initVal = var->initValue();
          if (initVal) {
            Expr * constInitVal = EvalPass::eval(initVal, true);
            if (constInitVal != NULL) {
              var->setInitValue(constInitVal);
            } else {
              DFAIL("Implement");
            }
          }
        }

        case Storage_Instance:
        case Storage_Local: {
          // For types that are generated by the compiler, add all of the member types
          // to the module so that the compiler can generated instances of those types.
          if (var->parentDefn() == Builtins::typeTypeDescriptor->typeDefn()) {
            if (var->type().defn() != NULL) {
              module->addSymbol(var->type().defn());
            }
          }
          break;
        }

        default:
          DFAIL("IllegalState");
          break;
      }

      target->passes().finish(VariableDefn::InitializerPass);
    }
  }

  return true;
}

}

/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Module.h"

#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/EvalPass.h"

#include "tart/Objects/Builtins.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

static const DefnPasses PASS_SET_RESOLVETYPE = DefnPasses::of(
    Pass_CreateMembers,
    Pass_ResolveAttributes,
    Pass_ResolveVarType);

static const DefnPasses PASS_SET_CODEGEN = DefnPasses::of(
    Pass_CreateMembers,
    Pass_ResolveAttributes,
    Pass_ResolveVarType,
    Pass_ResolveStaticInitializers);

VarAnalyzer::VarAnalyzer(ValueDefn * var)
  : DefnAnalyzer(var->module(), var->definingScope())
  , target(var)
{
  DASSERT(var != NULL);
}

VarAnalyzer::VarAnalyzer(ValueDefn * var, Module * module)
  : DefnAnalyzer(module, var->definingScope())
  , target(var)
{
  DASSERT(var != NULL);
}

bool VarAnalyzer::analyze(AnalysisTask task) {
  if (target->isTemplate()) {
    return true;
  }

  // Work out what passes need to be run.
  DefnPasses passesToRun;
  addPasses(target, passesToRun, PASS_SET_RESOLVETYPE);

  if (task == Task_PrepCodeGeneration) {
    addPasses(target, passesToRun, PASS_SET_CODEGEN);
  }

  // Run passes

  if (passesToRun.empty()) {
    if (!target->type().isDefined()) {
      return false;
    }

    return true;
  }

  DefnAnalyzer::analyze(target, passesToRun);

  if (passesToRun.contains(Pass_ResolveVarType)) {
    if (!resolveVarType()) {
      return false;
    }
  }

  if (passesToRun.contains(Pass_ResolveStaticInitializers)) {
    if (!resolveStaticInitializers()) {
      return false;
    }
  }

  return true;
}

bool VarAnalyzer::resolveVarType() {
  if (target->beginPass(Pass_ResolveVarType)) {
    const ASTVarDecl * ast = cast_or_null<ASTVarDecl>(target->ast());

    // Evaluate the explicitly declared type, if any
    if (!target->type().isDefined()) {
      DASSERT(ast != NULL);
      if (ast->type() != NULL) {
        TypeAnalyzer ta(module, target->definingScope());
        Type * varType = ta.typeFromAST(ast->type());
        if (varType == NULL) {
          target->finishPass(Pass_ResolveVarType);
          return false;
        }

        if (varType->isEqual(&VoidType::instance)) {
          diag.error(target) << "Variable type cannot be void";
        }

        //diag.info(target) << "Analyzing type of var '" << target << "' : " << varType;
        setTargetType(varType);
      }
    }

    analyzeType(target->type().type(), Task_PrepTypeComparison);

    // Evaluate the initializer expression, if any
    if (ast != NULL && ast->value() != NULL) {
      Scope * savedScope = activeScope;
      if (target->type().isDefined()) {
        analyzeType(target->type().type(), Task_PrepTypeComparison);

        if (target->type().typeClass() == Type::Enum) {
          // If the initializer is an enumerated type, then add that type's member scope
          // to the list of scopes.
          DelegatingScope * enumScope =
              new DelegatingScope(target->type().type()->memberScope(), activeScope);
          savedScope = setActiveScope(enumScope);
        }
      }

      ExprAnalyzer ea(module, activeScope);
      Expr * initExpr = ea.analyze(ast->value(), target->type().type());
      setActiveScope(savedScope);
      if (isErrorResult(initExpr)) {
        target->finishPass(Pass_ResolveVarType);
        return false;
      } else if (!initExpr->isSingular()) {
        diag.fatal(initExpr) << "Non-singular expression: " << initExpr;
        DASSERT_OBJ(initExpr->isSingular(), initExpr);
        target->finishPass(Pass_ResolveVarType);
        return false;
      } else if (initExpr->type()->isEqual(&VoidType::instance)) {
        diag.error(initExpr) << "Attempt to assign void expression '" << initExpr <<
            "' to variable '" << target << "'";
        target->finishPass(Pass_ResolveVarType);
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
        if (VariableDefn * vdef = dyn_cast<VariableDefn>(target)) {
          vdef->setInitValue(initExpr);
        } else if (ParameterDefn * pdef = dyn_cast<ParameterDefn>(target)) {
          pdef->setDefaultValue(initExpr);
        }
      }
    }

    DASSERT(target->type().isDefined());
    if (target->type().isSingular()) {
      target->addTrait(Defn::Singular);
    }

    target->finishPass(Pass_ResolveVarType);
  }


  DASSERT_OBJ(target->type().isNonVoidType(), target);
  return true;
}

void VarAnalyzer::setTargetType(Type * type) {
  if (VariableDefn * vdef = dyn_cast<VariableDefn>(target)) {
    vdef->setType(type);
  } else if (ParameterDefn * pdef = dyn_cast<ParameterDefn>(target)) {
    pdef->setType(type);
    if (pdef->getFlag(ParameterDefn::Variadic)) {
      pdef->setInternalType(getArrayTypeForElement(type));
    } else {
      pdef->setInternalType(type);
    }
  } else {
    DFAIL("Invalid operation for type");
  }
}

bool VarAnalyzer::resolveStaticInitializers() {
  if (target->beginPass(Pass_ResolveStaticInitializers)) {
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
            module->addSymbol(var->type().type()->typeDefn());
          }
          break;
        }

        default:
          DFAIL("IllegalState");
          break;
      }

      target->finishPass(Pass_ResolveStaticInitializers);
    }
  }

  return true;
}

}

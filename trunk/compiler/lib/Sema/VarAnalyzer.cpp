/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Sema/VarAnalyzer.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/FinalizeTypesPass.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"

namespace tart {
  
static const DefnPasses PASS_SET_RESOLVETYPE = DefnPasses::of(
  Pass_CreateMembers,
  Pass_ResolveVarType
);

VarAnalyzer::VarAnalyzer(ValueDefn * var)
  : DefnAnalyzer(var->module(), var->definingScope())
  , target(var)
{
  DASSERT(var != NULL);

  //const ASTVarDecl * ast = cast_or_null<ASTVarDecl>(target->getAST());
}

bool VarAnalyzer::analyze(AnalysisTask task) {
  if (target->isTemplate()) {
    return true;
  }

  // Work out what passes need to be run.
  DefnPasses passesToRun;
  addPasses(target, passesToRun, PASS_SET_RESOLVETYPE);
  
  // Run passes

  if (passesToRun.empty()) {
    return true;
  }

  DefnAnalyzer::analyze(target, passesToRun);

  if (passesToRun.contains(Pass_ResolveVarType)) {
    if (!resolveVarType()) {
      return false;
    }
  }
  
  return true;
}

bool VarAnalyzer::resolveVarType() {
  if (target->beginPass(Pass_ResolveVarType)) {
    const ASTVarDecl * ast = cast_or_null<ASTVarDecl>(target->getAST());
    
    // Evaluate the explicitly declared type, if any
    if (target->getType() == NULL) {
      DASSERT(ast != NULL);
      if (ast->getType() != NULL) {
        TypeAnalyzer ta(module, target->definingScope());
        Type * varType = ta.typeFromAST(ast->getType());
        if (varType == NULL) {
          return false;
        }
        
        //diag.info(target) << "Analyzing type of var '" << target << "' : " << varType;
        setTargetType(varType);
      }
    }

    // Evaluate the initializer expression, if any
    if (ast != NULL && ast->getValue() != NULL) {
      ExprAnalyzer ea(module, activeScope);
      Expr * initExpr = ea.reduceExpr(ast->getValue(), target->getType());
      if (isErrorResult(initExpr)) {
        return false;
      }

      if (!initExpr->isSingular()) {
        initExpr = TypeInferencePass::run(initExpr, target->getType());
      }

      initExpr = FinalizeTypesPass::run(initExpr);

      if (isErrorResult(initExpr)) {
        return false;
      } else if (!initExpr->isSingular()) {
        diag.fatal(initExpr) << "Non-singular expression: " << initExpr;
        DASSERT_OBJ(initExpr->isSingular(), initExpr);
      } else {
        Type * initType = initExpr->getType();
        DASSERT_OBJ(initType != NULL, target);
        
        if (initType->isEqual(&UnsizedIntType::instance)) {
          // Only if this is a var, not a let
          initType = &IntType::instance;
        }

        if (!initType->isSingular()) {
          diag.debug() << "Init expression for " << target << " is " <<
            initExpr << " with type " << initType;
        }

        DASSERT_OBJ(initType->isSingular(), initExpr);

        if (target->getType() == NULL) {
          setTargetType(initType);
        }

        initExpr = target->getType()->implicitCast(initExpr->getLocation(), initExpr);
        if (VariableDefn * vdef = dyn_cast<VariableDefn>(target)) {
          vdef->setInitValue(initExpr);
        } else if (ParameterDefn * pdef = dyn_cast<ParameterDefn>(target)) {
          pdef->setDefaultValue(initExpr);
        }
      }
    }
    
    DASSERT(target->getType() != NULL);
    
    if (target->getType()->isSingular()) {
      target->addTrait(Defn::Singular);
    }
    
    target->finishPass(Pass_ResolveVarType);
  }

  return true;
}

void VarAnalyzer::setTargetType(Type * type) {
  if (VariableDefn * vdef = dyn_cast<VariableDefn>(target)) {
    vdef->setType(type);
  } else if (ParameterDefn * pdef = dyn_cast<ParameterDefn>(target)) {
    pdef->setType(type);
  } else {
    DFAIL("Invalid operation for type");
  }
}

}

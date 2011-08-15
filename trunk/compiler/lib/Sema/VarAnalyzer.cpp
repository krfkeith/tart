/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Module.h"

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/NativeType.h"
#include "tart/Sema/TypeTransform.h"

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
  : DefnAnalyzer(var->module(), var->definingScope(), var, NULL)
  , target(var)
  , trace_(isTraceEnabled(var))
{
  DASSERT(var != NULL);
}

VarAnalyzer::VarAnalyzer(VariableDefn * var, Scope * scope, Module * module, Defn * subject,
    FunctionDefn * currentFunction)
  : DefnAnalyzer(module, scope, subject, currentFunction)
  , target(var)
  , trace_(isTraceEnabled(var))
{
  DASSERT(var != NULL);
}

bool VarAnalyzer::analyze(AnalysisTask task) {
  TaskInProgress tip(target, task);

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
    case Task_PrepReflection:
      return runPasses(PASS_SET_CONSTRUCT);

    case Task_PrepCodeGeneration:
      return runPasses(PASS_SET_COMPLETE);
  }
}

bool VarAnalyzer::runPasses(VariableDefn::PassSet passesToRun) {
  passesToRun.removeAll(target->passes().finished());
  if (passesToRun.empty()) {
    if (target->type() == NULL) {
      return false;
    }

    return true;
  }

  if (trace_) {
    diag.debug(target) << Format_Verbose << "Analyzing: " << target;
  }

  AutoIndent A(trace_);

  if (passesToRun.contains(VariableDefn::AttributePass) &&
      target->passes().begin(VariableDefn::AttributePass)) {
    if (trace_) {
      diag.debug() << "Resolve attributes";
    }
    if (!resolveAttributes(target)) {
      return false;
    }

    if (hasAnyRetainedAttrs(target)) {
      target->addTrait(Defn::Reflect);
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
  if (trace_) {
    diag.debug() << "Resolve variable type";
  }

  if (target->passes().begin(VariableDefn::VariableTypePass)) {
    const ASTVarDecl * ast = cast_or_null<ASTVarDecl>(target->ast());

    // Evaluate the explicitly declared type, if any
    if (target->type() == NULL) {
      DASSERT(ast != NULL);
      if (ast->type() != NULL) {
        TypeAnalyzer ta(module_, activeScope_);
        ta.setSubject(subject_);
        Type * varType = ta.typeFromAST(ast->type());
        if (varType == NULL) {
          target->passes().finish(VariableDefn::VariableTypePass);
          return false;
        }

        if (varType->isVoidType()) {
          diag.error(target) << "Variable type cannot be void";
        } else if (FunctionType * fnType = dyn_cast<FunctionType>(varType)) {
          if (!fnType->isStatic()) {
            DFAIL("Shoulda been converted to a Function object");
          }
        }

        //diag.info(target) << "Analyzing type of var '" << target << "' : " << varType;
        setTargetType(varType);
      }
    }

    analyzeType(target->type(), Task_PrepTypeComparison);

    // Evaluate the initializer expression, if any
    if (ast != NULL && ast->value() != NULL) {
      Scope * savedScope = activeScope_;
      if (target->type() != NULL) {
        const Type * targetType = dealias(target->type());
        if (targetType->typeClass() == Type::Enum) {
          // If the initializer is an enumerated type, then add that type's member scope
          // to the list of scopes.
          // TODO: Eliminate the notion of delegating scopes, and instead use a scope stack.
          DelegatingScope * enumScope =
              new DelegatingScope(const_cast<IterableScope *>(
                  targetType->memberScope()), activeScope_);
          savedScope = setActiveScope(enumScope);
        }
      }

      ExprAnalyzer ea(this, currentFunction_);
      Expr * initExpr = ea.analyze(ast->value(),
          (target->type() != NULL ? target->type() : &AnyType::instance), AO_IMPLICIT_CAST);
      setActiveScope(savedScope);
      if (isErrorResult(initExpr)) {
        target->passes().finish(VariableDefn::VariableTypePass);
        return false;
      } else if (!initExpr->isSingular()) {
        diag.fatal(initExpr) << "Non-singular expression: " << initExpr;
        DASSERT_OBJ(initExpr->isSingular(), initExpr);
        target->passes().finish(VariableDefn::VariableTypePass);
        return false;
      } else if (initExpr->type()->isVoidType()) {
        diag.error(initExpr) << "Attempt to assign void expression '" << initExpr <<
            "' to variable '" << target << "'";
        target->passes().finish(VariableDefn::VariableTypePass);
        return false;
      } else {
        const Type * initType = initExpr->type();
        DASSERT_OBJ(initType != NULL, target);

        // TODO: Only if this is a var, not a let
        if (target->type() == NULL) {
          initType = IntegerSizingTransform().transform(initType);
          setTargetType(initType);
          analyzeType(initType, Task_PrepTypeComparison);
        }

        // Special case for native array initializers.
        if (isa<NativeArrayType>(dealias(target->type()))) {
          if (ast->value()->nodeType() == ASTNode::ArrayLiteral) {
            initExpr = initializeNativeArray(initExpr);
          }
        }

        // TODO: Fold this into inferTypes.
        if (target->storageClass() == Storage_Local) {
          initExpr = ea.doImplicitCast(initExpr, target->type(), AO_IMPLICIT_CAST);
        } else {
          initExpr = target->type()->implicitCast(initExpr->location(), initExpr);
        }
        if (!isErrorResult(initExpr) && !isErrorResult(initExpr->type())) {
          DASSERT(initExpr->type()->irType() == target->type()->irType()) << "Variable type is " <<
              target->type() << " but initializer type is " << initExpr->type();
          DASSERT_OBJ(!target->type()->isScaffold(), target);
          target->setInitValue(initExpr);
        }
      }
    } else if (target->type() == NULL) {
      diag.error(target) << "Type of '" << target << "' cannot be determined";
    }

    if (target->type() != NULL && target->type()->isSingular()) {
      target->addTrait(Defn::Singular);
    }

    target->passes().finish(VariableDefn::VariableTypePass);
  }

  if (target->type() == NULL) {
    target->setType(&BadType::instance);
    return false;
  }
  DASSERT_OBJ(!target->type()->isVoidType(), target);
  return true;
}

void VarAnalyzer::setTargetType(const Type * type) {
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
  if (trace_) {
    diag.debug() << "Resolve initializer";
  }

  if (target->passes().begin(VariableDefn::InitializerPass)) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(target)) {
      switch (var->storageClass()) {
        case Storage_Static:
        case Storage_Global: {
          Expr * initVal = var->initValue();
          if (initVal) {
            if (var->defnType() == Defn::Let) {
              Expr * constInitVal = EvalPass::eval(module_, initVal, true);
              if (constInitVal != NULL) {
                var->setInitValue(constInitVal);
              } else {
                diag.debug(initVal) << "Not a constant: " << initVal;
                DFAIL("Implement");
              }
            }
          }
        }

        case Storage_Instance:
        case Storage_Local: {
          // Make sure that the type gets analyzed. We don't need to actually import it
          // however, since declaring a variable to be a type does not imply that we're
          // actually calling any of that type's methods.
          // (NOTE: changed it to add symbol, because queueSymbol has problems - what
          // if you queue it, partially analyze it, and then later addSymbol it
          // again - it doesn't get fully analyzed.)
          TypeDefn * typeDef = var->type()->typeDefn();
          if (typeDef != NULL && typeDef->isSingular()) {
            module_->addSymbol(typeDef);
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

Expr * VarAnalyzer::initializeNativeArray(Expr * initValue) {
  const NativeArrayType * nat = static_cast<const NativeArrayType *>(target->type());
  FnCallExpr * call = cast<FnCallExpr>(initValue);
  ArrayLiteralExpr * alit = cast<ArrayLiteralExpr>(call->arg(0));
  if (target->storageClass() != Storage_Global && target->storageClass() != Storage_Static) {
    diag.error(target) << "Initialization of native arrays is only supported for globals";
    return initValue;
  }

  if (alit->argCount() != nat->size()) {
    diag.error(target) << "Initializing native array of size " << nat->size() <<
        " with " << alit->argCount() << " elements";
    return initValue;
  }

  alit->setType(nat);
  return alit;
}

}

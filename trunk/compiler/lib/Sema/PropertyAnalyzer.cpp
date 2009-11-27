/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Module.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Sema/PropertyAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"

namespace tart {

static const PropertyDefn::PassSet PASS_SET_RESOLVETYPE = PropertyDefn::PassSet::of(
  PropertyDefn::AttributePass,
  PropertyDefn::AccessorCreationPass,
  PropertyDefn::PropertyTypePass,
  PropertyDefn::AccessorAnalysisPass
);

static const PropertyDefn::PassSet PASS_SET_COMPLETE = PropertyDefn::PassSet::of(
  PropertyDefn::AttributePass,
  PropertyDefn::AccessorCreationPass,
  PropertyDefn::PropertyTypePass,
  PropertyDefn::AccessorAnalysisPass,
  PropertyDefn::CompletionPass
);

PropertyAnalyzer::PropertyAnalyzer(PropertyDefn * prop)
  : DefnAnalyzer(prop->module(), prop->definingScope(), prop)
  , target(prop)
{
}

bool PropertyAnalyzer::analyze(AnalysisTask task) {
  TaskInProgress tip(target, task);

  if (target->isTemplate()) {
    return true;
  }

  switch (task) {
    default:
      return runPasses(PASS_SET_RESOLVETYPE);

    case Task_PrepCodeGeneration:
      return runPasses(PASS_SET_COMPLETE);
  }
}

bool PropertyAnalyzer::runPasses(PropertyDefn::PassSet passesToRun) {
  passesToRun.removeAll(target->passes().finished());
  if (passesToRun.empty()) {
    if (target->type() == NULL) {
      return false;
    }

    return true;
  }

  if (passesToRun.contains(PropertyDefn::AttributePass) &&
      target->passes().begin(PropertyDefn::AttributePass)) {
    if (!resolveAttributes(target)) {
      return false;
    }

    target->passes().finish(PropertyDefn::AttributePass);
  }

  if (passesToRun.contains(PropertyDefn::AccessorCreationPass) &&
      target->passes().begin(PropertyDefn::AccessorCreationPass)) {
    if (!createMembersFromAST(target)) {
      return false;
    }

    target->passes().finish(PropertyDefn::AccessorCreationPass);
  }

  if (passesToRun.contains(PropertyDefn::PropertyTypePass)) {
    if (!resolvePropertyType()) {
      return false;
    }
  }

  if (passesToRun.contains(PropertyDefn::AccessorAnalysisPass)) {
    if (target->getter() != NULL) {
      analyzeDefn(target->getter(), Task_PrepTypeComparison);
    }

    if (target->setter() != NULL) {
      analyzeDefn(target->setter(), Task_PrepTypeComparison);
    }

    target->passes().finish(PropertyDefn::AccessorAnalysisPass);
  }

  if (passesToRun.contains(PropertyDefn::CompletionPass)) {
    if (target->getter() != NULL) {
      analyzeDefn(target->getter(), Task_PrepCodeGeneration);
    }

    if (target->setter() != NULL) {
      analyzeDefn(target->setter(), Task_PrepCodeGeneration);
    }
    return false;
  }

  return true;
}

bool PropertyAnalyzer::resolvePropertyType() {
  if (target->passes().begin(PropertyDefn::PropertyTypePass)) {
    const ASTPropertyDecl * ast = cast_or_null<ASTPropertyDecl>(target->ast());

    // Evaluate the explicitly declared type, if any
    const Type * type = target->type();
    if (type == NULL) {
      DASSERT_OBJ(ast != NULL, target);
      DASSERT_OBJ(ast->type() != NULL, target);
      TypeAnalyzer ta(module, target->definingScope());
      type = ta.typeFromAST(ast->type());
      if (type == NULL) {
        return false;
      }

      target->setType(type);
    }

    if (target->type()->isSingular()) {
      target->addTrait(Defn::Singular);
    }

    TypeAnalyzer ta(module, activeScope);
    const ASTParamList & astParams = ast->params();

    if (target->getter() != NULL) {
      FunctionDefn * getter = target->getter();
      DASSERT_OBJ(getter->functionType() == NULL, getter);
      FunctionType * getterType = ta.typeFromFunctionAST(getter->functionDecl());
      DASSERT_OBJ(getterType->returnType() == NULL, getter);
      getterType->setReturnType(type);

      // Add the property parameters
      for (ASTParamList::const_iterator it = astParams.begin(); it != astParams.end(); ++it) {
        ASTParameter * aparam = *it;
        ParameterDefn * param = new ParameterDefn(module, aparam);
        getterType->addParam(new ParameterDefn(module, aparam));
      }

      getter->setFunctionType(getterType);
      module->addSymbol(getter);
    }

    if (target->setter() != NULL) {
      FunctionDefn * setter = target->setter();
      DASSERT_OBJ(setter->functionType() == NULL, setter);
      TypeAnalyzer ta(module, activeScope);
      FunctionType * setterType = ta.typeFromFunctionAST(setter->functionDecl());

      // See if the setter already has a 'value' parameter defined. If it does, we need
      // to temporarily remove it from the param list so that we can insert the property
      // params before it.
      ParameterDefn * valueParam = NULL;
      if (setterType->params().size() == 1) {
        valueParam = setterType->params().front();
        setterType->params().erase(setterType->params().begin());
      } else if (setterType->params().size() > 1) {
        diag.fatal(setter) << "Setter cannot have more than one explicit parameter.";
        return false;
      }

      // Add the property parameters
      for (ASTParamList::const_iterator it = astParams.begin(); it != astParams.end(); ++it) {
        ASTParameter * aparam = *it;
        ParameterDefn * param = new ParameterDefn(module, aparam);
        setterType->addParam(new ParameterDefn(module, aparam));
      }

      if (valueParam != NULL) {
        // Re-add the value param.
        setterType->params().push_back(valueParam);
        if (valueParam->type() == NULL) {
          valueParam->setType(type);
          valueParam->setInternalType(type);
        } else if (!valueParam->type()->isEqual(type)) {
          diag.fatal(setter) << "Setter parameter '" << valueParam->name() <<
              "' must be of type '" << type << "' but is instead type '" <<
              valueParam->type() << "'";
        }
      } else {
        // Create a value param.
        ParameterDefn * valueParam = new ParameterDefn(NULL, istrings.idValue);
        valueParam->setType(type);
        valueParam->setInternalType(type);
        valueParam->addTrait(Defn::Singular);
        setterType->addParam(valueParam);
      }

      DASSERT_OBJ(setterType->returnType() == NULL, setter);
      setterType->setReturnType(&VoidType::instance);

      setter->setFunctionType(setterType);
      module->addSymbol(setter);
    }

    target->passes().finish(PropertyDefn::PropertyTypePass);
  }

  return true;
}

}

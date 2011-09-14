/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Module.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Objects/Builtins.h"

#include "tart/Sema/PropertyAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"

#include "tart/Meta/MDReader.h"

namespace tart {

static const PropertyDefn::PassSet PASS_SET_RESOLVETYPE = PropertyDefn::PassSet::of(
  PropertyDefn::AttributePass,
  PropertyDefn::AccessorCreationPass,
  PropertyDefn::PropertyTypePass,
  PropertyDefn::AccessorTypePass,
  PropertyDefn::AccessorAnalysisPass
);

static const PropertyDefn::PassSet PASS_SET_COMPLETE = PropertyDefn::PassSet::of(
  PropertyDefn::AttributePass,
  PropertyDefn::AccessorCreationPass,
  PropertyDefn::PropertyTypePass,
  PropertyDefn::AccessorAnalysisPass,
  PropertyDefn::AccessorTypePass,
  PropertyDefn::CompletionPass
);

PropertyAnalyzer::PropertyAnalyzer(PropertyDefn * prop)
  : DefnAnalyzer(prop->module(), prop->definingScope(), prop, NULL)
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

    if (hasAnyRetainedAttrs(target)) {
      target->addTrait(Defn::Reflect);
    }

    target->passes().finish(PropertyDefn::AttributePass);
  }

  if (passesToRun.contains(PropertyDefn::PropertyTypePass)) {
    if (!resolvePropertyType()) {
      return false;
    }
  }

  if (passesToRun.contains(PropertyDefn::AccessorCreationPass) &&
      target->passes().begin(PropertyDefn::AccessorCreationPass)) {
    if (target->mdNode() != NULL) {
      if (!MDReader(module_, target).readPropertyAccessors(target)) {
        return false;
      }
    } else if (!createMembersFromAST(target)) {
      return false;
    }

    target->passes().finish(PropertyDefn::AccessorCreationPass);
  }

  if (passesToRun.contains(PropertyDefn::AccessorTypePass)) {
    if (!resolveAccessorType()) {
      return false;
    }
  }

  if (passesToRun.contains(PropertyDefn::AccessorAnalysisPass)) {
    if (target->getter() != NULL) {
      analyzeFunction(target->getter(), Task_PrepTypeComparison);
    }

    if (target->setter() != NULL) {
      analyzeFunction(target->setter(), Task_PrepTypeComparison);
    }

    target->passes().finish(PropertyDefn::AccessorAnalysisPass);
  }

  if (passesToRun.contains(PropertyDefn::CompletionPass)) {
    AnalysisTask task = Task_PrepCodeGeneration;
    if (target->getter() != NULL) {
      analyzeFunction(target->getter(), task);
    }

    if (target->setter() != NULL) {
      analyzeFunction(target->setter(), task);
    }
    return false;
  }

  return true;
}

bool PropertyAnalyzer::resolvePropertyType() {
  if (target->passes().begin(PropertyDefn::PropertyTypePass)) {

    // Evaluate the explicitly declared type, if any
    QualifiedType type = target->type();
    if (type.isNull()) {
      if (target->mdNode() != NULL) {
        if (!MDReader(module_, target).readPropertyType(target)) {
          return false;
        }
      } else {
        const ASTPropertyDecl * ast = cast_or_null<ASTPropertyDecl>(target->ast());
        DASSERT_OBJ(ast != NULL, target);
        DASSERT_OBJ(ast->type() != NULL, target);
        TypeAnalyzer ta(module(), target->definingScope());
        type = ta.qualifiedTypeFromAST(ast->type());
        if (type.isNull()) {
          return false;
        }

        target->setType(type);
      }
    }

    if (target->type()->isSingular()) {
      target->addTrait(Defn::Singular);
    }

    target->passes().finish(PropertyDefn::PropertyTypePass);
  }

  return true;
}

bool PropertyAnalyzer::resolveAccessorType() {
  if (target->passes().begin(PropertyDefn::AccessorTypePass)) {
    const Type * type = target->type();
    TypeAnalyzer ta(module(), activeScope());
    const ASTPropertyDecl * ast = cast_or_null<ASTPropertyDecl>(target->ast());

    if (target->getter() != NULL) {
      FunctionDefn * getter = target->getter();
      if (ast != NULL) {
        DASSERT_OBJ(getter->functionType() == NULL, getter);
        FunctionType * getterType = ta.typeFromFunctionAST(getter->functionDecl());
        DASSERT_OBJ(getterType->returnType().isNull(), getter);
        getterType->setReturnType(type);

        // Add the property parameters
        const ASTParamList & astParams = ast->params();
        for (ASTParamList::const_iterator it = astParams.begin(); it != astParams.end(); ++it) {
          ASTParameter * aparam = *it;
          getterType->addParam(new ParameterDefn(module(), aparam));
        }

        getter->setFunctionType(getterType);
        if (!getter->isAbstract() && getter->isSingular()) {
          module()->addSymbol(getter);
        }
      }
    }

    if (target->setter() != NULL) {
      FunctionDefn * setter = target->setter();
      DASSERT_OBJ(setter->functionType() == NULL, setter);
      if (ast != NULL) {
        TypeAnalyzer ta(module(), activeScope());
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
        const ASTParamList & astParams = ast->params();
        for (ASTParamList::const_iterator it = astParams.begin(); it != astParams.end(); ++it) {
          ASTParameter * aparam = *it;
          setterType->addParam(new ParameterDefn(module(), aparam));
        }

        if (valueParam != NULL) {
          // Re-add the value param.
          setterType->params().push_back(valueParam);
          if (valueParam->type() == NULL) {
            valueParam->setType(type);
            valueParam->setInternalType(type);
          } else if (!TypeRelation::isEqual(valueParam->type(), type)) {
            diag.fatal(setter) << "Setter parameter '" << valueParam->name() <<
                "' must be of type '" << type << "' but is instead type '" <<
                valueParam->type() << "'";
          }
        } else {
          // Create a value param.
          ParameterDefn * valueParam = new ParameterDefn(NULL, "value");
          valueParam->setType(type);
          valueParam->setInternalType(type);
          valueParam->addTrait(Defn::Singular);
          setterType->addParam(valueParam);
        }

        DASSERT_OBJ(setterType->returnType().isNull(), setter);
        setterType->setReturnType(&VoidType::instance);

        setter->setFunctionType(setterType);
        if (!setter->isAbstract() && setter->isSingular()) {
          module()->addSymbol(setter);
        }
      }
    }

    target->passes().finish(PropertyDefn::AccessorTypePass);
  }

  return true;
}

}

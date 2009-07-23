/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Sema/PropertyAnalyzer.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Module.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/FinalizeTypesPass.h"

namespace tart {
  
static const DefnPasses PASS_SET_RESOLVETYPE = DefnPasses::of(
  Pass_CreateMembers,
  Pass_ResolveVarType
);

PropertyAnalyzer::PropertyAnalyzer(PropertyDefn * prop)
  : DefnAnalyzer(prop->module(), prop->definingScope())
  , target(prop)
{
}

bool PropertyAnalyzer::analyze(AnalysisTask task) {
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
    if (!resolvePropertyType()) {
      return false;
    }
  }
  
  return true;
}

bool PropertyAnalyzer::resolvePropertyType() {
  if (target->beginPass(Pass_ResolveVarType)) {
    const ASTPropertyDecl * ast = cast_or_null<ASTPropertyDecl>(target->getAST());

    // Evaluate the explicitly declared type, if any
    Type * type = target->getType();
    if (type == NULL) {
      DASSERT_OBJ(ast != NULL, target);
      DASSERT_OBJ(ast->getType() != NULL, target);
      TypeAnalyzer ta(module, target->definingScope());
      type = ta.typeFromAST(ast->getType());
      if (type == NULL) {
        return false;
      }

      target->setType(type);
    }

    if (target->getType()->isSingular()) {
      target->addTrait(Defn::Singular);
    }

    TypeAnalyzer ta(module, activeScope);
    const ASTParamList & astParams = ast->params();

    if (target->getter() != NULL) {
      FunctionDefn * getter = target->getter();
      DASSERT_OBJ(getter->functionType() == NULL, getter);
      FunctionType * getterType = ta.typeFromFunctionAST(getter->getFunctionDecl());
      DASSERT_OBJ(getterType->returnType() == NULL, getter);
      getterType->setReturnType(type);

      // Add the property parameters
      for (ASTParamList::const_iterator it = astParams.begin(); it != astParams.end(); ++it) {
        ASTParameter * aparam = *it;
        ParameterDefn * param = new ParameterDefn(module, aparam);
        getterType->addParam(new ParameterDefn(module, aparam));
      }
      
      getter->setFunctionType(getterType);
      module->addXDef(getter);
      analyzeLater(getter);
    }

    if (target->setter() != NULL) {
      FunctionDefn * setter = target->setter();
      DASSERT_OBJ(setter->functionType() == NULL, setter);
      TypeAnalyzer ta(module, activeScope);
      FunctionType * setterType = ta.typeFromFunctionAST(setter->getFunctionDecl());

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
        if (valueParam->getType() == NULL) {
          valueParam->setType(type);
        } else if (!valueParam->getType()->isEqual(type)) {
          diag.fatal(setter) << "Setter parameter '" << valueParam->getName() <<
              "' must be of type '" << type << "' but is instead type '" <<
              valueParam->getType() << "'";
        }
      } else {
        // Create a value param.
        ParameterDefn * valueParam = new ParameterDefn(NULL, istrings.idValue);
        valueParam->setType(type);
        valueParam->addTrait(Defn::Singular);
        setterType->addParam(valueParam);
      }
      
      DASSERT_OBJ(setterType->returnType() == NULL, setter);
      setterType->setReturnType(&VoidType::instance);

      setter->setFunctionType(setterType);
      module->addXDef(setter);
      analyzeLater(setter);
    }

    target->finishPass(Pass_ResolveVarType);
  }

  return true;
}

#if 0
bool PropertyAnalyzer::resolveIndexerParameterTypes() {
  //return new FunctionType(returnType, params);


  //bool success = true;
  //if (target->beginPass(Pass_ResolveParameterTypes)) {
    //FunctionType * ftype = target->functionType();

    // Set the module reference for the parameter scope.
    //target->parameterScope().setModule(module);
    
    // For non-template functions, the active scope is the scope that 
    // encloses the function. For a template instance, the parent scope
    // will be the scope that defines the template variables.
    //Scope * savedScope = setActiveScope(target->definingScope());
    
    //if (target->isTemplate()) {
    //  // Get the template scope and set it as the active scope.
    //  analyzeTemplateSignature(target);
    //  TemplateSignature * tsig = target->templateSignature();
    //  activeScope = &tsig->paramScope();
    //}
    
    if (ftype != NULL) {
      ParameterList & params = ftype->params();
      for (ParameterList::iterator it = params.begin(); it != params.end(); ++it) {
        ParameterDefn * param = *it;
        //DASSERT_OBJ(param->definingScope() == NULL, target);
        //target->parameterScope().addMember(param);
        VarAnalyzer(param).analyze(Task_PrepCallOrUse);

        if (param->defaultValue() != NULL) {
          DFAIL("Implement");
        }
        
        if (param->getType() == NULL) {
          diag.fatal(param) << "No type specified for parameter '" <<
              param << "'";
        }
        
        // TODO: Change this to assign explicit types, or invent type
        // variables.
        //success &= AnalyzerBase::analyzeDefn(*it, InferTypesPass);
        
        // TODO: Should only add the param as a member if we "own" it.
        if (param->definingScope() == NULL && param->getName() != NULL) {
          target->parameterScope().addMember(param);
        }
      }
    }

    if (target->storageClass() == Storage_Instance && ftype->selfParam() == NULL) {
      ParameterDefn * selfParam = new ParameterDefn(module, istrings.idSelf);
      TypeDefn * selfType = target->enclosingClassDefn();
      DASSERT_OBJ(selfType != NULL, target);
      selfParam->setType(selfType->getTypeValue());
      selfParam->addTrait(Defn::Singular);
      selfParam->copyTrait(selfType, Defn::Final);
      selfParam->setFlag(ParameterDefn::Reference, true);
      ftype->setSelfParam(selfParam);
      target->parameterScope().addMember(selfParam);
    }

}
#endif

}

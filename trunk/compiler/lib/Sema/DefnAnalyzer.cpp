/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Template.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Common/PackageMgr.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/ParameterAssignments.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/StmtAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/SpecializeCandidate.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/FindExternalRefsPass.h"
#include "tart/Sema/EvalPass.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/Intrinsic.h"

namespace tart {

bool DefnAnalyzer::analyzeModule() {
  bool success = true;

  if (!createMembersFromAST(module)) {
    return false;
  }

  if (module->firstMember() == NULL) {
    diag.fatal() << "Module should have at least one definition";
    return false;
  }

  // Analyze all exported definitions.
  for (Defn * de = module->firstMember(); de != NULL; de = de->nextInScope()) {
    if (!de->isTemplate()) {
      if (analyzeDefn(de, Task_PrepCodeGeneration)) {
        module->addSymbol(de);
      } else {
        diag.info(de) << "Failed to analyze " << de;
        success = false;
      }
    }
  }

  // Now deal with the xrefs. Synthetic xrefs need to be analyzed all the
  // way down; Non-synthetic xrefs only need to be analyzed deep enough to
  // be externally referenced.

  // Analyze all external references.
  while (Defn * de = module->nextDefToAnalyze()) {
    if (analyzeDefn(de, Task_PrepCodeGeneration)) {
      FindExternalRefsPass::run(module, de);
    } else {
      success = false;
    }
  }

  flushAnalysisQueue();
  return success;
}

bool DefnAnalyzer::analyze(Defn * in, DefnPasses & passes) {

  // Create members of this scope.
  if (passes.contains(Pass_CreateMembers)) {
    createMembersFromAST(in);
  }

  // Resolve attributes
  if (passes.contains(Pass_ResolveAttributes) && !resolveAttributes(in)) {
    return false;
  }

  return true;
}

bool DefnAnalyzer::createMembersFromAST(Defn * in) {
  // Create members of this scope.
  if (in->beginPass(Pass_CreateMembers)) {
    if (in->getAST() != NULL) {
      ScopeBuilder::createScopeMembers(in);
    }

    in->finishPass(Pass_CreateMembers);
  }

  return true;
}

bool DefnAnalyzer::resolveAttributes(Defn * in) {
  if (in->beginPass(Pass_ResolveAttributes)) {
    if (in == Builtins::typeAttribute->typeDefn()) {
      // Don't evaluate attributes for class Attribute - it creates a circular dependency.
      CompositeType * attrType = cast<CompositeType>(Builtins::typeAttribute);
      attrType->setClassFlag(CompositeType::Attribute, true);
      attrType->attributeInfo().setTarget(AttributeInfo::CLASS);
      attrType->attributeInfo().setRetained(false);
      in->finishPass(Pass_ResolveAttributes);
      return true;
    }

    if (in->getAST() != NULL) {
      ExprAnalyzer ea(module, activeScope);
      const ASTNodeList & attrs = in->getAST()->attributes();
      for (ASTNodeList::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
        Expr * attrExpr = ea.reduceAttribute(*it);
        if (attrExpr != NULL) {
          //diag.info(in) << attrExpr;
          in->getAttrs().push_back(attrExpr);
        }
      }
    }

    applyAttributes(in);
    in->finishPass(Pass_ResolveAttributes);
  }

  return true;
}

void DefnAnalyzer::applyAttributes(Defn * in) {
  ExprList & attrs = in->getAttrs();
  for (ExprList::iterator it = attrs.begin(); it != attrs.end(); ++it) {
    Expr * attrExpr = ExprAnalyzer::inferTypes(*it, NULL);
    if (isErrorResult(attrExpr)) {
      continue;
    }

    // Handle @Intrinsic as a special case.
    Type * attrType = attrExpr->type();
    if (attrType == Builtins::typeIntrinsicAttribute) {
      handleIntrinsicAttribute(in, attrExpr);
      continue;
    }

    if (attrExpr->exprType() == Expr::CtorCall) {
      CompositeType * attrClass = cast<CompositeType>(attrType);
      if (!attrClass->isAttribute()) {
        diag.error(*it) << "Invalid attribute expression @" << *it;
        continue;
      }

      if (!attrClass->attributeInfo().canAttachTo(in)) {
        diag.error(*it) << "Attribute '" << attrClass << "' cannot apply to target '" << in << "'";
        continue;
      }

      Expr * attrVal = EvalPass::eval(attrExpr, true);
      if (attrVal != NULL && attrVal->exprType() == Expr::ConstObjRef) {
        ConstantObjectRef * attrObj = static_cast<ConstantObjectRef *>(attrVal);
        DASSERT_OBJ(attrVal->type()->isEqual(attrClass), attrVal->type());

        // Special case for @Attribute
        if (attrClass == Builtins::typeAttribute) {
          handleAttributeAttribute(in, attrObj);
          continue;
        }

        FunctionDefn * applyMethod = dyn_cast_or_null<FunctionDefn>(
            attrClass->lookupSingleMember("apply", true));
        if (applyMethod != NULL) {
          applyAttribute(in, attrObj, applyMethod);
        } else {
          diag.info(in) << "Unhandled attribute " << *it;
        }
      }
    }
  }
}

void DefnAnalyzer::applyAttribute(Defn * de, ConstantObjectRef * attrObj,
    FunctionDefn * applyMethod) {
  if (analyzeDefn(applyMethod, Task_PrepCodeGeneration)) {
    ExprList args;
    if (TypeDefn * tdef = dyn_cast<TypeDefn>(de)) {
      args.push_back(tdef->asExpr());
    } else if (ValueDefn * vdef = dyn_cast<ValueDefn>(de)) {
      args.push_back(new LValueExpr(attrObj->location(), NULL, vdef));
    } else {
      DFAIL("Implement");
    }

    applyMethod->eval(de->location(), attrObj, args);
  }
}

void DefnAnalyzer::handleIntrinsicAttribute(Defn * de, Expr * attrExpr) {
  if (FunctionDefn * func = dyn_cast<FunctionDefn>(de)) {
    func->setIntrinsic(Intrinsic::get(func->qualifiedName().c_str()));
  } else {
    diag.fatal(attrExpr) << "Only functions can be Intrinsics";
  }
}

void DefnAnalyzer::handleAttributeAttribute(Defn * de, ConstantObjectRef * attrObj) {
  int target = attrObj->getMemberValueAsInt("target");
  int retention = attrObj->getMemberValueAsInt("retention");
  int propagation = attrObj->getMemberValueAsInt("propagation");

  TypeDefn * targetTypeDefn = cast<TypeDefn>(de);
  CompositeType * targetType = cast<CompositeType>(targetTypeDefn->typeValue());
  targetType->setClassFlag(CompositeType::Attribute, true);
  targetType->attributeInfo().setTarget(target);
  targetType->attributeInfo().setRetained(retention);
}

void DefnAnalyzer::importIntoScope(const ASTImport * import, Scope * targetScope) {
  if (import->unpack()) {
    DFAIL("Implement import namespace");
  }

  ExprList importDefs;
  Scope * saveScope = setActiveScope(NULL); // Inhibit unqualified search.
  if (lookupName(importDefs, import->path())) {
    targetScope->addMember(new ExplicitImportDefn(module, import->asName(), importDefs));
  } else {
    diag.fatal(import) << "Not found '" << import << "'";
  }
  setActiveScope(saveScope);
}

// Handle explicit specialization.
Defn * DefnAnalyzer::specialize(const SourceLocation & loc, DefnList & defns,
    const ASTNodeList & args) {
  TypeList argList; // Template args, not function args.
  bool isSingularArgList = true;  // True if all args are fully resolved.

  ExprAnalyzer ea(module, activeScope);
  for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
    ConstantExpr * cb = ea.reduceConstantExpr(*it, NULL);
    if (isErrorResult(cb)) {
      return NULL;
    }

    Type * typeArg = NULL;
    if (ConstantType * ctype = dyn_cast<ConstantType>(cb)) {
      typeArg = dealias(ctype->value());
      if (TypeDefn * tdef = typeArg->typeDefn()) {
        typeArg = tdef->typeValue();
      }
    }

    if (typeArg == NULL) {
      typeArg = NonTypeConstant::get(cb);
    }

    if (!cb->isSingular()) {
      isSingularArgList = false;
    }

    argList.push_back(typeArg);
  }

  SpCandidates candidates;
  for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
    Defn * de = *it;
    if (de->isTemplate()) {
      analyzeTemplateSignature(de);
      const TemplateSignature * tsig = de->templateSignature();
      if (tsig->params().size() == argList.size()) {
        // Attempt unification of pattern variables with template args.
        SpecializeCandidate * spc = new SpecializeCandidate(de);
        SourceContext candidateSite(de->location(), NULL, de, Format_Type);
        if (spc->unify(&candidateSite, argList)) {
          candidates.push_back(spc);
        }
      }
    } else if (de->isTemplateInstance()) {
      diag.fatal(de) << "Shouldn't encounter template instance here";
      DFAIL("IllegalState - template instances should have been removed.");
    } else {
      diag.debug() << Format_Verbose << de->defnType();
    }
  }

  if (candidates.empty()) {
    diag.error(loc) << "No templates found which match template arguments [" << args << "]";
    for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
      diag.info(*it) << Format_Type << "candidate: " << *it;
    }

    ea.dumpScopeHierarchy();
    return NULL;
  }

  //if (!isSingularArgList) {
  //  return new
  //}

  // TODO: Do template overload resolution.
  // TODO: Use parameter assignments.
  if (candidates.size() == 1) {
    SpecializeCandidate * spc = candidates.front();
    TemplateSignature * tsig = spc->getTemplateDefn()->templateSignature();
    return tsig->instantiate(loc, spc->env());
  } else {
    DFAIL("Implement");
  }
}

void DefnAnalyzer::analyzeTemplateSignature(Defn * de) {
  TemplateSignature * tsig = de->templateSignature();
  DASSERT_OBJ(tsig != NULL, de);

  if (tsig->getAST() != NULL) {
    DASSERT_OBJ(de->definingScope() != NULL, de);
    const ASTNodeList & paramsAst = tsig->getAST()->params();
    TypeList & params = tsig->params();

    for (ASTNodeList::const_iterator it = paramsAst.begin(); it != paramsAst.end(); ++it) {
      ExprAnalyzer ea(de->module(), de->definingScope());
      Expr * param = ea.reducePattern(*it, tsig);
      if (param != NULL) {
        // Now, add a definition to the parameter scope for this param.
        if (ConstantType * type = dyn_cast<ConstantType>(param)) {
          params.push_back(type->value());
        } else if (ConstantExpr * cexp = dyn_cast<ConstantExpr>(param)) {
          params.push_back(NonTypeConstant::get(cexp));
        }
      }
    }

    // Create definitions for each pattern var type.
    /*PatternVarList & ptypes = tsig->getTypeVars();
    for (PatternVarList::iterator it = ptypes.begin(); it != ptypes.end();
        ++it) {
      PatternVar * pv = *it;
      TypeDefn * talias = new TypeDefn(de->module(), pv->name(), pv);
      tsig->paramScope().addMember(talias);
    }*/

    // Create definitions for each pattern var expr.

#if 0
    PatternVarExprList & ptypes = tsig->getExprVars();
    for (PatternVarExprList::iterator it = ptypes.begin(); it != ptypes.end();
        ++it) {
      PatternVarExpr * pv = *it;
      VariableDefn * var = new VariableDefn(
          Defn::Let, de->module(), pv->name());
      var->setInitValue(pv);
      tsig->paramScope().addMember(var);
    }
#endif

    // Remove the AST so that we don't re-analyze this template.
    tsig->setAST(NULL);
  }

  // TODO: Also analyse requirement expressions.
  //DFAIL("Implement");
#if 0
  tsig->paramScope().setParentScope(de->getParentScope());
  for (ParameterList::iterator it = tsig->params().begin();
      it != tsig->params().end(); ++it) {
    ParameterDefn * param = *it;

    if (param->beginPass(Pass_ResolveVarType)) {
      analyzeTemplateParam(param);

      // TODO: Check for subclass of Type.
      if (param->type()->isEqual(Builtins::typeType)) {
        TypeParameterDefn * tparam = new TypeParameterDefn(param);
        tsig->paramScope().addMember(tparam);
      } else {
        // TODO: Not sure that this is correct.
        tsig->paramScope().addMember(param);
      }

      param->finishPass(Pass_ResolveVarType);
    }
  }
#endif
}

void DefnAnalyzer::addPasses(Defn * de, DefnPasses & toRun, const DefnPasses & requested) {
  toRun.addAll(requested);
  toRun.removeAll(de->getFinished());
}

}

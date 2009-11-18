/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/UnitType.h"
#include "tart/CFG/TupleType.h"
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
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/FindExternalRefsPass.h"
#include "tart/Sema/EvalPass.h"
#include "tart/Objects/Builtins.h"
#include "tart/Objects/Intrinsic.h"

namespace tart {

extern BuiltinMemberRef<VariableDefn> module_types;
extern BuiltinMemberRef<VariableDefn> module_methods;
extern BuiltinMemberRef<VariableDefn> method_typeParams;

bool DefnAnalyzer::analyzeModule() {
  bool success = true;
  bool requireReflection = false;

  if (!createMembersFromAST(module)) {
    return false;
  }

  if (module->firstMember() == NULL) {
    diag.fatal() << "Module should have at least one definition";
    return false;
  }

  // Analyze all exported definitions.
  for (Defn * de = module->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->isTemplate()) {
      analyzeTemplateSignature(de);
    }

    if (!de->hasUnboundTypeParams()) {
      if (analyzeDefn(de, Task_PrepCodeGeneration)) {
        module->addSymbol(de);
        if (!de->hasTrait(Defn::Nonreflective)) {
          requireReflection = true;
        }
      } else {
        success = false;
        diag.recovered();
      }
    }
  }

  // If reflection is required, import those types.
  if (module->isReflectionEnabled()) {
    Builtins::loadReflectionClasses();
    analyzeType(Builtins::typeType, Task_PrepMemberLookup);
    analyzeType(Builtins::typeModule, Task_PrepCodeGeneration);
    analyzeType(Builtins::typeMethod, Task_PrepCodeGeneration);
    analyzeType(Builtins::typeComplexType, Task_PrepCodeGeneration);
    if (requireReflection) {
      module->addSymbol(Builtins::typeModule->typeDefn());
      module->addSymbol(module_types.get()->type().defn());
      module->addSymbol(module_methods.get()->type().defn());
      module->addSymbol(method_typeParams.get()->type().defn());
    }
  }

  analyzeType(Builtins::typeTypeInfoBlock, Task_PrepCodeGeneration);
  analyzeDefn(Builtins::funcTypecastError, Task_PrepTypeGeneration);

  // Now deal with the xrefs. Synthetic xrefs need to be analyzed all the
  // way down; Non-synthetic xrefs only need to be analyzed deep enough to
  // be externally referenced.

  // Analyze all external references.
  while (Defn * de = module->nextDefToAnalyze()) {
    if (analyzeDefn(de, Task_PrepCodeGeneration)) {
      if (module->isReflectionEnabled() && module->exportDefs().count(de) > 0) {
        analyzeDefn(de, Task_PrepReflection);
      }

      FindExternalRefsPass::run(module, de);
    } else {
      success = false;
      diag.recovered();
    }
  }

  /*for (DefnSet::iterator it = module->exportDefs().begin(); it != module->exportDefs().end(); ++it) {
    diag.debug() << "Export " << Format_Verbose << *it;
  }*/

  // Prevent further symbols from being added.
  module->finishPass(Pass_ResolveModuleMembers);
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
    if (in->ast() != NULL) {
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
      attrType->typeDefn()->addTrait(Defn::Nonreflective);
      in->finishPass(Pass_ResolveAttributes);
      return true;
    }

    if (in->ast() != NULL) {
      ExprAnalyzer ea(module, activeScope, subject());
      const ASTNodeList & attrs = in->ast()->attributes();
      for (ASTNodeList::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
        Expr * attrExpr = ea.reduceAttribute(*it);
        if (attrExpr != NULL) {
          //diag.info(in) << attrExpr;
          in->attrs().push_back(attrExpr);
        }
      }
    }

    applyAttributes(in);

    // Also propagate attributes from the parent defn.
    Defn * parent = in->parentDefn();
    if (parent != NULL) {
      for (ExprList::const_iterator it = parent->attrs().begin(); it != parent->attrs().end(); ++it) {
        propagateMemberAttributes(parent, in);
      }
    }

    in->finishPass(Pass_ResolveAttributes);
  }

  return true;
}

bool DefnAnalyzer::propagateSubtypeAttributes(Defn * baseDefn, Defn * target) {
  const ExprList & baseAttributes = baseDefn->attrs();
  for (ExprList::const_iterator it = baseAttributes.begin(); it != baseAttributes.end(); ++it) {
    if (const CompositeType * attrType = dyn_cast<CompositeType>((*it)->type())) {
      DASSERT(attrType->isAttribute());
      if (attrType->attributeInfo().propagation() & AttributeInfo::SUBTYPES) {
        if (!propagateAttribute(target, *it)) {
          return false;
        }
      }
    }
  }

  return true;
}

bool DefnAnalyzer::propagateMemberAttributes(Defn * scopeDefn, Defn * target) {
  const ExprList & scopeAttributes = scopeDefn->attrs();
  for (ExprList::const_iterator it = scopeAttributes.begin(); it != scopeAttributes.end(); ++it) {
    if (const CompositeType * attrType = dyn_cast<CompositeType>((*it)->type())) {
      DASSERT(attrType->isAttribute());
      if (attrType->attributeInfo().propagation() & AttributeInfo::MEMBERS) {
        if (!propagateAttribute(target, *it)) {
          return false;
        }
      }
    }
  }

  return true;
}

bool DefnAnalyzer::propagateAttribute(Defn * in, Expr * attr) {
  const CompositeType * attrType = cast<CompositeType>(attr->type());
  DASSERT(attrType->isAttribute());
  for (ExprList::const_iterator it = in->attrs().begin(); it != in->attrs().end(); ++it) {
    Expr * existingAttr = *it;
    const CompositeType * existingAttrType = cast<CompositeType>(existingAttr->type());
    if (existingAttrType->isEqual(attrType)) {
      return true;
    }
  }

  if (attrType->attributeInfo().canAttachTo(in)) {
    in->attrs().push_back(attr);
    if (attr->exprType() == Expr::ConstObjRef) {
      ConstantObjectRef * attrObj = static_cast<ConstantObjectRef *>(attr);
      FunctionDefn * applyMethod = dyn_cast_or_null<FunctionDefn>(
          attrType->lookupSingleMember("apply", true));
      if (applyMethod != NULL) {
        applyAttribute(in, attrObj, applyMethod);
      } else {
        diag.info(in) << "Unhandled attribute " << attr;
      }
    }
  }

  return true;
}

void DefnAnalyzer::applyAttributes(Defn * in) {
  ExprList & attrs = in->attrs();
  for (ExprList::iterator it = attrs.begin(); it != attrs.end(); ++it) {
    Expr * attrExpr = ExprAnalyzer::inferTypes(subject(), *it, NULL);
    if (isErrorResult(attrExpr)) {
      continue;
    }

    *it = attrExpr;

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

      // Evaluate the attribute. If it returns NULL, it simply means that it could not
      // be evaluated at compile-time.
      Expr * attrVal = EvalPass::eval(attrExpr, true);
      if (isErrorResult(attrVal)) {
        continue;
      }

      // Replace the attribute with its compiled representation.
      *it = attrVal;

      if (attrVal->exprType() == Expr::ConstObjRef) {
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
  if (analyzeDefn(applyMethod, Task_PrepEvaluation)) {
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
  int target = attrObj->memberValueAsInt("target");
  int retention = attrObj->memberValueAsInt("retention");
  int propagation = attrObj->memberValueAsInt("propagation");

  TypeDefn * targetTypeDefn = cast<TypeDefn>(de);
  CompositeType * targetType = cast<CompositeType>(targetTypeDefn->typeValue());
  targetType->setClassFlag(CompositeType::Attribute, true);
  targetType->attributeInfo().setTarget(target);
  targetType->attributeInfo().setRetained(retention);
  targetType->attributeInfo().setPropagation(propagation);
  if (!retention) {
    // If a type is not retained, then don't generate reflection information for it.
    targetTypeDefn->addTrait(Defn::Nonreflective);
  }
}

void DefnAnalyzer::importIntoScope(const ASTImport * import, Scope * targetScope) {
  if (import->unpack()) {
    DFAIL("Implement import namespace");
  }

  ExprList importDefs;
  Scope * saveScope = setActiveScope(&Builtins::module); // Inhibit unqualified search.
  if (lookupName(importDefs, import->path(), true)) {
    targetScope->addMember(new ExplicitImportDefn(module, import->asName(), importDefs));
  } else if (lookupName(importDefs, import->path(), false)) {
    targetScope->addMember(new ExplicitImportDefn(module, import->asName(), importDefs));
  } else {
    diag.fatal(import) << "Not found '" << import << "'";
  }

  setActiveScope(saveScope);
}

void DefnAnalyzer::analyzeTemplateSignature(Defn * de) {
  TemplateSignature * tsig = de->templateSignature();
  DASSERT_OBJ(tsig != NULL, de);

  if (tsig->ast() != NULL) {
    DASSERT_OBJ(de->definingScope() != NULL, de);
    const ASTNodeList & paramsAst = tsig->ast()->params();
    TypeRefList params;

    for (ASTNodeList::const_iterator it = paramsAst.begin(); it != paramsAst.end(); ++it) {
      ExprAnalyzer ea(de->module(), de->definingScope(), de);
      Expr * param = ea.reducePattern(*it, tsig);
      if (param != NULL) {
        // Now, add a definition to the parameter scope for this param.
        if (TypeLiteralExpr * type = dyn_cast<TypeLiteralExpr>(param)) {
          params.push_back(type->value());
        } else if (ConstantExpr * cexp = dyn_cast<ConstantExpr>(param)) {
          params.push_back(UnitType::get(cexp));
        }
      }
    }

    tsig->setTypeParams(TupleType::get(params));

    if (!de->hasUnboundTypeParams()) {
      de->addTrait(Defn::Singular);
    }

    // Remove the AST so that we don't re-analyze this template.
    tsig->setAST(NULL);
  }
}

void DefnAnalyzer::addPass(Defn * de, DefnPasses & toRun, const DefnPass requested) {
  toRun.add(requested);
  toRun.removeAll(de->finished());
}

void DefnAnalyzer::addPasses(Defn * de, DefnPasses & toRun, const DefnPasses & requested) {
  toRun.addAll(requested);
  toRun.removeAll(de->finished());
}

Module * DefnAnalyzer::moduleForDefn(const Defn * def) {
  if (def->isTemplateInstance()) {
    return def->templateInstance()->srcModule();
  } else {
    return def->module();
  }
}

}

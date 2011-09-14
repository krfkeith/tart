/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Module.h"
#include "tart/Defn/NamespaceDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/TemplateConditions.h"
#include "tart/Defn/TypeDefn.h"

#include "tart/Expr/Exprs.h"

#include "tart/Type/CompositeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/UnitType.h"
#include "tart/Type/TypeRelation.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/PackageMgr.h"

#include "tart/Meta/MDReader.h"

#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Sema/ParameterAssignments.h"
#include "tart/Sema/TemplateParamAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/FindExternalRefsPass.h"
#include "tart/Sema/EvalPass.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"
#include "tart/Type/Attribute.h"

namespace tart {

namespace reflect {
  namespace FunctionType {
    extern SystemClassMember<TypeDefn> CallAdapterFnType;
  }
}

extern SystemNamespaceMember<FunctionDefn> gc_allocContext;
extern SystemNamespaceMember<FunctionDefn> gc_alloc;
extern SystemClassMember<FunctionDefn> functionType_checkArgs;

// -------------------------------------------------------------------
// DefnAnalyzer

bool DefnAnalyzer::analyzeModule() {
  bool success = true;

  if (module_->passes().begin(Module::ScopeCreationPass)) {
    if (!createMembersFromAST(module_)) {
      return false;
    }
    module_->passes().finish(Module::ScopeCreationPass);
  }

  if (module_->firstMember() == NULL) {
    diag.fatal() << "Module should have at least one definition";
    return false;
  }

  // Analyze all exported definitions.
  for (Defn * de = module_->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->isTemplate() || de->isPartialInstantiation()) {
      analyzeTemplateSignature(de);
    } else if (!de->hasUnboundTypeParams()) {
      if (analyzeCompletely(de)) {
        module_->addSymbol(de);
      } else {
        success = false;
        diag.recovered();
      }
    }

    if (de->isSingular()) {
      addReflectionInfo(de);
    }
  }

  if (module_->isReflectionEnabled()) {
    module_->addSymbol(Builtins::typeModule.typeDefn());
    module_->addSymbol(Builtins::typeTypeList.typeDefn());
    analyzeType(Builtins::typeCompositeType.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeEnumType.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeDerivedType.get(), Task_PrepConstruction);
    analyzeType(Builtins::typePrimitiveType.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeFunctionType.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeTypeList.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeAttributeList.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeMethod.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeMethodList.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeProperty.get(), Task_PrepConstruction);
    analyzeType(Builtins::typePropertyList.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeField.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeFieldList.get(), Task_PrepConstruction);
    analyzeType(Builtins::typeDataMember.get(), Task_PrepConstruction);
    analyzeDefn(functionType_checkArgs.get(), Task_PrepCodeGeneration);
  }
  analyzeType(Builtins::typeTypeInfoBlock.get(), Task_PrepCodeGeneration);
  analyzeType(Builtins::typeTraceAction.get(), Task_PrepConstruction);
  analyzeType(Builtins::typeStaticRoot.get(), Task_PrepConstruction);
  if (Builtins::funcTypecastError != NULL) {
    analyzeFunction(Builtins::funcTypecastError, Task_PrepTypeGeneration);
    analyzeFunction(gc_allocContext, Task_PrepCodeGeneration);
    analyzeFunction(gc_alloc, Task_PrepConstruction);
  }
  analyzeDefn(reflect::FunctionType::CallAdapterFnType.get(), Task_PrepCodeGeneration);

  // Now deal with the xrefs. Synthetic xrefs need to be analyzed all the
  // way down; Non-synthetic xrefs only need to be analyzed deep enough to
  // be externally referenced.

  // Analyze all external references.
  while (Defn * de = module_->nextDefToAnalyze()) {
    bool isExport = module_->exportDefs().count(de) > 0;
    AnalysisTask task = Task_PrepTypeGeneration;
    if (isExport) {
      task = Task_PrepCodeGeneration;
    }

    if (isTraceEnabled(de)) {
      diag.debug(de) << Format_Verbose << "Next in queue: " << de;
    }

    if (analyzeDefn(de, task)) {
      if (module_->isReflectionEnabled() && isExport) {
        analyzeDefn(de, Task_PrepReflection);
      }

      if (isExport) {
        FindExternalRefsPass::run(module_, de);
        addReflectionInfo(de);
      }
    } else {
      success = false;
      diag.recovered();
    }
  }

  // Prevent further symbols from being added.
  module_->passes().finish(Module::CompletionPass);
  return success;
}

bool DefnAnalyzer::createMembersFromAST(Defn * in) {
  // Create members of this scope.
  if (in->ast() != NULL || in->mdNode() != NULL) {
    ScopeBuilder::createScopeMembers(in);
  }

  return true;
}

bool DefnAnalyzer::resolveAttributes(Defn * in) {
  if (in == Builtins::typeAttribute->typeDefn()) {
    // Don't evaluate attributes for class Attribute - it creates a circular dependency.
    CompositeType * attrType = Builtins::typeAttribute.get();
    attrType->setClassFlag(CompositeType::Attribute, true);
    attrType->attributeInfo().setTarget(AttributeInfo::CLASS);
    attrType->attributeInfo().setRetention(AttributeInfo::NONE);
    return true;
  }

  if (in->mdNode() != NULL) {
    if (!MDReader(module_, in).readAttributeList(in)) {
      return false;
    }
  } else if (in->ast() != NULL) {
    ExprAnalyzer ea(this, NULL);
    const ASTNodeList & attrs = in->ast()->attributes();
    for (ASTNodeList::const_iterator it = attrs.begin(); it != attrs.end(); ++it) {
      Expr * attrExpr = ea.reduceAttribute(*it);
      if (attrExpr != NULL) {
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
    if (TypeRelation::isEqual(existingAttrType, attrType)) {
      return true;
    }
  }

  if (attrType->attributeInfo().canAttachTo(in)) {
    in->attrs().push_back(attr);
    if (attr->exprType() == Expr::ConstObjRef) {
      ConstantObjectRef * attrObj = static_cast<ConstantObjectRef *>(attr);
      // TODO - actually look up multiple 'apply' members and find the correct one.
      FunctionDefn * applyMethod = dyn_cast_or_null<FunctionDefn>(
          attrType->lookupSingleMember("apply", true));
      if (applyMethod != NULL) {
        applyAttribute(in, attrObj, applyMethod);
      } else if (!attrType->attributeInfo().isRetained()) {
        diag.info(in) << "Unhandled attribute " << attr;
      }
    }
  }

  return true;
}

void DefnAnalyzer::applyAttributes(Defn * in) {
  ExprList & attrs = in->attrs();
  ExprAnalyzer ea(this, NULL);
  for (ExprList::iterator it = attrs.begin(); it != attrs.end(); ++it) {
    Expr * attrExpr = ea.inferTypes(subject(), *it, NULL);
    if (isErrorResult(attrExpr)) {
      continue;
    }

    *it = attrExpr;

    // Handle @Intrinsic as a special case.
    const Type * attrType = attrExpr->type();
    if (attrType == Builtins::typeIntrinsicAttribute) {
      handleIntrinsicAttribute(in, attrExpr);
      continue;
    }

    if (attrExpr->exprType() == Expr::CtorCall) {
      const CompositeType * attrClass = cast<CompositeType>(attrType);
      if (!attrClass->isAttribute()) {
        diag.error(attrExpr) << "Invalid attribute type: " << attrType;
        diag.recovered();
        continue;
      }

      if (!attrClass->attributeInfo().canAttachTo(in)) {
        diag.error(attrExpr) << "Attribute '" << attrClass << "' cannot apply to target '" << in << "'";
        diag.recovered();
        continue;
      }

      // Evaluate the attribute. If it returns NULL, it simply means that it could not
      // be evaluated at compile-time.
      attrExpr = EvalPass::eval(module_, attrExpr, true);
      if (isErrorResult(attrExpr)) {
        continue;
      }

      // Replace the attribute with its compiled representation.
      *it = attrExpr;
    }

    if (attrExpr->exprType() == Expr::ConstObjRef) {
      const CompositeType * attrClass = cast<CompositeType>(attrType);
      ConstantObjectRef * attrObj = static_cast<ConstantObjectRef *>(attrExpr);
      DASSERT_OBJ(TypeRelation::isEqual(attrExpr->type(), attrClass), attrExpr->type());

      // Special case for @Attribute
      if (attrClass == Builtins::typeAttribute) {
        handleAttributeAttribute(in, attrObj);
        continue;
      }

      FunctionDefn * applyMethod = dyn_cast_or_null<FunctionDefn>(
          attrClass->lookupSingleMember("apply", true));
      if (applyMethod != NULL) {
        applyAttribute(in, attrObj, applyMethod);
      } else if (!attrClass->attributeInfo().isRetained()) {
        diag.info(in) << "Unhandled attribute " << *it;
      }
    }
  }
}

void DefnAnalyzer::applyAttribute(Defn * de, ConstantObjectRef * attrObj,
    FunctionDefn * applyMethod) {
  if (analyzeFunction(applyMethod, Task_PrepEvaluation)) {
    ExprList args;
    if (TypeDefn * tdef = dyn_cast<TypeDefn>(de)) {
      args.push_back(tdef->asExpr());
    } else if (ValueDefn * vdef = dyn_cast<ValueDefn>(de)) {
      args.push_back(LValueExpr::get(attrObj->location(), attrObj, vdef));
    } else {
      DFAIL("Implement");
    }

    applyMethod->eval(de->location(), module_, attrObj, args);
  }
}

void DefnAnalyzer::handleIntrinsicAttribute(Defn * de, Expr * attrExpr) {
  if (FunctionDefn * func = dyn_cast<FunctionDefn>(de)) {
    func->setFlag(FunctionDefn::Intrinsic);
    func->setFlag(FunctionDefn::Final);
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
  targetType->attributeInfo().setRetention(retention);
  targetType->attributeInfo().setPropagation(propagation);
}

bool DefnAnalyzer::hasAnyRetainedAttrs(Defn * in) {
  const ExprList & attrs = in->attrs();
  for (ExprList::const_iterator it = attrs.begin(), itEnd = attrs.end(); it != itEnd; ++it) {
    if (const CompositeType * attrClass = dyn_cast<CompositeType>((*it)->type())) {
      if (attrClass->attributeInfo().isRetained()) {
        return true;
      }
    }
  }

  return false;
}

void DefnAnalyzer::importIntoScope(const ASTImport * import, IterableScope * targetScope) {
  ExprList importDefs;
  Scope * saveScope = setActiveScope(&Builtins::module); // Inhibit unqualified search.
  diag.recovered();
  bool found = lookupName(importDefs, import->path(), LOOKUP_ABS_PATH);
  if (!found) {
    found = lookupName(importDefs, import->path(), LOOKUP_DEFAULT);
  }

  if (found) {
    if (import->unpack()) {
      if (importDefs.size() > 1) {
        diag.error(import) << "Ambiguous import statement due to multiple definitions of " <<
            import->path();
        diag.recovered();
      }

      Expr * impExpr = importDefs.front();
      Scope * impScope = NULL;
      if (ScopeNameExpr * se = dyn_cast<ScopeNameExpr>(impExpr)) {
        if (Module * mod = dyn_cast<Module>(se->value())) {
          createMembersFromAST(mod);
          //analyzeDefn(mod, Task_PrepMemberLookup);
          impScope = mod;
        } else if (NamespaceDefn * ns = dyn_cast<NamespaceDefn>(se->value())) {
          analyzeNamespace(ns, Task_PrepMemberLookup);
          impScope = &ns->memberScope();
        }
      } else if (TypeLiteralExpr * tl = dyn_cast<TypeLiteralExpr>(impExpr)) {
        if (tl->type()->typeDefn() != NULL) {
          analyzeTypeDefn(tl->type()->typeDefn(), Task_PrepMemberLookup);
        }
        impScope = const_cast<IterableScope *>(tl->value()->memberScope());
      }

      if (impScope != NULL) {
        targetScope->auxScopes().insert(impScope);
      } else {
        diag.error(import) << "Invalid type for import: " << impExpr;
        diag.recovered();
      }
    } else {
      targetScope->addMember(new ExplicitImportDefn(module_, import->asName(), importDefs));
    }
  } else {
    diag.error(import) << "Not found '" << import << "'";
    diag.recovered();
  }

  setActiveScope(saveScope);
}

void DefnAnalyzer::analyzeTemplateSignature(Defn * de) {
  Template * tm = de->templateSignature();
  DASSERT_OBJ(tm != NULL, de);

  if (de->mdNode() != NULL && tm->ast() == NULL) {
    // Create definitions from Metadata Node.
    MDReader(de->module(), de).readMembers(de);
  }

  if (tm->typeParams() == NULL) {
    DASSERT_OBJ(de->definingScope() != NULL, de);
    const ASTNodeList & paramsAst = tm->ast()->params();
    QualifiedTypeList params;
    ASTConstNodeList defaults;

    TemplateParamAnalyzer tpa(de);
    for (ASTNodeList::const_iterator it = paramsAst.begin(); it != paramsAst.end(); ++it) {
      const ASTNode * node = *it;
      const ASTNode * paramDefault = NULL;

      // If the template argument has a default value.
      if (node->nodeType() == ASTNode::Assign) {
        const ASTOper * op = static_cast<const ASTOper *>(node);
        node = op->arg(0);
        paramDefault = op->arg(1);
      }

      Type * param = tpa.typeFromAST(node);
      if (param != NULL) {
        params.push_back(param);
        defaults.push_back(paramDefault);
      }
    }

    tm->setTypeParams(TupleType::get(params));

    TypeAnalyzer ta(de->module(), &tm->paramScope());
    ExprAnalyzer ea(&ta, ta.currentFunction());
    int argCount = 0;
    for (ASTConstNodeList::const_iterator it = defaults.begin(); it != defaults.end(); ++it) {
      const ASTNode * node = *it;
      QualifiedType param = params[argCount];
      Type * paramDefault = NULL;
      if (node != NULL) {
        if (tm->isVariadic()) {
          diag.error(node) << "default parameter values not allowed on variadic template";
        }

        if (Qualified<TypeVariable> tv = param.dyn_cast<TypeVariable>()) {
          if (tv->valueType() != NULL) {
            ConstantExpr * defaultValue = dyn_cast_or_null<ConstantExpr>(
                ea.reduceConstantExpr(node, tv->valueType()));
            if (isErrorResult(defaultValue)) {
              break;
            }

            paramDefault = UnitType::get(defaultValue);
          }
        }

        if (paramDefault == NULL) {
          paramDefault = ta.typeFromAST(node);
        }
      }

      ++argCount;
      if (paramDefault == NULL) {
        // If there's no default, then this arg (and all args which precede it) are required.
        tm->setNumRequiredArgs(argCount);
      }

      tm->typeParamDefaults().push_back(paramDefault);
    }

    if (!de->hasUnboundTypeParams()) {
      de->addTrait(Defn::Singular);
    }
  }
}

void DefnAnalyzer::addReflectionInfo(Defn * in) {
  bool isExport = in->isSynthetic() || in->module() == module_;
  bool isReflected = isExport && in->isReflected();
  bool trace = isTraceEnabled(in);

  if (trace) {
    diag.debug() << Format_Verbose << "Adding reflection info for: " << in;
  }

  if (TypeDefn * tdef = dyn_cast<TypeDefn>(in)) {
    switch (tdef->typeValue()->typeClass()) {
      case Type::Class:
      case Type::Interface: {
        CompositeType * ctype = static_cast<CompositeType *>(tdef->typeValue());
        if (ctype->isAttribute() && !ctype->attributeInfo().isRetained() &&
            ctype != Builtins::typeAttribute) {
          isExport = false;
        }

        if (isExport && module_->reflect(tdef)) {
          reflectTypeMembers(ctype);
        }
        break;
      }

      case Type::Struct:
      case Type::Protocol:
        if (isExport) {
          module_->reflect(tdef);
        }

        break;

      case Type::Enum:
        if (isExport && module_->reflect(tdef)) {
          module_->addSymbol(Builtins::typeEnumType.typeDefn());
        }
        break;

      default:
        break;
    }
  } else if (FunctionDefn * fn = dyn_cast<FunctionDefn>(in)) {
    if (!fn->isIntrinsic() && !fn->isExtern() && fn->isSingular()) {
      if (isReflected) {
        reflectType(fn->type());
        module_->reflect(fn->mergeTo() ? fn->mergeTo() : fn);
      }
    }
  } else if (PropertyDefn * prop = dyn_cast<PropertyDefn>(in)) {
    if (isReflected) {
      if (prop->getter() != NULL) {
        module_->addSymbol(prop->getter());
      }
      if (prop->setter() != NULL) {
        module_->addSymbol(prop->setter());
      }
      module_->reflect(prop);
      reflectType(prop->type());
    }
  } else if (VariableDefn * var = dyn_cast<VariableDefn>(in)) {
    if (isReflected) {
      if (var->storageClass() == Storage_Global || var->storageClass() == Storage_Static) {
        module_->reflect(var);
        reflectType(var->type());
      }
    }
  }
}

bool DefnAnalyzer::reflectType(const Type * type) {
  TypeDefn * tdef = type->typeDefn();
  if (tdef != NULL) {
    addReflectionInfo(tdef);
    return true;
  }

  switch (type->typeClass()) {
    case Type::Primitive:
    case Type::Enum:
    case Type::Class:
    case Type::Interface:
      break;

    case Type::Function: {
      const FunctionType * ft = static_cast<const FunctionType *>(type);
      ExprAnalyzer ea(module_, activeScope_, subject_, currentFunction_);
      for (ParameterList::const_iterator it = ft->params().begin();
          it != ft->params().end(); ++it) {
        const Type * paramType = (*it)->internalType();
        reflectType(paramType);
        // Cache the unbox function for this type.
        if (paramType->isBoxableType()) {
          ea.getUnboxFn(SourceLocation(), paramType);
        } else if (paramType->isReferenceType()) {
          ea.getDowncastFn(SourceLocation(), paramType);
        }
      }

      reflectType(ft->returnType());
      if (ft->returnType()->isBoxableType()) {
        // Cache the boxing function for this type.
        ea.coerceToObjectFn(ft->returnType());
      }
      break;
    }

    case Type::Tuple:
    case Type::Union:
    case Type::NAddress:
    case Type::NArray: {
      for (size_t i = 0; i < type->numTypeParams(); ++i) {
        reflectType(type->typeParam(i).type());
      }
      break;
    }

    default:
      break;
  }

  return false;
}

void DefnAnalyzer::reflectTypeMembers(CompositeType * type) {
  analyzeTypeDefn(type->typeDefn(), Task_PrepCodeGeneration);

  if (type->typeDefn()->isReflected()) {
    // If we're doing detailed reflection, then reflect the members of this type.
    for (Defn * de = type->firstMember(); de != NULL; de = de->nextInScope()) {
      if (de->isSingular()) {
        switch (de->defnType()) {
          case Defn::Typedef:
          case Defn::Namespace:
          case Defn::Var:
          case Defn::Let:
          case Defn::Property:
          case Defn::Indexer:
          case Defn::Function:
            addReflectionInfo(de);
            break;

          default:
            break;
        }
      }
    }
  }

  ClassList & bases = type->bases();
  for (ClassList::iterator it = bases.begin(); it != bases.end(); ++it) {
    reflectType(*it);
  }

  for (size_t i = 0; i < type->numTypeParams(); ++i) {
    reflectType(type->typeParam(i).type());
  }

  if (type->noArgConstructor()) {
    module_->addSymbol(type->noArgConstructor());
  }
}

Module * DefnAnalyzer::moduleForDefn(const Defn * def) {
  if (def->isTemplateInstance()) {
    return def->templateInstance()->templateDefn()->module();
  } else {
    return def->module();
  }
}

}

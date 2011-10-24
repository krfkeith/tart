/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/FunctionType.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/NativeType.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/Module.h"
#include "tart/Expr/Closure.h"

#include "tart/Common/Diagnostics.h"

#include "tart/Sema/FunctionAnalyzer.h"
#include "tart/Sema/FunctionMergePass.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/StmtAnalyzer.h"
#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/ConstructorAnalyzer.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"
#include "tart/Objects/Intrinsic.h"

#include "tart/Meta/MDReader.h"

namespace tart {

static const FunctionDefn::PassSet PASS_SET_RESOLVETYPE = FunctionDefn::PassSet::of(
  FunctionDefn::AttributePass,
  FunctionDefn::ParameterTypePass,
  FunctionDefn::ModifierPass,
  FunctionDefn::ReturnTypePass
);

static const FunctionDefn::PassSet PASS_SET_EVAL = FunctionDefn::PassSet::of(
  FunctionDefn::AttributePass,
  FunctionDefn::ParameterTypePass,
  FunctionDefn::ModifierPass,
  FunctionDefn::ControlFlowPass,
  FunctionDefn::ReturnTypePass
);

static const FunctionDefn::PassSet PASS_SET_PREP_CONVERSION = FunctionDefn::PassSet::of(
  FunctionDefn::AttributePass,
  FunctionDefn::ParameterTypePass,
  FunctionDefn::ModifierPass,
  FunctionDefn::ReturnTypePass,
  FunctionDefn::PrepConversionPass
);

static const FunctionDefn::PassSet PASS_SET_CODEGEN = FunctionDefn::PassSet::of(
  FunctionDefn::AttributePass,
  FunctionDefn::ParameterTypePass,
  FunctionDefn::ModifierPass,
  FunctionDefn::ControlFlowPass,
  FunctionDefn::ReturnTypePass,
  FunctionDefn::MergePass,
  FunctionDefn::ReflectionPass,
  FunctionDefn::CompletionPass
);

static const FunctionDefn::PassSet PASS_SET_REFLECT = FunctionDefn::PassSet::of(
  FunctionDefn::AttributePass,
  FunctionDefn::ParameterTypePass,
  FunctionDefn::ReturnTypePass,
  FunctionDefn::ReflectionPass
);

FunctionAnalyzer::FunctionAnalyzer(FunctionDefn * func)
  : DefnAnalyzer(func->module(), func->definingScope(), func, func)
  , target(func)
{
  DASSERT(func != NULL);
}

bool FunctionAnalyzer::analyze(AnalysisTask task) {
  TaskInProgress tip(target, task);
  switch (task) {
    case Task_PrepMemberLookup:
    case Task_PrepTypeComparison:
    case Task_PrepTypeGeneration:
    case Task_PrepConstruction:
      return runPasses(PASS_SET_RESOLVETYPE);

    case Task_PrepConversion:
      return runPasses(PASS_SET_PREP_CONVERSION);

    case Task_PrepEvaluation:
      return runPasses(PASS_SET_EVAL);

    case Task_PrepCodeGeneration:
      return runPasses(PASS_SET_CODEGEN);

    case Task_PrepReflection:
      return runPasses(PASS_SET_REFLECT);

    default:
      return true;
  }
}

bool FunctionAnalyzer::runPasses(FunctionDefn::PassSet passesToRun) {
  passesToRun.removeAll(target->passes().finished());
  if (passesToRun.empty()) {
    return true;
  }

  if (passesToRun.contains(FunctionDefn::AttributePass)) {
    if (!resolveAttributes(target)) {
      return false;
    }

    if (hasAnyRetainedAttrs(target)) {
      target->addTrait(Defn::Reflect);
    }

    if (target->isIntrinsic() && target->intrinsic() == NULL) {
      target->setIntrinsic(Intrinsic::get(target->location(), target->qualifiedName()));
      if (target->intrinsic() == NULL) {
        diag.error(target) << "No such intrinsic: " << target->qualifiedName();
      }
    }

    if (target->isTraceMethod()) {
      CompositeType * ctype = const_cast<CompositeType *>(target->definingClass());
      DASSERT_OBJ(ctype != NULL, target);
      ctype->traceMethods().push_back(target);
    }

    target->passes().finish(FunctionDefn::AttributePass);
  }

  if (passesToRun.contains(FunctionDefn::ModifierPass) && !resolveModifiers()) {
    return false;
  }

  if (passesToRun.contains(FunctionDefn::ParameterTypePass) && !resolveParameterTypes()) {
    return false;
  }

  if (passesToRun.contains(FunctionDefn::ReturnTypePass) && !resolveReturnType()) {
    return false;
  }

  if (passesToRun.contains(FunctionDefn::ControlFlowPass) && !createCFG()) {
    return false;
  }

  if (passesToRun.contains(FunctionDefn::MergePass) && !merge()) {
    return false;
  }

  if (passesToRun.contains(FunctionDefn::PrepConversionPass) &&
      !analyzeRecursive(Task_PrepConversion, FunctionDefn::PrepConversionPass)) {
    return false;
  }

  if (passesToRun.contains(FunctionDefn::ReflectionPass) && !createReflectionData()) {
    return false;
  }

  // In this case, it's OK if it's already running. All we care about is that it eventually
  // completes, not that it completes right now.
  if (passesToRun.contains(FunctionDefn::CompletionPass) &&
      !target->passes().isRunning(FunctionDefn::CompletionPass) &&
      !analyzeRecursive(Task_PrepTypeGeneration, FunctionDefn::CompletionPass)) {
    return false;
  }

  return true;
}

bool FunctionAnalyzer::resolveModifiers() {
  bool success = true;

  if (target->passes().begin(FunctionDefn::ModifierPass)) {
    bool isIntrinsic = target->isIntrinsic();
    bool isAbstract = target->isAbstract();
    bool isUndefined = target->isUndefined();
    bool isExtern = target->isExtern();
    bool isInterfaceMethod = false;
    bool isReadOnly = target->isReadOnly();

    if (isIntrinsic && isExtern) {
      diag.error(target) << "Function '" << target->name() <<
          "' cannot be both external and intrinsic.";
      success = false;
    }

    if (isReadOnly && target->storageClass() != Storage_Instance) {
      diag.error(target) << "Only instance methods can be declared 'readonly";
      success = false;
    }

    // Handle 'read-only' declaration
    if (isReadOnly) {
      target->setFlag(FunctionDefn::ReadOnlySelf, true);
    }

    if (target->storageClass() == Storage_Instance) {
      TypeDefn * parentClass = target->enclosingClassDefn();
      if (parentClass != NULL) {
        // TODO: When should we *not* do this?
        if (parentClass->isReadOnly() || parentClass->isImmutable()) {
          target->setFlag(FunctionDefn::ReadOnlySelf, true);
        }
      }
    }

    // Functions defined in interfaces or protocols must not have a body.
    TypeDefn * enclosingClassDefn = target->enclosingClassDefn();
    if (enclosingClassDefn != NULL && isa<CompositeType>(enclosingClassDefn->typePtr())) {
      const CompositeType * enclosingClass = cast<CompositeType>(enclosingClassDefn->typePtr());

      switch (enclosingClass->typeClass()) {
        case Type::Interface:
        case Type::Protocol: {
          isInterfaceMethod = true;
          if (isAbstract) {
            diag.error(target) << "Interface or protocol method '" << target->name() <<
                " cannot be abstract";
            success = false;
          } else if (isUndefined) {
            diag.error(target) << "Interface or protocol method '" << target->name() <<
                " cannot be undefined";
            success = false;
          } else if (isExtern) {
            diag.error(target) << "Interface or protocol method '" << target->name() <<
                " cannot be external";
            success = false;
          }

          target->setFlag(FunctionDefn::InterfaceMethod);
          break;
        }

        case Type::Class: {
          if (isAbstract && !enclosingClass->isAbstract()) {
            diag.error(target) << "Method '" << target->name() <<
                "' declared abstract in non-abstract class";
            success = false;
          }

          break;
        }

        case Type::Struct: {
          if (isAbstract) {
            diag.error(target) << "Struct method '" << target->name() << " cannot be abstract";
            success = false;
          } else if (isUndefined) {
            diag.error(target) << "Struct method '" << target->name() << " cannot be undefined";
            success = false;
          }

          break;
        }

        default:
          DFAIL("What?");
          break;
      }

      // Add the constructor flag if it has the name 'construct'
      if (target->name() == "construct") {
        target->setFlag(FunctionDefn::Ctor);
      }

    } else {
      if (target->storageClass() == Storage_Global) {
        if (isAbstract) {
          diag.error(target) << "Global function '" << target->name() << " cannot be abstract";
          success = false;
        } else if (isUndefined) {
          diag.error(target) << "Global function '" << target->name() << " cannot be undefined";
          success = false;
        }
      }
    }

    if (target->functionDecl() != NULL) {
      bool hasBody = target->hasBody();
      if (isInterfaceMethod) {
        if (hasBody) {
          diag.error(target) << "Method body not allowed for method '" << target->name() <<
              "' defined in interface or protocol.";
          success = false;
        }
      } else if (isExtern || isIntrinsic || isAbstract || isUndefined) {
        if (hasBody) {
          const char * keyword = "abstract";
          if (isIntrinsic) {
            keyword = "@Intrinsic";
          } else if (isExtern) {
            keyword = "@Extern";
          } else if (isUndefined) {
            keyword = "with 'undef'";
          }

          diag.error(target) << "Method '" << target->name() << "' declared " << keyword <<
              " cannot have a body.";
          success = false;
        }
      } else if (!hasBody) {
        diag.error(target) << "Method body required for non-abstract method '"
            << target->name() << "'.";
        success = false;
      }
    }

    target->passes().finish(FunctionDefn::ModifierPass);
  }

  return success;
}

bool FunctionAnalyzer::resolveParameterTypes() {
  bool success = true;
  if (target->passes().begin(FunctionDefn::ParameterTypePass)) {
    bool trace = isTraceEnabled(target);
    if (trace) {
      diag.debug(target) << Format_Type << "Analyzing parameter types for " << target;
    }

    if (target->mdNode() != NULL) {
      if (!MDReader(module_, target).readFunctionType(target)) {
        return false;
      }
    }

    FunctionType * ftype = target->functionType();

    // For non-template functions, the active scope is the scope that
    // encloses the function. For a template instance, the parent scope
    // will be the scope that defines the template variables.
    Scope * savedScope = setActiveScope(target->definingScope());
    bool isFromTemplate =
        target->isTemplate() || target->isTemplateMember() || target->isPartialInstantiation();

    if (target->isTemplate()) {
      // Get the template scope and set it as the active scope.
      analyzeTemplateSignature(target);
      Template * tm = target->templateSignature();
      setActiveScope(&tm->paramScope());
    } else if (target->isTemplateMember()) {
      for (Defn * parent = target->parentDefn(); parent != NULL; parent = parent->parentDefn()) {
        if (parent->isTemplate()) {
          analyzeTemplateSignature(parent);
          break;
        }
      }
    }

    if (ftype == NULL) {
      DASSERT(target->ast() != NULL);
      TypeAnalyzer ta(target->sourceModule(), activeScope());
      ta.setTypeLookupOptions(isFromTemplate ? LOOKUP_NO_RESOLVE : LOOKUP_DEFAULT);
      ftype = ta.functionTypeFromAST(target->functionDecl());
      if (ftype == NULL) {
        success = false;
      } else {
        target->setFunctionType(ftype);
      }
    }

    if (ftype != NULL) {
      ParameterList & params = ftype->params();
      for (ParameterList::iterator it = params.begin(); it != params.end(); ++it) {
        ParameterDefn * param = *it;
        if (!VarAnalyzer(param, target->definingScope(), module_, target, target)
            .analyze(Task_PrepTypeComparison)) {
          success = false;
        }

        if (!param->type()) {
          diag.error(param) << "No type specified for parameter '" << param << "'";
        } else {
          if (param->isVariadic() && param->type()->isSingular()) {
            if (trace) {
              diag.debug(target) << Format_Type << "  Analyzing variadic param " << param;
            }
            analyzeType(param->type(), Task_PrepTypeComparison);
            param->setInternalType(getArrayTypeForElement(param->type()));
            DASSERT_OBJ(param->internalType()->isSingular(), param);
            analyzeType(param->internalType(), Task_PrepConstruction);
            module()->addSymbol(param->internalType()->typeDefn());
          }

          // TODO: Check for other unsafe types.
          if (param->type()->typeClass() == Type::NAddress) {
            target->addTrait(Defn::Unsafe);
          }
        }

        // TODO: Should only add the param as a member if we "own" it.
        if (param->definingScope() == NULL && !param->name().empty()) {
          target->parameterScope().addMember(param);
        }

        if (trace) {
          diag.indent();
          diag.debug(target) << Format_Type << "Parameter " << param;
          diag.unindent();
        }
      }
    }

    TypeDefn * parentClass = target->enclosingClassDefn();
    if (parentClass != NULL) {
      if (const CompositeType * ctype = dyn_cast<CompositeType>(parentClass->typePtr())) {
        if (ctype->isFinal()) {
          target->setFlag(FunctionDefn::Final);
        }
      }
    }

    if (target->storageClass() == Storage_Instance && ftype->selfParam() == NULL) {
      ParameterDefn * selfParam = new ParameterDefn(module(), "self");
      QualifiedType selfType = target->enclosingClassDefn()->value();
      DASSERT_OBJ(selfType, target);
      if (target->isReadOnlySelf()) {
        selfType.addQualifiers(QualifiedType::READONLY);
      } else {
        selfType.removeQualifiers(QualifiedType::MUTABILITY_MASK);
      }
      analyzeType(selfType.unqualified(), Task_PrepMemberLookup);
      selfParam->setLocation(target->location());
      selfParam->setType(selfType);
      selfParam->setInternalType(selfType);
      selfParam->addTrait(Defn::Singular);
      selfParam->addTrait(Defn::Synthetic);
      selfParam->setFlag(ParameterDefn::Reference);
      ftype->setSelfParam(selfParam);
      target->parameterScope().addMember(selfParam);
    }

    setActiveScope(savedScope);
    target->passes().finish(FunctionDefn::ParameterTypePass);
  }

  return success;
}

bool FunctionAnalyzer::createCFG() {
  bool success = true;

  if (target->hasUnboundTypeParams() || target->isTemplateMember() || !target->isSingular()) {
    // Don't build CFG for templates
    return true;
  }

  if (target->passes().begin(FunctionDefn::ControlFlowPass)) {
    if (target->isUndefined()) {
      // May be null when running automated tests.
      if (Builtins::funcUndefinedMethod) {
        module()->addSymbol(Builtins::funcUndefinedMethod);
      }
    } else if (target->body() == NULL) {
      const Stmt * astBody = NULL;
      if (target->functionDecl() != NULL) {
        astBody = target->functionDecl()->body();
      } else if (target->mdNode() != NULL) {
        astBody = MDReader(module_, target).readFunctionBody(target);
      }

      if (astBody != NULL) {
        StmtAnalyzer sa(target, astBody);
        success = sa.buildCFG();
        if (success) {
          // Make sure that the constructor calls the superclass and initializes all fields.
          if (target->isCtor() && target->isSingular()) {
            TypeDefn * clsDefn = cast<TypeDefn>(target->parentDefn());
            CompositeType * cls = cast<CompositeType>(clsDefn->mutableTypePtr());
            if (cls->typeClass() == Type::Class || cls->typeClass() == Type::Struct) {
              ConstructorAnalyzer(cls).run(target);
            }
          }

          if (!target->isNested() && !target->closureEnvs().empty()) {
            visitClosureEnvs(target->closureEnvs());
          }
        }
      }
    }

    target->passes().finish(FunctionDefn::ControlFlowPass);
  }

  return success;
}

void FunctionAnalyzer::visitClosureEnvs(const ExprList & closureEnvs) {
  for (ExprList::const_iterator it = closureEnvs.begin(); it != closureEnvs.end(); ++it) {
    ClosureEnvExpr * closure = static_cast<ClosureEnvExpr *>(*it);
    CompositeType * envType = closure->envType();

    // First, propagate 'assignedTo' flags downward.
    for (Defn * de = envType->firstMember(); de != NULL; de = de->nextInScope()) {
      if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
        // See if this variable was assigned to at any point.
        bool isAssignedTo = false;
        for (VariableDefn * v = var; v != NULL; v = v->closureBinding()) {
          if (v->isAssignedTo()) {
            isAssignedTo = true;
            break;
          }
        }

        const Type * varType = var->type().type();
        if (isAssignedTo) {
          var->setIsAssignedTo();
          varType = getMutableRefType(varType);
          analyzeType(varType, Task_PrepCodeGeneration);
          module_->addSymbol(varType->typeDefn());
          module_->addSymbol(cast<CompositeType>(varType)->super()->typeDefn());
          var->setSharedRefType(varType);
          var->closureBinding()->setSharedRefType(varType);
          target->setFlag(FunctionDefn::MakesAllocs);
          target->setFlag(FunctionDefn::HasSafePoints);
        }
      }
    }
  }
}

bool FunctionAnalyzer::resolveReturnType() {
  bool success = true;

  FunctionType * funcType = target->functionType();
  QualifiedType returnType = funcType->returnType();

  if (returnType.isNull() && target->passes().isRunning(FunctionDefn::ReturnTypePass)) {
    diag.fatal(target) << "Recursive function must have explicit return type.";
    return false;
  }

  if (target->hasUnboundTypeParams()) {
    if (target->passes().begin(FunctionDefn::ReturnTypePass)) {
      // We can't do type inference on a template, since the types are unknown.
      // (And also because we haven't built a CFG).
      // Templates that don't have an explicit return type are assumed void.
      if (returnType.isNull()) {
        funcType->setReturnType(&VoidType::instance);
      }

      target->passes().finish(FunctionDefn::ReturnTypePass);
    }

    return true;
  }

  if (target->passes().begin(FunctionDefn::ReturnTypePass)) {
    SourceLocation  returnTypeLoc;
    TypeList returnTypes;
    if (returnType.isNull()) {
      returnType = &VoidType::instance;
      funcType->setReturnType(returnType);
    }

    DASSERT_OBJ(returnType != &NullType::instance, returnType);
    // TODO: Check for other unsafe types.
    if (returnType->typeClass() == Type::NAddress) {
      target->addTrait(Defn::Unsafe);
    }

    target->passes().finish(FunctionDefn::ReturnTypePass);
  }

  return success;
}

bool FunctionAnalyzer::merge() {
  bool success = true;

  if (target->hasUnboundTypeParams() || target->isTemplateMember() || !target->isSingular() ||
      diag.getErrorCount() > 0) {
    target->passes().finish(FunctionDefn::MergePass);
    return true;
  }

  if (target->passes().begin(FunctionDefn::MergePass)) {
    // If we try to analyze re-entrantly, then just don't coalesce.
    target->passes().finish(FunctionDefn::MergePass);

    if (target->hasBody() && target->isSynthetic() && target->hasTrait(Defn::Mergeable)) {
      Defn * de = findLessSpecializedInstance(target);
      if (de != NULL) {
        FunctionDefn * mergeTo = cast<FunctionDefn>(de);
        if (mergeTo != NULL) {
          AnalyzerBase::analyzeFunction(mergeTo, Task_PrepCodeGeneration);
          bool canMerge = FunctionMergePass().visit(target, mergeTo);
          if (canMerge) {
            target->setMergeTo(mergeTo);
            module_->addSymbol(mergeTo);
          }
        }
      }
    }
  }

  return success;
}

bool FunctionAnalyzer::createReflectionData() {
  if (target->passes().begin(FunctionDefn::ReflectionPass)) {
    FunctionType * ftype = target->functionType();
    bool doReflect = module_->isReflectionEnabled() && target->isReflected();
    if (!target->isSingular() || target->defnType() == Defn::Macro) {
      // Don't reflect uninstantiated templates
      doReflect = false;
    } else if (target->storageClass() == Storage_Local) {
      // Don't reflect local methods
      doReflect = false;
    } else {
      if (doReflect) {
        if (ftype->selfParam() != NULL) {
          const Type * selfType = ftype->selfParam()->type().type();
          if (const CompositeType * cself = dyn_cast<CompositeType>(selfType)) {
            // Don't reflect structs (for now) or protocols.
            if (cself->typeClass() == Type::Struct || cself->typeClass() == Type::Protocol) {
              target->removeTrait(Defn::Reflect);
              doReflect = false;
            }
          } else {
            // Don't reflect functions of non-composite types.
            target->removeTrait(Defn::Reflect);
            doReflect = false;
          }
        }
      }
    }

    // Generate the invoke function.
    if (doReflect) {
      // Add the other arguments
      for (ParameterList::const_iterator it = target->params().begin();
          it != target->params().end(); ++it) {
        const Type * paramType = dealias((*it)->type().type());
        if (paramType->isBoxableType()) {
          // Cache the unbox function for this type.
          ExprAnalyzer(this, target).getUnboxFn(SourceLocation(), paramType);
        } else if (paramType->isReferenceType()) {
          ExprAnalyzer(this, target).getDowncastFn(SourceLocation(), paramType);
        } else {
          // For the moment we can't handle non-reference types in reflection.
          doReflect = false;
        }
      }

      if (!ftype->returnType()->isVoidType()) {
        QualifiedType returnType = dealias(ftype->returnType());
        if (returnType->isBoxableType()) {
          // Cache the boxing function for this type.
          ExprAnalyzer(this, target).coerceToObjectFn(returnType.type());
        } else if (!returnType->isReferenceType()) {
          // For the moment we can't handle non-reference types in reflection.
          doReflect = false;
        }
      }

      if (doReflect) {
        ftype->setIsInvocable(true);
      } else {
        target->removeTrait(Defn::Reflect);
      }
    }

    target->passes().finish(FunctionDefn::ReflectionPass);
  }

  return true;
}

bool FunctionAnalyzer::analyzeRecursive(AnalysisTask task, FunctionDefn::AnalysisPass pass) {
  if (target->passes().begin(pass)) {
    analyzeType(target->functionType(), task);
    target->passes().finish(pass);
  }

  return true;
}

void FunctionAnalyzer::warnConflict(
  const SourceLocation & prevLoc, const Type * prevType,
  const SourceLocation & nextLoc, const Type * nextType) const {
  //returnTypeConflict = true;
  diag.fatal(nextLoc) << "Returned value type '" << nextType <<
      "' conflicts with function result type '" << prevType << "'",
  diag.info(prevLoc) << "used here.";
}

} // namespace tart

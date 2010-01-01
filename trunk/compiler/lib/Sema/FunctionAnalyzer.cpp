/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/FunctionType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/Block.h"
#include "tart/CFG/Module.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"

#include "tart/Sema/FunctionAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/StmtAnalyzer.h"
#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/ConstructorAnalyzer.h"

#include "tart/Objects/Builtins.h"

#define INFER_RETURN_TYPE 0

namespace tart {

extern SystemClassMember<VariableDefn> functionType_invoke;
extern SystemClassMember<VariableDefn> functionType_paramTypes;
extern SystemClassMember<VariableDefn> method_typeParams;
extern SystemClassMember<VariableDefn> member_attributes;

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

    target->passes().finish(FunctionDefn::AttributePass);
  }

  if (passesToRun.contains(FunctionDefn::ParameterTypePass) && !resolveParameterTypes()) {
    return false;
  }

  if (passesToRun.contains(FunctionDefn::ModifierPass) && !resolveModifiers()) {
    return false;
  }

#if INFER_RETURN_TYPE
  if (passesToRun.contains(FunctionDefn::ReturnTypePass)) {
    passesToRun.add(FunctionDefn::ControlFlowPass);
  }
#endif

  if (passesToRun.contains(FunctionDefn::ReturnTypePass) && !resolveReturnType()) {
    return false;
  }

  if (passesToRun.contains(FunctionDefn::ControlFlowPass) && !createCFG()) {
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

bool FunctionAnalyzer::resolveParameterTypes() {
  bool success = true;
  if (target->passes().begin(FunctionDefn::ParameterTypePass)) {
    FunctionType * ftype = target->functionType();

    // Set the module reference for the parameter scope.
    //target->parameterScope().setModule(module);

    // For non-template functions, the active scope is the scope that
    // encloses the function. For a template instance, the parent scope
    // will be the scope that defines the template variables.
    Scope * savedScope = setActiveScope(target->definingScope());

    if (target->isTemplate()) {
      // Get the template scope and set it as the active scope.
      analyzeTemplateSignature(target);
      TemplateSignature * tsig = target->templateSignature();
      activeScope = &tsig->paramScope();
    } else if (target->isTemplateMember()) {
      for (Defn * parent = target->parentDefn(); parent != NULL; parent = parent->parentDefn()) {
        if (parent->isTemplate()) {
          analyzeTemplateSignature(parent);
          TemplateSignature * tsig = parent->templateSignature();
          activeScope = &tsig->paramScope();
          break;
        }
      }
    }

    if (ftype == NULL) {
      DASSERT(target->ast() != NULL);
      TypeAnalyzer ta(module, activeScope);
      ftype = ta.typeFromFunctionAST(target->functionDecl());
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
        VarAnalyzer(param, module, target, target).analyze(Task_PrepTypeComparison);

        if (param->type() == NULL) {
          diag.error(param) << "No type specified for parameter '" << param << "'";
        //} else if (param->type()->typeClass() == Type::NArray) {
        //  diag.error(param) << "Invalid parameter type (native array)";
        } else if (param->getFlag(ParameterDefn::Variadic)) {
          if (param->internalType()->isSingular()) {
            module->addSymbol(param->internalType()->typeDefn());
          }
        }

        // TODO: Should only add the param as a member if we "own" it.
        if (param->definingScope() == NULL && param->name() != NULL) {
          target->parameterScope().addMember(param);
        }
      }
    }

    TypeDefn * parentClass = target->enclosingClassDefn();
    if (parentClass != NULL) {
      if (CompositeType * ctype = dyn_cast<CompositeType>(parentClass->typeValue())) {
        if (ctype->isFinal()) {
          target->setFlag(FunctionDefn::Final);
        }
      }
    }

    if (target->storageClass() == Storage_Instance && ftype->selfParam() == NULL) {
      ParameterDefn * selfParam = new ParameterDefn(module, istrings.idSelf);
      TypeDefn * selfType = target->enclosingClassDefn();
      DASSERT_OBJ(selfType != NULL, target);
      analyzeType(selfType->typeValue(), Task_PrepMemberLookup);
      selfParam->setType(selfType->typeValue());
      selfParam->setInternalType(selfType->typeValue());
      selfParam->addTrait(Defn::Singular);
      selfParam->setFlag(ParameterDefn::Reference, true);
      ftype->setSelfParam(selfParam);
      target->parameterScope().addMember(selfParam);
    }

    setActiveScope(savedScope);
    target->passes().finish(FunctionDefn::ParameterTypePass);
  }

  return success;
}

bool FunctionAnalyzer::resolveModifiers() {
  bool success = true;

  if (target->hasUnboundTypeParams() || target->isTemplateMember()) {
    // Don't build CFG for templates
    return true;
  }

  if (target->passes().begin(FunctionDefn::ModifierPass)) {
    bool isIntrinsic = target->isIntrinsic();
    bool isAbstract = target->isAbstract();
    bool isUndefined = target->isUndefined();
    bool isExtern = target->isExtern();
    bool isInterfaceMethod = false;

    if (isIntrinsic && isExtern) {
      diag.error(target) << "Function '" << target->name() <<
          "' cannot be both external and intrinsic.";
      success = false;
    }

    // Functions defined in interfaces or protocols must not have a body.
    TypeDefn * enclosingClassDefn = target->enclosingClassDefn();
    if (enclosingClassDefn != NULL) {
      CompositeType * enclosingClass = cast<CompositeType>(enclosingClassDefn->typeValue());

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
          break;
      }

      // Add the constructor flag if it has the name 'construct'
      if (target->name() == istrings.idConstruct) {
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

bool FunctionAnalyzer::createCFG() {
  bool success = true;

  if (target->hasUnboundTypeParams() || target->isTemplateMember()) {
    // Don't build CFG for templates
    return true;
  }

  if (target->passes().begin(FunctionDefn::ControlFlowPass)) {
    if (target->hasBody() && target->blocks().empty()) {
      StmtAnalyzer sa(target);
      success = sa.buildCFG();

      // Generate the list of predecessor blocks for each block.
      for (BlockList::iterator b = target->blocks().begin(); b != target->blocks().end(); ++b) {
        Block * blk = *b;
        BlockList & succs = blk->succs();
        for (BlockList::iterator s = succs.begin(); s != succs.end(); ++s) {
          (*s)->preds().push_back(blk);
        }
      }

      // Make sure that the constructor calls the superclass and initializes all fields.
      if (target->isCtor() && target->isSingular()) {
        TypeDefn * clsDefn = cast<TypeDefn>(target->parentDefn());
        CompositeType * cls = cast<CompositeType>(clsDefn->typeValue());
        if (cls->typeClass() == Type::Class || cls->typeClass() == Type::Struct) {
          ConstructorAnalyzer(cls).run(target);
        }

      }
    } else if (target->isUndefined()) {
      // Push a dummy block for undefined method.
      Block * block = new Block("undef_entry");
      target->blocks().push_back(block);
      module->addSymbol(Builtins::typeUnsupportedOperationException->typeDefn());
    }

    target->passes().finish(FunctionDefn::ControlFlowPass);
  }

  return success;
}

bool FunctionAnalyzer::resolveReturnType() {
  bool success = true;

  FunctionType * funcType = target->functionType();
  const Type * returnType = funcType->returnType();

  if (returnType == NULL && target->passes().isRunning(FunctionDefn::ReturnTypePass)) {
    diag.fatal(target) << "Recursive function must have explicit return type.";
    return false;
  }

  if (target->hasUnboundTypeParams()) {
    if (target->passes().begin(FunctionDefn::ReturnTypePass)) {
      // We can't do type inference on a template, since the types are unknown.
      // (And also because we haven't built a CFG).
      // Templates that don't have an explicit return type are assumed void.
      if (returnType == NULL) {
        funcType->setReturnType(&VoidType::instance);
      }

      target->passes().finish(FunctionDefn::ReturnTypePass);
    }

    return true;
  }

  if (target->passes().begin(FunctionDefn::ReturnTypePass)) {
    SourceLocation  returnTypeLoc;
    TypeList returnTypes;
    if (returnType == NULL) {
      returnType = &VoidType::instance;
#if INFER_RETURN_TYPE
      BlockList & blocks = target->blocks();
      for (BlockList::iterator it = blocks.begin(); it != blocks.end(); ++it) {
        Block * bk = *it;
        if (bk->terminator() == BlockTerm_Return) {
          Type * type = &VoidType::instance;
          Expr * returnExpr = bk->termValue();
          SourceLocation loc;
          if (returnExpr != NULL) {
            type = returnExpr->type();
            loc = returnExpr->location();
          }

          if (isErrorResult(type)) {
            break;
          } else if (returnTypes.empty()) {
            returnTypeLoc = loc;
            returnTypes.push_back(type);
          } else {
            // See if 'type' supercedes any existing types.
            bool insertNew = true;
            for (TypeList::iterator tp = returnTypes.begin(); tp != returnTypes.end();) {
              Type * old = *tp;
              if ((*tp)->isEqual(type)) {
                insertNew = false;
                ++tp;
              } else {
                // Find which type encompasses the other
                ConversionRank tcOld = (*tp)->canConvert(type);
                ConversionRank tcNew = type->canConvert(*tp);
                if (tcNew > tcOld) {
                  tp = returnTypes.erase(tp);
                } else if (tcOld > tcNew) {
                  insertNew = false;
                  ++tp;
                } else {
                  ++tp;
                }
              }
            }

            if (insertNew) {
              returnTypes.push_back(type);
            }
          }
        }
      }

      if (returnTypes.size() == 0) {
        returnType = &VoidType::instance;
      } else if (returnTypes.size() == 1) {
        returnType = returnTypes.front();
      } else {
        diag.fatal(target) << "Ambiguous return type";
        for (TypeList::iterator it = returnTypes.begin(); it != returnTypes.end(); ++it) {
          diag.info() << *it;
        }

        returnType = &BadType::instance;
      }

#endif
      funcType->setReturnType(returnType);
    }

    DASSERT_OBJ(returnType != &NullType::instance, returnType);

    // Add implicit casts to return statements if needed.
    BlockList & blocks = target->blocks();
    bool isVoidFunc = returnType->isVoidType();
    for (BlockList::iterator it = blocks.begin(); it != blocks.end(); ++it) {
      Block * bk = *it;
      if (bk->terminator() == BlockTerm_Return) {
        Expr * returnExpr = bk->termValue();
        SourceLocation loc = bk->termLocation();

        if (returnExpr != NULL) {
          if (isVoidFunc) {
            diag.error(returnExpr) << "function return type is void, return value not allowed";
            break;
          }

          const Type * type = returnExpr->type();
          if (!returnType->isEqual(type)) {
            AnalyzerBase(module, activeScope, subject())
                .analyzeType(returnType, Task_PrepTypeComparison);
            returnExpr = returnType->implicitCast(loc, returnExpr);
            if (returnExpr != NULL) {
              bk->exitReturn(loc, returnExpr);
            }
          }

          if (returnExpr) {
            loc = returnExpr->location();
          }
        } else if (!isVoidFunc) {
          // See if we can convert a void expression to the return type.
          Expr * voidExpr = ConstantNull::get(loc, &VoidType::instance);
          returnExpr = returnType->implicitCast(loc, voidExpr);
          if (returnExpr != NULL) {
            bk->exitReturn(loc, returnExpr);
          } else {
            diag.error(loc) << "return value required";
            break;
          }
        }
      }
    }

    target->passes().finish(FunctionDefn::ReturnTypePass);
  }

  return success;
}

bool FunctionAnalyzer::createReflectionData() {
  if (target->passes().begin(FunctionDefn::ReflectionPass)) {
    bool doReflect = true;
    if (!target->isSingular() || target->defnType() == Defn::Macro) {
      // Don't reflect uninstantiated templates
      doReflect = false;
    } else if (target->storageClass() == Storage_Local
        || target->storageClass() == Storage_Closure) {
      // Don't reflect local methods
      doReflect = false;
      target->addTrait(Defn::Nonreflective);
    } else {
      // Don't reflect if any parent is non-reflective.
      for (Defn * de = target; de != NULL; de = de->parentDefn()) {
        if (de->hasTrait(Defn::Nonreflective)) {
          target->addTrait(Defn::Nonreflective);
          doReflect = false;
          break;
        }
      }

      if (doReflect) {
        FunctionType * ftype = target->functionType();
        if (ftype->selfParam() != NULL) {
          const Type * selfType = ftype->selfParam()->type();
          if (const CompositeType * cself = dyn_cast<CompositeType>(selfType)) {
            // Don't reflect structs (for now) or protocols.
            if (cself->typeClass() == Type::Struct || cself->typeClass() == Type::Protocol) {
              target->addTrait(Defn::Nonreflective);
              doReflect = false;
            }
          } else {
            // Don't reflect functions of non-composite types.
            target->addTrait(Defn::Nonreflective);
            doReflect = false;
          }
        }
      }
    }

    // Generate the invoke function.
    if (doReflect) {
      FunctionType * ftype = target->functionType();

      // Add the other arguments
      for (ParameterList::const_iterator it = target->params().begin();
          it != target->params().end(); ++it) {
        const Type * paramType = dealias((*it)->type());
        if (paramType->isBoxableType()) {
          // Cache the unbox function for this type.
          ExprAnalyzer(module, activeScope, subject_, target)
              .getUnboxFn(SourceLocation(), paramType);
        } else if (!paramType->isReferenceType()) {
          // For the moment we can't handle non-reference types in reflection.
          doReflect = false;
        }
      }

      if (!ftype->returnType()->isVoidType()) {
        const Type * returnType = dealias(ftype->returnType());
        if (returnType->isBoxableType()) {
          // Cache the boxing function for this type.
          ExprAnalyzer(module, activeScope, subject_, target).coerceToObjectFn(returnType);
        } else if (!returnType->isReferenceType()) {
          // For the moment we can't handle non-reference types in reflection.
          doReflect = false;
        }
      }

      if (!doReflect) {
        target->addTrait(Defn::Nonreflective);
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

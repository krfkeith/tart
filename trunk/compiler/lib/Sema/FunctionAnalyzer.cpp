/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Sema/FunctionAnalyzer.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/Block.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/StmtAnalyzer.h"
#include "tart/Sema/VarAnalyzer.h"
#include "tart/Objects/Builtins.h"

namespace tart {

static const DefnPasses PASS_SET_RESOLVETYPE = DefnPasses::of(
  Pass_ResolveAttributes,
  Pass_ResolveParameterTypes,
  Pass_ResolveReturnType
);

static const DefnPasses PASS_SET_CODEGEN = DefnPasses::of(
  Pass_ResolveAttributes,
  Pass_ResolveParameterTypes,
  Pass_CreateCFG,
  Pass_ResolveReturnType
);

FunctionAnalyzer::FunctionAnalyzer(FunctionDefn * func)
  : DefnAnalyzer(func->module(), func->definingScope())
  , target(func)
{
  DASSERT(func != NULL);
}

bool FunctionAnalyzer::analyze(AnalysisTask task) {
  // Work out what passes need to be run.

  DefnPasses passesToRun;
  switch (task) {
    case Task_PrepMemberLookup:
      break;
    
    case Task_PrepCallOrUse:
    case Task_PrepOverloadSelection:
    case Task_InferType:
      addPasses(target, passesToRun, PASS_SET_RESOLVETYPE);
      break;

    case Task_PrepCodeGeneration:
      addPasses(target, passesToRun, PASS_SET_CODEGEN);
      break;
  }
  
  // Run passes

  if (passesToRun.empty()) {
    return true;
  }

  DefnAnalyzer::analyze(target, passesToRun);

  if (passesToRun.contains(Pass_ResolveParameterTypes) &&
      !resolveParameterTypes()) {
    return false;
  }

  if (passesToRun.contains(Pass_ResolveReturnType)) {
    passesToRun.add(Pass_CreateCFG);
  }

  if (passesToRun.contains(Pass_CreateCFG) && !createCFG()) {
    return false;
  }

  if (passesToRun.contains(Pass_ResolveReturnType) && !resolveReturnType()) {
    return false;
  }

  return true;
}

bool FunctionAnalyzer::resolveParameterTypes() {
  bool success = true;
  if (target->beginPass(Pass_ResolveParameterTypes)) {
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
    }
    
    if (ftype == NULL) {
      DASSERT(target->getAST() != NULL);
      TypeAnalyzer ta(module, activeScope);
      ftype = ta.typeFromFunctionAST(target->getFunctionDecl());
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
        VarAnalyzer(param).analyze(Task_PrepCallOrUse);

        if (param->getType() == NULL) {
          diag.fatal(param) << "No type specified for parameter '" << param << "'";
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

    setActiveScope(savedScope);
    target->finishPass(Pass_ResolveParameterTypes);
  }

  return success;
}

bool FunctionAnalyzer::createCFG() {
  bool success = true;
  
  if (target->isTemplate()) {
    // Don't build CFG for templates
    return true;
  }
  
  if (target->beginPass(Pass_CreateCFG)) {
    bool isIntrinsic = target->isIntrinsic();
    bool isExtern = target->isExtern();
    bool isInterfaceMethod = false;
    
    if (isIntrinsic && isExtern) {
      diag.fatal(target) << "Function '" << target->getName() <<
          "' cannot be both external and intrinsic.";
    }
    
    // Functions defined in interfaces or protocols must not have a body.
    TypeDefn * enclosingClassDefn = target->enclosingClassDefn();
    if (enclosingClassDefn != NULL) {
      CompositeType * enclosingClass = cast<CompositeType>(enclosingClassDefn->getTypeValue());
      if (enclosingClass->typeClass() == Type::Interface
          || enclosingClass->typeClass() == Type::Protocol) {
        isInterfaceMethod = true;
      }
    }

    if (target->getFunctionDecl() != NULL) {
      bool hasBody = target->getFunctionDecl()->getBody() != NULL || !target->blocks().empty();
      
      if (isInterfaceMethod) {
        if (hasBody) {
          diag.fatal(target) << "Method body not allowed for method '" << target->getName() <<
              "' defined in interface or protocol.";
        }
      } else if (isExtern || isIntrinsic) {
        if (hasBody) {
          diag.fatal(target) << "External method '" << target->getName() << "' cannot have a body.";
        }
      } else if (!hasBody) {
        diag.fatal(target) << "Function '" << target->getName() << "' has no body.";
      } else if (target->blocks().empty()) {
        StmtAnalyzer sa(target);
        success = sa.buildCFG();
      }
    }

    target->finishPass(Pass_CreateCFG);
  }

  return success;
}

bool FunctionAnalyzer::resolveReturnType() {
  bool success = true;

  FunctionType * funcType = target->functionType();
  Type * returnType = funcType->returnType();
  
  if (returnType == NULL && target->isPassRunning(Pass_ResolveReturnType)) {
    diag.fatal(target) << "Recursive function must have explicit return type.";
    return false;
  }

  if (target->isTemplate()) {
    if (target->beginPass(Pass_ResolveReturnType)) {
      // We can't do type inference on a template, since the types are unknown.
      // (And also because we haven't built a CFG).
      // Templates that don't have an explicit return type are assumed void.
      if (returnType == NULL) {
        funcType->setReturnType(&VoidType::instance);
      }

      target->finishPass(Pass_ResolveReturnType);
    }
    
    return true;
  }

  if (target->beginPass(Pass_ResolveReturnType)) {
    SourceLocation  returnTypeLoc;
    TypeList returnTypes;
    if (returnType == NULL) {
      BlockList & blocks = target->blocks();
      for (BlockList::iterator it = blocks.begin(); it != blocks.end(); ++it) {
        Block * bk = *it;
        if (bk->terminator() == BlockTerm_Return) {
          Type * type = &VoidType::instance;
          Expr * returnExpr = bk->termValue();
          SourceLocation loc;
          if (returnExpr != NULL) {
            type = returnExpr->getType();
            loc = returnExpr->getLocation();
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
  
      funcType->setReturnType(returnType);
    }

    DASSERT_OBJ(returnType != &NullType::instance, returnType);
    
    // Add implicit casts to return statements if needed.
    BlockList & blocks = target->blocks();
    bool isVoidFunc = returnType == &VoidType::instance;
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

          Type * type = returnExpr->getType();
          if (!returnType->isEqual(type)) {
            returnExpr = returnType->implicitCast(loc, returnExpr);
            if (returnExpr != NULL) {
              bk->exitReturn(loc, returnExpr);
            }
          }

          if (returnExpr) {
            loc = returnExpr->getLocation();
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

    target->finishPass(Pass_ResolveReturnType);
  }

  return success;
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

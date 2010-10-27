/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Exprs.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Constant.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/TypeConstraint.h"
#include "tart/Objects/Builtins.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/SpCandidate.h"
#include <llvm/DerivedTypes.h>

namespace tart {

Expr * ExprAnalyzer::reduceCall(const ASTCall * call, const Type * expected) {
  const ASTNode * callable = call->func();
  const ASTNodeList & args = call->args();

  if (callable->nodeType() == ASTNode::Id ||
      callable->nodeType() == ASTNode::Member ||
      callable->nodeType() == ASTNode::Specialize) {
    return callName(call->location(), callable, args, expected);
  } else if (callable->nodeType() == ASTNode::Super) {
    return callSuper(call->location(), args, expected);
  } else if (callable->nodeType() == ASTNode::BuiltIn) {
    // Built-in type constructor
    Defn * tdef = static_cast<const ASTBuiltIn *>(callable)->value();
    return callExpr(call->location(), cast<TypeDefn>(tdef)->asExpr(), args, expected);
  } else if (callable->nodeType() == ASTNode::GetElement) {
    return callExpr(call->location(), reduceElementRef(
        static_cast<const ASTOper *>(callable), false), args, expected);
  }

  diag.fatal(call) << "Not a callable expression " << call;
  DFAIL("Invalid call type");
}

Expr * ExprAnalyzer::callName(SLC & loc, const ASTNode * callable, const ASTNodeList & args,
    const Type * expected, bool isOptional) {

  // Specialize works here because lookupName handles explicit specializations.
  DASSERT(callable->nodeType() == ASTNode::Id ||
      callable->nodeType() == ASTNode::Member ||
      callable->nodeType() == ASTNode::Specialize);

  bool isUnqualified = callable->nodeType() == ASTNode::Id;
  bool success = true;

  ExprList results;
  lookupName(results, callable, (isOptional || isUnqualified) ? LOOKUP_DEFAULT : LOOKUP_REQUIRED);

  // If there were no results, and it was a qualified search, then
  // it's an error. (If it's unqualified, then there are still things
  // left to try.)
  if (results.empty() && !isUnqualified) {
    return isOptional ? NULL : &Expr::ErrorVal;
  }

  // Try getting the lookup results as a type definition.
  TypeList typeList;
  if (!results.empty() && getTypesFromExprs(loc, results, typeList)) {
    // TODO: Handle ambiguous type resolution.
    if (typeList.size() > 1) {
      diag.error(loc) << "Multiple definitions for '" << callable << "'";
      return &Expr::ErrorVal;
    }

    // TODO: We could pass the whole list to callName, and add them as overloads.
    Type * type = typeList.front();
    if (type->typeDefn() == NULL) {
      diag.error(loc) << "Type '" << type << "' is not constructable";
      return &Expr::ErrorVal;
    }

    return callConstructor(loc, type->typeDefn(), args);
  }

  CallExpr * call = new CallExpr(Expr::Call, loc, NULL);
  call->setExpectedReturnType(expected);
  for (ExprList::iterator it = results.begin(); it != results.end(); ++it) {
    //success &= addOverload(call, *it, args);
    if (LValueExpr * lv = dyn_cast<LValueExpr>(*it)) {
      if (FunctionDefn * func = dyn_cast<FunctionDefn>(lv->value())) {
        success &= addOverload(call, lvalueBase(lv), func, args);

        // If there's a base pointer, then it's not really unqualified.
        //if (lv->base() != NULL) {
        //  isUnqualified = false;
        //}
      } else if (VariableDefn * var = dyn_cast<VariableDefn>(lv->value())) {
        if (!analyzeVariable(var, Task_PrepTypeComparison)) {
          return false;
        }

        if (const FunctionType * ft = dyn_cast<FunctionType>(var->type())) {
          success &= addOverload(call, lv, ft, args);
        } else if (const BoundMethodType * bmt = dyn_cast<BoundMethodType>(var->type())) {
          success &= addOverload(call, lv, bmt->fnType(), args);
        } else {
          diag.fatal(loc) << *it << " is not callable.";
        }
      } else if (const FunctionType * ftype = dyn_cast<FunctionType>((*it)->type())) {
        success &= addOverload(call, *it, ftype, args);
      }
    } else {
      diag.fatal(loc) << *it << " is not callable.";
    }
  }

  if (!reduceArgList(args, call)) {
    return &Expr::ErrorVal;
  }

  // If it's unqualified, then do ADL.
  if (isUnqualified && !args.empty()) {
    const char * name = static_cast<const ASTIdent *>(callable)->value();
    lookupByArgType(call, name, args);
  }

  if (!success) {
    return &Expr::ErrorVal;
  } else if (results.empty()) {
    // Search failed, run it again with required to report errors.
    lookupName(results, callable, LOOKUP_REQUIRED);
    return &Expr::ErrorVal;
  } else if (call->candidates().empty()) {
    // Generate the calling signature in a buffer.
    StrFormatStream fs;
    fs << Format_Dealias << callable << "(";
    formatExprTypeList(fs, call->args());
    fs << ")";
    if (expected != NULL) {
      fs << " -> " << expected;
    }
    fs.flush();

    diag.error(loc) << "No matching method for call to " << fs.str() << ", candidates are:";
    for (ExprList::iterator it = results.begin(); it != results.end(); ++it) {
      Expr * resultMethod = *it;
      if (LValueExpr * lval = dyn_cast<LValueExpr>(*it)) {
        diag.info(lval->value()) << Format_Type << lval->value();
      } else {
        diag.info(*it) << *it;
      }
    }

    return &Expr::ErrorVal;
  }

  call->setType(reduceReturnType(call));
  return call;
}

void ExprAnalyzer::lookupByArgType(CallExpr * call, const char * name, const ASTNodeList & args) {
  //diag.debug(call) << "ADL: " << name;
  DefnList defns;
  llvm::SmallPtrSet<const Type *, 16> typesSearched;

  const ExprList & callArgs = call->args();
  for (ExprList::const_iterator it = callArgs.begin(); it != callArgs.end(); ++it) {
    Expr * arg = *it;
    if (arg->type() != NULL && arg->type()->isSingular()) {
      const Type * argType = dealias(arg->type());
      if (argType != NULL && typesSearched.insert(argType)) {
        AnalyzerBase::analyzeType(argType, Task_PrepMemberLookup);

        // TODO: Also include overrides
        // TODO: Refactor
        TypeDefn * argTypeDefn = argType->typeDefn();
        if (argTypeDefn != NULL && argTypeDefn->definingScope() != NULL) {
          if (!argTypeDefn->definingScope()->lookupMember(name, defns, true)) {
            // If nothing was found, also check ancestor clases
            if (const CompositeType * ctype = dyn_cast<CompositeType>(argType)) {
              ClassSet ancestors;
              ctype->ancestorClasses(ancestors);
              for (ClassSet::const_iterator it = ancestors.begin(); it != ancestors.end(); ++it) {
                if (typesSearched.insert(*it)) {
                  TypeDefn * ancestorDefn = (*it)->typeDefn();
                  if (ancestorDefn != NULL && ancestorDefn->definingScope() != NULL) {
                    ancestorDefn->definingScope()->lookupMember(name, defns, true);
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  llvm::SmallPtrSet<FunctionDefn *, 32> methodsFound;

  // Set of methods already found.
  Candidates & cclist = call->candidates();
  for (Candidates::iterator cc = cclist.begin(); cc != cclist.end(); ++cc) {
    methodsFound.insert((*cc)->method());
  }

  for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
    if (FunctionDefn * f = dyn_cast<FunctionDefn>(*it)) {
      if ((f->storageClass() == Storage_Static || f->storageClass() == Storage_Global) &&
          methodsFound.insert(f)) {
        //diag.debug(f) << "Adding ADL overload " << f;
        addOverload(call, NULL, f, args);
      }
    }
  }
}

Expr * ExprAnalyzer::callExpr(SLC & loc, Expr * callable, const ASTNodeList & args,
    const Type * expected) {
  if (isErrorResult(callable)) {
    return callable;
  } else if (TypeLiteralExpr * typeExpr = dyn_cast<TypeLiteralExpr>(callable)) {
    // It's a type.
    return callConstructor(loc, typeExpr->value()->typeDefn(), args);
  } else if (LValueExpr * lval = dyn_cast<LValueExpr>(callable)) {
    CallExpr * call = new CallExpr(Expr::Call, loc, NULL);
    call->setExpectedReturnType(expected);
    if (FunctionDefn * func = dyn_cast<FunctionDefn>(lval->value())) {
      addOverload(call, lvalueBase(lval), func, args);
    } else {
      diag.error(loc) << func << " is not a callable expression.";
      return &Expr::ErrorVal;
    }

    if (!reduceArgList(args, call)) {
      return &Expr::ErrorVal;
    }

    call->setType(reduceReturnType(call));
    return call;

  } else if (SpecializeExpr * spe = dyn_cast<SpecializeExpr>(callable)) {
    CallExpr * call;
    const SpCandidateList & candidates = spe->candidates();

    if (!candidates.empty()) {
      SpCandidate * spFront = candidates.front();
      if (isa<FunctionDefn>(spFront->def())) {
        // Regular function call
        call = new CallExpr(Expr::Call, loc, NULL);
        call->setExpectedReturnType(expected);
        for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
          SpCandidate * sp = *it;
          if (FunctionDefn * func = dyn_cast<FunctionDefn>(sp->def())) {
            addOverload(call, sp->base(), func, args, sp);
          } else {
            diag.error(loc) << sp->def() << " is incompatible with " << spFront->def();
          }
        }
      } else if (isa<TypeDefn>(spFront->def())) {
        // Constructor call
        call = new CallExpr(Expr::Construct, loc, NULL);
        call->setExpectedReturnType(expected);
        for (SpCandidateList::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
          SpCandidate * sp = *it;
          if (TypeDefn * tdef = dyn_cast<TypeDefn>(sp->def())) {
            //DASSERT(candidates.size() == 1);
            if (!AnalyzerBase::analyzeType(tdef->typeValue(), Task_PrepConstruction)) {
              return &Expr::ErrorVal;
            }

            // TODO: Do we need to pass in spCandidate?
            checkAccess(loc, tdef);
            if (!addOverloadedConstructors(loc, call, tdef, args, sp)) {
              return &Expr::ErrorVal;
            }
            //return callConstructor(loc, tdef, args);
          } else {
            diag.error(loc) << sp->def() << " is incompatible with " << spFront->def();
          }
        }
      } else {
        diag.error(loc) << spFront->def() << " is not a callable expression.";
        return &Expr::ErrorVal;
      }
    }

    if (!reduceArgList(args, call)) {
      return &Expr::ErrorVal;
    }

    call->setType(reduceReturnType(call));
    return call;
  } else {
    diag.fatal(callable) << Format_Verbose << "Unimplemented function type";
    DFAIL("Unimplemented");
  }
}

Expr * ExprAnalyzer::callSuper(SLC & loc, const ASTNodeList & args, const Type * expected) {
  if (currentFunction_ == NULL || currentFunction_->storageClass() != Storage_Instance) {
    diag.fatal(loc) << "'super' only callable from instance methods";
    return &Expr::ErrorVal;
  }

  TypeDefn * enclosingClassDefn = currentFunction_->enclosingClassDefn();
  CompositeType * enclosingClass = cast<CompositeType>(enclosingClassDefn->typeValue());
  CompositeType * superClass = enclosingClass->super();

  if (superClass == NULL) {
    diag.fatal(loc) << "class '" << enclosingClass << "' has no super class";
    return &Expr::ErrorVal;
  }

  DefnList methods;
  if (!superClass->memberScope()->lookupMember(currentFunction_->name(), methods, true)) {
    diag.error(loc) << "Superclass method '" << currentFunction_->name() <<
        " not found in class " << enclosingClass;
    return &Expr::ErrorVal;
  }

  ParameterDefn * selfParam = currentFunction_->functionType()->selfParam();
  DASSERT_OBJ(selfParam != NULL, currentFunction_);
  DASSERT_OBJ(selfParam->type() != NULL, currentFunction_);
  TypeDefn * selfType = selfParam->type()->typeDefn();
  DASSERT_OBJ(selfType != NULL, currentFunction_);
  Expr * selfExpr = LValueExpr::get(selfParam->location(), NULL, selfParam);
  selfExpr = superClass->implicitCast(loc, selfExpr);

  CallExpr * call = new CallExpr(Expr::ExactCall, loc, NULL);
  call->setExpectedReturnType(expected);
  for (DefnList::iterator it = methods.begin(); it != methods.end(); ++it) {
    if (FunctionDefn * func = dyn_cast<FunctionDefn>(*it)) {
      addOverload(call, selfExpr, func, args);
    } else {
      diag.fatal(loc) << *it << " is not callable.";
    }
  }

  if (!reduceArgList(args, call)) {
    return &Expr::ErrorVal;
  }

  call->setType(reduceReturnType(call));
  return call;
}

Expr * ExprAnalyzer::callConstructor(SLC & loc, TypeDefn * tdef, const ASTNodeList & args) {
  Type * type = tdef->typeValue();
  //TypeDefn * tdef = type->typeDefn();
  checkAccess(loc, tdef);
  if (!tdef->isTemplate() && !tdef->isTemplateMember()) {
    module_->addSymbol(tdef);
  }

  // First thing we need to know is how much tdef has been analyzed.
  if (!AnalyzerBase::analyzeType(type, Task_PrepConstruction)) {
    return &Expr::ErrorVal;
  }

  CallExpr * call = new CallExpr(Expr::Construct, loc, tdef->asExpr());
  call->setExpectedReturnType(type);

  if (!addOverloadedConstructors(loc, call, tdef, args, NULL)) {
    return &Expr::ErrorVal;
  }

  if (!reduceArgList(args, call)) {
    return &Expr::ErrorVal;
  }

  call->setType(reduceReturnType(call));
  return call;
}

bool ExprAnalyzer::addOverloadedConstructors(SLC & loc, CallExpr * call, TypeDefn * tdef,
    const ASTNodeList & args, SpCandidate * sp) {
  Type * type = tdef->typeValue();
  DefnList methods;
  if (tdef->isTemplate() && tdef->hasUnboundTypeParams()) {
    if (lookupTemplateMember(methods, tdef, istrings.idConstruct, loc)) {
      Expr * newExpr = new NewExpr(loc, type);
      DASSERT(!methods.empty());
      for (DefnList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
        FunctionDefn * cons = cast<FunctionDefn>(*it);
        if (analyzeFunction(cons, Task_PrepTypeComparison)) {
          DASSERT(cons->type() != NULL);
          DASSERT(cons->returnType() == NULL || cons->returnType()->isVoidType());
          DASSERT(cons->storageClass() == Storage_Instance);
          DASSERT(cons->isTemplate() || cons->isTemplateMember());
          cons->setFlag(FunctionDefn::Ctor);
          addOverload(call, newExpr, cons, args, sp);
        }
      }
    } else if (lookupTemplateMember(methods, tdef, istrings.idCreate, loc)) {
      DASSERT(!methods.empty());
      for (DefnList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
        FunctionDefn * create = cast<FunctionDefn>(*it);
        if (create->storageClass() == Storage_Static) {
          if (analyzeFunction(create, Task_PrepTypeComparison)) {
            DASSERT(create->type() != NULL);
            //Type * returnType = create->returnType();
            addOverload(call, NULL, create, args, sp);
          }
        }
      }
    } else {
      diag.error(loc) << "No constructors found for type " << tdef;
      DFAIL("Implement constructor inheritance for templates.");
      return false;
    }
  } else {
    if (type->memberScope()->lookupMember(istrings.idConstruct, methods, false)) {
      Expr * newExpr = new NewExpr(loc, type);
      DASSERT(!methods.empty());
      for (DefnList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
        FunctionDefn * cons = cast<FunctionDefn>(*it);
        DASSERT(cons->type() != NULL);
        DASSERT(cons->isCtor());
        DASSERT(cons->returnType() == NULL || cons->returnType()->isVoidType());
        DASSERT(cons->storageClass() == Storage_Instance);
        addOverload(call, newExpr, cons, args, sp);
      }
    } else if (type->memberScope()->lookupMember(istrings.idCreate, methods, false)) {
      DASSERT(!methods.empty());
      for (DefnList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
        FunctionDefn * create = cast<FunctionDefn>(*it);
        DASSERT(create->type() != NULL);
        if (create->storageClass() == Storage_Static) {
          //const Type * returnType = create->returnType();
          addOverload(call, NULL, create, args, sp);
        }
      }
    } else if (type->memberScope()->lookupMember(istrings.idConstruct, methods, true)) {
      Expr * newExpr = new NewExpr(loc, type);
      DASSERT(!methods.empty());
      for (DefnList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
        FunctionDefn * cons = cast<FunctionDefn>(*it);
        DASSERT(cons->type() != NULL);
        DASSERT(cons->isCtor());
        DASSERT(cons->returnType() == NULL || cons->returnType()->isVoidType());
        DASSERT(cons->storageClass() == Storage_Instance);
        addOverload(call, newExpr, cons, args, sp);
      }
    } else {
      diag.error(loc) << "No constructors found for type " << tdef;
      return false;
    }
  }

  if (!call->hasAnyCandidates()) {
    diag.error(loc) << "No constructor found matching input arguments (" <<
      args << "), candidates are:";
    for (DefnList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
      diag.info(*it) << Format_Verbose << *it;
    }

    return false;
  }

  return true;
}

/** Attempt a coercive cast, that is, try to find a 'coerce' method that will convert
    to 'toType'. */
CallExpr * ExprAnalyzer::tryCoerciveCast(Expr * in, const Type * toType) {
  if (const CompositeType * ctype = dyn_cast<CompositeType>(toType)) {
    if (!ctype->coercers().empty()) {
      if (!analyzeType(ctype, Task_PrepConversion)) {
        return NULL;
      }

      CallExpr * call = new CallExpr(Expr::Call, in->location(), NULL);
      call->setExpectedReturnType(toType);
      call->args().push_back(in);

      for (MethodList::const_iterator it = ctype->coercers().begin();
          it != ctype->coercers().end(); ++it) {
        addOverload(call, NULL, *it, call->args());
      }

      if (call->candidates().empty()) {
        return NULL;
      }

      call->setType(reduceReturnType(call));
      return call;
    }
  }

  return NULL;
}

bool ExprAnalyzer::reduceArgList(const ASTNodeList & in, CallExpr * call) {
  ExprList & args = call->args();
  for (size_t i = 0; i < in.size(); ++i) {
    const ASTNode * arg = in[i];
    if (arg->nodeType() == ASTNode::Keyword) {
      arg = static_cast<const ASTKeywordArg *>(arg)->arg();
    }

    const Type * paramType = getMappedParameterType(call, i);
    if (paramType == NULL) {
      return false;
    }

    Expr * ex = reduceExpr(arg, paramType);
    if (isErrorResult(ex)) {
      return false;
    }

    args.push_back(ex);
  }

  return true;
}

const Type * ExprAnalyzer::reduceReturnType(CallExpr * call) {
  const Type * ty = call->singularResultType();
  if (ty != NULL) {
    if (call->isSingular()) {
      DASSERT_OBJ(ty->isSingular(), call);
    }

    return ty;
  }

  return new ResultOfConstraint(call);
}

const Type * ExprAnalyzer::getMappedParameterType(CallExpr * call, int index) {
  const Type * ty = call->singularParamType(index);
  if (ty != NULL) {
    return ty;
  }

  return new ParameterOfConstraint(call, index);
}

bool ExprAnalyzer::addOverload(CallExpr * call, Expr * callable, const ASTNodeList & args) {
  if (const BoundMethodType * bmt = dyn_cast<BoundMethodType>(callable->type())) {
    return addOverload(call, callable, bmt->fnType(), args);
  }

  if (LValueExpr * lv = dyn_cast<LValueExpr>(callable)) {
    if (FunctionDefn * func = dyn_cast<FunctionDefn>(lv->value())) {
      return addOverload(call, lvalueBase(lv), func, args);
    }
  }

  diag.fatal(call) << callable << " is not callable.";
  return false;
}

bool ExprAnalyzer::addOverload(CallExpr * call, Expr * baseExpr, FunctionDefn * method,
    const ASTNodeList & args) {
  if (!analyzeFunction(method, Task_PrepConversion)) {
    return false;
  }

  DASSERT_OBJ(method->type() != NULL, method);
  ParameterAssignments pa;
  ParameterAssignmentsBuilder builder(pa, method->functionType());
  if (builder.assignFromAST(args)) {
    call->candidates().push_back(new CallCandidate(call, baseExpr, method, pa));
  }

  return true;
}

bool ExprAnalyzer::addOverload(CallExpr * call, Expr * baseExpr, FunctionDefn * method,
    const ASTNodeList & args, SpCandidate * sp) {
  if (!analyzeFunction(method, Task_PrepConversion)) {
    return false;
  }

  DASSERT_OBJ(method->type() != NULL, method);
  ParameterAssignments pa;
  ParameterAssignmentsBuilder builder(pa, method->functionType());
  if (builder.assignFromAST(args)) {
    call->candidates().push_back(new CallCandidate(call, baseExpr, method, pa, sp));
  }

  return true;
}

bool ExprAnalyzer::addOverload(CallExpr * call, Expr * fn, const FunctionType * ftype,
    const ASTNodeList & args) {
  //if (!analyzeType(ftype, Task_PrepOverloadSelection)) {
  //  return false;
  //}

  DASSERT_OBJ(ftype != NULL, fn);
  ParameterAssignments pa;
  ParameterAssignmentsBuilder builder(pa, ftype);
  if (builder.assignFromAST(args)) {
    call->candidates().push_back(new CallCandidate(call, fn, ftype, pa));
  }

  return true;
}

bool ExprAnalyzer::addOverload(CallExpr * call, Expr * baseExpr, FunctionDefn * method,
    const ExprList & args) {
  if (!analyzeFunction(method, Task_PrepConversion)) {
    return false;
  }

  DASSERT_OBJ(method->type() != NULL, method);
  ParameterAssignments pa;
  ParameterAssignmentsBuilder builder(pa, method->functionType());
  for (size_t i = 0; i < args.size(); ++i) {
    builder.addPositionalArg();
  }

  if (!builder.check()) {
    return false;
  }

  call->candidates().push_back(new CallCandidate(call, baseExpr, method, pa));
  return true;
}

void ExprAnalyzer::noCandidatesError(CallExpr * call, const ExprList & methods) {
  // Generate the calling signature in a buffer.
  StrFormatStream fs;
  //fs << Format_Dealias << callable << "(";
  formatExprTypeList(fs, call->args());
  fs << ")";
  if (call->expectedReturnType() != NULL) {
    fs << " -> " << call->expectedReturnType();
  }
  fs.flush();

  diag.error(call) << "No matching method for call to " << fs.str() << ", candidates are:";
  for (ExprList::const_iterator it = methods.begin(); it != methods.end(); ++it) {
    Expr * method = *it;
    if (LValueExpr * lval = dyn_cast<LValueExpr>(*it)) {
      diag.info(lval->value()) << Format_Type << lval->value();
    } else {
      diag.info(*it) << method;
    }
  }
}

} // namespace tart

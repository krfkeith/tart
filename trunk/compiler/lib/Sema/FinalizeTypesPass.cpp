/* ================================================================ *
 TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/TupleType.h"
#include "tart/CFG/NativeType.h"
#include "tart/CFG/Template.h"

#include "tart/Sema/FinalizeTypesPassImpl.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// FinalizeTypesPassImpl

Expr * FinalizeTypesPass::run(Defn * source, Expr * in, bool tryCoerciveCasts) {
  FinalizeTypesPassImpl instance(source);
  instance.tryCoerciveCasts_ = tryCoerciveCasts;
  return instance.runImpl(in);
}

Expr * FinalizeTypesPass::runImpl(Expr * in) {
  Expr * result = visitExpr(in);
  DASSERT(result->isSingular());
  return result;
}

Expr * FinalizeTypesPassImpl::visitLValue(LValueExpr * in) {
  if (in->type() == NULL) {
    in->setType(in->value()->type());
  }

  DASSERT_OBJ(in->type() != NULL, in);
  DASSERT_OBJ(in->type()->isSingular(), in);
  if (in->base() != NULL) {
    in->setBase(visitExpr(in->base()));
  }

  return in;
}

Expr * FinalizeTypesPassImpl::visitBoundMethod(BoundMethodExpr * in) {
  DASSERT_OBJ(in->type() != NULL, in);
  DASSERT_OBJ(in->type()->isSingular(), in);
  if (in->selfArg() != NULL) {
    in->setSelfArg(visitExpr(in->selfArg()));
  }

  return in;
}

Expr * FinalizeTypesPassImpl::visitScopeName(ScopeNameExpr * in) {
  DFAIL("Implement");
}

Expr * FinalizeTypesPassImpl::visitElementRef(BinaryExpr * in) {
  // Cast array index to integer type.
  // TODO: Check if it's long first.
  Expr * first = visitExpr(in->first());
  Expr * second = visitExpr(in->second());
  if (!isErrorResult(first) && !isErrorResult(second)) {
    bool isAlreadyInt = false;
    if (const PrimitiveType * ptype = dyn_cast_or_null<PrimitiveType>(second->type())) {
      isAlreadyInt = isIntegerType(ptype->typeId());
    }

    if (!isAlreadyInt) {
      second = IntType::instance.implicitCast(in->location(), second);
    }

    in->setFirst(first);
    in->setSecond(second);
    DASSERT_OBJ(first->isSingular(), first);
    DASSERT_OBJ(second->isSingular(), second);
    DASSERT_OBJ(in->isSingular(), in);
  }

  return in;
}

Expr * FinalizeTypesPassImpl::visitAssign(AssignmentExpr * in) {
  return visitAssignImpl(in);
}

Expr * FinalizeTypesPassImpl::visitPostAssign(AssignmentExpr * in) {
  return visitAssignImpl(in);
}

Expr * FinalizeTypesPassImpl::visitAssignImpl(AssignmentExpr * in) {
  DASSERT_OBJ(in->toExpr()->type() != NULL, in);
  Expr * from = visitExpr(in->fromExpr());
  Expr * to = visitExpr(in->toExpr());
  if (!isErrorResult(from) && !isErrorResult(to)) {
    DASSERT_OBJ(to->isSingular(), to);

    if (!AnalyzerBase::analyzeType(to->type(), Task_PrepTypeComparison)) {
      return in;
    }

    from = to->type()->implicitCast(from->location(), from);
    if (!isErrorResult(from)) {
      in->setFromExpr(from);
      in->setToExpr(to);
    }

    if (LValueExpr * lval = dyn_cast<LValueExpr>(to)) {
      if (ParameterDefn * param = dyn_cast<ParameterDefn>(lval->value())) {
        param->setFlag(ParameterDefn::LValueParam, true);
      }
    }

    DASSERT_OBJ(in->isSingular(), in);
  }

  return in;
}

Expr * FinalizeTypesPassImpl::visitCall(CallExpr * in) {
  if (in->candidates().size() == 1) {
    CallCandidate * cd = in->candidates().front();
    FunctionDefn * method = cd->method();

    if (method == NULL) {
      return visitIndirectCall(in);
    }

    // Handle the case where the method is a template, or is contained within a template.
    bool isTemplateMethod = method->isTemplate() || method->isTemplateMember();
    if (isTemplateMethod) {
      method = cast_or_null<FunctionDefn>(doPatternSubstitutions(in->location(), method, cd->env()));
      if (method == NULL || !AnalyzerBase::analyzeValueDefn(method, Task_PrepTypeComparison)) {
        return &Expr::ErrorVal;
      }

      DASSERT_OBJ(method->isSingular(), method);
      cd->setMethod(method);
      cd->setFunctionType(method->functionType());

      if (method->isCtor()) {
        NewExpr * ne = cast<NewExpr>(cd->base());
        ne->setType(method->functionType()->selfParam()->type());
      }
    }

    // Insure that we can see the method.
    AnalyzerBase::checkAccess(in->location(), subject_, method);

    // Handle argument conversions.
    ExprList callingArgs;
    if (!coerceArgs(cd, in->args(), callingArgs)) {
      return &Expr::ErrorVal;
    }

    // Handle 'self' param
    Expr * selfArg = NULL;
    if (cd->base() != NULL) {
      selfArg = visitExpr(cd->base());
      if (selfArg == NULL) {
        return &Expr::ErrorVal;
      }

      if (!AnalyzerBase::analyzeType(selfArg->type(), Task_PrepTypeComparison)) {
        return &Expr::ErrorVal;
      }
    }

    // See if we can evaluate the call at compile-time.
    // TODO: Dereference any 'let' statements to constants if possible.
    Expr * expr = method->eval(in->location(), selfArg, callingArgs);
    if (expr != NULL) {
      // Special case for dynamic cast, which is returned by typecast[T] intrinsic.
      if (expr->exprType() == Expr::UnboxCast) {
        expr = handleUnboxCast(static_cast<CastExpr *>(expr));
      }
      DASSERT_OBJ(expr->isSingular(), expr);
      DASSERT(expr->isSingular());
      return expr;
    }

    // Assert that there are no more unsized ints.
    if (!method->isIntrinsic()) {
      for (ExprList::iterator it = callingArgs.begin(); it != callingArgs.end(); ++it) {
        DASSERT_OBJ(!(*it)->type()->isUnsizedIntType(), *it);
      }
    }

    // Return a function call expression
    Expr::ExprType callType = Expr::FnCall;
    if (in->exprType() == Expr::Construct && method->isCtor()) {
      callType = Expr::CtorCall;
    } else if (method->storageClass() == Storage_Instance &&
        !method->isFinal() && !method->isCtor() && in->exprType() != Expr::ExactCall) {
      callType = Expr::VTableCall;
    }

    FnCallExpr * result = new FnCallExpr(callType, in->location(), method, selfArg);
    result->args().append(callingArgs.begin(), callingArgs.end());

    if (callType == Expr::CtorCall) {
      DASSERT_OBJ(method->functionType()->selfParam() != NULL, method);
      DASSERT_OBJ(method->functionType()->selfParam()->type() != NULL, method);
      DASSERT_OBJ(!method->functionType()->selfParam()->type()->isVoidType(), method);
      result->setType(method->functionType()->selfParam()->type());
    } else {
      //DASSERT_OBJ(strcmp(method->name(), "construct") != 0, method);
      DASSERT_OBJ(method->returnType() != NULL, method);
      result->setType(method->returnType());
      if (method->storageClass() != Storage_Instance) {
        result->setSelfArg(NULL);
      }
    }

    DASSERT_OBJ(result->isSingular(), result);
    return result;
  }

  diag.error(in) << in << " has " << in->candidates().size() << " candidates";
  for (Candidates::iterator it = in->candidates().begin(); it != in->candidates().end(); ++it) {
    CallCandidate * cc = *it;
    cc->updateConversionRank();
    if (cc->env().empty()) {
      diag.info(cc->method()) << Format_Type << cc->method() << " [" << cc->conversionRank() << "]";
    } else {
      diag.info(cc->method()) << Format_Type  << cc->method() << " with " <<
          Format_Dealias << cc->env() <<" [" << cc->conversionRank() << "] ";
    }
  }

  return &Expr::ErrorVal;
}

Expr * FinalizeTypesPassImpl::visitIndirectCall(CallExpr * in) {
  CallCandidate * cd = in->candidates().front();
  DASSERT(cd->base() != NULL);
  DASSERT(in->exprType() != Expr::Construct);

  ExprList callingArgs;
  if (!coerceArgs(cd, in->args(), callingArgs)) {
    return &Expr::ErrorVal;
  }

  // Handle 'self' param
  Expr * fnValue = visitExpr(cd->base());
  if (fnValue == NULL) {
    return &Expr::ErrorVal;
  }

  if (!AnalyzerBase::analyzeType(
      const_cast<FunctionType *>(cd->functionType()), Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  if (!AnalyzerBase::analyzeType(fnValue->type(), Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  if (const FunctionType * fnType = dyn_cast<FunctionType>(fnValue->type())) {
    if (!fnType->isStatic()) {
      diag.error(in) << "Attempt to call function expression '" << fnValue << "'" <<
          " with no object";
      return &Expr::ErrorVal;
    }
  } else if (const BoundMethodType * bmType = dyn_cast<BoundMethodType>(fnValue->type())) {
    // TODO: Extra checks needed?
  } else {
    diag.error(in) << "Non-callable type '" << fnValue->type() << "'";
    return &Expr::ErrorVal;
  }

  // Return a function call expression
  IndirectCallExpr * result = new IndirectCallExpr(Expr::IndirectCall, in->location(), fnValue);
  result->args().append(callingArgs.begin(), callingArgs.end());

  result->setType(cd->functionType()->returnType());
  //if (method->storageClass() != Storage_Instance) {
  //  result->setSelfArg(NULL);
  //}

  DASSERT_OBJ(result->isSingular(), result);
  return result;
}

bool FinalizeTypesPassImpl::coerceArgs(CallCandidate * cd, const ExprList & args, ExprList & outArgs) {

  const FunctionType * fnType = cd->functionType();
  size_t paramCount = fnType->params().size();
  size_t argCount = args.size();
  outArgs.resize(paramCount);
  std::fill(outArgs.begin(), outArgs.end(), (Expr *)NULL);

  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    int paramIndex = cd->parameterIndex(argIndex);
    ParameterDefn * param = fnType->params()[paramIndex];
    const Type * paramType = param->type();
    DASSERT(paramType->isSingular());
    Expr * argVal = visitExpr(args[argIndex]);
    if (isErrorResult(argVal)) {
      return false;
    }

    Expr * castArgVal = addCastIfNeeded(argVal, paramType);
    if (castArgVal == NULL) {
      diag.error(argVal) << "Unable to convert argument of type " << argVal->type() << " to " <<
          paramType;
      return false;
    }

    if (param->isVariadic()) {
      // Handle variadic parameters - build an array literal.
      ArrayLiteralExpr * arrayParam = cast_or_null<ArrayLiteralExpr>(outArgs[paramIndex]);
      if (arrayParam == NULL) {
        arrayParam = AnalyzerBase::createArrayLiteral(argVal->location(), paramType);
        AnalyzerBase::analyzeType(arrayParam->type(), Task_PrepMemberLookup);
        DASSERT(arrayParam->isSingular());
        outArgs[paramIndex] = arrayParam;
      }

      DASSERT(castArgVal->isSingular());
      arrayParam->appendArg(castArgVal);
    } else {
      outArgs[paramIndex] = castArgVal;
    }
  }

  // Fill in default params
  for (size_t paramIndex = 0; paramIndex < paramCount; ++paramIndex) {
    if (outArgs[paramIndex] == NULL) {
      ParameterDefn * param = fnType->params()[paramIndex];
      if (param->isVariadic()) {
        // Pass a null array - possibly a static singleton.
        ArrayLiteralExpr * arrayParam = AnalyzerBase::createArrayLiteral(
            param->location(), param->type());
        //diag.debug() << "Creating default array literal of type " << param->type() << " calling method " << cd->method();
        AnalyzerBase::analyzeType(arrayParam->type(), Task_PrepMemberLookup);
        outArgs[paramIndex] = arrayParam;
      } else {
        outArgs[paramIndex] = param->initValue();
        DASSERT_OBJ(outArgs[paramIndex] != NULL, param);
      }
    }
  }

  return true;
}

Defn * FinalizeTypesPassImpl::doPatternSubstitutions(SLC & loc, Defn * def, BindingEnv & env) {
  // First perform pattern substitutions on the parent definition.
  Defn * parent = def->parentDefn();
  if (parent != NULL && (parent->isTemplate() || parent->isTemplateMember())) {
    parent = doPatternSubstitutions(loc, parent, env);
    if (parent == NULL) {
      return NULL;
    }

    if (!AnalyzerBase::analyzeDefn(parent, Task_PrepMemberLookup)) {
      return NULL;
    }

    Defn * newdef = NULL;
    DefnList defns;

    if (TypeDefn * tdef = dyn_cast<TypeDefn>(parent)) {
      tdef->typeValue()->memberScope()->lookupMember(def->name(), defns, false);
    } else {
      DFAIL("Implement pattern substitutions for parent scopes other than types");
    }

    // Attempt to find the instantiated definition that corresponds with the original template
    // definition 'def'. This can be identified because it has the same value for 'ast'.
    for (DefnList::iterator it = defns.begin(); it != defns.end(); ++it) {
      if ((*it)->ast() == def->ast()) {
        newdef = *it;
        break;
      }
    }

    if (newdef == NULL) {
      diag.fatal(loc) << "Missing definition in template instantiation " << def;
      return NULL;
    }

    def = newdef;
  }

  if (def->isTemplate()) {
    TemplateSignature * tsig = def->templateSignature();

    // Check to make sure that all template params are bound.
    size_t numVars = tsig->patternVarCount();
    for (size_t i = 0; i < numVars; ++i) {
      const PatternVar * var = tsig->patternVar(i);
      const Type * value = env.get(var);
      if (value == NULL || !value->isSingular()) {
        diag.fatal(loc) << "Unable to deduce template parameter '" << var << "' for '" <<
            Format_Verbose << def << "'";
        return NULL;
      }
    }

    def = tsig->instantiate(loc, env, true);
    DASSERT_OBJ(def->isSingular(), def);
  }

  DASSERT_OBJ(def->isSingular(), def);
  return def;
}

Expr * FinalizeTypesPassImpl::visitCast(CastExpr * in) {
  Expr * arg = visitExpr(in->arg());

  // Eliminate redundant cast if not needed.
  if (in->type()->isEqual(arg->type())) {
    return arg;
  }

  if (in->exprType() == Expr::UnboxCast) {
    return handleUnboxCast(in);
  }

  // Attempt to cast
  arg = in->type()->explicitCast(in->location(), arg);
  return arg ? arg : &Expr::ErrorVal;
}

Expr * FinalizeTypesPassImpl::visitInstanceOf(InstanceOfExpr * in) {
  Expr * value = visitExpr(in->value());
  const Type * tyFrom = dealias(value->type());
  const Type * tyTo = dealias(in->toType());

  if (tyFrom == NULL || tyTo == NULL) {
    return NULL;
  }

  DASSERT_OBJ(tyFrom->isSingular(), tyFrom);
  DASSERT_OBJ(tyTo->isSingular(), tyTo);

  if (tyFrom->isEqual(tyTo)) {
    return new BinaryExpr(Expr::Prog2, in->location(), &BoolType::instance, value,
        ConstantInteger::getConstantBool(in->location(), true));
  }

  if (const UnionType * ut = dyn_cast<UnionType>(tyFrom)) {
    in->setValue(value);
    return visitUnionTest(in, value, ut, tyTo);
  }

  const CompositeType * ctTo = dyn_cast<CompositeType>(tyTo);
  const CompositeType * ctFrom = dyn_cast<CompositeType>(tyFrom);

  bool isConstTrue = false; // Test always succeeds
  bool isConstFalse = false; // Test always fails

  if (ctTo != NULL && ctFrom != NULL) {
    AnalyzerBase::checkAccess(in->location(), subject_, ctTo->typeDefn());
    if (ctTo->typeClass() == Type::Struct) {
      // Structs are not polymorphic, so we know the answer at compile time.
      if (ctTo->typeClass() != Type::Struct) {
        isConstFalse = true;
      } else if (ctFrom->isSubclassOf(ctTo)) {
        // to struct from struct.
        isConstTrue = true;
      } else {
        isConstFalse = true;
      }
    } else if (ctTo->typeClass() == Type::Class) {
      if (ctTo->typeClass() == Type::Class) {
        // to class from class
        if (ctFrom->isSubclassOf(ctTo)) {
          isConstTrue = true;
        } else if (!ctTo->isSubclassOf(ctFrom)) {
          isConstFalse = true;
        }
      } else if (ctTo->typeClass() == Type::Interface) {
        // to interface from class.
        if (ctFrom->isSubclassOf(ctTo)) {
          isConstTrue = true;
        }
      } else {
        isConstFalse = true;
      }
    } else if (ctTo->typeClass() == Type::Interface) {
      if (ctTo->typeClass() == Type::Class) {
        // to class from interface
        if (ctFrom->isSubclassOf(ctTo)) {
          isConstTrue = true;
        }
      } else if (ctTo->typeClass() == Type::Interface) {
        // to interface from interface
        if (ctFrom->isSubclassOf(ctTo)) {
          isConstTrue = true;
        }
      } else {
        isConstFalse = true;
      }
    } else {
      DFAIL("IllegalState");
    }
  } else {
    isConstFalse = true;
  }

  // TODO: If isConstTrue or isConstFalse is set, we can also
  // strip off any expressions that have no side effects.

  if (isConstTrue) {
    return ConstantInteger::getConstantBool(in->location(), true);
  } else if (isConstFalse) {
    return ConstantInteger::getConstantBool(in->location(), false);
  } else {
    in->setValue(value);
    in->setToType(ctTo);
    return in;
  }
}

Expr * FinalizeTypesPassImpl::visitUnionTest(InstanceOfExpr * in, Expr * value,
    const UnionType * from, const Type * to) {

  // List of member types that are subtype of the 'to' type.
  TypeList matchingTypes;
  ConversionRank bestRank = Incompatible;
  for (TupleType::const_iterator it = from->members().begin(); it != from->members().end(); ++it) {
    Type * memberType = const_cast<Type *>(dealias(*it));
    // TODO: Should this use conversion test, or subtype test?
    ConversionRank rank = to->canConvert(memberType);
    if (rank >= ExactConversion) {
      if (rank > bestRank) {
        bestRank = rank;
        matchingTypes.clear();
        matchingTypes.push_back(memberType);
      } else if (rank == bestRank) {
        matchingTypes.push_back(memberType);
      }
    }
  }

  if (matchingTypes.empty()) {
    // The test can never succeed, but we still need to evaluate the side effects.
    return new BinaryExpr(Expr::Prog2, in->location(), &BoolType::instance, value,
        ConstantInteger::getConstantBool(in->location(), false));
  } else if (matchingTypes.size() == 1) {
    // Simplest case
    Type * memberType = matchingTypes.front();

    // If the member type is exactly equal to the type we are testing for, then that
    // is the simplest case. Otherwise, we'll need to do additional tests on the value.
    if (memberType->isEqual(to)) {
      return in;
      //in->setTypeIndex(index);
      //class InstanceOfExpr : public Expr {
      //int index = from->getTypeIndex(memberType);
    }

    diag.info(in) << "Found " << matchingTypes.size() << " type in union: " << memberType;
    DFAIL("Implement");
  } else {
    diag.info(in) << "Found " << matchingTypes.size() << " types in union.";
    DFAIL("Implement");
  }

  //const TypeList & members() const { return members_; }
  DFAIL("Implement");
}

Expr * FinalizeTypesPassImpl::visitRefEq(BinaryExpr * in) {
  Expr * v1 = visitExpr(in->first());
  Expr * v2 = visitExpr(in->second());

  if (isErrorResult(v1) || isErrorResult(v2)) {
    return in;
  }

  in->setFirst(v1);
  in->setSecond(v2);
  const Type * t1 = in->first()->type();
  const Type * t2 = in->second()->type();
  DASSERT_OBJ(t1 != NULL, in->first());
  DASSERT_OBJ(t2 != NULL, in->second());

  if (t1->isReferenceType()) {
    if (!t2->isReferenceType()) {
      diag.fatal(in) << "Can't compare reference type '" << t1 <<
      "' with non-reference type '" << t2 << "'";
      return in;
    }

    const Type * tr = findCommonType(t1, t2);
    if (tr == NULL) {
      diag.fatal(in) << "Can't compare incompatible types '" << t1 <<
      "' and '" << t2 << "'";
      return in;
    }

    in->setFirst(addCastIfNeeded(in->first(), tr));
    in->setSecond(addCastIfNeeded(in->second(), tr));
    return in;
  } else if (isa<PointerType>(t1) || isa<AddressType>(t1)) {
    const Type * e0 = t1->typeParam(0);
    if (isa<PointerType>(t2) || isa<AddressType>(t2)) {
      if (e0->isEqual(t2->typeParam(0))) {
        if (isa<PointerType>(t2)) {
          in->setFirst(addCastIfNeeded(in->first(), t2));
        } else {
          in->setSecond(addCastIfNeeded(in->second(), t1));
        }

        return in;
      }
    } else if (t2->isReferenceType()) {
      if (e0->isEqual(t2)) {
        in->setSecond(addCastIfNeeded(in->second(), t1));
        return in;
      } else if (t2->isEqual(&NullType::instance)) {
        in->setSecond(addCastIfNeeded(in->second(), t1));
        return in;
      }

      diag.error(in) << "Can't compare " << t1 << " to " << t2;
      DFAIL("Implement");
    }

    diag.error(in) << "Can't compare " << t1 << " to " << t2;
    DFAIL("Implement");
  } else if (t2->isReferenceType()) {
    diag.fatal(in) << "Can't compare non-reference type '" << t1 <<
    "' with reference type '" << t2 << "'";
    return in;
  } else if (t1->typeClass() == Type::NPointer
      && t2->typeClass() == Type::NPointer
      && t1 == t2) {
    return in;
  } else if (t1->typeClass() == Type::NAddress
      && t2->typeClass() == Type::NAddress
      && t1 == t2) {
    return in;
  } else {
    // Otherwise, it's just a regular equality test.
    diag.error(in) << "Can't compare " << t1 << " and " << t2;
    DFAIL("Implement");
  }
}

Expr * FinalizeTypesPassImpl::addCastIfNeeded(Expr * in, const Type * toType) {
  return ExprAnalyzer(subject_->module(), subject_->definingScope(), subject_, NULL)
        .doImplicitCast(in, toType, tryCoerciveCasts_);
}

Expr * FinalizeTypesPassImpl::handleUnboxCast(CastExpr * in) {
  if (const PrimitiveType * ptype = dyn_cast<PrimitiveType>(in->type())) {
    return ExprAnalyzer(subject_->module(), subject_->definingScope(), subject_, NULL)
        .doUnboxCast(in->arg(), in->type());
  }

  return in;
}

} // namespace tart

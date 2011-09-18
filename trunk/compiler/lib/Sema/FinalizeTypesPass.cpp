/* ================================================================ *
 TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeConversion.h"
#include "tart/Type/TypeRelation.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/AmbiguousPhiType.h"

#include "tart/Sema/FinalizeTypesPassImpl.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/TypeTransform.h"
#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// FinalizeTypesPassImpl

Expr * FinalizeTypesPass::run(Defn * source, Expr * in, BindingEnv & env) {
  FinalizeTypesPassImpl instance(source, env);
  return instance.runImpl(in);
}

Expr * FinalizeTypesPass::runImpl(Expr * in) {
  Expr * result = visitExpr(in);
  if (!diag.inRecovery()) {
    DASSERT(result->isSingular()) << "Result " << result << " is non-singular.";
  }
  return result;
}

Expr * FinalizeTypesPassImpl::visitConstantInteger(ConstantInteger * in) {
  return in;
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
  DASSERT(first->isSingular()) << "Non-singular expression: " << first;
  Expr * second = visitExpr(in->second());
  if (!isErrorResult(first) && !isErrorResult(second)) {
    bool isAlreadyInt = false;
    if (const PrimitiveType * ptype = dyn_cast_or_null<PrimitiveType>(second->type())) {
      isAlreadyInt = isIntegerTypeId(ptype->typeId());
    }

    if (!isAlreadyInt) {
      second = Int32Type::instance.implicitCast(in->location(), second);
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

    Expr * castResult = addCastIfNeeded(from, to->type());
    if (!isErrorResult(castResult)) {
      in->setFromExpr(castResult);
      in->setToExpr(to);
    }

    if (LValueExpr * lval = dyn_cast<LValueExpr>(to)) {
      if (ParameterDefn * param = dyn_cast<ParameterDefn>(lval->value())) {
        param->setFlag(ParameterDefn::LValueParam, true);
        param->setIsAssignedTo();
      } else if (VariableDefn * var = dyn_cast<VariableDefn>(lval->value())) {
        var->setIsAssignedTo();
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
      return visitCallExpr(in);
    }

    // Handle the case where the method is a template, or is contained within a template.
    DASSERT_OBJ(!method->isScaffold(), method);
    bool isTemplateMethod = method->isTemplate() || method->isTemplateMember();
    if (isTemplateMethod) {
      GC * context = in->singularCandidate();
      QualifiedTypeVarMap varValues;
      env_.toTypeVarMap(varValues, context);
      method = cast_or_null<FunctionDefn>(
          doPatternSubstitutions(in->location(), method, varValues));
      if (method == NULL || !AnalyzerBase::analyzeFunction(method, Task_PrepTypeComparison)) {
        return &Expr::ErrorVal;
      }

      DASSERT_OBJ(method->isSingular(), method);
      DASSERT_OBJ(!method->isScaffold(), method);
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

      // If the self argument is a struct type, and it's a parameter of the calling
      // function, then we need to insure that we can take the address of the struct
      // by converting it into an LValue.
      if (LValueExpr * lval = dyn_cast<LValueExpr>(selfArg)) {
        if (ParameterDefn * param = dyn_cast<ParameterDefn>(lval->value())) {
          if (param->type()->typeClass() == Type::Struct && !param->isVariadic()) {
            param->setFlag(ParameterDefn::LValueParam, true);
          }
        }
      }
    }

    // See if we can evaluate the call at compile-time.
    // TODO: Dereference any 'let' statements to constants if possible.
    Expr * expr = method->eval(in->location(), subject_->module(), selfArg, callingArgs);
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
        !method->isFinal() && !method->isCtor() && in->exprType() != Expr::SuperCall) {
      callType = Expr::VTableCall;
    }

    // PrepConstruction is just a convenient way to run FieldPass.
    AnalyzerBase::analyzeType(method->type(), Task_PrepConstruction);
    FnCallExpr * result = new FnCallExpr(callType, in->location(), method, selfArg);
    result->args().append(callingArgs.begin(), callingArgs.end());

    if (callType == Expr::CtorCall) {
      DASSERT_OBJ(method->functionType()->selfParam() != NULL, method);
      DASSERT_OBJ(method->functionType()->selfParam()->type(), method);
      DASSERT_OBJ(!method->functionType()->selfParam()->type()->isVoidType(), method);
      result->setType(method->functionType()->selfParam()->type());
    } else {
      //DASSERT_OBJ(strcmp(method->name(), "construct") != 0, method);
      DASSERT(method->returnType()) << "Missing return type for " << method;
      result->setType(method->returnType());
      if (method->storageClass() != Storage_Instance) {
        result->setSelfArg(NULL);
      }
    }

    DASSERT_OBJ(result->isSingular(), result);
    DASSERT_OBJ(result->type()->isSingular(), result);
    return result;
  }

  if (in->candidates().empty()) {
    if (in->exprType() == Expr::SuperCall) {
      diag.error(in) << "No matching functions found for call to super(" <<
          bindFormat(formatExprTypeList, in->args()) << ")";
      return &Expr::ErrorVal;
    } else if (in->function() != NULL) {
      diag.error(in) << "No matching functions found for call to " << in->function() << "(" <<
        bindFormat(formatExprTypeList, in->args()) << ")";
      return &Expr::ErrorVal;
    }
  }

  diag.error(in) << in << " has " << in->candidates().size() << " candidates";
  for (Candidates::iterator it = in->candidates().begin(); it != in->candidates().end(); ++it) {
    CallCandidate * cc = *it;
    cc->updateConversionRank();
    if (env_.empty()) {
      diag.info(cc->method()) << Format_Type << cc->method() << " [" << cc->conversionRank() << "]";
    } else {
      diag.info(cc->method()) << Format_Type  << cc->method() << " with " <<
          Format_Dealias << env_ <<" [" << cc->conversionRank() << "] ";
    }
  }

  return &Expr::ErrorVal;
}

Expr * FinalizeTypesPassImpl::visitCallExpr(CallExpr * in) {
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

  const Type * type = dealias(fnValue->type());
  if (const FunctionType * fnType = dyn_cast<FunctionType>(type)) {
    if (!fnType->isStatic()) {
      diag.error(in) << "Attempt to call function expression '" << fnValue << "'" <<
          " with no object";
      return &Expr::ErrorVal;
    }
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

bool FinalizeTypesPassImpl::coerceArgs(
    CallCandidate * cd, const ExprList & args, ExprList & outArgs)
{
  const FunctionType * fnType = cd->functionType();
  size_t paramCount = fnType->params().size();
  size_t argCount = args.size();
  outArgs.resize(paramCount);
  std::fill(outArgs.begin(), outArgs.end(), (Expr *)NULL);

  for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
    int paramIndex = cd->parameterIndex(argIndex);
    ParameterDefn * param = fnType->params()[paramIndex];
    const Type * paramType = param->type().type();
    DASSERT(paramType->isSingular());
    Expr * argVal = visitExpr(args[argIndex]);
    if (isErrorResult(argVal)) {
      return false;
    }

    Expr * castArgVal = addCastIfNeeded(argVal, paramType);
    if (isErrorResult(castArgVal)) {
      return false;
    }

    if (cd->method() && !cd->method()->isIntrinsic()) {
      DASSERT(!castArgVal->type()->isUnsizedIntType());
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
            cd->callExpr()->location(), param->type().type());
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

Defn * FinalizeTypesPassImpl::doPatternSubstitutions(SLC & loc, Defn * def,
    const QualifiedTypeVarMap & varValues)
{
  // First perform pattern substitutions on the parent definition.
  Defn * parent = def->parentDefn();
  if (parent != NULL && (parent->isTemplate() || parent->isTemplateMember())) {
    parent = doPatternSubstitutions(loc, parent, varValues);
    if (parent == NULL) {
      return NULL;
    }

    Defn * newdef = NULL;
    DefnList defns;

    if (TypeDefn * tdef = dyn_cast<TypeDefn>(parent)) {
      if (!AnalyzerBase::analyzeTypeDefn(tdef, Task_PrepMemberLookup)) {
        return NULL;
      }
      // If the original was a constructor, make sure that any implicit constructors
      // get created before searching.
      if (const FunctionDefn * fn = dyn_cast<FunctionDefn>(def)) {
        if (fn->isCtor() && !AnalyzerBase::analyzeTypeDefn(tdef, Task_PrepConstruction)) {
          return NULL;
        }
      }
      tdef->typePtr()->memberScope()->lookupMember(def->name(), defns, false);
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
    Template * tm = def->templateSignature();

    // Check to make sure that all template params are bound.
    size_t numVars = tm->patternVarCount();
    for (size_t i = 0; i < numVars; ++i) {
      const TypeVariable * var = tm->patternVar(i);
      QualifiedTypeVarMap::const_iterator it = varValues.find(var);
      if (it == varValues.end()) {
        diag.fatal(loc) << "Unable to deduce template parameter '" << var << "' for '" <<
            Format_Verbose << def << "' in environment " << env_;
        return NULL;
      } else {
        QualifiedType value = it->second;
        if (!value->isSingular()) {
          value = NormalizeTransform().transform(value);
          if (!value->isSingular()) {
            value->isSingular();
            diag.error(loc) << "Unable to narrow template parameter '" << var << "' for '" <<
                Format_Verbose << def << "'";
            diag.info() << "Environment = " << env_;
            diag.info() << Format_Type << "Value = " << value;
            DFAIL("???");
            return NULL;
          }
        }
      }
    }

    def = tm->instantiate(loc, varValues, Template::Singular | Template::NonScaffold);
    DASSERT_OBJ(def->isSingular(), def);
  }

  DASSERT_OBJ(def->isSingular(), def);
  return def;
}

Expr * FinalizeTypesPassImpl::visitCast(CastExpr * in) {
  Expr * arg = visitExpr(in->arg());

  // Eliminate redundant cast if not needed.
  if (TypeRelation::isEqual(in->type(), arg->type())) {
    return arg;
  }

  if (in->exprType() == Expr::UnboxCast) {
    return handleUnboxCast(in);
  }

  switch (in->exprType()) {
    case Expr::UnionMemberCast:
    case Expr::CheckedUnionMemberCast:
    case Expr::TryCast:
    case Expr::BitCast:
      in->setArg(arg);
      return in;

    default:
      // Attempt to cast
      arg = in->type()->explicitCast(in->location(), arg);
      return arg ? arg : &Expr::ErrorVal;
  }
}

Expr * FinalizeTypesPassImpl::visitInstanceOf(InstanceOfExpr * in) {
  Expr * value = visitExpr(in->value());
  const Type * tyFrom = dealias(value->type());
  const Type * tyTo = dealias(in->toType());

  if (tyFrom == NULL || tyTo == NULL) {
    return NULL;
  }

  if (!AnalyzerBase::analyzeType(tyFrom, Task_PrepTypeComparison)) {
    return NULL;
  }

  if (!AnalyzerBase::analyzeType(tyTo, Task_PrepTypeComparison)) {
    return NULL;
  }

  DASSERT_OBJ(tyFrom->isSingular(), tyFrom);
  DASSERT_OBJ(tyTo->isSingular(), tyTo);

  if (TypeRelation::isEqual(tyFrom, tyTo)) {
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
      } else if (TypeRelation::isSubclass(ctFrom, ctTo)) {
        // to struct from struct.
        isConstTrue = true;
      } else {
        isConstFalse = true;
      }
    } else if (ctTo->typeClass() == Type::Class) {
      if (ctTo->typeClass() == Type::Class) {
        // to class from class
        if (TypeRelation::isSubclass(ctFrom, ctTo)) {
          isConstTrue = true;
        } else if (!TypeRelation::isSubclass(ctTo, ctFrom)) {
          isConstFalse = true;
        }
      } else if (ctTo->typeClass() == Type::Interface) {
        // to interface from class.
        if (TypeRelation::isSubclass(ctFrom, ctTo)) {
          isConstTrue = true;
        }
      } else {
        isConstFalse = true;
      }
    } else if (ctTo->typeClass() == Type::Interface) {
      if (ctTo->typeClass() == Type::Class) {
        // to class from interface
        if (TypeRelation::isSubclass(ctFrom, ctTo)) {
          isConstTrue = true;
        }
      } else if (ctTo->typeClass() == Type::Interface) {
        // to interface from interface
        if (TypeRelation::isSubclass(ctFrom, ctTo)) {
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
    Type * memberType = const_cast<Type *>(dealias(it->type()));
    // TODO: Should this use conversion test, or subtype test?
    ConversionRank rank = TypeConversion::check(memberType, to);
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
    if (TypeRelation::isEqual(memberType, to)) {
      return in;
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

  if (const UnionType * u1 = dyn_cast<UnionType>(t1)) {
    ConversionRank rank = TypeConversion::convert(in->second(), u1, &v2);
    if (rank >= ExactConversion) {
      in->setSecond(v2);
      return in;
    }
  } else if (const UnionType * u2 = dyn_cast<UnionType>(t2)) {
    ConversionRank rank = TypeConversion::convert(in->first(), u2, &v1);
    if (rank >= ExactConversion) {
      in->setFirst(v1);
      return in;
    }
  }

  if (t1->isReferenceType()) {
    if (!t2->isReferenceType()) {
      diag.fatal(in) << "Can't compare reference type '" << t1 <<
      "' with non-reference type '" << t2 << "'";
      return in;
    }

    // Comparison with Null is allowed for all reference types.
    if (t1->isNullType() || t2->isNullType()) {
      if (t1->isNullType()) {
        in->setFirst(ConstantNull::get(in->first()->location(), t2));
      }

      if (t2->isNullType()) {
        in->setSecond(ConstantNull::get(in->second()->location(), t1));
      }

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
  } else if (isa<AddressType>(t1)) {
    QualifiedType e0 = t1->typeParam(0);
    if (isa<AddressType>(t2)) {
      if (TypeRelation::isEqual(e0, t2->typeParam(0))) {
        in->setSecond(addCastIfNeeded(in->second(), t1));
        return in;
      }
    } else if (t2->isReferenceType()) {
      if (TypeRelation::isEqual(e0, t2)) {
        in->setSecond(addCastIfNeeded(in->second(), t1));
        return in;
      } else if (t2->isNullType()) {
        in->setSecond(addCastIfNeeded(in->second(), t1));
        return in;
      }

      diag.error(in) << "Can't compare " << t1 << " to " << t2;
      DFAIL("Implement");
    }

    diag.error(in) << "Can't compare " << t1 << " to " << t2;
    DFAIL("Implement");
//  } else if (const UnionType * u1 = dyn_cast<UnionType>(t1)) {

  } else if (t2->isReferenceType()) {
    diag.fatal(in) << "Can't compare non-reference type '" << t1 <<
    "' with reference type '" << t2 << "'";
    return in;
  } else if (t1->typeClass() == Type::NAddress
      && t2->typeClass() == Type::NAddress
      && t1 == t2) {
    return in;
  } else {
    const Type * tr = findCommonType(t1, t2);
    if (tr == NULL) {
      // Otherwise, it's just a regular equality test.
      diag.error(in) << "Can't compare " << t1 << " and " << t2;
    } else {
      in->setFirst(addCastIfNeeded(in->first(), tr));
      in->setSecond(addCastIfNeeded(in->second(), tr));
    }

    return in;
  }
}

Expr * FinalizeTypesPassImpl::visitTupleCtor(TupleCtorExpr * in) {
  ExprList & args = in->args();
  QualifiedTypeList types;
  DASSERT(args.size() > 0);
  for (ExprList::iterator it = args.begin(); it != args.end(); ++it) {
    Expr * arg = visitExpr(*it);
    types.push_back(arg->type());
    *it = arg;
  }

  in->setType(TupleType::get(types));
  return in;
}

Expr * FinalizeTypesPassImpl::visitTypeLiteral(TypeLiteralExpr * in) {
  const Type * type = in->value();
  if (const TypeAssignment * tb = dyn_cast<TypeAssignment>(type)) {
    if (tb->value()) {
      return new TypeLiteralExpr(in->location(), tb->value().unqualified());
    }

    return in;
  }

  if (isa<TypeVariable>(type)) {
    diag.fatal(in) << "Unbound type var " << in;
    return in;
  }

  return in;
}

Expr * FinalizeTypesPassImpl::visitIf(IfExpr * in) {
  CFGPass::visitIf(in);
  if (const AmbiguousPhiType * phi = dyn_cast_or_null<AmbiguousPhiType>(in->type())) {
    QualifiedType singularCommon = getCommonPhiType(phi);
    if (singularCommon) {
      in->setType(singularCommon);
    }
  }

  if (in->type() != NULL && !in->type()->isVoidType()) {
    in->setThenVal(addCastIfNeeded(in->thenVal(), in->type()));
    if (in->elseVal() != NULL) {
      in->setElseVal(addCastIfNeeded(in->elseVal(), in->type()));
    }
  }

  return in;
}

Expr * FinalizeTypesPassImpl::visitSwitch(SwitchExpr * in) {
  CFGPass::visitSwitch(in);
  if (const AmbiguousPhiType * phi = dyn_cast_or_null<AmbiguousPhiType>(in->type())) {
    if (phi->common()) {
      in->setType(phi->common());
    }
  }

  if (in->type() != NULL && !in->type()->isVoidType()) {
    for (SwitchExpr::iterator it = in->begin(); it != in->end(); ++it) {
      *it = addCastIfNeeded(*it, in->type());
    }

    if (in->elseCase() != NULL) {
      in->setElseCase(addCastIfNeeded(in->elseCase(), in->type()));
    }
  }

  return in;
}

Expr * FinalizeTypesPassImpl::visitMatch(MatchExpr * in) {
  CFGPass::visitMatch(in);
  if (const AmbiguousPhiType * phi = dyn_cast_or_null<AmbiguousPhiType>(in->type())) {
    if (phi->common()) {
      in->setType(phi->common());
    }
  }

  if (in->type() != NULL && !in->type()->isVoidType()) {
    for (MatchExpr::iterator it = in->begin(); it != in->end(); ++it) {
      *it = addCastIfNeeded(*it, in->type());
    }

    if (in->elseCase() != NULL) {
      in->setElseCase(addCastIfNeeded(in->elseCase(), in->type()));
    }
  }

  return in;
}

Expr * FinalizeTypesPassImpl::addCastIfNeeded(Expr * in, QualifiedType toType, unsigned options) {
  return ExprAnalyzer(subject_->module(), subject_->definingScope(), subject_, NULL)
        .doImplicitCast(in, toType, options);
}

Expr * FinalizeTypesPassImpl::handleUnboxCast(CastExpr * in) {
  if (isa<PrimitiveType>(in->type())) {
    return ExprAnalyzer(subject_->module(), subject_->definingScope(), subject_, NULL)
        .doUnboxCast(in->arg(), in->type());
  }

  return in;
}

QualifiedType FinalizeTypesPassImpl::getCommonPhiType(const AmbiguousPhiType * phi) {
  if (Qualified<TypeConstraint> tc = phi->common().dyn_cast_or_null<TypeConstraint>()) {
    if (tc->singularValue()) {
      return tc->singularValue();
    }
  }
  return phi->common();
}

} // namespace tart

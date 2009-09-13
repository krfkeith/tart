/* ================================================================ *
 TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/UnionType.h"
#include "tart/CFG/Template.h"
#include "tart/Sema/FinalizeTypesPass.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// FinalizeTypesPass

Expr * FinalizeTypesPass::run(Expr * in) {
  FinalizeTypesPass instance;
  return instance.runImpl(in);
}

Expr * FinalizeTypesPass::runImpl(Expr * in) {
  Expr * result = visitExpr(in);
  DASSERT(result->isSingular());
  return result;
}

Expr * FinalizeTypesPass::visitLValue(LValueExpr * in) {
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

Expr * FinalizeTypesPass::visitScopeName(ScopeNameExpr * in) {
  DFAIL("Implement");
}

Expr * FinalizeTypesPass::visitElementRef(BinaryExpr * in) {
  // Cast array index to integer type.
  // TODO: Check if it's long first.
  Expr * first = visitExpr(in->first());
  Expr * second = visitExpr(in->second());
  if (!isErrorResult(first) && !isErrorResult(second)) {
    second = IntType::instance.implicitCast(in->location(), second);
    in->setFirst(first);
    in->setSecond(second);
    DASSERT_OBJ(first->isSingular(), first);
    DASSERT_OBJ(second->isSingular(), second);
    DASSERT_OBJ(in->isSingular(), in);
  }

  return in;
}

Expr * FinalizeTypesPass::visitAssign(AssignmentExpr * in) {
  return visitAssignImpl(in);
}

Expr * FinalizeTypesPass::visitPostAssign(AssignmentExpr * in) {
  return visitAssignImpl(in);
}

Expr * FinalizeTypesPass::visitAssignImpl(AssignmentExpr * in) {
  DASSERT_OBJ(in->toExpr()->type() != NULL, in);
  Expr * from = visitExpr(in->fromExpr());
  Expr * to = visitExpr(in->toExpr());
  if (!isErrorResult(from) && !isErrorResult(to)) {
    DASSERT_OBJ(to->isSingular(), to);

    if (!AnalyzerBase::analyzeType(to->type(), Task_PrepMemberLookup)) {
      return in;
    }

    from = to->type()->implicitCast(from->location(), from);
    in->setFromExpr(from);
    in->setToExpr(to);

    if (LValueExpr * lval = dyn_cast<LValueExpr>(to)) {
      if (ParameterDefn * param = dyn_cast<ParameterDefn>(lval->value())) {
        param->setFlag(ParameterDefn::LValueParam, true);
      }
    }

    DASSERT_OBJ(in->isSingular(), in);
  }

  return in;
}

Expr * FinalizeTypesPass::visitCall(CallExpr * in) {
  ExprList & args = in->args();
  size_t argCount = in->argCount();

  if (in->candidates().size() == 1) {
    CallCandidate * cd = in->candidates().front();
    FunctionDefn * method = cd->method();
    bool isTemplateMethod = method->isTemplate() || method->isTemplateMember();

    // Handle the case where the method is a template, or is contained within a template.
    if (isTemplateMethod) {
      method = cast_or_null<FunctionDefn>(doPatternSubstitutions(in->location(), method, cd->env()));
      if (method == NULL || !AnalyzerBase::analyzeValueDefn(method, Task_PrepCallOrUse)) {
        return &Expr::ErrorVal;
      }

      DASSERT_OBJ(method->isSingular(), method);
      cd->setMethod(method);

      if (method->isCtor()) {
        NewExpr * ne = cast<NewExpr>(cd->base());
        ne->setType(method->functionType()->selfParam()->type());
      }
    }

    size_t paramCount = method->functionType()->params().size();
    ExprList callingArgs(paramCount);
    callingArgs.resize(paramCount);
    std::fill(callingArgs.begin(), callingArgs.end(), (Expr *)NULL);

    for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
      int paramIndex = cd->parameterIndex(argIndex);
      ParameterDefn * param = method->functionType()->params()[paramIndex];
      Type * paramType = param->type();
      //Type * paramType = cd->paramType(argIndex);
      DASSERT(paramType->isSingular());
      Expr * argVal = visitExpr(args[argIndex]);
      if (isErrorResult(argVal)) {
        return &Expr::ErrorVal;
      }

      Expr * castArgVal = addCastIfNeeded(argVal, paramType);
      if (castArgVal == NULL) {
        diag.error(argVal) << "Unable to convert argument of type " << argVal->type() <<
        " to " << paramType;
        return &Expr::ErrorVal;
      }

      args[argIndex] = castArgVal;
      if (param->isVariadic()) {
        // Handle variadic parameters - build an array literal.
        ArrayLiteralExpr * arrayParam = cast_or_null<ArrayLiteralExpr>(callingArgs[paramIndex]);
        if (arrayParam == NULL) {
          arrayParam = AnalyzerBase::createArrayLiteral(argVal->location(), paramType);
          DASSERT(arrayParam->isSingular());
          callingArgs[paramIndex] = arrayParam;
        }

        DASSERT(castArgVal->isSingular());
        arrayParam->appendArg(castArgVal);
      } else {
        callingArgs[paramIndex] = castArgVal;
      }
    }

    // Fill in default params
    for (size_t paramIndex = 0; paramIndex < paramCount; ++paramIndex) {
      if (callingArgs[paramIndex] == NULL) {
        ParameterDefn * param = method->functionType()->params()[paramIndex];
        if (param->isVariadic()) {
          // Pass a null array - possibly a static singleton.
          ArrayLiteralExpr * arrayParam = AnalyzerBase::createArrayLiteral(
              param->location(), param->type());
          callingArgs[paramIndex] = arrayParam;
        } else {
          callingArgs[paramIndex] = param->defaultValue();
          DASSERT_OBJ(callingArgs[paramIndex] != NULL, param);
        }
      }
    }

    // Handle 'self' param
    Expr * selfArg = NULL;
    if (cd->base() != NULL) {
      selfArg = visitExpr(cd->base());
      if (selfArg == NULL) {
        return &Expr::ErrorVal;
      }
    }

    // See if we can evaluate the call at compile-time.
    // TODO: Dereference any 'let' statements to constants if possible.
    Expr * expr = method->eval(in->location(), selfArg, callingArgs);
    if (expr != NULL) {
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
      result->setType(method->functionType()->selfParam()->type());
    } else {
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

Defn * FinalizeTypesPass::doPatternSubstitutions(SLC & loc, Defn * def, BindingEnv & env) {
  // First perform pattern substitutions on the parent definition.
  Defn * parent = def->parentDefn();
  if (parent != NULL && (parent->isTemplate() || parent->isTemplateMember())) {
    parent = doPatternSubstitutions(loc, parent, env);
    if (parent == NULL) {
      return NULL;
    }

    if (!AnalyzerBase::analyzeDefn(parent, Task_PrepCallOrUse)) {
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

Expr * FinalizeTypesPass::visitInstantiate(InstantiateExpr * in) {
  DFAIL("Implement");
}

Expr * FinalizeTypesPass::visitCast(CastExpr * in) {
  Expr * arg = visitExpr(in->arg());

  // Eliminate redundant cast if not needed.
  if (in->type()->isEqual(arg->type())) {
    return arg;
  }

  // Attempt to cast
  arg = in->type()->implicitCast(in->location(), arg);
  return arg ? arg : &Expr::ErrorVal;
}

Expr * FinalizeTypesPass::visitInstanceOf(InstanceOfExpr * in) {
  Expr * value = visitExpr(in->value());
  Type * tyFrom = dealias(value->type());
  Type * tyTo = dealias(in->toType());

  if (tyFrom == NULL || tyTo == NULL) {
    return NULL;
  }

  DASSERT_OBJ(tyFrom->isSingular(), tyFrom);
  DASSERT_OBJ(tyTo->isSingular(), tyTo);

  if (tyFrom->isEqual(tyTo)) {
    return new BinaryExpr(Expr::Prog2, in->location(), &BoolType::instance, value,
        ConstantInteger::getConstantBool(in->location(), true));
  }

  if (UnionType * ut = dyn_cast<UnionType>(tyFrom)) {
    in->setValue(value);
    return visitUnionTest(in, value, ut, tyTo);
  }

  CompositeType * ctTo = dyn_cast<CompositeType>(tyTo);
  CompositeType * ctFrom = dyn_cast<CompositeType>(tyFrom);

  bool isConstTrue = false; // Test always succeeds
  bool isConstFalse = false; // Test always fails

  if (ctTo != NULL && ctFrom != NULL) {
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

Expr * FinalizeTypesPass::visitUnionTest(InstanceOfExpr * in, Expr * value, UnionType * from,
    Type * to) {
  // List of member types that are subtype of the 'to' type.
  TypeList matchingTypes;
  ConversionRank bestRank = Incompatible;
  for (TypeList::iterator it = from->members().begin(); it != from->members().end(); ++it) {
    Type * memberType = dealias(*it);
    // TODO: Should this use conversion test, or subtype test?
    ConversionRank rank = to->convert(memberType);
    if (rank != Incompatible) {
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
    return new BinaryExpr(Expr::Prog2, in->location(), &BoolType::instance, value,
        ConstantInteger::getConstantBool(in->location(), false));
  } else if (matchingTypes.size() == 1) {
    // Simplest case
    Type * memberType = matchingTypes.front();
    //int index = from->getTypeIndex(memberType);

    // If the member type is exactly equal to the type we are testing for, then that
    // is the simplest case. Otherwise, we'll need to do additional tests on the value.
    if (memberType->isEqual(to)) {
      return in;
      //in->setTypeIndex(index);
      //class InstanceOfExpr : public Expr {

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

Expr * FinalizeTypesPass::visitRefEq(BinaryExpr * in) {
  Type * t0 = in->first()->type();
  Type * t1 = in->second()->type();
  DASSERT_OBJ(t0 != NULL, in->first());
  DASSERT_OBJ(t1 != NULL, in->second());

  if (t0->isReferenceType()) {
    if (!t1->isReferenceType()) {
      diag.fatal(in) << "Can't compare reference type '" << t0 <<
      "' with non-reference type '" << t1 << "'";
      return in;
    }

    Type * tr = findCommonType(t0, t1);
    if (tr == NULL) {
      diag.fatal(in) << "Can't compare incompatible types '" << t0 <<
      "' and '" << t1 << "'";
      return in;
    }

    in->setFirst(addCastIfNeeded(in->first(), tr));
    in->setSecond(addCastIfNeeded(in->second(), tr));
    return in;
  } else if (t1->isReferenceType()) {
    diag.fatal(in) << "Can't compare non-reference type '" << t0 <<
    "' with reference type '" << t1 << "'";
    return in;
  } else {
    // Otherwise, it's just a regular equality test.
    DFAIL("Implement");
  }
}

Expr * FinalizeTypesPass::addCastIfNeeded(Expr * in, Type * toType) {
  DASSERT(in != NULL);
  if (isErrorResult(toType)) {
    return in;
  }

  in = LValueExpr::constValue(in);
  if (!AnalyzerBase::analyzeType(toType, Task_PrepMemberLookup)) {
    return in;
  }

  return toType->implicitCast(in->location(), in);
}

} // namespace tart

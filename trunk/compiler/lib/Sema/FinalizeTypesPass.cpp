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
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// FinalizeTypesPass

Expr * FinalizeTypesPass::run(Expr * in) {
  FinalizeTypesPass instance;
  return instance.runImpl(in);
}

Expr * FinalizeTypesPass::runImpl(Expr * in) {
  return visitExpr(in);
}

Expr * FinalizeTypesPass::visitLValue(LValueExpr * in) {
  if (in->getType() == NULL) {
    in->setType(in->value()->getType());
  }

  DASSERT_OBJ(in->getType() != NULL, in);
  DASSERT_OBJ(in->getType()->isSingular(), in);
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
    second = IntType::instance.implicitCast(in->getLocation(), second);
    in->setFirst(first);
    in->setSecond(second);
    DASSERT_OBJ(first->isSingular(), first);
    DASSERT_OBJ(second->isSingular(), second);
    DASSERT_OBJ(in->isSingular(), in);
  }

  return in;
}

Expr * FinalizeTypesPass::visitAssign(AssignmentExpr * in) {
  DASSERT_OBJ(in->getToExpr()->getType() != NULL, in);
  Expr * from = visitExpr(in->getFromExpr());
  Expr * to = visitExpr(in->getToExpr());
  if (!isErrorResult(from) && !isErrorResult(to)) {
    DASSERT_OBJ(to->isSingular(), to);

    from = to->getType()->implicitCast(from->getLocation(), from);
    in->setFromExpr(from);
    in->setToExpr(to);

    DASSERT_OBJ(in->isSingular(), in);
  }

  return in;
}

Expr * FinalizeTypesPass::visitPostAssign(AssignmentExpr * in) {
  DASSERT_OBJ(in->getToExpr()->getType() != NULL, in);
  Expr * from = visitExpr(in->getFromExpr());
  Expr * to = visitExpr(in->getToExpr());
  if (!isErrorResult(from) && !isErrorResult(to)) {
    DASSERT_OBJ(to->isSingular(), to);

    from = to->getType()->implicitCast(from->getLocation(), from);
    in->setFromExpr(from);
    in->setToExpr(to);

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

    // If it's a template, get the template params that were deduced.
    if (method->isTemplate()) {
      TemplateSignature * tsig = method->templateSignature();

      // Check to make sure that all template params are bound.
      size_t numVars = tsig->patternVarCount();
      for (size_t i = 0; i < numVars; ++i) {
        const PatternVar * var = tsig->patternVar(i);
        Type * value = cd->env().get(var);
        if (value == NULL || !value->isSingular()) {
          diag.fatal(in) << "Unable to deduce template parameters for call " << in;
          return &Expr::ErrorVal;
        }
      }

      method = cast<FunctionDefn>(tsig->instantiate(in->getLocation(), cd->env()));
      if (!AnalyzerBase::analyzeValueDefn(method, Task_PrepCallOrUse)) {
        return &Expr::ErrorVal;
      }
      
      cd->setMethod(method);
    }
    
    size_t paramCount = method->functionType()->params().size();
    ExprList callingArgs(paramCount);
    callingArgs.resize(paramCount);
    std::fill(callingArgs.begin(), callingArgs.end(), (Expr *)NULL);

    for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
      Type * paramType = cd->paramType(argIndex);
      Expr * argVal = visitExpr(args[argIndex]);
      if (isErrorResult(argVal)) {
        return &Expr::ErrorVal;
      }
      
      Expr * castArgVal = addCastIfNeeded(argVal, paramType);
      if (castArgVal == NULL) {
        diag.error(argVal) << "Unable to convert argument of type " << argVal->getType() <<
            " to " << paramType;
        return &Expr::ErrorVal;
      }

      callingArgs[cd->getParameterIndex(argIndex)] = args[argIndex] = castArgVal;
    }
    
    // Fill in default params
    for (size_t paramIndex = 0; paramIndex < paramCount; ++paramIndex) {
      if (callingArgs[paramIndex] == NULL) {
        ParameterDefn * param = method->functionType()->params()[paramIndex];
        callingArgs[paramIndex] = param->defaultValue();
        DASSERT_OBJ(callingArgs[paramIndex] != NULL, param);
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
    Expr * expr = method->eval(in->getLocation(), selfArg, callingArgs);
    if (expr != NULL) {
      DASSERT_OBJ(expr->isSingular(), expr);
      return expr;
    }
    
    // Assert that there are no more unsized ints.
    if (!method->isIntrinsic()) {
      for (ExprList::iterator it = callingArgs.begin(); it != callingArgs.end(); ++it) {
        DASSERT_OBJ(!(*it)->getType()->isUnsizedIntType(), *it);
      }
    }

    // Return a function call expression
    bool isCtorCall = in->exprType() == Expr::Construct && method->isCtor();
    FnCallExpr * result = new FnCallExpr(
      isCtorCall ? Expr::CtorCall : Expr::FnCall,
      in->getLocation(), method, selfArg);
    result->args().append(callingArgs.begin(), callingArgs.end());

    if (isCtorCall) {
      DASSERT_OBJ(method->functionType()->selfParam() != NULL, method);
      result->setType(method->functionType()->selfParam()->getType());
    } else {
      DASSERT_OBJ(method->returnType() != NULL, method);
      result->setType(method->returnType());
    }

    DASSERT_OBJ(result->isSingular(), result);
    return result;
  }
  
  diag.error(in) << in << " has " << in->candidates().size() << " candidates";
  return &Expr::ErrorVal;
}

Expr * FinalizeTypesPass::visitInstantiate(InstantiateExpr * in) {
  DFAIL("Implement");
}

Expr * FinalizeTypesPass::visitCast(CastExpr * in) {
  Expr * arg = visitExpr(in->arg());
  
  // Eliminate redundant cast if not needed.
  if (in->getType()->isEqual(arg->getType())) {
    return arg;
  }

  // Attempt to cast
  arg = in->getType()->implicitCast(in->getLocation(), arg);
  return arg ? arg : &Expr::ErrorVal;
}

Expr * FinalizeTypesPass::visitInstanceOf(InstanceOfExpr * in) {
  Expr * value = visitExpr(in->getValue());
  Type * tyFrom = dealias(value->getType());
  Type * tyTo = dealias(in->getToType());
  
  if (tyFrom == NULL || tyTo == NULL) {
    return NULL;
  }
  
  DASSERT_OBJ(tyFrom->isSingular(), tyFrom);
  DASSERT_OBJ(tyTo->isSingular(), tyTo);
  
  if (tyFrom->isEqual(tyTo)) {
    return new BinaryExpr(Expr::Prog2, in->getLocation(), &BoolType::instance, value,
        ConstantInteger::getConstantBool(in->getLocation(), true));
  }
  
  if (UnionType * ut = dyn_cast<UnionType>(tyFrom)) {
    in->setValue(value);
    return visitUnionTest(in, value, ut, tyTo);
  }

  CompositeType * ctTo = dyn_cast<CompositeType>(tyTo);
  CompositeType * ctFrom = dyn_cast<CompositeType>(tyFrom);

  bool isConstTrue = false;   // Test always succeeds
  bool isConstFalse = false;  // Test always fails
  
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
    return ConstantInteger::getConstantBool(in->getLocation(), true);
  } else if (isConstFalse) {
    return ConstantInteger::getConstantBool(in->getLocation(), false);
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
    return new BinaryExpr(Expr::Prog2, in->getLocation(), &BoolType::instance, value,
        ConstantInteger::getConstantBool(in->getLocation(), false));
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
  Type * t0 = in->first()->getType();
  Type * t1 = in->second()->getType();
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
  return toType->implicitCast(in->getLocation(), in);
}

} // namespace tart

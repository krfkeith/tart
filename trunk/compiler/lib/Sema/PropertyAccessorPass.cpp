/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/Exprs.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PropertyDefn.h"
#include "tart/CFG/Module.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Sema/PropertyAccessorPass.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// PropertyAccessorPass

Expr * PropertyAccessorPass::runImpl(Expr * in) {
  return visitExpr(in);
}

Expr * PropertyAccessorPass::visitLValue(LValueExpr * in) {
  in = cast<LValueExpr>(CFGPass::visitLValue(in));

  if (in->value()->defnType() == Defn::MacroArg) {
    VariableDefn * macroArg = static_cast<VariableDefn *>(in->value());
    macroArg->setInitValue(visitExpr(macroArg->initValue()));
  }

  if (PropertyDefn * prop = dyn_cast<PropertyDefn>(in->value())) {
    return getPropertyValue(in->location(), in->base(), prop);
  }

  return in;
}

#if 0
Expr * PropertyAccessorPass::visitCall(CallExpr * in) {
  in = cast<CallExpr>(CFGPass::visitCall(in));

  if (LValueExpr * lval = dyn_cast<LValueExpr>(in->function())) {
    if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
      return getParamPropertyValue(in->location(), in);
    }
  }

  return in;
}
#endif

Expr * PropertyAccessorPass::visitAssign(AssignmentExpr * in) {
  in = cast<AssignmentExpr>(CFGPass::visitAssign(in));
  if (LValueExpr * lval = dyn_cast<LValueExpr>(in->toExpr())) {
    if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
      return setPropertyValue(in->location(), lval->base(), prop, in->fromExpr());
    }
#if 0
  } else if (CallExpr * call = dyn_cast<CallExpr>(in->toExpr())) {
    if (LValueExpr * lval = dyn_cast<LValueExpr>(call->function())) {
      if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
        Expr * setProp = setParamPropertyValue(in->location(), call, in->fromExpr());
        if (isErrorResult(setProp)) {
          return &Expr::ErrorVal;
        }
      } else {
        diag.error(in) << "Cannot assign to a method call";
      }
    } else {
      diag.error(in) << "Cannot assign to a method call";
    }
#endif
  }


  return in;
}

Expr * PropertyAccessorPass::visitPostAssign(AssignmentExpr * in) {
  in = cast<AssignmentExpr>(CFGPass::visitPostAssign(in));
  if (LValueExpr * lval = dyn_cast<LValueExpr>(in->toExpr())) {
    if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
      DFAIL("Implement post-assign to property");
      return setPropertyValue(in->location(), lval->base(), prop, in->fromExpr());
    }
#if 0
  } else if (CallExpr * call = dyn_cast<CallExpr>(in->toExpr())) {
    if (LValueExpr * lval = dyn_cast<LValueExpr>(call->function())) {
      if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
        Expr * setProp = setParamPropertyValue(in->location(), call, in->fromExpr());
        if (isErrorResult(setProp)) {
          return &Expr::ErrorVal;
        }

        DFAIL("Implement PostAssign of indexed property");
      } else {
        diag.error(in) << "Cannot assign to a method call";
      }
    } else {
      diag.error(in) << "Cannot assign to a method call";
    }
#endif
  }


  return in;
}

Expr * PropertyAccessorPass::getPropertyValue(SLC & loc, Expr * basePtr, PropertyDefn * prop) {
  if (!AnalyzerBase::analyzeProperty(prop, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(prop->isSingular(), prop);
  DASSERT_OBJ(prop->getter()->isSingular(), prop);

  FunctionDefn * getter = prop->getter();
  if (getter == NULL) {
    diag.fatal(prop) << "No getter for property '" << prop->name() << "'";
    return &Expr::ErrorVal;
  }

  if (!AnalyzerBase::analyzeFunction(getter, Task_PrepMemberLookup)) {
    return &Expr::ErrorVal;
  }

  //ExprList callingArgs;
  // TODO: Compile-time evaluation if possible.
  /*Expr * expr = getter->eval(in->location(), callingArgs);
   if (expr != NULL) {
   return expr;
   }*/

  Expr::ExprType callType = Expr::FnCall;
  if (basePtr != NULL) {
    if (basePtr->type()->typeClass() == Type::Interface ||
        (basePtr->type()->typeClass() == Type::Class && !getter->isFinal())) {
      callType = Expr::VTableCall;
    } else if (basePtr->type()->typeClass() == Type::Struct) {
      if (LValueExpr * lval = dyn_cast<LValueExpr>(basePtr)) {
        if (ParameterDefn * param = dyn_cast<ParameterDefn>(lval->value())) {
          if (!param->isVariadic()) {
            // TODO: Do we need this code in the other cases?
            param->setFlag(ParameterDefn::LValueParam, true);
          }
        }
      }
    }
  }

  FnCallExpr * getterCall = new FnCallExpr(callType, loc, getter, basePtr);
  getterCall->setType(prop->type());
  module_->addSymbol(getter);
  return getterCall;
}

Expr * PropertyAccessorPass::setPropertyValue(SLC & loc, Expr * basePtr, PropertyDefn * prop,
    Expr * value) {

  DASSERT(value != NULL);
  if (!AnalyzerBase::analyzeProperty(prop, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  FunctionDefn * setter = prop->setter();
  if (setter == NULL) {
    diag.error(loc) << "Attempt to set value of read-only property '" << prop << "'";
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(prop->isSingular(), prop);
  DASSERT_OBJ(setter->isSingular(), prop);

  if (!AnalyzerBase::analyzeFunction(setter, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  //ExprList callingArgs;
  //callingArgs.push_back(value);

  // TODO: Compile-time evaluation if possible.
  /*Expr * expr = getter->eval(in->location(), callingArgs);
   if (expr != NULL) {
   return expr;
   }*/

  Expr::ExprType callType = Expr::FnCall;
  if (basePtr != NULL) {
    if (basePtr->type()->typeClass() == Type::Interface ||
        (basePtr->type()->typeClass() == Type::Class && !setter->isFinal())) {
      callType = Expr::VTableCall;
    }
  }

  FnCallExpr * setterCall = new FnCallExpr(callType, loc, setter, basePtr);
  setterCall->setType(prop->type());
  setterCall->appendArg(value);
  module_->addSymbol(setter);
  return setterCall;
}

#if 0
Expr * PropertyAccessorPass::getParamPropertyValue(SLC & loc, CallExpr * call) {

  LValueExpr * lval = cast<LValueExpr>(call->function());
  PropertyDefn * prop = cast<PropertyDefn>(lval->value());
  Expr * basePtr = lval->base();
  //const ASTNodeList & args = call->args();

  if (!AnalyzerBase::analyzeProperty(prop, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(prop->isSingular(), prop);
  DASSERT_OBJ(prop->getter()->isSingular(), prop);

  FunctionDefn * getter = prop->getter();
  if (getter == NULL) {
    diag.fatal(prop) << "No getter for property '" << prop->name() << "'";
    return &Expr::ErrorVal;
  }

  if (!AnalyzerBase::analyzeFunction(getter, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(getter->returnType()->isEqual(prop->type()), getter);
  DASSERT_OBJ(!getter->returnType()->isVoidType(), getter);

#if 0

  CallExpr * call = new CallExpr(Expr::Call, loc, NULL);
  call->setExpectedReturnType(prop->type());
  if (!addOverload(call, basePtr, getter, args)) {
    return &Expr::ErrorVal;
  }

  if (!reduceArgList(args, call)) {
    return &Expr::ErrorVal;
  }

  if (call->candidates().empty()) {
    // Generate the calling signature in a buffer.
    std::stringstream callsig;
    FormatStream fs(callsig);
    fs << Format_Dealias << basePtr << "[";
    formatExprTypeList(fs, call->args());
    fs << "]";
    fs << " -> " << prop->type();

    diag.error(loc) << "No matching method for call to " << callsig.str() << ", candidates are:";
    /*for (ExprList::iterator it = results.begin(); it != results.end(); ++it) {
     Expr * resultMethod = *it;
     if (LValueExpr * lval = dyn_cast<LValueExpr>(*it)) {
     diag.info(lval->value()) << Format_Type << lval->value();
     } else {
     diag.info(*it) << *it;
     }
     }*/

    return &Expr::ErrorVal;
  }

  call->setType(reduceReturnType(call));
  return call;

#else

  // TODO: Type check args against function signature.

  ExprList castArgs;
  size_t argCount = call->args().size();
  for (size_t i = 0; i < argCount; ++i) {
    const Type * paramType = getter->functionType()->param(i)->type();
    Expr * arg = call->arg(i);
    arg = ExprAnalyzer::inferTypes(subject_, arg, paramType);
    Expr * castArg = paramType->implicitCast(arg->location(), arg);
    if (isErrorResult(castArg)) {
      return &Expr::ErrorVal;
    }

    castArgs.push_back(castArg);
  }

  Expr::ExprType callType = Expr::FnCall;
  if (basePtr->type()->typeClass() == Type::Interface ||
      (basePtr->type()->typeClass() == Type::Class && !getter->isFinal())) {
    callType = Expr::VTableCall;
  }

  FnCallExpr * getterCall = new FnCallExpr(callType, loc, getter, basePtr);
  getterCall->setType(prop->type());
  getterCall->args().append(castArgs.begin(), castArgs.end());
  module_->addSymbol(getter);
  return getterCall;
#endif
}

Expr * PropertyAccessorPass::setParamPropertyValue(SLC & loc, CallExpr * call, Expr * value) {

  LValueExpr * lval = cast<LValueExpr>(call->function());
  PropertyDefn * prop = cast<PropertyDefn>(lval->value());
  Expr * basePtr = lval->base();

  if (!AnalyzerBase::analyzeProperty(prop, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  FunctionDefn * setter = prop->setter();
  if (setter == NULL) {
    diag.error(loc) << "Attempt to set value of read-only property '" << prop << "'";
    return &Expr::ErrorVal;
  }

  DASSERT_OBJ(prop->isSingular(), prop);
  DASSERT_OBJ(setter->isSingular(), prop);

  if (!AnalyzerBase::analyzeFunction(setter, Task_PrepTypeComparison)) {
    return &Expr::ErrorVal;
  }

  // TODO: Type check args against function signature.

  ExprList castArgs;
  size_t argCount = call->args().size();
  for (size_t i = 0; i < argCount; ++i) {
    Expr * arg = call->arg(i);
    Expr * castArg = setter->functionType()->param(i)->type()->implicitCast(arg->location(), arg);
    if (isErrorResult(castArg)) {
      return &Expr::ErrorVal;
    }

    castArgs.push_back(castArg);
  }

  Expr::ExprType callType = Expr::FnCall;
  if (basePtr->type()->typeClass() == Type::Interface ||
      (basePtr->type()->typeClass() == Type::Class && !setter->isFinal())) {
    callType = Expr::VTableCall;
  }

  // TODO: Handle multiple overloads of the same property.
  // TODO: Change this to a CallExpr for type inference.
  FnCallExpr * setterCall = new FnCallExpr(callType, loc, setter, basePtr);
  setterCall->setType(prop->type());
  setterCall->args().append(castArgs.begin(), castArgs.end());
  // TODO: Remove this cast when we do the above.
  if (!value->isSingular()) {
    value = ExprAnalyzer::inferTypes(subject_, value, prop->type());
    if (isErrorResult(value)) {
      return value;
    }
  }

  value = prop->type()->implicitCast(loc, value);
  if (value != NULL) {
    setterCall->appendArg(value);
  }

  module_->addSymbol(setter);
  return setterCall;
}
#endif

} // namespace tart
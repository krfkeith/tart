/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Defn/Module.h"
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
    Expr * base = in->base() != NULL ? visitExpr(in->base()) : NULL;
    return getPropertyValue(in->location(), base, prop);
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
  Expr * fromExpr = visitExpr(in->fromExpr());
  if (LValueExpr * lval = dyn_cast<LValueExpr>(in->toExpr())) {
    if (PropertyDefn * prop = dyn_cast<PropertyDefn>(lval->value())) {
      Expr * base = lval->base() != NULL ? visitExpr(lval->base()) : NULL;
      return setPropertyValue(in->location(), base, prop, fromExpr);
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
  } else if (isa<FnCallExpr>(in->toExpr())) {
    diag.error(in) << "Not an l-value: " << in->toExpr();
  }

  Expr * toExpr = visitExpr(in->toExpr());
  in->setFromExpr(fromExpr);
  in->setToExpr(toExpr);
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

  if (prop->getter() == NULL) {
    diag.error(loc) << "Missing 'get' accessor for property " << prop;
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

  if (!basePtr->type().isWritable() && !prop->isMutable()) {
    diag.error(loc) << "Attempt to write to read-only field '" << prop << "'";
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

} // namespace tart

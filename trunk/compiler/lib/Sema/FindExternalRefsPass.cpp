/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/Module.h"
#include "tart/Sema/FindExternalRefsPass.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Objects/Builtins.h"

namespace tart {

/// -------------------------------------------------------------------
/// FindExternalRefsPass

Defn * FindExternalRefsPass::run(Module * m, Defn * in) {
  FindExternalRefsPass instance(m);
  return instance.runImpl(in);
}

Defn * FindExternalRefsPass::runImpl(Defn * in) {
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(in)) {
    if (CompositeType * ctype = dyn_cast<CompositeType>(tdef->typeValue())) {
      if (tdef->isSynthetic()) {
        ctype->addMethodDefsToModule(module);
      }

      const DefnList & staticFields = ctype->staticFields();
      for (DefnList::const_iterator it = staticFields.begin(); it != staticFields.end(); ++it) {
        module->addSymbol(*it);
        if (VariableDefn * var = dyn_cast<VariableDefn>(*it)) {
          if (var->initValue()) {
            visitExpr(var->initValue());
          }
        }
      }
    }
  }

  if (FunctionDefn * fn = dyn_cast<FunctionDefn>(in)) {
    if (!fn->isIntrinsic() && !fn->isExtern()) {
      visit(fn);
    }
  }

  return in;
}

void FindExternalRefsPass::addSymbol(Defn * de) {
  if (FunctionDefn * fn = dyn_cast<FunctionDefn>(de)) {
    addFunction(fn);
  } else if (de->storageClass() == Storage_Static || de->storageClass() == Storage_Global) {
    if (de->isSynthetic()) {
      module->addSymbol(de);
    }
  } else if (de->storageClass() == Storage_Local) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
      if (var->initValue() != NULL) {
        visitExpr(var->initValue());
      }
    }
  }
}

bool FindExternalRefsPass::addFunction(FunctionDefn * fn) {
  if (!fn->isIntrinsic() && !fn->isExtern()) {
    return module->addSymbol(fn);
  }

  return false;
}

Expr * FindExternalRefsPass::visitLValue(LValueExpr * in) {
  visitExpr(in->base());
  addSymbol(in->value());
  return in;
}

Expr * FindExternalRefsPass::visitBoundMethod(BoundMethodExpr * in) {
  visitExpr(in->selfArg());
  addSymbol(in->method());
  return in;
}

Expr * FindExternalRefsPass::visitFnCall(FnCallExpr * in) {
  if (addFunction(in->function())) {
    CFGPass::visitFnCall(in);
  } else {
    visitExpr(in->selfArg());
    visitExprArgs(in);
  }

  return in;
}

Expr * FindExternalRefsPass::visitNew(NewExpr * in) {
  TypeDefn * tdef = in->type()->typeDefn();
  if (tdef != NULL) {
    module->addSymbol(tdef);
  }

  return in;
}

Expr * FindExternalRefsPass::visitConstantObjectRef(ConstantObjectRef * in) {
  CompositeType * ctype = cast<CompositeType>(in->type());
  module->addSymbol(ctype->typeDefn());
  return CFGPass::visitConstantObjectRef(in);
}

Expr * FindExternalRefsPass::visitArrayLiteral(ArrayLiteralExpr * in) {
  CompositeType * arrayType = cast<CompositeType>(in->type());
  DASSERT(arrayType->passes().isFinished(CompositeType::ScopeCreationPass));
  Defn * allocFunc = arrayType->lookupSingleMember("alloc");
  DASSERT(allocFunc != NULL);
  addSymbol(arrayType->typeDefn());
  addSymbol(allocFunc);
  return in;
}

Expr * FindExternalRefsPass::visitInstanceOf(InstanceOfExpr * in) {
  if (CompositeType * cls = dyn_cast<CompositeType>(in->toType())) {
    addSymbol(cls->typeDefn());
  }

  return CFGPass::visitInstanceOf(in);
}

} // namespace tart

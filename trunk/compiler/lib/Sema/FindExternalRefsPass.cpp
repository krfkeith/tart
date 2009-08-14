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
#include "tart/Sema/ExprAnalyzer.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// FindExternalRefsPass

Defn * FindExternalRefsPass::run(Module * m, Defn * in) {
  FindExternalRefsPass instance(m);
  return instance.runImpl(in);
}

Defn * FindExternalRefsPass::runImpl(Defn * in) {
  if (TypeDefn * tdef = dyn_cast<TypeDefn>(in)) {
    if (CompositeType * ctype = dyn_cast<CompositeType>(tdef->getTypeValue())) {
      if (tdef->isSynthetic()) {
        ctype->addMethodXDefs(module);
      }

      ctype->addStaticXDefs(module);
    }
  }
  
  if (FunctionDefn * func = dyn_cast<FunctionDefn>(in)) {
    if (func->isIntrinsic() || func->isExtern()) {
      return in;
    }

    //diag.info(in) << "Visiting " << in;
    visit(func);
  }

  return in;
}

void FindExternalRefsPass::addXRef(Defn * de) {
  if (de->storageClass() == Storage_Static || de->storageClass() == Storage_Global) {
    if (de->isSynthetic() /*&& de->module() != module*/) {
      // Don't XRef intrinsics.
      if (FunctionDefn * func = dyn_cast<FunctionDefn>(de)) {
        if (func->isIntrinsic()) {
          return;
        }
        
        //if (func->isCtor()) {
        //  
        //}
      }

      module->addXDef(de);
    }
  } else if (de->storageClass() == Storage_Local) {
    if (VariableDefn * var = dyn_cast<VariableDefn>(de)) {
      if (var->initValue() != NULL) {
        visitExpr(var->initValue());
      }
    }
  }
}

Expr * FindExternalRefsPass::visitLValue(LValueExpr * in) {
  addXRef(in->value());
  return in;
}

Expr * FindExternalRefsPass::visitFnCall(FnCallExpr * in) {
  addXRef(in->function());
  CFGPass::visitFnCall(in);
  return in;
}

Expr * FindExternalRefsPass::visitArrayLiteral(ArrayLiteralExpr * in) {
  CompositeType * arrayType = cast<CompositeType>(in->type());
  Defn * allocFunc = arrayType->lookupSingleMember("alloc");
  addXRef(arrayType->typeDefn());
  addXRef(allocFunc);
  return in;
}

} // namespace tart

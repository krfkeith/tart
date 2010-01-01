/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/AST/Stmt.h"
#include "tart/Sema/MacroExpansionPass.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// MacroExpansionPass

Expr * MacroExpansionPass::runImpl(Expr * in) {
  return visitExpr(in);
}

Expr * MacroExpansionPass::visitFnCall(FnCallExpr * in) {
  if (in->function()->defnType() == Defn::Macro) {
    FunctionDefn * macro = in->function();
    FunctionType * mtype = macro->functionType();
    const Type * returnType = dealias(mtype->returnType());

    // Note - type could be 'void'
    VariableDefn * retVal = NULL;
    if (!returnType->isVoidType()) {
      LocalScope * retValScope = new LocalScope(macro->definingScope());
      retValScope->setScopeName("macro-return");
      stAn.getTarget()->localScopes().push_back(retValScope);
      retVal = new VariableDefn(Defn::Var, NULL, "__retval");
      retVal->setType(returnType);
      retVal->setStorageClass(Storage_Local);
      retVal->addTrait(Defn::Singular);
      retValScope->addMember(retVal);
    }

    LocalScope paramScope(macro->definingScope());
    paramScope.setScopeName("macro-params");
    size_t argCount = in->argCount();
    for (size_t i = 0; i < argCount; ++i) {
      ParameterDefn * param = macro->params()[i];
      Expr * arg = in->arg(i);
      DASSERT(arg->type()->isEqual(param->internalType()));
      VariableDefn * binding = new VariableDefn(Defn::MacroArg, NULL, param->name());
      binding->setInitValue(arg);
      binding->setType(param->internalType());
      binding->setStorageClass(Storage_Local);
      paramScope.addMember(binding);
    }

    LValueExpr * retLVal = NULL;
    LValueExpr * savedRetVal = NULL;
    if (retVal != NULL) {
      retLVal = LValueExpr::get(in->location(), NULL, retVal);
      savedRetVal = stAn.setMacroReturnVal(retLVal);
      DASSERT_OBJ(retLVal->isSingular(), retLVal);
    }

    Block * returnBlock = stAn.createBlock("return");

    Scope * savedScope = stAn.setActiveScope(&paramScope);
    Block * savedReturnBlock = stAn.setMacroReturnTarget(returnBlock);
    const Type * savedReturnType = stAn.setReturnType(returnType);
    const Stmt * macroBody = macro->functionDecl()->body();

    stAn.buildStmtCFG(macroBody);

    // If control fell off the end of the macro, then branch to return block.
    Block * finalBlock = stAn.insertionBlock();
    if (finalBlock != NULL && !finalBlock->hasTerminator()) {
      finalBlock->branchTo(macroBody->finalLocation(), returnBlock);
    }

    stAn.setReturnType(savedReturnType);
    stAn.setMacroReturnTarget(savedReturnBlock);
    stAn.setActiveScope(savedScope);
    if (savedRetVal != NULL) {
      stAn.setMacroReturnVal(savedRetVal);
    }

    // Move return block after macro blocks.
    std::remove(macro->blocks().begin(), macro->blocks().end(), returnBlock);
    macro->blocks().push_back(returnBlock);
    stAn.setInsertPos(returnBlock);

    if (retLVal != NULL) {
      return retLVal;
    } else {
      // Return a dummy expression, it won't be used.
      return new UnaryExpr(Expr::NoOp, in->location(), &VoidType::instance, in);
    }
  }

  return in;
}

} // namespace tart

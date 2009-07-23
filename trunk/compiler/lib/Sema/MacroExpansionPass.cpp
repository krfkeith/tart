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
/// FinalizeTypesPass

Expr * MacroExpansionPass::runImpl(Expr * in) {
  return visitExpr(in);
}

Expr * MacroExpansionPass::visitFnCall(FnCallExpr * in) {
  if (in->function()->defnType() == Defn::Macro) {
    FunctionDefn * macro = in->function();
    FunctionType * mtype = macro->functionType();
    Type * returnType = dealias(mtype->returnType());

    // Note - type could be 'void'
    VariableDefn * retVal = NULL;
    if (!returnType->isVoidType()) {
      LocalScope * retValScope = new LocalScope(macro->definingScope());
      retValScope->setScopeName("macro-return");
      stAn.getTarget()->localScopes().push_back(retValScope);
      retVal = new VariableDefn(Defn::Var, NULL, "__retval");
      retVal->setType(mtype->returnType());
      retVal->setStorageClass(Storage_Local);
      retVal->addTrait(Defn::Singular);
      retValScope->addMember(retVal);
    }

    LocalScope paramScope(macro->definingScope());
    paramScope.setScopeName("macro-params");
    size_t argCount = in->argCount();
    for (size_t i = 0; i < argCount; ++i) {
      ParameterDefn * param = mtype->params()[i];
      Expr * arg = in->arg(i);
      // TODO: Not sure a var is the right thing to use here - really want an alias.
      VariableDefn * binding = new VariableDefn(Defn::Let, NULL, param->getName());
      binding->setInitValue(arg);
      binding->setType(param->getType());
      binding->setStorageClass(Storage_Local);
      paramScope.addMember(binding);
    }

    LValueExpr * retLVal = NULL;
    LValueExpr * savedRetVal = NULL;
    if (retVal != NULL) {
      retLVal = new LValueExpr(in->getLocation(), NULL, retVal);
      savedRetVal = stAn.setMacroReturnVal(retLVal);
      DASSERT_OBJ(retLVal->isSingular(), retLVal);
    }

    Block * returnBlock = stAn.createBlock("return");

    Scope * savedScope = stAn.setActiveScope(&paramScope);
    Block * savedReturnBlock = stAn.setMacroReturnTarget(returnBlock);
    const Stmt * macroBody = macro->getFunctionDecl()->getBody();

    stAn.buildStmtCFG(macroBody);

    // If control fell off the end of the macro, then branch to return block.
    Block * finalBlock = stAn.insertionBlock();
    if (finalBlock != NULL && !finalBlock->hasTerminator()) {
      finalBlock->branchTo(macroBody->getFinalLocation(), returnBlock);
    }
    
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
      return new UnaryExpr(Expr::NoOp, in->getLocation(), &VoidType::instance, in);
    }
  }

  return in;
}

} // namespace tart

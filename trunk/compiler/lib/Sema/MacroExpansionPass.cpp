/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/CFG/FunctionRegion.h"
#include "tart/CFG/LexicalBlockRegion.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Defn/Module.h"

#include "tart/AST/Stmt.h"

#include "tart/Sema/MacroExpansionPass.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"

namespace tart {

/// -------------------------------------------------------------------
/// MacroExpansionPass

Expr * MacroExpansionPass::runImpl(Expr * in) {
  return visitExpr(in);
}

Expr * MacroExpansionPass::visitFnCall(FnCallExpr * in) {
  in->setSelfArg(visitExpr(in->selfArg()));
  visitExprArgs(in);
  if (in->function()->defnType() == Defn::Macro) {
    FunctionDefn * macro = in->function();
    FunctionType * mtype = macro->functionType();
    const Type * returnType = dealias(mtype->returnType());

    // Add a dependency on the macro's module. Do this here because
    // the module reference will be removed during the expansion.
    stAn.module()->addModuleDependency(macro);

    FunctionDefn * scopeFn = stAn.function();

    // Note - type could be 'void'
    VariableDefn * retVal = NULL;
    if (!returnType->isVoidType()) {
      LocalScope * retValScope = new LocalScope(
          macro->definingScope(), stAn.activeScope()->region());
      retValScope->setScopeName("macro-return");
      stAn.function()->localScopes().push_back(retValScope);
      retVal = new VariableDefn(Defn::Var, NULL, "__retval");
      retVal->setType(returnType);
      retVal->setStorageClass(Storage_Local);
      retVal->addTrait(Defn::Singular);
      retValScope->addMember(retVal);
    }

    FunctionRegion * macroRegion = new FunctionRegion(macro, macro->location().region);
    LexicalBlockRegion * blockRegion = new LexicalBlockRegion(SourceLocation(macroRegion,
        macro->location().begin, macro->location().end), in->location());
    LocalScope paramScope(macro->definingScope(), blockRegion);
    paramScope.setScopeName("macro-params");

    if (in->selfArg() != NULL) {
      // TODO: Do we really want to re-evaluate 'self' each time we access a member var?
      VariableDefn * binding = new VariableDefn(Defn::MacroArg, NULL, istrings.idSelf);
      binding->createQualifiedName(NULL);
      binding->setInitValue(in->selfArg());
      binding->setType(in->selfArg()->type());
      binding->setStorageClass(Storage_Local);
      paramScope.addMember(binding);
    }

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
    if (retVal != NULL) {
      retLVal = LValueExpr::get(in->location(), NULL, retVal);
      DASSERT_OBJ(retLVal->isSingular(), retLVal);
    }
    LValueExpr * savedRetVal = stAn.setMacroReturnVal(retLVal);

    Scope * savedScope = stAn.setActiveScope(&paramScope);
    bool saveInMacroExpansion = stAn.setInMacroExpansion(true);
    const Type * savedReturnType = stAn.setReturnType(returnType);
    const Stmt * macroBody = macro->functionDecl()->body();

    Defn * saveSubject = stAn.setSubject(macro);
    Expr * bodyExpr = stAn.reduceExpr(macroBody, NULL);
    if (bodyExpr != NULL) {
      bodyExpr = new LocalProcedureExpr(bodyExpr->location(), bodyExpr);
    }
    stAn.setSubject(saveSubject);

    stAn.setReturnType(savedReturnType);
    stAn.setInMacroExpansion(saveInMacroExpansion);
    stAn.setActiveScope(savedScope);
    stAn.setMacroReturnVal(savedRetVal);

    if (retLVal != NULL) {
      // Execute the macro body and return the result.
      return new BinaryExpr(Expr::Prog2, in->location(), retLVal->type(), bodyExpr, retLVal);
    } else {
      // Just evaluate the macro body and ignore the result.
      return bodyExpr;
    }
  }

  return in;
}

} // namespace tart

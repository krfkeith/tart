/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Module.h"

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"

#include "tart/Type/FunctionType.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/TypeRelation.h"

#include "tart/AST/Stmt.h"

#include "tart/Meta/MDReader.h"

#include "tart/Sema/MacroExpansionPass.h"
#include "tart/Common/Diagnostics.h"

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

    // Note - type could be 'void'
    VariableDefn * retVal = NULL;
    if (!returnType->isVoidType()) {
      LocalScope * retValScope = new LocalScope(macro->definingScope());
      retValScope->setScopeName("macro-return");
      stAn.function()->localScopes().push_back(retValScope);
      retVal = new VariableDefn(Defn::Var, NULL, "__retval");
      retVal->setType(returnType);
      retVal->setStorageClass(Storage_Local);
      retVal->addTrait(Defn::Singular);
      retValScope->addMember(retVal);
    }

    LocalScope paramScope(macro->definingScope());
    paramScope.setScopeName("macro-params");

    if (in->selfArg() != NULL) {
      // TODO: Do we really want to re-evaluate 'self' each time we access a member var?
      VariableDefn * binding = new VariableDefn(Defn::MacroArg, NULL, "self");
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
      DASSERT(TypeRelation::isEqual(arg->type(), param->internalType()));
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

    const Stmt * macroBody;
    if (macro->ast() != NULL) {
      macroBody = macro->functionDecl()->body();
    } else if (macro->mdNode() != NULL) {
      macroBody = MDReader(macro->module(), macro).readFunctionBody(macro);
      if (macroBody == NULL) {
        return &Expr::ErrorVal;
      }

      // Save the AST
      ASTFunctionDecl * ast = new ASTFunctionDecl(ASTNode::Macro, macro->location(), macro->name(),
          ASTParamList(), NULL, DeclModifiers());
      ast->setBody(const_cast<Stmt *>(macroBody));
      macro->setAst(ast);
    }

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

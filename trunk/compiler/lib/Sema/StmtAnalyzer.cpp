/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"
#include "tart/Defn/Defn.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/EnumType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Defn/Module.h"
#include "tart/Expr/Closure.h"

#include "tart/AST/Stmt.h"

#include "tart/Sema/StmtAnalyzer.h"
#include "tart/Sema/VarAnalyzer.h"
#include "tart/Sema/TypeInference.h"
#include "tart/Sema/MacroExpansionPass.h"
#include "tart/Sema/PropertyAccessorPass.h"
#include "tart/Sema/ScopeBuilder.h"
#include "tart/Sema/EvalPass.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

#include "tart/Common/Diagnostics.h"

#define IMPLICIT_SELF 1

namespace tart {

// A scope which allows definitions in the enclosing class to be looked up
// via the 'self' parameter.
class SelfScope: public DelegatingScope {
  ParameterDefn * selfParam;
  Expr * selfExpr;

public:
  SelfScope(Scope * delegate, Scope * parent) :
    DelegatingScope(delegate, parent), selfParam(NULL), selfExpr(NULL) {
  }

  void setSelfParam(ParameterDefn * self) {
    selfParam = self;
  }

  Expr * baseExpr() {
    if (selfExpr == NULL) {
      selfExpr = LValueExpr::get(selfParam->location(), NULL, selfParam);
    }

    return selfExpr;
  }
};

/// -------------------------------------------------------------------
/// StmtAnalyzer

StmtAnalyzer::StmtAnalyzer(FunctionDefn * func, const Stmt * body)
  : ExprAnalyzer(func->module(), &func->parameterScope(), func, func)
  , body_(body)
{
}

bool StmtAnalyzer::buildCFG() {
  DASSERT(body_ != NULL);

  // Create a temporary scope to allow lookup of the function parameters.
  DelegatingScope parameterScope(&function()->parameterScope(), function()->definingScope());
  setActiveScope(&parameterScope);

  // If this is an instance method, then set up the implicit 'self'
  // scope as well. This scope searches the type of the self parameter,
  // and is always searched immediately after the parameter scope.
  if (function()->storageClass() == Storage_Instance) {
    ParameterDefn * selfParam = function()->functionType()->selfParam();
    DASSERT_OBJ(selfParam != NULL, function());
    DASSERT_OBJ(selfParam->type(), function());
    TypeDefn * selfType = selfParam->type()->typeDefn();
    DASSERT_OBJ(selfType != NULL, function());

#if IMPLICIT_SELF
    // Uncomment to allow 'self' to be searched implicitly.
    SelfScope * selfScope =
        new SelfScope(selfType->value()->mutableMemberScope(), function()->definingScope());
    selfScope->setSelfParam(selfParam);
    parameterScope.setParentScope(selfScope);
#endif
  } else if (function()->storageClass() == Storage_Local) {
    ParameterDefn * selfParam = function()->functionType()->selfParam();
    DASSERT_OBJ(selfParam != NULL, function());
    DASSERT_OBJ(selfParam->type(), function());
    DASSERT_OBJ(selfParam->initValue() != NULL, function());
    if (ClosureEnvExpr * env = dyn_cast<ClosureEnvExpr>(selfParam->initValue())) {
      parameterScope.setParentScope(env);
    }
  }

  // Create the initial block.
  Expr * bodyExpr = dyn_cast_or_null<SeqExpr>(reduceExpr(body_, NULL));
  bodyExpr = MacroExpansionPass::run(*this, bodyExpr);
  if (bodyExpr == NULL) {
    return false;
  }

  function()->setBody(bodyExpr);
  // Convert all property accesses to getters and setters.
  PropertyAccessorPass::run(module(), subject(), function()->body());
  return true;
}

} // namespace tart

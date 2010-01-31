/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Sema/FoldConstantsPass.h"
#include "tart/Sema/CallCandidate.h"
#include "tart/Sema/AnalyzerBase.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// FoldConstantsPass

Expr * FoldConstantsPass::visitCall(CallExpr * in) {
  CFGPass::visitCall(in);

  // Check if all input arguments are constants, especially constant integers.
  bool hasConstArgs = true;
  bool hasConstIntArgs = true;
  for (ExprList::const_iterator it = in->args().begin(); it != in->args().end(); ++it) {
    if ((*it)->isConstant()) {
      const Type * ty = (*it)->type();
      if (!ty->isUnsizedIntType()) {
        hasConstIntArgs = false;
      }
    } else {
      hasConstArgs = false;
      hasConstIntArgs = false;
    }
  }

  if (hasConstIntArgs) {
    // We need to locate an overload. In this case, it's an exact match we are interested in.
    // Note that we don't care about the return type.
    Candidates & clist = in->candidates();
    for (Candidates::const_iterator it = clist.begin(); it != clist.end(); ++it) {
      bool exactMatch = true;
      CallCandidate * cc = *it;
      if (cc->method() != NULL) {
        DASSERT_OBJ(cc->method()->passes().isFinished(FunctionDefn::ParameterTypePass),
            cc->method());
      } else {
        exactMatch = false;
      }

      size_t argCount = cc->argCount();
      for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
        const Type * paramType = cc->paramType(argIndex);
        if (!paramType->isUnsizedIntType()) {
          exactMatch = false;
        }
      }

      if (exactMatch) {
        const FunctionType * ftype = cc->functionType();
        size_t paramCount = ftype->params().size();
        ExprList callingArgs;
        callingArgs.resize(paramCount);
        std::fill(callingArgs.begin(), callingArgs.end(), (Expr *)NULL);
        for (size_t argIndex = 0; argIndex < argCount; ++argIndex) {
          //Typeef paramType = cc->paramType(argIndex);
          callingArgs[cc->parameterIndex(argIndex)] = in->arg(argIndex);
        }

        // Fill in default params
        for (size_t paramIndex = 0; paramIndex < paramCount; ++paramIndex) {
          if (callingArgs[paramIndex] == NULL) {
            ParameterDefn * param = ftype->params()[paramIndex];
            if (param->initValue() != NULL) {
              callingArgs[paramIndex] = param->initValue();
            } else if (param->isVariadic()) {
              // Empty array literal.
              Expr * arrayParam = AnalyzerBase::createArrayLiteral(in->location(), param->type());
              AnalyzerBase::analyzeType(arrayParam->type(), Task_PrepConstruction);
              DASSERT(arrayParam->isSingular());
              callingArgs[paramIndex] = arrayParam;
            }

            DASSERT_OBJ(callingArgs[paramIndex] != NULL, param);
          }
        }

        Expr * result = cc->method()->eval(in->location(), cc->base(), callingArgs);
        if (result != NULL) {
          return result;
        }

        break;
      }
    }
  }

  return in;
}

Expr * FoldConstantsPass::visitInitVar(InitVarExpr * in) {
  return CFGPass::visitInitVar(in);
}

} // namespace tart

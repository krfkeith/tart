/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_GATHERCONSTRAINTSPASS_H
#define TART_SEMA_INFER_GATHERCONSTRAINTSPASS_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

#ifndef TART_SEMA_INFER_CALLSITE_H
#include "tart/Sema/Infer/CallSite.h"
#endif

#ifndef TART_SEMA_INFER_CONVERSIONSITE_H
#include "tart/Sema/Infer/ConversionSite.h"
#endif

//#ifndef TART_SEMA_BINDINGENV_H
//#include "tart/Sema/BindingEnv.h"
//#endif

namespace tart {

/// -------------------------------------------------------------------
/// Pass to collect all of the call sites and conversion sites in the
/// expression tree.

class GatherConstraintsPass : public CFGPass {
public:
  GatherConstraintsPass(CallSites & callSites, ConversionSites & cstrSites)
    : calls_(callSites)
    , conversions_(cstrSites)
  {}

private:
  CallSites & calls_;
  ConversionSites & conversions_;
  llvm::SmallSet<const Expr *, 16> visited_;

  Expr * visitCall(CallExpr * in);
  Expr * visitAssign(AssignmentExpr * in);
  Expr * visitPostAssign(AssignmentExpr * in);
  Expr * visitTupleCtor(TupleCtorExpr * in);
  Expr * visitIf(IfExpr * in);
  Expr * visitSwitch(SwitchExpr * in);
  Expr * visitMatch(MatchExpr * in);

  void visitPHI(Expr * in);
};

} // namespace tart

#endif

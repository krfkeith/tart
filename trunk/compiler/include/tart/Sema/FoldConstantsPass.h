/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_FOLDCONSTANTSPASS_H
#define TART_SEMA_FOLDCONSTANTSPASS_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Pass to fold all operations on constants into a single constant.
/// This helps the type inference pass run better.
class FoldConstantsPass : public CFGPass {
public:
  FoldConstantsPass(Module * module) : module_(module) {}

private:
  Module * module_;

  Expr * visitCall(CallExpr * in);
  Expr * visitInitVar(InitVarExpr * in);
};

} // namespace tart

#endif

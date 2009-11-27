/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_SEMA_MACROEXPANSIONPASS_H
#define TART_SEMA_MACROEXPANSIONPASS_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

#ifndef TART_SEMA_STMTANALYZER_H
#include "tart/Sema/StmtAnalyzer.h"
#endif

namespace tart {
  
/// -------------------------------------------------------------------
/// Function pass expands all macros inline.
class MacroExpansionPass : public CFGPass {
private:
  StmtAnalyzer & stAn;

  MacroExpansionPass(StmtAnalyzer & st) : stAn(st) {}
  Expr * runImpl(Expr * in);
  
public:

  /** Run this pass on the specified expression. */
  static Expr * run(StmtAnalyzer & st, Expr * e) {
    MacroExpansionPass instance(st);
    return instance.runImpl(e);
  }
  
  Expr * visitFnCall(FnCallExpr * in);
};

} // namespace tart

#endif

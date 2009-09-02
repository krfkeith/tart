/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_SEMA_EXPREVALUATOR_H
#define TART_SEMA_EXPREVALUATOR_H

namespace tart {
  
class Scope;
class Expr;

/// -------------------------------------------------------------------
/// Expression analyzer
class ExprEvaluator {
public:
  /** Constructor. */
  ExprEvaluator(Scope * activeScope)
    : activeScope_(activeScope)
  {}
  
  Expr * eval(Expr * in);

private:
  Scope * activeScope_;
};

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_FINALIZETYPESPASS_H
#define TART_SEMA_FINALIZETYPESPASS_H

#ifndef TART_SEMA_CFGPASS_H
#include "tart/Sema/CFGPass.h"
#endif

namespace tart {

class BindingEnv;

/// -------------------------------------------------------------------
/// Function pass which assigns final types to all expressions and
/// inserts implicit casts as needed.
class FinalizeTypesPass : public CFGPass {
public:

  /** Run this pass on the specified expression. */
  static Expr * run(Defn * source, Expr * in, BindingEnv &env);

protected:
  Expr * runImpl(Expr * in);
};

} // namespace tart

#endif

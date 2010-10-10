/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_OBJECTS_INTRINSIC_H
#define TART_OBJECTS_INTRINSIC_H

#ifndef TART_CFG_DEFN_H
#include "tart/CFG/Defn.h"
#endif

#include "llvm/Intrinsics.h"

namespace llvm {
  class Value;
}

namespace tart {

class CodeGenerator;
class FnCallExpr;

/// -------------------------------------------------------------------
/// Intrinsic functions are ones that are implemented directly in the
/// compiler rather than being specified in tart source. Note that
/// in some cases, several overloaded methods can point to the same
/// instrinsic structure.
///
/// Intrinsics work by replacing the function call with generated code.
/// This replacement can happen either in the analysis phase or in the
/// code generation phase. In the former case, the replacement is done
/// by substituting expression nodes. In the latter case, the replacement
/// is done via a custom code generator function.
class Intrinsic {
private:
  const char * name;

  static llvm::StringMap<Intrinsic *> intrinsicMap;

public:
  Intrinsic(const char * n) : name(n) {
    intrinsicMap[name] = this;
  }

  virtual ~Intrinsic() {}

  /** The fully qualified name of this intrinsic. */
  const char * getName() { return name; }

  /** Analysis-time implementation of the intrinsic. This returns an
      expression node which replaces the function call. Returning NULL
      indicates that no replacement should be done. */
  virtual Expr * eval(const SourceLocation & loc, const FunctionDefn * method, Expr * self,
      const ExprList & args, Type * expectedReturn) const {
    return NULL;
  }

  /** Code-generation implementation of the intrinsic. */
  virtual llvm::Value * generate(CodeGenerator & cg, const FnCallExpr * call) const;

  /** Lookup an intrinsic by name. */
  static Intrinsic * get(const SourceLocation & loc, const char * name);
};

} // namespace tart

#endif

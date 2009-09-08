/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_CALLCANDIDATE_H
#define TART_SEMA_CALLCANDIDATE_H

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#ifndef TART_SEMA_PARAMETERASSIGNMENTS_H
#include "tart/Sema/ParameterAssignments.h"
#endif

#ifndef TART_SEMA_UNIFIER_H
#include "tart/Sema/BindingEnv.h"
#endif

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A call candidate
class CallCandidate : public GC {
public:
  CallCandidate(CallExpr * call, Expr * baseExpr, FunctionDefn * m,
      const ParameterAssignments & params);

  /** The call expression that this is a candidate for. */
  CallExpr * callExpr() const { return callExpr_; }

  /** The base ('self') expression for the method. */
  Expr * base() const { return base_; }

  /** The callable method. */
  FunctionDefn * method() const { return method_; }
  void setMethod(FunctionDefn * m) { method_ = m; }

  /** The mapping of input args to formal parameters. */
  const ParameterAssignments & paramAssignments() const {
    return paramAssignments_;
  }

  /** For the argument at position 'argIndex', return which parameter it was mapped to. */
  int parameterIndex(int argIndex) const {
    return paramAssignments_[argIndex];
  }

  /** Get the number of non-default arguments in this call. */
  size_t argCount() const { return paramAssignments_.size(); }

  /** For a given input argument index, return the type of the parameter
      that this argument will be assigned to. This may not be the same
      as the formally declared parameter type, as pattern variable
      substitutions may have occurred. */
  Type * paramType(int argIndex) const;

  /** Get the return type of this candidate. */
  Type * resultType() const { return resultType_; }
  void setResultType(Type * type) { resultType_ = type; }

  /** The compatibility rank for the least compatible argument. */
  ConversionRank conversionRank() const { return conversionRank_; }

  /** Update the compatibility score for this candidate. */
  ConversionRank updateConversionRank();

  /** Return true if this specified call candidate has the same type
      as this one. */
  bool isEqual(const CallCandidate * other) const;

  /** Return true if this candidate is more specific than the one given. */
  bool isMoreSpecific(const CallCandidate * other) const;

  /** Return true if the method and base pointer are singular. */
  bool isSingular() const;

  /** Perform unification on the candidate and its arguments. */
  bool unify(CallExpr * callExpr);

  /** Return the environment containing type bindings created during
      unification between the template function and the actual params. */
  const BindingEnv & env() const { return bindingEnv_; }
  BindingEnv & env() { return bindingEnv_; }

  /** If this candidate has not already been culld, then set it's pruning
      depth to the current depth, otherwise leave it as-is. Returns true
      if the pruningDepth changed state. */
  bool cull(int depth) {
    if (pruningDepth_ == 0) {
      pruningDepth_ = depth;
      return true;
    }

    return false;
  }

  /** If this candidate was culld at a level equal or higher than the
      specified depth, then un-cull it, otherwise leave it culld. */
  bool decull(int depth) {
    if (pruningDepth_ >= depth) {
      pruningDepth_ = 0;
      return true;
    }

    return false;
  }

  /** Return true if the candidate has been culld. */
  bool isCulled() const { return pruningDepth_ != 0; }

  // Overrides

  void trace() const;

private:
  CallExpr * callExpr_;
  Expr * base_;
  FunctionDefn * method_;
  ConversionRank conversionRank_;
  int pruningDepth_;
  ParameterAssignments paramAssignments_;
  BindingEnv bindingEnv_;
  Type * resultType_;
  TypeList paramTypes_;
  bool isTemplate_;
};

FormatStream & operator<<(FormatStream & out, const CallCandidate & cc);

} // namespace tart

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_CALLCANDIDATE_H
#define TART_SEMA_CALLCANDIDATE_H

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef TART_DEFN_TEMPLATE_H
#include "tart/Defn/Template.h"
#endif

#ifndef TART_SEMA_PARAMETERASSIGNMENTS_H
#include "tart/Sema/ParameterAssignments.h"
#endif

#ifndef TART_SEMA_BINDINGENV_H
#include "tart/Sema/BindingEnv.h"
#endif

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

namespace tart {

class CallExpr;
class SpCandidate;
class FunctionDefn;

/// -------------------------------------------------------------------
/// A call candidate
class CallCandidate : public GC, public Formattable {
public:
  enum RelativeSpecificity {
    NOT_MORE_SPECIFIC = 0,
    EQUAL_SPECIFICITY,
    MORE_SPECIFIC,
  };

  CallCandidate(CallExpr * call, Expr * baseExpr, FunctionDefn * m,
      const ParameterAssignments & param, SpCandidate * spCandidate = NULL);

  CallCandidate(CallExpr * call, Expr * fnExpr, const FunctionType * ftype,
      const ParameterAssignments & params);

  /** Rename all type variables in the type signature, as preparation for
      unification. */
  void relabelTypeVars(BindingEnv & env);

  /** The call expression that this is a candidate for. */
  CallExpr * callExpr() const { return callExpr_; }

  /** The base ('self') expression for the method. */
  Expr * base() const { return base_; }

  /** The callable method. May be null if the call is via a pointer. */
  FunctionDefn * method() const { return method_; }
  void setMethod(FunctionDefn * m) { method_ = m; }

  /** The method type. */
  const FunctionType * functionType() const { return fnType_; }
  void setFunctionType(FunctionType * fnType) { fnType_ = fnType; }

  /** The provision object for this call candidate. */
  Provision * primaryProvision() const { return primaryProvision_; }

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
  QualifiedType paramType(int argIndex) const;

  /** Get the return type of this candidate. */
  QualifiedType resultType() const { return resultType_; }

  /** The list of relabeled type parameters. */
  const TupleType * typeParams() const { return typeParams_; }

  /** The compatibility rank for the least compatible argument. */
  ConversionRank conversionRank() const { return conversionRank_; }

  /** The number of conversions that occurred at the least compatible rank. */
  int conversionCount() const { return conversionCount_; }

  /** Update the compatibility score for this candidate. */
  ConversionRank updateConversionRank();

  /** True if any of the types in the function signature are error types. */
  bool hasErrors() const;

  /** Return true if *this* candidate is more specific than the one given. */
  bool isMoreSpecific(const CallCandidate * other) const;

  /** Return true if the method and base pointer are singular. */
  bool isSingular() const;

  /** Perform unification on the candidate and its arguments. */
  bool unify(CallExpr * callExpr, BindingEnv & env, FormatStream * errStrm = NULL);

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

  /** Return true if the method is a template or a template member. */
  bool isTemplate() const { return isTemplate_; }

  /** Print the list of all type params and their bindings to the debug output. */
  void dumpTypeParams() const;

  // Overrides

  void trace() const;
  void format(FormatStream & out) const;

private:
  void combineConversionRanks(ConversionRank newRank);

  /** Return whether lhs is equal, or more specific than, rhs. If neither of those
      is true, it does not mean that lhs is less specific - it may mean that the
      two types are incomparable. */
  static RelativeSpecificity isMoreSpecific(QualifiedType lhs, QualifiedType rhs);

  CallExpr * callExpr_;
  Expr * base_;
  FunctionDefn * method_;
  ConversionRank conversionRank_;
  int conversionCount_;
  int pruningDepth_;
  Provision * primaryProvision_;
  ParameterAssignments paramAssignments_;
  const FunctionType * fnType_;
  QualifiedType resultType_;
  QualifiedTypeList paramTypes_;
  const TupleType * typeParams_;
  const TupleType * typeArgs_;
  TemplateConditionList conditions_;
  SpCandidate * spCandidate_;
  bool isTemplate_;
  bool trace_;
};

/// -------------------------------------------------------------------
/// A provision that a given call candidate has not been culled.
class CandidateNotCulledProvision : public Provision {
public:
  enum { kIID = 0x100 };

  CandidateNotCulledProvision(const CallCandidate * target) : target_(target) {}

  // Overrides

  virtual bool isType(uint32_t ptype) const { return ptype == kIID; }
  bool check() const { return !target_->isCulled(); }
  void trace() const { target_->mark(); }
  void format(FormatStream & out) const {
    target_->format(out);
  }

  bool contradicts(const Provision * p) const {
    if (const CandidateNotCulledProvision * cnc = dyn_cast<CandidateNotCulledProvision>(p)) {
      return cnc->target_ != target_ && cnc->target_->callExpr() == target_->callExpr();
    }
    return false;
  }

  static inline bool classof(const CandidateNotCulledProvision *) { return true; }
  static inline bool classof(const Provision * prov) {
    return prov->isType(CandidateNotCulledProvision::kIID);
  }

private:
  const CallCandidate * target_;
};

} // namespace tart

#endif

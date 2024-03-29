/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_AMBIGUOUSRESULTTYPE_H
#define TART_TYPE_AMBIGUOUSRESULTTYPE_H

#ifndef TART_TYPE_AMBIGUOUSTYPE_H
#include "tart/Type/AmbiguousType.h"
#endif

namespace tart {

// Forward declarations
class CallCandidate;

/// -------------------------------------------------------------------
/// A type constraint representing the result of a method, where we don't
/// know exactly which method will be chosen yet.
class AmbiguousResultType : public TypeSetConstraint {
public:
  AmbiguousResultType(CallExpr * call)
    : TypeSetConstraint(AmbiguousResult)
    , callExpr_(call)
  {}

  /** The call expression. */
  CallExpr * expr() const { return callExpr_; }

  /** The result type for a given candidate. */
  QualifiedType candidateResultType(const CallCandidate * cc) const;

  // Overrides

  void listProspects(ProspectList & out, const ProvisionSet & add) const;
  void expandImpl(QualifiedTypeSet & out, unsigned qualifiers) const;
  void trace() const;
  void format(FormatStream & out) const;

  static inline bool classof(const AmbiguousResultType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() == AmbiguousResult;
  }

private:
  CallExpr * callExpr_;
};

} // namespace tart

#endif // TART_TYPE_AMBIGUOUSRESULTTYPE_H

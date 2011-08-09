/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_PARAMETERASSIGNMENT_H
#define TART_SEMA_PARAMETERASSIGNMENT_H

#ifndef TART_CFG_CFG_H
#include "tart/CFG/CFG.h"
#endif

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

namespace tart {

class FunctionType;

/// -------------------------------------------------------------------
/// A class which specifies, for each source argument, the parameter
/// index to which that argument will be assigned.
class ParameterAssignments {
private:
  friend class ParameterAssignmentsBuilder;

  llvm::SmallVector<int, 16> value;

public:
  typedef llvm::SmallVector<int, 16>::iterator iterator;
  typedef llvm::SmallVector<int, 16>::const_iterator const_iterator;

  ParameterAssignments() {}
  ParameterAssignments(const ParameterAssignments & src) : value(src.value) {}
  const ParameterAssignments & operator=(const ParameterAssignments & src) {
    value = src.value;
    return *this;
  }

  size_t size() const { return value.size(); }
  int operator[](int index) const { return value[index]; }
};

/// -------------------------------------------------------------------
/// Builder class used to build a parameter assignment.
class ParameterAssignmentsBuilder {
private:

  ParameterAssignments & target;
  llvm::SmallVector<bool, 16> inUse;
  const FunctionType * ftype;
  size_t paramCount;
  size_t nextPositionalParam;

  bool keywordAdded;
  bool valid;

public:
  ParameterAssignmentsBuilder(ParameterAssignments & p, const FunctionType * f);

  /** Add a positional argument. */
  bool addPositionalArg();

  /** Add a keyword argument. */
  bool addKeywordArg(StringRef kw);

  /** Check that there are no unassigned parameters. */
  bool check();

  /** Returns true if the assignment succeeded. */
  bool isValid() const { return valid; }

  /** Assign parameters given an ASTNodeList of positional and keyword
      parameters */
  bool assignFromAST(const ASTNodeList & args);
};

} // namespace tart

#endif

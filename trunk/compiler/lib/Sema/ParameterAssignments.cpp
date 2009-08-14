/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Sema/ParameterAssignments.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

/// -------------------------------------------------------------------
/// Builder class used to build a parameter assignment.

ParameterAssignmentsBuilder::ParameterAssignmentsBuilder(
    ParameterAssignments & pa, const FunctionType * f)
  : target(pa)
  , ftype(f)
  , nextPositionalParam(0)
  , keywordAdded(false)
  , valid(true)
{
  paramCount = ftype->params().size();
  inUse.resize(paramCount, false);
}

bool ParameterAssignmentsBuilder::addPositionalArg() {
  if (!valid) {
    return false;
  }
  
  if (keywordAdded) {
    valid = false;
    return false;
  }
  
  if (nextPositionalParam >= paramCount) {
    valid = false;
    return false;
  }
 
  ParameterDefn * param = ftype->params()[nextPositionalParam];
  
  // Keyword-only args cannot be defined positionally.
  if (param->getFlag(ParameterDefn::KeywordOnly)) {
    valid = false;
    return false;
  }

  target.value.push_back(nextPositionalParam);
  inUse[nextPositionalParam] = true;

  // If it's a variadic parameter, then assign all subsequent positional arguments to it.
  if (!param->isVariadic()) {
    nextPositionalParam += 1;
  }

  return true;
}

bool ParameterAssignmentsBuilder::addKeywordArg(const char * kw) {
  int paramIndex = ftype->getParamNameIndex(kw);

  // TODO: Return failure codes instead of false, for better
  // reporting of errors.
  if (paramIndex < 0) {
    valid = false;
    return false; // No such param
  }

  // Check if argument already assigned.
  // TODO: We could allow variadic keyword arguments, which would allow the same keyword to
  // be assigned multiple times to the same parameter.
  if (inUse[paramIndex]) {
    valid = false;
    return false;
  }

  // Add to index
  target.value.push_back(paramIndex);
  inUse[paramIndex] = true;
  return true;
}

bool ParameterAssignmentsBuilder::check() {
  if (!valid) {
    return false;
  }
  
  // Insure that any unspecified arguments have default values.
  for (size_t i = 0; i < paramCount; ++i) {
    ParameterDefn * param = ftype->params()[i];
    if (!inUse[i] && param->defaultValue() == NULL && !param->isVariadic()) {
      valid = false;
      return false;
    }
  }
  
  return true;
}

bool ParameterAssignmentsBuilder::assignFromAST(const ASTNodeList & args) {
  for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
    if ((*it)->getNodeType() == ASTNode::Keyword) {
      if (!addKeywordArg(static_cast<const ASTKeywordArg *>(*it)->getKeyword())) {
        return false;
      }
    } else if (!addPositionalArg()) {
      return false;
    }
  }
  
  if (!check()) {
    return false;
  }
  
  return true;
}

} // namespace tart

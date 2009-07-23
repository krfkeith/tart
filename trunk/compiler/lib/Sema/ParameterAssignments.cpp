/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Sema/ParameterAssignments.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"

namespace tart {

/// -------------------------------------------------------------------
/// Builder class used to build a parameter assignment.

ParameterAssignmentsBuilder::ParameterAssignmentsBuilder(
    ParameterAssignments & pa, const FunctionType * f)
  : target(pa)
  , ftype(f)
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
  
  size_t paramIndex = target.size();
  if (paramIndex >= paramCount) {
    valid = false;
    return false;
  }

  target.value.push_back(paramIndex);
  inUse[paramIndex] = true;
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
    if (!inUse[i] && ftype->params()[i]->defaultValue() == NULL) {
      valid = false;
      return false;
    }
  }
  
  return true;
}

bool ParameterAssignmentsBuilder::assignFromAST(const ASTNodeList & args) {
  for (ASTNodeList::const_iterator it = args.begin(); it != args.end(); ++it) {
    if ((*it)->getNodeType() == ASTNode::Keyword) {
      if (!addKeywordArg(
          static_cast<const ASTKeywordArg *>(*it)->getKeyword())) {
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

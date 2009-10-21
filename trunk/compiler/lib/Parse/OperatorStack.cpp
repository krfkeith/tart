/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Parse/OperatorStack.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

bool OperatorStack::pushOperand(ASTNode * operand) {
  if (stack.back().operand != NULL) {
    diag.fatal(operand->location()) << "missing operator";
    return false;
  }
  stack.back().operand = operand;
  return true;
}

bool OperatorStack::pushOperator(ASTNode * oper, int32_t prec,
    Associativity assoc) {
  if (stack.back().operand == NULL) {
    diag.fatal(oper->location()) << "missing operand";
    return false;
  }

  if (!reduce(prec, assoc))
    return false;

  stack.push_back(Entry());
  stack.back().operatorExp = oper;
  stack.back().operatorPrec = prec;
  stack.back().operAssoc = assoc;
  stack.back().operand = NULL;
  return true;
}

bool OperatorStack::reduce(int32_t operatorPrec, bool rightAssoc) {
  while (stack.size() > 1) {
    Entry & back = stack.back();
    if (back.operatorPrec < operatorPrec)
      break;
    ASTNode * eop = back.operatorExp;
    ASTNodeList & args = eop->nodeType() == ASTNode::Call ?
        static_cast<ASTCall *>(eop)->args() :
        static_cast<ASTOper *>(eop)->args();
    ASTNode * val = back.operand;
    assert(val != NULL);
    stack.pop_back();
    args.push_back(stack.back().operand);
    args.push_back(val);
    eop->location() = stack.back().operand->location() | val->location();
    stack.back().operand = eop;
  }
  return true;
}

bool OperatorStack::reduceAll() {
  if (!reduce(0, 0))
    return false;
  assert(stack.size() == 1);
  return true;
}

}

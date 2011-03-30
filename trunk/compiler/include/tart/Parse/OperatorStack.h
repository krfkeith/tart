/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_PARSE_OPERATORSTACK_H
#define TART_PARSE_OPERATORSTACK_H

#ifndef TART_LEX_LEXER_H
#include "tart/Lex/Lexer.h"
#endif

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
// Operator associativity enum
enum Associativity {
  Left,
  Right
};

/// -------------------------------------------------------------------
/// Operator stack for operator precedence parsing.
class OperatorStack {
public:
  // Contains an operator/operand pair. The bottom element of the
  // stack contains only an operand:
  //
  //    [NULL value][op value][op value] ...
  struct Entry {
    ASTNode * operand;
    ASTNode * operatorExp;
    int32_t operatorPrec;
    Associativity operAssoc;
  };

  typedef llvm::SmallVector<Entry, 8> tStackType;

  tStackType stack;

  OperatorStack(ASTNode * initialExpr) {
    stack.push_back(Entry());
    stack.back().operand = initialExpr;
  }
  bool pushOperand(ASTNode * operand);
  bool pushOperator(ASTNode * oper, int32_t prec, Associativity assoc);
  bool reduce(int32_t precedence, bool rightAssoc);
  bool reduceAll();

  ASTNode * getExpression() const {
    return stack.front().operand;
  }
};

}

#endif

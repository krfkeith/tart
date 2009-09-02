/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_LEX_TOKENS_H
#define TART_LEX_TOKENS_H

// Lexer

namespace tart {

#ifdef DEFINE_TOKEN
#undef DEFINE_TOKEN
#endif

#define DEFINE_TOKEN(x) Token_##x,

enum TokenType {
  #include "Tokens.def"
  Token_Last
};

// Return the name of the specified token.
const char * GetTokenName(TokenType tt);

}

#endif

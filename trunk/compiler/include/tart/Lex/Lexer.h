/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_LEX_LEXER_H
#define TART_LEX_LEXER_H

#ifndef TART_COMMON_SOURCEFILE_H
#include "tart/Common/SourceFile.h"
#endif

#ifndef TART_LEX_TOKENS_H
#include "tart/Lex/Tokens.h"
#endif

namespace tart {

// -------------------------------------------------------------------
// Lexical analyzer
class Lexer {
public:
  enum LexerError {
    ERROR_NONE = 0,
    ILLEGAL_CHAR,
    UNTERMINATED_COMMENT,
    UNTERMINATED_STRING,
    MALFORMED_ESCAPE_SEQUENCE,
    INVALID_UNICODE_CHAR,
    EMPTY_CHAR_LITERAL,
    MULTI_CHAR_LITERAL,
  };

  /** Constructor */
  Lexer(ProgramSource * srcFile_);

  /** Destructor closes the stream. */
  ~Lexer() {
    srcFile_->close();
  }

  /** Get the next token */
  TokenType next();

  /** Current value of the token. */
  const std::string & tokenValue() const { return tokenValue_; }

  /** Location of the token in the source file. */
  const SourceLocation & tokenLocation() {
    tokenLocation_.end = currentOffset_;
    return tokenLocation_;
  }

  /** Get the current accumulated doc comment for this token. */
  const std::string & docComment() const { return docComment_; }
  std::string & docComment() { return docComment_; }

  /** Clear the accumulated doc comment. */
  void clearDocComment() { docComment_.clear(); }

  /** Current error code. */
  LexerError errorCode() const { return errorCode_; }

  /** Add 'charVal' to the current token value, encoded as UTF-8. */
  bool encodeUnicodeChar(long charVal);

private:
  // Source file containing the buffer
  ProgramSource     * srcFile_;         // Pointer to source file buffer
  std::istream      & stream_;          // Input stream
  int                 ch_;              // Previously read char.
  size_t              currentOffset_;   // Current char count in file
  size_t              lineStartOffset_; // Read position at line start
  size_t              tokenStartOffset_;// Start position of current token
  SourceLocation      tokenLocation_;   // Start source location of current token
  std::string         tokenValue_;      // String value of token
  std::string         docComment_;      // Accumulated doc comment
  uint16_t            lineIndex_;       // Current line index
  LexerError          errorCode_;       // Error code

  // Read the next character.
  void readCh();
};

}

#endif

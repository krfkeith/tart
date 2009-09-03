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
    tokenLocation_.end = currentOffset;
    return tokenLocation_;
  }

  /** Get the current accumulated doc comment for this token. */
  const std::string & docComment() const { return docComment_; }
  std::string & docComment() { return docComment_; }

  /** Clear the accumulated doc comment. */
  void clearDocComment() { docComment_.clear(); }

private:
  // Source file containing the buffer
  ProgramSource     * srcFile_;         // Pointer to source file buffer
  std::istream      & stream_;          // Input stream
  int                 ch;               // Previously read char.
  size_t              currentOffset;    // Current char count in file
  size_t              lineStartOffset;  // Read position at line start
  size_t              tokenStartOffset; // Start position of current token
  SourceLocation      tokenLocation_;   // Start source location of current token
  std::string         tokenValue_;      // String value of token
  std::string         docComment_;      // Accumulated doc comment
  uint16_t            lineIndex;        // Current line index

  // Read the next character.
  void readCh();
};

}

#endif

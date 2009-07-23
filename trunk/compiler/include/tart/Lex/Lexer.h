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
private:
  // Source file containing the buffer
  ProgramSource     * srcFile;        // Pointer to source file buffer
  std::istream      & stream;         // Input stream
  int                 ch;             // Previously read char.
  size_t              currentOffset;  // Current char count in file
  size_t              lineStartOffset;// Read position at line start
  size_t              tokenStartOffset;// Start position of current token
  SourceLocation      tokenLocation;  // Start source location of current token
  std::string         tokenValue;     // String value of token
  std::string         docCommentText; // Accumulated doc comment
  uint16_t            lineIndex;      // Current line index
  
  // Read the next character.
  void readCh();

public:
  /** Constructor */
  Lexer(ProgramSource * srcFile);
  
  /** Destructor closes the stream. */
  ~Lexer() {
    srcFile->close();
  }
  
  /** Get the next token */
  TokenType next();

  /** Current value of the token. */
  const std::string & getTokenValue() const { return tokenValue; }

  /** Location of the token in the source file. */
  const SourceLocation & getTokenLocation() {
    tokenLocation.end = currentOffset;
    return tokenLocation;
  }

  /** Get the current accumulated doc comment for this token. */
  const std::string & getDocComment() const { return docCommentText; }

  /** Clear the accumulated doc comment. */
  void clearDocComment() { docCommentText.clear(); }
};

}

#endif

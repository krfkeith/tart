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

#ifndef TART_AST_DOCCOMMENT_H
#include "tart/AST/DocComment.h"
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

  /** Comments can point forward to the next declaration, or backwards to the previous one. */
  enum CommentDirection {
    UNKNOWN = 0,
    FORWARD,
    BACKWARD,
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
  const DocComment & docComment() const { return docComment_; }
  DocComment & docComment() { return docComment_; }

  /** Clear the accumulated doc comment. */
  void clearDocComment();

  /** Get the current doc comment, and transfer its contents to 'dst', which must be empty.
      'direction' specifies whether we want a forward or backward comment; If the current
      comment's direction is not the requested direction, then no action is taken.
   */
  void takeDocComment(DocComment & dst, CommentDirection direction = FORWARD);

  /** Set the direction of the current doc comment. You can't change direction once it's known */
  void setCommentDirection(CommentDirection dir);

  /** Current error code. */
  LexerError errorCode() const { return errorCode_; }

  /** Add 'charVal' to the current token value, encoded as UTF-8. */
  bool encodeUnicodeChar(long charVal);

private:
  // Source file containing the buffer
  ProgramSource     * srcFile_;         // Pointer to source file buffer
  std::istream      & stream_;          // Input stream
  int                 ch_;              // Previously read char.
  uint32_t            currentOffset_;   // Current char count in file
  uint32_t            lineStartOffset_; // Read position at line start
  uint32_t            tokenStartOffset_;// Start position of current token
  SourceLocation      tokenLocation_;   // Start source location of current token
  std::string         tokenValue_;      // String value of token
  DocComment          docComment_;      // Accumulated doc comment
  CommentDirection    docCommentDir_;   // Comment direction
  uint16_t            lineIndex_;       // Current line index
  LexerError          errorCode_;       // Error code

  // Read the next character.
  void readCh();
};

}

#endif

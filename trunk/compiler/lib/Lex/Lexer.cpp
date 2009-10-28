/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Lex/Lexer.h"
#include "tart/Common/Diagnostics.h"
#include <algorithm>

namespace tart {

namespace {
  // TODO: Add support for unicode letters.
  bool isNameStartChar(char ch) {
    return (ch >= 'a' && ch <= 'z') ||
        (ch >= 'A' && ch <= 'Z') ||
        (ch == '_');
  }

  bool isNameChar(char ch) {
    return (ch >= 'a' && ch <= 'z') ||
        (ch >= 'A' && ch <= 'Z') ||
        (ch >= '0' && ch <= '9') ||
        (ch == '_');
  }

  bool isDigitChar(char ch) {
    return (ch >= '0' && ch <= '9');
  }

  bool isHexDigitChar(char ch) {
    return (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'f' || ch >= 'A' && ch <= 'F');
  }

  // Instead of a hash table or sorted list, we rely on the compiler to
  // be smart about optimizing a switch statement that is known to
  // have a small number of cases.
  TokenType LookupKeyword(const char * kw) {
    char ch = kw[0];
    if (ch >= 'a' && ch <= 'z') {
      switch ((uint8_t)ch) {
        case 'a':
          if (strcmp(kw, "as") == 0) return Token_As;
          if (strcmp(kw, "and") == 0) return Token_LogicalAnd;
          if (strcmp(kw, "abstract") == 0) return Token_Abstract;
          break;

        case 'b':
          if (strcmp(kw, "bool") == 0) return Token_BoolType;
          if (strcmp(kw, "byte") == 0) return Token_ByteType;
          if (strcmp(kw, "break") == 0) return Token_Break;
          break;

        case 'c':
          if (strcmp(kw, "class") == 0) return Token_Class;
          if (strcmp(kw, "classify") == 0) return Token_Classify;
          if (strcmp(kw, "catch") == 0) return Token_Catch;
          if (strcmp(kw, "char") == 0) return Token_CharType;
          if (strcmp(kw, "continue") == 0) return Token_Continue;
          if (strcmp(kw, "const") == 0) return Token_Const;
          if (strcmp(kw, "case") == 0) return Token_Case;
          break;

        case 'd':
          if (strcmp(kw, "def") == 0) return Token_Def;
          //if (strcmp(kw, "defover") == 0) return Token_Def;
          if (strcmp(kw, "double") == 0) return Token_DoubleType;
          break;

        case 'e':
          if (strcmp(kw, "else") == 0) return Token_Else;
          if (strcmp(kw, "enum") == 0) return Token_Enum;
          break;

        case 'f':
          if (strcmp(kw, "float") == 0) return Token_FloatType;
          if (strcmp(kw, "for") == 0) return Token_For;
          if (strcmp(kw, "finally") == 0) return Token_Finally;
          if (strcmp(kw, "fn") == 0) return Token_Function;
          if (strcmp(kw, "final") == 0) return Token_Final;
          if (strcmp(kw, "false") == 0) return Token_False;
          break;

        case 'i':
          if (strcmp(kw, "is") == 0) return Token_Is;
          if (strcmp(kw, "in") == 0) return Token_In;
          if (strcmp(kw, "int") == 0) return Token_IntType;
          if (strcmp(kw, "if") == 0) return Token_If;
          if (strcmp(kw, "import") == 0) return Token_Import;
          if (strcmp(kw, "interface") == 0) return Token_Interface;
          if (strcmp(kw, "isa") == 0) return Token_Isa;
          if (strcmp(kw, "int8") == 0) return Token_ByteType;
          if (strcmp(kw, "int16") == 0) return Token_ShortType;
          if (strcmp(kw, "int32") == 0) return Token_IntType;
          if (strcmp(kw, "int64") == 0) return Token_LongType;
          break;

        case 'g':
          if (strcmp(kw, "get") == 0) return Token_Get;
          break;

        case 'l':
          if (strcmp(kw, "let") == 0) return Token_Let;
          if (strcmp(kw, "long") == 0) return Token_LongType;
          break;

        case 'm':
          if (strcmp(kw, "module") == 0) return Token_Module;
          if (strcmp(kw, "macro") == 0) return Token_Macro;
          break;

        case 'n':
          if (strcmp(kw, "not") == 0) return Token_LogicalNot;
          if (strcmp(kw, "null") == 0) return Token_Null;
          if (strcmp(kw, "namespace") == 0) return Token_Namespace;
          break;

        case 'o':
          if (strcmp(kw, "or") == 0) return Token_LogicalOr;
          break;

        case 'p':
          if (strcmp(kw, "public") == 0) return Token_Public;
          if (strcmp(kw, "private") == 0) return Token_Private;
          if (strcmp(kw, "protected") == 0) return Token_Protected;
          if (strcmp(kw, "protocol") == 0) return Token_Protocol;
          break;

        case 'r':
          if (strcmp(kw, "return") == 0) return Token_Return;
          if (strcmp(kw, "repeat") == 0) return Token_Repeat;
          if (strcmp(kw, "readonly") == 0) return Token_Readonly;
          if (strcmp(kw, "redef") == 0) return Token_Redef;
          break;

        case 's':
          if (strcmp(kw, "short") == 0) return Token_ShortType;
          if (strcmp(kw, "struct") == 0) return Token_Struct;
          if (strcmp(kw, "sbyte") == 0) return Token_ByteType;
          if (strcmp(kw, "set") == 0) return Token_Set;
          if (strcmp(kw, "static") == 0) return Token_Static;
          if (strcmp(kw, "super") == 0) return Token_Super;
          if (strcmp(kw, "switch") == 0) return Token_Switch;
          break;

        case 't':
          if (strcmp(kw, "try") == 0) return Token_Try;
          if (strcmp(kw, "throw") == 0) return Token_Throw;
          if (strcmp(kw, "true") == 0) return Token_True;
          if (strcmp(kw, "typealias") == 0) return Token_Typealias;
          if (strcmp(kw, "typecast") == 0) return Token_Typecast;
          break;

        case 'u':
          if (strcmp(kw, "ubyte") == 0) return Token_UByteType;
          if (strcmp(kw, "ushort") == 0) return Token_UShortType;
          if (strcmp(kw, "uint") == 0) return Token_UIntType;
          if (strcmp(kw, "ulong") == 0) return Token_ULongType;
          if (strcmp(kw, "uint8") == 0) return Token_UByteType;
          if (strcmp(kw, "uint16") == 0) return Token_UShortType;
          if (strcmp(kw, "uint32") == 0) return Token_UIntType;
          if (strcmp(kw, "uint64") == 0) return Token_ULongType;
          if (strcmp(kw, "undef") == 0) return Token_Undef;
          break;

        case 'v':
          if (strcmp(kw, "var") == 0) return Token_Var;
          if (strcmp(kw, "void") == 0) return Token_VoidType;
          break;

        case 'w':
          if (strcmp(kw, "while") == 0) return Token_While;
          if (strcmp(kw, "while") == 0) return Token_Where;
          break;

        case 'y':
          if (strcmp(kw, "yield") == 0) return Token_Yield;
          break;
      }
    }

    return Token_Ident;
  }
}

Lexer::Lexer(ProgramSource * src)
    : srcFile_(src)
    , stream_(src->open())
    , errorCode_(ERROR_NONE)
{
  ch_ = 0;
  readCh();
  lineStartOffset_ = tokenStartOffset_ = currentOffset_ = 0;
  tokenLocation_.file = srcFile_->get();
  tokenLocation_.begin = 0;
  tokenLocation_.end = 0;
  lineIndex_ = 1;
}

inline void Lexer::readCh() {
  if (ch_ >= 0) {
    currentOffset_++;
  }

  ch_ = stream_.get();
}

TokenType Lexer::next() {

  // Whitespace loop
  for (;;) {
    if (ch_ < 0) {
      srcFile_->newLine(currentOffset_);
      return Token_End;
    } else if (ch_ == ' ' || ch_ == '\t' || ch_ == '\b') {
      // Horizontal whitespace
      readCh();
    } else if (ch_ == '\n') {
      // Linefeed
      readCh();
      srcFile_->newLine(currentOffset_);
      lineStartOffset_ = currentOffset_;
      lineIndex_ += 1;
    } else if (ch_ == '\r') {
      // Carriage return. Look for CRLF pair and count as 1 line.
      readCh();
      if (ch_ == '\n') {
        readCh();
      }
      srcFile_->newLine(currentOffset_);
      lineStartOffset_ = currentOffset_;
      lineIndex_ += 1;
    } else if (ch_ == '/') {
      // Check for comment start
      readCh();
      bool inDocComment = false;
      if (ch_ == '/') {
        readCh();
        if (ch_ == '/') {
          // Doc comment
          readCh();
          inDocComment = true;
        }

        while (ch_ >= 0 && ch_ != '\n' && ch_ != '\r') {
          if (inDocComment)
            docComment_.push_back(ch_);
          readCh();
        }
        if (inDocComment)
          docComment_.push_back('\n');
      } else if (ch_ == '*') {
        readCh();
        if (ch_ == '*') {
          // Doc comment
          readCh();
          inDocComment = true;
          docComment_.clear();
        }

        for (;;) {
          if (ch_ < 0) {
            errorCode_ = UNTERMINATED_COMMENT;
            return Token_Error;
          }
          if (ch_ == '*') {
            readCh();
            if (ch_ == '/') {
              readCh();
              break;
            } else {
              if (inDocComment)
                docComment_.push_back('*');
              // Push the star we skipped, and reprocess the following char
            }
          } else {
            if (ch_ == '\r' || ch_ == '\n') {
              // Carriage return.
              // Look for CRLF pair and count as 1 line.
              if (ch_ == '\r') {
                readCh();
              }
              if (ch_ == '\n') {
                readCh();
              }
              if (inDocComment)
                docComment_.push_back('\n');
              srcFile_->newLine(currentOffset_);
              lineStartOffset_ = currentOffset_;
              lineIndex_ += 1;
            } else {
              if (inDocComment)
                docComment_.push_back(ch_);
              readCh();
            }
          }
        }
        if (inDocComment)
          docComment_.push_back('\n');
      } else {
        // What comes after a '/' char.
        if (ch_ == '=') {
          readCh();
          return Token_AssignSlash;
        }
        return Token_Slash;
      }
    } else {
      break;
    }
  }

  tokenStartOffset_ = currentOffset_;
  tokenLocation_.begin = currentOffset_;

  // Identifier
  if (isNameStartChar(ch_)) {
    tokenValue_.clear();
    tokenValue_.push_back(ch_);
    readCh();
    while (isNameChar(ch_)) {
      tokenValue_.push_back(ch_);
      readCh();
    }

    // Check for keyword
    return LookupKeyword(tokenValue_.c_str());
  }

  // Number
  if (isDigitChar(ch_) || ch_ == '.') {
    bool isFloat = false;
    tokenValue_.clear();

    // Hex number check
    if (ch_ == '0') {
      tokenValue_.push_back('0');
      readCh();
      if (ch_ == 'X' || ch_ == 'x') {
        tokenValue_.push_back('x');
        readCh();
        for (;;) {
          if (isHexDigitChar(ch_)) {
            tokenValue_.push_back(ch_);
            readCh();
          } else if (ch_ == '_') {
            readCh();
          } else {
            break;
          }
        }

        return Token_Integer;
      }
    }

    // Integer part
    for (;;) {
      if (isDigitChar(ch_)) {
        tokenValue_.push_back(ch_);
        readCh();
      } else if (ch_ == '_') {
        readCh();
      } else {
        break;
      }
    }

    // Fractional part
    if (ch_ == '.') {
      readCh();

      // Special case of '..' range token and '...' ellipsis token.
      if (ch_ == '.') {
        if (!tokenValue_.empty()) {
          stream_.putback('.');
          return Token_Integer;
        }
        readCh();
        if (ch_ == '.') {
          readCh();
          return Token_Ellipsis;
        }
        return Token_Range;
      }

      // Check for case where this isn't a decimal point,
      // but just a dot token.
      if (!isDigitChar(ch_) && tokenValue_.empty()) {
        return Token_Dot;
      }

      // It's a float
      isFloat = true;

      tokenValue_.push_back('.');
      for (;;) {
        if (isDigitChar(ch_)) {
          tokenValue_.push_back(ch_);
          readCh();
        } else if (ch_ == '_') {
          readCh();
        } else {
          break;
        }
      }
    }

    // Exponent part
    if ((ch_ == 'e' || ch_ == 'E')) {
      isFloat = true;
      tokenValue_.push_back(ch_);
      readCh();
      if ((ch_ == '+' || ch_ == '-')) {
        tokenValue_.push_back(ch_);
        readCh();
      }
      for (;;) {
        if (isDigitChar(ch_)) {
          tokenValue_.push_back(ch_);
          readCh();
        } else if (ch_ == '_') {
          readCh();
        } else {
          break;
        }
      }
    }

    if ((ch_ == 'f' || ch_ == 'F')) {
      isFloat = true;
      tokenValue_.push_back(ch_);
      readCh();
    }

    if (isFloat) {
      return Token_Float;
    }

    return Token_Integer;
  }

  // Punctionation
  switch (ch_) {
    case ':':
      readCh();
      if (ch_ == ':') {
        readCh();
        return Token_DoubleColon;
      }
      return Token_Colon;

    case '+':
      readCh();
      if (ch_ == '+') {
        readCh();
        return Token_Increment;
      }
      if (ch_ == '=') {
        readCh();
        return Token_AssignPlus;
      }
      return Token_Plus;

    case '-':
      readCh();
      if (ch_ == '-') {
        readCh();
        return Token_Decrement;
      }
      if (ch_ == '=') {
        readCh();
        return Token_AssignMinus;
      }
      if (ch_ == '>') {
        readCh();
        return Token_ReturnType;
      }
      return Token_Minus;

    case '*':
      readCh();
      if (ch_ == '=') {
        readCh();
        return Token_AssignStar;
      }
      return Token_Star;

    case '%':
      readCh();
      if (ch_ == '=') {
        readCh();
        return Token_AssignPercent;
      }
      return Token_Percent;

    case '^':
      readCh();
      if (ch_ == '=') {
        readCh();
        return Token_AssignCaret;
      }
      return Token_Caret;

    case '|':
      readCh();
      if (ch_ == '=') {
        readCh();
        return Token_AssignBar;
      }
      if (ch_ == '|') {
        readCh();
        return Token_DoubleBar;
      }
      return Token_Bar;

    case '&':
      readCh();
      if (ch_ == '=') {
        readCh();
        return Token_AssignAmpersand;
      }
      if (ch_ == '&') {
        readCh();
        return Token_DoubleAmp;
      }
      return Token_Ampersand;

    case '~':
      readCh();
      if (ch_ == '=') {
        readCh();
        return Token_AssignTilde;
      }
      return Token_Tilde;

    case '>':
      readCh();
      if (ch_ == '=') {
        readCh();
        if (ch_ == '?') {
          readCh();
          return Token_PossGreaterEqual;
        }
        return Token_GreaterEqual;
      }
      if (ch_ == '?') {
        readCh();
        return Token_PossGreater;
      }
      if (ch_ == '>') {
        readCh();
        if (ch_ == '=') {
          readCh();
          return Token_AssignRShift;
        }
        return Token_RShift;
      }
      if (ch_ == ':') {
        return Token_IsSuperclass;
      }
      return Token_Greater;

    case '<':
      readCh();
      if (ch_ == '=') {
        readCh();
        if (ch_ == '?') {
          readCh();
          return Token_PossLessEqual;
        }
        return Token_LessEqual;
      }
      if (ch_ == '?') {
        readCh();
        return Token_PossLess;
      }
      if (ch_ == '<') {
        readCh();
        if (ch_ == '=') {
          readCh();
          return Token_AssignLShift;
        }
        return Token_LShift;
      }
      if (ch_ == ':') {
        return Token_IsSubclass;
      }
      return Token_Less;

    case '=':
      readCh();
      if (ch_ == '=') {
        readCh();
        return Token_Equal;
      }
      return Token_Assign;

    case '!':
      readCh();
      if (ch_ == '=') {
        readCh();
        return Token_NotEqual;
      }
      return Token_Exclam;

    case '{':
      readCh();
      return Token_LBrace;

    case '}':
      readCh();
      return Token_RBrace;

    case '[':
      readCh();
      return Token_LBracket;

    case ']':
      readCh();
      return Token_RBracket;

    case '(':
      readCh();
      return Token_LParen;

    case ')':
      readCh();
      return Token_RParen;

    case ';':
      readCh();
      return Token_Semi;

    case ',':
      readCh();
      return Token_Comma;

    case '?':
      readCh();
      return Token_QMark;

    case '$':
      readCh();
      return Token_DollarSign;

    case '@':
      readCh();
      return Token_AtSign;

    case '"':
    case '\'': {
        // String literal
        tokenValue_.clear();
        char quote = ch_;
        int charCount = 0;
        readCh();
        for (;;) {
          if (ch_ < 0) {
            errorCode_ = UNTERMINATED_STRING;
            return Token_Error;
          } else if (ch_ == quote) {
            readCh();
            break;
          } else if (ch_ == '\\') {
            readCh();
            switch (ch_) {
              case '0':
                tokenValue_.push_back('\0');
                readCh();
                break;
              case '\\':
                tokenValue_.push_back('\\');
                readCh();
                break;
              case '\'':
                tokenValue_.push_back('\'');
                readCh();
                break;
              case '\"':
                tokenValue_.push_back('\"');
                readCh();
                break;
              case 'r':
                tokenValue_.push_back('\r');
                readCh();
                break;
              case 'n':
                tokenValue_.push_back('\n');
                readCh();
                break;
              case 't':
                tokenValue_.push_back('\t');
                readCh();
                break;
              case 'b':
                tokenValue_.push_back('\b');
                readCh();
                break;
              case 'v':
                tokenValue_.push_back('\v');
                readCh();
                break;
              case 'x': {
                // Parse a hexidecimal character in a string.
                char charbuf[3];
                size_t  len = 0;
                readCh();
                while (isHexDigitChar(ch_) && len < 2) {
                  charbuf[len++] = ch_;
                  readCh();
                }

                if (len == 0) {
                  errorCode_ = MALFORMED_ESCAPE_SEQUENCE;
                  return Token_Error;
                }

                charbuf[len] = 0;
                long charVal = strtoul(charbuf, NULL, 16);
                tokenValue_.push_back(charVal);
                break;
              }

              case 'u':
              case 'U': {
                  // Parse a unicode character literal in a string.
                  size_t maxLen = (ch_ == 'u' ? 4 : 8);
                  char charbuf[9];
                  size_t  len = 0;
                  readCh();
                  while (isHexDigitChar(ch_) && len < maxLen) {
                    charbuf[len++] = ch_;
                    readCh();
                  }
                  if (len == 0) {
                    // TODO: Report it
                    errorCode_ = MALFORMED_ESCAPE_SEQUENCE;
                    return Token_Error;
                  }

                  charbuf[len] = 0;
                  long charVal = strtoul(charbuf, NULL, 16);

                  if (quote == '"') {
                    if (!encodeUnicodeChar(charVal)) {
                      errorCode_ = INVALID_UNICODE_CHAR;
                      return Token_Error;
                    }
                  } else {
                    // For character literals, since the token value is a string, then just
                    // encode it as a hex number.
                    len = snprintf(charbuf, 9, "%0.8x", charVal);
                    tokenValue_.append(charbuf, len);
                  }

                  break;
                }
              default:
                tokenValue_.push_back(ch_);
                readCh();
                break;
            }
          } else if (ch_ >= ' ') {
            tokenValue_.push_back(ch_);
            readCh();
          } else {
            errorCode_ = MALFORMED_ESCAPE_SEQUENCE;
            return Token_Error;
          }

          ++charCount;
        }

        if (quote == '\'') {
          if (charCount != 1) {
            errorCode_ = (charCount == 0 ? EMPTY_CHAR_LITERAL : MULTI_CHAR_LITERAL);
            return Token_Error;
          }
          return Token_Char;
        } else {
          return Token_String;
        }

        return quote == '"' ? Token_String : Token_Char;
      }

    default:
      break;
  }

  tokenValue_.push_back(ch_);
  errorCode_ = ILLEGAL_CHAR;
  return Token_Error;
}

bool Lexer::encodeUnicodeChar(long charVal) {
  if (charVal < 0x80) {
    tokenValue_.push_back(charVal);
  } else if (charVal < 0x800) {
    tokenValue_.push_back(0xc0 | (charVal >> 6));
    tokenValue_.push_back(0x80 | (charVal & 0x3f));
  } else if (charVal < 0x10000) {
    tokenValue_.push_back(0xe0 | (charVal >> 12));
    tokenValue_.push_back(0x80 | ((charVal >> 6) & 0x3f));
    tokenValue_.push_back(0x80 | (charVal & 0x3f));
  } else if (charVal < 0x100000) {
    tokenValue_.push_back(0xf0 | (charVal >> 18));
    tokenValue_.push_back(0x80 | ((charVal >> 12) & 0x3f));
    tokenValue_.push_back(0x80 | ((charVal >> 6) & 0x3f));
    tokenValue_.push_back(0x80 | (charVal & 0x3f));
  } else {
    errorCode_ = INVALID_UNICODE_CHAR;
    return false;
  }

  return true;
}

}

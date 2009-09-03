/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Lex/Lexer.h"
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
          if (strcmp(kw, "case") == 0) return Token_Case;
          break;

        case 'd':
          if (strcmp(kw, "def") == 0) return Token_Def;
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
          if (strcmp(kw, "function") == 0) return Token_Function;
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
          if (strcmp(kw, "requires") == 0) return Token_Requires;
          break;

        case 's':
          if (strcmp(kw, "short") == 0) return Token_ShortType;
          if (strcmp(kw, "struct") == 0) return Token_Struct;
          //if (strcmp(kw, "sbyte") == 0) return Token_SInt8;
          //if (strcmp(kw, "sint8") == 0) return Token_SInt8;
          //if (strcmp(kw, "sint16") == 0) return Token_SInt16;
          //if (strcmp(kw, "sint32") == 0) return Token_SInt32;
          //if (strcmp(kw, "sint64") == 0) return Token_SInt64;
          if (strcmp(kw, "set") == 0) return Token_Set;
          if (strcmp(kw, "static") == 0) return Token_Static;
          if (strcmp(kw, "super") == 0) return Token_Super;
          if (strcmp(kw, "switch") == 0) return Token_Switch;
          break;

        case 't':
          if (strcmp(kw, "try") == 0) return Token_Try;
          if (strcmp(kw, "throw") == 0) return Token_Throw;
          if (strcmp(kw, "true") == 0) return Token_True;
          if (strcmp(kw, "typeof") == 0) return Token_TypeOf;
          break;

        case 'u':
          if (strcmp(kw, "ubyte") == 0) return Token_UByteType;
          if (strcmp(kw, "ushort") == 0) return Token_UShortType;
          if (strcmp(kw, "uint") == 0) return Token_UIntType;
          if (strcmp(kw, "ulong") == 0) return Token_ULongType;
          //if (strcmp(kw, "uint8") == 0) return Token_UInt8;
          //if (strcmp(kw, "uint16") == 0) return Token_UInt16;
          //if (strcmp(kw, "uint32") == 0) return Token_UInt32;
          //if (strcmp(kw, "uint64") == 0) return Token_UInt64;
          break;

        case 'v':
          if (strcmp(kw, "var") == 0) return Token_Var;
          if (strcmp(kw, "void") == 0) return Token_VoidType;
          break;

        case 'w':
          if (strcmp(kw, "while") == 0) return Token_While;
          break;

        case 'y':
          if (strcmp(kw, "yield") == 0) return Token_Yield;
          break;
      }
    } else if (ch == '_') {
      if (strcmp(kw, "__intrinsic") == 0) return Token_Intrinsic;
      if (strcmp(kw, "__ref") == 0) return Token_Ref;
      //if (strcmp(kw, "__reinterpret") == 0) return Token_Offset;
    }

    return Token_Ident;
  }
}

Lexer::Lexer(ProgramSource * src)
    : srcFile_(src)
    , stream_(src->open()) {
  readCh();
  lineStartOffset = tokenStartOffset = currentOffset = 0;
  tokenLocation_.file = srcFile_->get();
  tokenLocation_.begin = 0;
  tokenLocation_.end = 0;
  lineIndex = 1;
}

inline void Lexer::readCh() {
  ch = stream_.get();
  if (ch >= 0)
    currentOffset++;
}

TokenType Lexer::next() {

  // Whitespace loop
  for (;;) {
    if (ch < 0) {
      srcFile_->newLine(currentOffset);
      return Token_End;
    } else if (ch == ' ' || ch == '\t' || ch == '\b') {
      // Horizontal whitespace
      readCh();
    } else if (ch == '\n') {
      // Linefeed
      readCh();
      srcFile_->newLine(currentOffset);
      lineStartOffset = currentOffset;
      lineIndex += 1;
    } else if (ch == '\r') {
      // Carriage return. Look for CRLF pair and count as 1 line.
      readCh();
      if (ch == '\n') {
        readCh();
      }
      srcFile_->newLine(currentOffset);
      lineStartOffset = currentOffset;
      lineIndex += 1;
    } else if (ch == '/') {
      // Check for comment start
      readCh();
      bool inDocComment = false;
      if (ch == '/') {
        readCh();
        if (ch == '/') {
          // Doc comment
          readCh();
          inDocComment = true;
        }

        while (ch >= 0 && ch != '\n' && ch != '\r') {
          if (inDocComment)
            docComment_.push_back(ch);
          readCh();
        }
        if (inDocComment)
          docComment_.push_back('\n');
      } else if (ch == '*') {
        readCh();
        if (ch == '*') {
          // Doc comment
          readCh();
          inDocComment = true;
          docComment_.clear();
        }

        for (;;) {
          if (ch < 0) {
            // TODO: Flag an error
            return Token_Error;
          }
          if (ch == '*') {
            readCh();
            if (ch == '/') {
              readCh();
              break;
            } else {
              if (inDocComment)
                docComment_.push_back('*');
              // Push the star we skipped, and reprocess the following char
            }
          } else {
            if (ch == '\r' || ch == '\n') {
              // Carriage return.
              // Look for CRLF pair and count as 1 line.
              if (ch == '\r') {
                readCh();
              }
              if (ch == '\n') {
                readCh();
              }
              if (inDocComment)
                docComment_.push_back('\n');
              srcFile_->newLine(currentOffset);
              lineStartOffset = currentOffset;
              lineIndex += 1;
            } else {
              if (inDocComment)
                docComment_.push_back(ch);
              readCh();
            }
          }
        }
        if (inDocComment)
          docComment_.push_back('\n');
      } else {
        // What comes after a '/' char.
        if (ch == '=') {
          readCh();
          return Token_AssignSlash;
        }
        return Token_Slash;
      }
    } else {
      break;
    }
  }

  tokenStartOffset = currentOffset;
  tokenLocation_.begin = currentOffset;

  // Identifier
  if (isNameStartChar(ch)) {
    tokenValue_.clear();
    tokenValue_.push_back(ch);
    readCh();
    while (isNameChar(ch)) {
      tokenValue_.push_back(ch);
      readCh();
    }

    // Check for keyword
    return LookupKeyword(tokenValue_.c_str());
  }

  // Number
  if (isDigitChar(ch) || ch == '.') {
    bool isFloat = false;
    tokenValue_.clear();

    // Hex number check
    if (ch == '0') {
      tokenValue_.push_back('0');
      readCh();
      if (ch == 'X' || ch == 'x') {
        tokenValue_.push_back('x');
        readCh();
        while (isHexDigitChar(ch)) {
          tokenValue_.push_back(ch);
          readCh();
        }

        return Token_Integer;
      }
    }

    // Integer part
    while (isDigitChar(ch)) {
      tokenValue_.push_back(ch);
      readCh();
    }

    // Fractional part
    if (ch == '.') {
      readCh();

      // Special case of '..' range token and '...' ellipsis token.
      if (ch == '.') {
        if (!tokenValue_.empty()) {
          stream_.putback('.');
          return Token_Integer;
        }
        readCh();
        if (ch == '.') {
          readCh();
          return Token_Ellipsis;
        }
        return Token_Range;
      }

      // Check for case where this isn't a decimal point,
      // but just a dot token.
      if (!isDigitChar(ch) && tokenValue_.empty()) {
        return Token_Dot;
      }

      // It's a float
      isFloat = true;

      tokenValue_.push_back('.');
      while (isDigitChar(ch)) {
        tokenValue_.push_back(ch);
        readCh();
      }
    }

    // Exponent part
    if ((ch == 'e' || ch == 'E')) {
      isFloat = true;
      tokenValue_.push_back(ch);
      readCh();
      if ((ch == '+' || ch == '-')) {
        tokenValue_.push_back(ch);
        readCh();
      }
      while (isDigitChar(ch)) {
        tokenValue_.push_back(ch);
        readCh();
      }
    }

    if ((ch == 'f' || ch == 'F')) {
      isFloat = true;
      tokenValue_.push_back(ch);
      readCh();
    }

    if (isFloat) {
      return Token_Float;
    }

    return Token_Integer;
  }

  // Punctionation
  switch (ch) {
    case ':':
      readCh();
      if (ch == ':') {
        readCh();
        return Token_DoubleColon;
      }
      return Token_Colon;

    case '+':
      readCh();
      if (ch == '+') {
        readCh();
        return Token_Increment;
      }
      if (ch == '=') {
        readCh();
        return Token_AssignPlus;
      }
      return Token_Plus;

    case '-':
      readCh();
      if (ch == '-') {
        readCh();
        return Token_Decrement;
      }
      if (ch == '=') {
        readCh();
        return Token_AssignMinus;
      }
      if (ch == '>') {
        readCh();
        return Token_ReturnType;
      }
      return Token_Minus;

    case '*':
      readCh();
      if (ch == '=') {
        readCh();
        return Token_AssignStar;
      }
      return Token_Star;

    case '%':
      readCh();
      if (ch == '=') {
        readCh();
        return Token_AssignPercent;
      }
      return Token_Percent;

    case '^':
      readCh();
      if (ch == '=') {
        readCh();
        return Token_AssignCaret;
      }
      return Token_Caret;

    case '|':
      readCh();
      if (ch == '=') {
        readCh();
        return Token_AssignBar;
      }
      if (ch == '|') {
        readCh();
        return Token_DoubleBar;
      }
      return Token_Bar;

    case '&':
      readCh();
      if (ch == '=') {
        readCh();
        return Token_AssignAmpersand;
      }
      if (ch == '&') {
        readCh();
        return Token_DoubleAmp;
      }
      return Token_Ampersand;

    case '~':
      readCh();
      if (ch == '=') {
        readCh();
        return Token_AssignTilde;
      }
      return Token_Tilde;

    case '>':
      readCh();
      if (ch == '=') {
        readCh();
        if (ch == '?') {
          readCh();
          return Token_PossGreaterEqual;
        }
        return Token_GreaterEqual;
      }
      if (ch == '?') {
        readCh();
        return Token_PossGreater;
      }
      if (ch == '>') {
        readCh();
        if (ch == '=') {
          readCh();
          return Token_AssignRShift;
        }
        return Token_RShift;
      }
      return Token_Greater;

    case '<':
      readCh();
      if (ch == '=') {
        readCh();
        if (ch == '?') {
          readCh();
          return Token_PossLessEqual;
        }
        return Token_LessEqual;
      }
      if (ch == '?') {
        readCh();
        return Token_PossLess;
      }
      if (ch == '<') {
        readCh();
        if (ch == '=') {
          readCh();
          return Token_AssignLShift;
        }
        return Token_LShift;
      }
      return Token_Less;

    case '=':
      readCh();
      if (ch == '=') {
        readCh();
        return Token_Equal;
      }
      return Token_Assign;

    case '!':
      readCh();
      if (ch == '=') {
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
        char quote = ch;
        readCh();
        for (;;) {
          if (ch < 0) {
            // TODO: Report it
            return Token_Error;
          } else if (ch == quote) {
            readCh();
            break;
          } else if (ch == '\\') {
            readCh();
            switch (ch) {
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
              case 'x':
              case 'u':
              case 'U': {
                  // Parse a hexidecimal character in a string.
                  size_t maxLen = (ch == 'x' ? 2 : (ch == 'u' ? 4 : 8));
                  char charbuf[9];
                  size_t  len = 0;
                  readCh();
                  while (isHexDigitChar(ch) && len < maxLen) {
                    charbuf[len++] = ch;
                    readCh();
                  }
                  if (len == 0) {
                    // TODO: Report it
                    return Token_Error;
                  }
                  charbuf[len] = 0;
                  tokenValue_.push_back(strtoul(charbuf, NULL, 16));
                  break;
                }
              default:
                tokenValue_.push_back(ch);
                readCh();
                break;
            }
          } else if (ch >= ' ') {
            tokenValue_.push_back(ch);
            readCh();
          } else {
            // TODO: Report it
            return Token_Error;
          }
        }
        return quote == '"' ? Token_String : Token_Char;
      }

    default:
      break;
  }

  // TODO: Report it
  return Token_Error;
}

}

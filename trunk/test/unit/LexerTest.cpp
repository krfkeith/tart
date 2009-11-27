/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/Lex/Lexer.h"
#include "FakeSourceFile.h"

namespace {

using namespace tart;

// The fixture for testing class Foo.
class LexerTest : public testing::Test {
 protected:
  LexerTest() {}

  virtual void SetUp() {}
  virtual void TearDown() {}

  /** A function that scans a single token. */
  TokenType LexToken(const char * srcText) {
      FakeSourceFile  src(srcText);
      Lexer           lex(&src);

      TokenType result = lex.next();
      EXPECT_EQ(Token_End, lex.next());
      return result;
  }

  /** A function that scans a single token and returns an error. */
  TokenType LexTokenError(const char * srcText) {
      FakeSourceFile  src(srcText);
      Lexer           lex(&src);
      return lex.next();
  }
};

TEST_F(LexerTest, SingleTokens) {
  // Whitespace
  EXPECT_EQ(Token_End, LexToken(""));
  EXPECT_EQ(Token_End, LexToken(" "));
  EXPECT_EQ(Token_End, LexToken("\t"));
  EXPECT_EQ(Token_End, LexToken("\r\n"));

  // Idents
  EXPECT_EQ(Token_Ident, LexToken("_"));
  EXPECT_EQ(Token_Ident, LexToken("a"));
  EXPECT_EQ(Token_Ident, LexToken("z"));
  EXPECT_EQ(Token_Ident, LexToken("azAZ_01"));
  EXPECT_EQ(Token_Ident, LexToken(" z "));

  // Numbers
  EXPECT_EQ(Token_Integer, LexToken("0"));
  EXPECT_EQ(Token_Integer, LexToken(" 0 "));
  EXPECT_EQ(Token_Integer, LexToken("1"));
  EXPECT_EQ(Token_Integer, LexToken("9"));
  EXPECT_EQ(Token_Integer, LexToken("10"));
  EXPECT_EQ(Token_Integer, LexToken("0x10af"));
  EXPECT_EQ(Token_Integer, LexToken("0X10af"));
  EXPECT_EQ(Token_Float, LexToken("0."));
  EXPECT_EQ(Token_Float, LexToken(" 0. "));
  EXPECT_EQ(Token_Float, LexToken(".0"));
  EXPECT_EQ(Token_Float, LexToken("0f"));
  EXPECT_EQ(Token_Float, LexToken("0e12"));
  EXPECT_EQ(Token_Float, LexToken("0e+12"));
  EXPECT_EQ(Token_Float, LexToken("0e-12"));
  EXPECT_EQ(Token_Float, LexToken("0.0e12f"));

  // Grouping tokens
  EXPECT_EQ(Token_LBrace, LexToken("{"));
  EXPECT_EQ(Token_RBrace, LexToken("}"));
  EXPECT_EQ(Token_LParen, LexToken("("));
  EXPECT_EQ(Token_RParen, LexToken(")"));
  EXPECT_EQ(Token_LBracket, LexToken("["));
  EXPECT_EQ(Token_RBracket, LexToken("]"));

  // Delimiters
  EXPECT_EQ(Token_Semi, LexToken(";"));
  EXPECT_EQ(Token_Colon, LexToken(":"));
  EXPECT_EQ(Token_Comma, LexToken(","));
  EXPECT_EQ(Token_AtSign, LexToken("@"));

  // Operator tokens
  EXPECT_EQ(Token_Assign, LexToken("="));
  //EXPECT_EQ(Token_AssignOp, LexToken(""));
  EXPECT_EQ(Token_ReturnType, LexToken("->"));
  EXPECT_EQ(Token_Plus, LexToken("+"));
  EXPECT_EQ(Token_Minus, LexToken("-"));
  EXPECT_EQ(Token_Star, LexToken("*"));
  EXPECT_EQ(Token_Slash, LexToken("/"));
  EXPECT_EQ(Token_Ampersand, LexToken("&"));
  EXPECT_EQ(Token_Percent, LexToken("%"));
  EXPECT_EQ(Token_Bar, LexToken("|"));
  EXPECT_EQ(Token_Caret, LexToken("^"));
  EXPECT_EQ(Token_Tilde, LexToken("~"));
  EXPECT_EQ(Token_Exclam, LexToken("!"));
  EXPECT_EQ(Token_QMark, LexToken("?"));
  EXPECT_EQ(Token_Increment, LexToken("++"));
  EXPECT_EQ(Token_Decrement, LexToken("--"));
  EXPECT_EQ(Token_DoubleAmp, LexToken("&&"));
  EXPECT_EQ(Token_DoubleBar, LexToken("||"));
  EXPECT_EQ(Token_DoubleColon, LexToken("::"));

  // Relational operators
  EXPECT_EQ(Token_Less, LexToken("<"));
  EXPECT_EQ(Token_Greater, LexToken(">"));
  EXPECT_EQ(Token_LessEqual, LexToken("<="));
  EXPECT_EQ(Token_GreaterEqual, LexToken(">="));
  EXPECT_EQ(Token_PossLess, LexToken("<?"));
  EXPECT_EQ(Token_PossGreater, LexToken(">?"));
  EXPECT_EQ(Token_PossLessEqual, LexToken("<=?"));
  EXPECT_EQ(Token_PossGreaterEqual, LexToken(">=?"));
  EXPECT_EQ(Token_Equal, LexToken("=="));
  EXPECT_EQ(Token_NotEqual, LexToken("!="));

  EXPECT_EQ(Token_LShift, LexToken("<<"));
  EXPECT_EQ(Token_RShift, LexToken(">>"));
  //EXPECT_EQ(Token_Scope, LexToken(""));

  // Joiners
  EXPECT_EQ(Token_Dot, LexToken("."));
  EXPECT_EQ(Token_Range, LexToken(".."));
  EXPECT_EQ(Token_Ellipsis, LexToken("..."));

  // Operator keywords
  EXPECT_EQ(Token_LogicalAnd, LexToken("and"));
  EXPECT_EQ(Token_LogicalOr, LexToken("or"));
  EXPECT_EQ(Token_LogicalNot, LexToken("not"));
  EXPECT_EQ(Token_As, LexToken("as"));
  EXPECT_EQ(Token_Is, LexToken("is"));
  EXPECT_EQ(Token_In, LexToken("in"));

  // Access Keywords
  EXPECT_EQ(Token_Public, LexToken("public"));
  EXPECT_EQ(Token_Private, LexToken("private"));
  EXPECT_EQ(Token_Protected, LexToken("protected"));

  // Primtypes
  EXPECT_EQ(Token_BoolType, LexToken("bool"));
  EXPECT_EQ(Token_CharType, LexToken("char"));
  EXPECT_EQ(Token_ByteType, LexToken("byte"));
  EXPECT_EQ(Token_IntType, LexToken("int"));
  EXPECT_EQ(Token_ShortType, LexToken("short"));
  EXPECT_EQ(Token_LongType, LexToken("long"));
  EXPECT_EQ(Token_UByteType, LexToken("ubyte"));
  EXPECT_EQ(Token_UIntType, LexToken("uint"));
  EXPECT_EQ(Token_UShortType, LexToken("ushort"));
  EXPECT_EQ(Token_ULongType, LexToken("ulong"));
  EXPECT_EQ(Token_FloatType, LexToken("float"));
  EXPECT_EQ(Token_DoubleType, LexToken("double"));

  // Metatypes
  EXPECT_EQ(Token_Class, LexToken("class"));
  EXPECT_EQ(Token_Struct, LexToken("struct"));
  EXPECT_EQ(Token_Enum, LexToken("enum"));
  EXPECT_EQ(Token_Var, LexToken("var"));
  EXPECT_EQ(Token_Let, LexToken("let"));
  EXPECT_EQ(Token_Def, LexToken("def"));

  EXPECT_EQ(Token_Module, LexToken("module"));
  EXPECT_EQ(Token_Import, LexToken("import"));

  // Statement keywords
  EXPECT_EQ(Token_If, LexToken("if"));
  EXPECT_EQ(Token_Else, LexToken("else"));
  EXPECT_EQ(Token_Repeat, LexToken("repeat"));
  EXPECT_EQ(Token_For, LexToken("for"));
  EXPECT_EQ(Token_While, LexToken("while"));
  EXPECT_EQ(Token_Return, LexToken("return"));

  EXPECT_EQ(Token_Try, LexToken("try"));
  EXPECT_EQ(Token_Catch, LexToken("catch"));
  EXPECT_EQ(Token_Finally, LexToken("finally"));
  EXPECT_EQ(Token_Switch, LexToken("switch"));
  EXPECT_EQ(Token_Classify, LexToken("classify"));

  // String literals
  EXPECT_EQ(Token_String, LexToken("\"\""));
  EXPECT_EQ(Token_Char, LexToken("'a'"));

  // Erroneous tokens
  EXPECT_EQ(Token_Error, LexTokenError("#"));
}

TEST_F(LexerTest, StringLiterals) {

  {
    FakeSourceFile  src("\"\"");
    Lexer           lex(&src);

    EXPECT_EQ(Token_String, lex.next());
    EXPECT_EQ((size_t)0, lex.tokenValue().length());
  }

  {
    std::string     expected("abc\n\r$");
    FakeSourceFile  src("\"abc\\n\\r\\$\"");
    Lexer           lex(&src);

    EXPECT_EQ(Token_String, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }

  {
    std::string     expected("\x01\xAA\xBB");
    FakeSourceFile  src("\"\\x01\\xAA\\xBB\"");
    Lexer           lex(&src);

    EXPECT_EQ(Token_String, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }

  {
    std::string     expected("\x01\u00AA\u00BB");
    FakeSourceFile  src("\"\\x01\\uAA\\uBB\"");
    Lexer           lex(&src);

    EXPECT_EQ(Token_String, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }

  {
    std::string     expected("\u2100");
    FakeSourceFile  src("\"\\u2100\"");
    Lexer           lex(&src);

    EXPECT_EQ(Token_String, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }

  {
    std::string     expected("\U00012100");
    FakeSourceFile  src("\"\\U00012100\"");
    Lexer           lex(&src);

    EXPECT_EQ(Token_String, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }
}

TEST_F(LexerTest, CharLiterals) {

  {
    FakeSourceFile  src("\'a\'");
    Lexer           lex(&src);

    EXPECT_EQ(Token_Char, lex.next());
    EXPECT_EQ((size_t)1, lex.tokenValue().length());
  }

  {
    std::string     expected("\x01");
    FakeSourceFile  src("'\\x01'");
    Lexer           lex(&src);

    EXPECT_EQ(Token_Char, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }

  {
    std::string     expected("\xAA");
    FakeSourceFile  src("'\\xAA'");
    Lexer           lex(&src);

    EXPECT_EQ(Token_Char, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }

  {
    std::string     expected("000000aa");
    FakeSourceFile  src("'\\uAA'");
    Lexer           lex(&src);

    EXPECT_EQ(Token_Char, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }

  {
    std::string     expected("00002100");
    FakeSourceFile  src("'\\u2100\'");
    Lexer           lex(&src);

    EXPECT_EQ(Token_Char, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }

  {
    std::string     expected("00012100");
    FakeSourceFile  src("'\\U00012100'");
    Lexer           lex(&src);

    EXPECT_EQ(Token_Char, lex.next());
    EXPECT_EQ(expected, lex.tokenValue());
  }
}

TEST_F(LexerTest, Comments) {

  // Comments
  EXPECT_EQ(Token_End, LexToken("/* comment */"));
  EXPECT_EQ(Token_End, LexToken(" /* comment */ "));
  EXPECT_EQ(Token_End, LexToken("//"));
  EXPECT_EQ(Token_End, LexToken("// comment\n"));
  EXPECT_EQ(Token_End, LexToken(" //\n "));
  EXPECT_EQ(Token_Integer, LexToken("  /* comment */10/* comment */ "));
  EXPECT_EQ(Token_Integer, LexToken("  /* comment */ 10 /* comment */ "));
  EXPECT_EQ(Token_Integer, LexToken("  /// comment\n 10 // comment\n "));
  EXPECT_EQ(Token_Integer, LexToken("  /// comment\n10// comment\n "));

  // Unterminated comment
  EXPECT_EQ(Token_Error, LexTokenError("/* comment"));
}

TEST_F(LexerTest, Location) {

  FakeSourceFile  src("\n\n   aaaaa    ");
  Lexer           lex(&src);

  EXPECT_EQ(Token_Ident, lex.next());

  EXPECT_EQ(5u, lex.tokenLocation().begin);
  EXPECT_EQ(10u, lex.tokenLocation().end);

  TokenPosition tl = src.tokenPosition(lex.tokenLocation());
  EXPECT_EQ(3u, tl.beginLine);
  EXPECT_EQ(3u, tl.beginCol);
  EXPECT_EQ(3u, tl.endLine);
  EXPECT_EQ(8u, tl.endCol);

  std::string line;
  EXPECT_TRUE(src.readLineAt(2, line));
  EXPECT_EQ("   aaaaa    ", line);
}

#if 0
TEST_F(LexerTest, RealFile) {
    using namespace tart;

    SourceFile      src("out/Debug/test/unit/lexer/lexer01.tart");
    //SourceFile      src("lexer/lexer01.tart");
    Lexer           lex(&src);

    EXPECT_EQ(true, src.isValid());
    EXPECT_EQ(Token_Ident, lex.next());
    EXPECT_EQ(Token_Integer, lex.next());
    EXPECT_EQ(Token_Float, lex.next());
    EXPECT_EQ(Token_Dot, lex.next());
    EXPECT_EQ(Token_Range, lex.next());
    EXPECT_EQ(Token_Ellipsis, lex.next());
    EXPECT_EQ(Token_End, lex.next());
}
#endif

}  // namespace

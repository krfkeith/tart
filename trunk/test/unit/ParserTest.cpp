/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/AST/ASTNode.h"
#include "tart/AST/Stmt.h"
#include "tart/Parse/Parser.h"
#include "tart/Common/InternedString.h"
#include "tart/Common/Diagnostics.h"
#include "FakeSourceFile.h"
#include "TestHelpers.h"

namespace {
using namespace tart;

class ParserTest : public testing::Test {
protected:
  ParserTest() {}

  virtual void SetUp() {}
  virtual void TearDown() {}

  template <class T>
  T * parse(T * (Parser::*parseFunc)(), const char * srctext,
      int expectedErrors) {
    if (expectedErrors != 0) {
      diag.setMinSeverity(Diagnostics::Off);
    }

    FakeSourceFile  src(srctext);
    Parser parser(&src, NULL);
    T * result = (parser.*parseFunc)();
    if (!expectedErrors) {
      EXPECT_TRUE(result != NULL) << "[src = " << srctext << "]";
    }

    if (expectedErrors != diag.getErrorCount()) {
      EXPECT_EQ(expectedErrors, diag.getErrorCount()) << "[src = " << srctext << "]";
    }

    diag.reset();
    diag.setMinSeverity(Diagnostics::Debug);
    return result;
  }

  ASTNode * parseExpression(const char * srctext, int expectedErrors = 0) {
    return parse(&Parser::expression, srctext, expectedErrors);
  }

  ASTNode * parseType(const char * srctext, int expectedErrors = 0) {
    return parse(&Parser::typeExpression, srctext, expectedErrors);
  }

  Stmt * parseStatement(const char * srctext, int expectedErrors = 0) {
    return parse(&Parser::statement, srctext, expectedErrors);
  }

  ASTDecl * parseDeclaration(const char * srctext, int expectedErrors = 0) {
    return parse(&Parser::declaration, srctext, expectedErrors);
  }
};

TEST_F(ParserTest, InternStrings) {
  // Test that two iterned strings have the same pointer value.
  const char * s0 = istrings.intern("foo");
  const char * s1 = istrings.intern("foo");
  ASSERT_EQ(s0, s1);
}

TEST_F(ParserTest, Types) {

  ASTNode * ast;

  // Parsing tests for type names.

  ast = parseType("X");
  ASSERT_EQ(ASTNode::Id, ast->nodeType());
  EXPECT_AST_EQ("X", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(1u, ast->location().end);

  ast = parseType("(X)");
  ASSERT_EQ(ASTNode::Id, ast->nodeType());
  EXPECT_AST_EQ("X", ast);
  EXPECT_EQ(1u, ast->location().begin);
  EXPECT_EQ(2u, ast->location().end);

  ast = parseType("(", 1);
  ASSERT_EQ(NULL, ast);

  ast = parseType("X[]");
  ASSERT_EQ(ASTNode::Array, ast->nodeType());
  EXPECT_AST_EQ("Array(X)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  //EXPECT_EQ(3, ast->location().end);

  ast = parseType("X[", 1);
  ASSERT_EQ(NULL, ast);

  ast = parseType("X.Y");
  ASSERT_EQ(ASTNode::Member, ast->nodeType());
  EXPECT_AST_EQ("X.Y", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseType("bool");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("bool", ast);

  ast = parseType("char");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("char", ast);

  ast = parseType("int8");
  //ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("int8", ast);

  ast = parseType("int32");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("int32", ast);

  ast = parseType("int64");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("int64", ast);

  ast = parseType("uint32");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("uint32", ast);

  ast = parseType("float");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("float", ast);

  ast = parseType("void");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("void", ast);

  // Function type
  ast = parseType("fn :int32 -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (:int32) -> int32", ast);

  // Function type
  ast = parseType("fn (x) -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (x) -> int32", ast);

  ast = parseType("fn (:int32) -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (:int32) -> int32", ast);

  ast = parseType("fn (x:int32) -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (x:int32) -> int32", ast);

  ast = parseType("fn (xx:int32, y:int32) -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (xx:int32, y:int32) -> int32", ast);

  ast = parseType("fn (:int32 -> int32", 1);
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  //ASSERT_EQ(NULL, ast);
}

TEST_F(ParserTest, Terminals) {
  ASTNode * ast;

  // Integers
  ast = parseExpression("10");
  ASSERT_EQ(ASTNode::LitInt, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "10", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(2u, ast->location().end);

  ast = parseExpression("0x10");
  ASSERT_EQ(ASTNode::LitInt, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "16", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(4u, ast->location().end);

  ast = parseExpression("0x100000000");
  ASSERT_EQ(ASTNode::LitInt, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "4294967296", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(11u, ast->location().end);

  // Floats
  ast = parseExpression("1.0");
  ASSERT_EQ(ASTNode::LitDouble, ast->nodeType());
  ASSERT_FLOAT_EQ(1.0, dyn_cast<ASTDoubleLiteral>(ast)->value().convertToDouble());
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1.0f");
  ASSERT_EQ(ASTNode::LitFloat, ast->nodeType());
  ASSERT_FLOAT_EQ(1.0, dyn_cast<ASTFloatLiteral>(ast)->value().convertToFloat());

  ast = parseExpression("5.0e3f");
  ASSERT_EQ(ASTNode::LitFloat, ast->nodeType());
  ASSERT_FLOAT_EQ(5.0e3f, dyn_cast<ASTFloatLiteral>(ast)->value().convertToFloat());

  // Character literals
  ast = parseExpression("'c'");
  ASSERT_EQ(ASTNode::LitChar, ast->nodeType());
  ASSERT_EQ(uint32_t('c'), dyn_cast<ASTCharLiteral>(ast)->value());
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  // String literals
  ast = parseExpression("\"c\"");
  ASSERT_EQ(ASTNode::LitString, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "\"c\"", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  // Boolean literals
  ast = parseExpression("true");
  ASSERT_EQ(ASTNode::LitBool, ast->nodeType());
  ASSERT_EQ(true, dyn_cast<ASTBoolLiteral>(ast)->value());
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(4u, ast->location().end);

  ast = parseExpression("false");
  ASSERT_EQ(ASTNode::LitBool, ast->nodeType());
  ASSERT_EQ(false, dyn_cast<ASTBoolLiteral>(ast)->value());

  // ASTIdent
  ast = parseExpression("X");
  ASSERT_EQ(ASTNode::Id, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "X", ast);
}

TEST_F(ParserTest, SimpleExpressions) {
  ASTNode * ast;

  // TODO: Update these tests to conform to new AST style.
  ast = parseExpression("1+1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixAdd(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1-1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixSubtract(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1*1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixMultiply(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1/1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixDivide(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1%1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixModulus(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1&1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixBitAnd(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1|1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixBitOr(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1^1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixBitXor(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(3u, ast->location().end);

  ast = parseExpression("1 and 1");
  ASSERT_EQ(ASTNode::LogicalAnd, ast->nodeType());
  EXPECT_AST_EQ("LogicalAnd(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(7u, ast->location().end);

  ast = parseExpression("1 or 1");
  ASSERT_EQ(ASTNode::LogicalOr, ast->nodeType());
  EXPECT_AST_EQ("LogicalOr(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(6u, ast->location().end);

  ast = parseExpression("1 << 1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixLShift(1, 1)", ast);

  ast = parseExpression("1 >> 1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixRShift(1, 1)", ast);

  ast = parseExpression("1..1");
  ASSERT_EQ(ASTNode::Range, ast->nodeType());
  EXPECT_AST_EQ("Range(1, 1)", ast);

  ast = parseExpression("1==1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixEQ(1, 1)", ast);

  ast = parseExpression("1!=1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixNE(1, 1)", ast);

  ast = parseExpression("1<1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixLT(1, 1)", ast);

  ast = parseExpression("1>1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixGT(1, 1)", ast);

  ast = parseExpression("1<=1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixLE(1, 1)", ast);

  ast = parseExpression("1>=1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixGE(1, 1)", ast);

  ast = parseExpression("1<?1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixPossiblyLT(1, 1)", ast);

  ast = parseExpression("1>?1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixPossiblyGT(1, 1)", ast);

  ast = parseExpression("1<=?1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixPossiblyLE(1, 1)", ast);

  ast = parseExpression("1>=?1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixPossiblyGE(1, 1)", ast);

#if 0
  ast = parseExpression("1 as Foo");
  ASSERT_EQ(ASTNode::AsType, ast->nodeType());
  EXPECT_AST_EQ("AsType(1, Foo)", ast);
#endif

  ast = parseExpression("1 is 2");
  ASSERT_EQ(ASTNode::Is, ast->nodeType());
  EXPECT_AST_EQ("Is(1, 2)", ast);

  ast = parseExpression("1 is not 2");
  ASSERT_EQ(ASTNode::IsNot, ast->nodeType());
  EXPECT_AST_EQ("IsNot(1, 2)", ast);

  ast = parseExpression("1 in 2");
  ASSERT_EQ(ASTNode::In, ast->nodeType());
  EXPECT_AST_EQ("In(1, 2)", ast);

  ast = parseExpression("1 not in 2");
  ASSERT_EQ(ASTNode::NotIn, ast->nodeType());
  EXPECT_AST_EQ("NotIn(1, 2)", ast);

  ast = parseExpression("X.Y");
  ASSERT_EQ(ASTNode::Member, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "X.Y", ast);
}

TEST_F(ParserTest, ComplexExpressions) {
  ASTNode * ast;

  // TODO: Update these tests to conform to new AST style.
  ast = parseExpression("1+(1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixAdd(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(4u, ast->location().end);

  ast = parseExpression("(1-1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixSubtract(1, 1)", ast);
}

TEST_F(ParserTest, Arguments) {
  ASTNode * ast;

  ast = parseExpression("f()");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("f()", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(2u, ast->location().end);

  ast = parseExpression("f(1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("f(1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(2u, ast->location().end);

  ast = parseExpression("f(1, 1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("f(1, 1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(2u, ast->location().end);

  ast = parseExpression("f(1, n=1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("f(1, n=1)", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(2u, ast->location().end);

  ast = parseExpression("f(1,)", 1);
  ASSERT_EQ(NULL, ast);

  ast = parseExpression("f(a=1, 1)", 1);
  ASSERT_EQ(NULL, ast);
}

TEST_F(ParserTest, Statements) {
  Stmt * ast;

  ast = parseStatement("f();");
  ASSERT_EQ(Stmt::Expression, ast->nodeType());
  EXPECT_AST_EQ("f();", ast);
  EXPECT_EQ(0u, ast->location().begin);
  EXPECT_EQ(4u, ast->location().end);

  ast = parseStatement("a = b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("Assign(a, b);", ast);

  ast = parseStatement("return b;");
  ASSERT_EQ(ASTNode::Return, ast->nodeType());
  EXPECT_AST_EQ("return b;", ast);

  ast = parseStatement("return b if c;");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (c, return b;)", ast);

  ast = parseStatement("yield b;");
  ASSERT_EQ(ASTNode::Yield, ast->nodeType());
  EXPECT_AST_EQ("yield b;", ast);

  ast = parseStatement("throw b;");
  ASSERT_EQ(ASTNode::Throw, ast->nodeType());
  EXPECT_AST_EQ("throw b;", ast);

  ast = parseStatement("break;");
  ASSERT_EQ(ASTNode::Break, ast->nodeType());
  EXPECT_AST_EQ("break;", ast);

  ast = parseStatement("continue;");
  ASSERT_EQ(ASTNode::Continue, ast->nodeType());
  EXPECT_AST_EQ("continue;", ast);

  ast = parseStatement("continue if a;");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (a, continue;)", ast);

  ast = parseStatement("if a {}");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (a, {})", ast);

  //ast = parseStatement("if (a) b; else c;");
  //ASSERT_EQ(ASTNode::If, ast->nodeType());
  //EXPECT_AST_EQ("If (a, b;, c;)", ast);

  ast = parseStatement("if a {} else {}");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (a, {}, {})", ast);

  ast = parseStatement("if (a) {} else {}");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (a, {}, {})", ast);

  ast = parseStatement("while a {}");
  ASSERT_EQ(ASTNode::While, ast->nodeType());
  EXPECT_AST_EQ("While (a, {})", ast);

  ast = parseStatement("while (a) {}");
  ASSERT_EQ(ASTNode::While, ast->nodeType());
  EXPECT_AST_EQ("While (a, {})", ast);

  ast = parseStatement("while let a = b {}");
  ASSERT_EQ(ASTNode::While, ast->nodeType());
  EXPECT_AST_EQ("While (let a = b, {})", ast);

  ast = parseStatement("while let a, a = b {}");
  ASSERT_EQ(ASTNode::While, ast->nodeType());
  EXPECT_AST_EQ("While (varlist vlist(let a, let a) = b, {})", ast);

  ast = parseStatement("for a; b; c {}");
  ASSERT_EQ(ASTNode::For, ast->nodeType());
  EXPECT_AST_EQ("For (var a; b; c; {})", ast);

  //ast = parseStatement("for (a, b = 1; b; c) {}");
  //ASSERT_EQ(ASTNode::For, ast->nodeType());
  //EXPECT_AST_EQ("For (Assign(Tuple(var a, var b), 1); b; c; {})", ast);

  ast = parseStatement("for a in b {}");
  ASSERT_EQ(ASTNode::ForEach, ast->nodeType());
  EXPECT_AST_EQ("ForEach (var a; b; {})", ast);

  ast = parseStatement("try {} catch e:Ex {} else {} finally {}");
  ASSERT_EQ(ASTNode::Try, ast->nodeType());
  EXPECT_AST_EQ("Try ({} Catch (let e:Ex; {}); Else {} Finally {})", ast);

  ast = parseStatement("switch n { case 1 {} case 2 {} else {}}");
  ASSERT_TRUE(ast != NULL);
  ASSERT_EQ(ASTNode::Switch, ast->nodeType());
  ASSERT_AST_EQ("Switch (n,  case 1 {{}}; case 2 {{}}; {};)", ast);

  ast = parseStatement("classify n { as e:T {} as e:T {} else {}}");
  ASSERT_TRUE(ast != NULL);
  ASSERT_EQ(ASTNode::Classify, ast->nodeType());
  ASSERT_AST_EQ("Classify (n,  as (let e:T, {}); as (let e:T, {}); {};)", ast);
}

TEST_F(ParserTest, Decls) {
  ASTNode * ast;

  // Parsing tests for declarations.
  ast = parseDeclaration("namespace X {}");
  ASSERT_EQ(ASTDecl::Namespace, ast->nodeType());
  EXPECT_AST_EQ("namespace X", ast);

  ast = parseDeclaration("class X {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_AST_EQ("class X", ast);

  ast = parseDeclaration("struct X {}");
  ASSERT_EQ(ASTDecl::Struct, ast->nodeType());
  EXPECT_AST_EQ("struct X", ast);

  ast = parseDeclaration("interface X {}");
  ASSERT_EQ(ASTDecl::Interface, ast->nodeType());
  EXPECT_AST_EQ("interface X", ast);

  ast = parseDeclaration("enum X {}");
  ASSERT_EQ(ASTDecl::Enum, ast->nodeType());
  EXPECT_AST_EQ("enum X", ast);

  ast = parseDeclaration("let X:int32;");
  ASSERT_EQ(ASTNode::Let, ast->nodeType());
  EXPECT_AST_EQ("let X:int32", ast);

  ast = parseDeclaration("var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_AST_EQ("var X:int32", ast);

  ast = parseDeclaration("def X:int32 {}");
  ASSERT_EQ(ASTNode::Prop, ast->nodeType());
  EXPECT_AST_EQ("def X:int32", ast);

  ast = parseDeclaration("def [x:int32]:int32;");
  ASSERT_EQ(ASTNode::Idx, ast->nodeType());
  EXPECT_AST_EQ("def $index(x:int32):int32", ast);

  ast = parseDeclaration("def X();");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X ()", ast);

  ast = parseDeclaration("def X(x:int32);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32)", ast);

  ast = parseDeclaration("def X(x:int32 = 3);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32 = 3)", ast);

  ast = parseDeclaration("def X(x:int32, y:int32);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32, y:int32)", ast);

  ast = parseDeclaration("def X(x:int32, y:int32) -> int32;");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32, y:int32) -> int32", ast);

  ast = parseDeclaration("def X[%T](x:int32, y:int32) -> int32;");
  ASSERT_EQ(ASTNode::Template, ast->nodeType());
  EXPECT_AST_EQ("[%T] def X (x:int32, y:int32) -> int32", ast);

  ast = parseDeclaration("class X[%T] {}");
  ASSERT_EQ(ASTDecl::Template, ast->nodeType());
  EXPECT_AST_EQ("[%T] class X", ast);
}

}  // namespace

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/AST/ASTNode.h"
#include "tart/AST/Stmt.h"
#include "tart/Defn/Module.h"
#include "tart/Parse/Parser.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Meta/ASTReader.h"
#include "tart/Meta/ASTWriter.h"
#include "FakeSourceFile.h"
#include "TestHelpers.h"

namespace {
using namespace tart;

class ASTSerializationTest : public testing::Test {
protected:
  ASTSerializationTest() : testModule(new Module("test", NULL)) {}

  Module * testModule;

  // Function which parses, serializes, deserializes, and then returns an ASTNode.
  template <class T>
  T * parse(T * (Parser::*parseFunc)(), const char * srctext) {
    diag.reset();
    FakeSourceFile  src(srctext);
    Parser parser(&src, testModule);
    ASTNode * parserOutput = (parser.*parseFunc)();
    EXPECT_TRUE(parserOutput != NULL) << "[src = " << srctext << "]";
    EXPECT_FALSE(parserOutput->isInvalid()) << "[src = " << srctext << "]";
    ASTWriter writer;
    writer.write(parserOutput);
    ASTReader reader(SourceLocation(), writer.str());
    ASTNode * readerOutput = reader.read();
    return cast_or_null<T>(readerOutput);
  }

  ASTNode * testExpression(const char * srctext) {
    return parse(&Parser::expression, srctext);
  }

  ASTNode * testType(const char * srctext) {
    return parse(&Parser::typeExpression, srctext);
  }

  Stmt * testStatement(const char * srctext) {
    return parse(&Parser::statement, srctext);
  }

  ASTDecl * testDeclaration(const char * srctext) {
    return parse(&Parser::declaration, srctext);
  }
};

TEST_F(ASTSerializationTest, Types) {

  ASTNode * ast;

  // Parsing tests for type names.

  ast = testType("X");
  ASSERT_EQ(ASTNode::Id, ast->nodeType());
  EXPECT_AST_EQ("X", ast);

  ast = testType("(X)");
  ASSERT_EQ(ASTNode::Id, ast->nodeType());
  EXPECT_AST_EQ("X", ast);

  ast = testType("X[]");
  ASSERT_EQ(ASTNode::Array, ast->nodeType());
  EXPECT_AST_EQ("Array(X)", ast);

  ast = testType("X.Y");
  ASSERT_EQ(ASTNode::Member, ast->nodeType());
  EXPECT_AST_EQ("X.Y", ast);

  ast = testType("bool");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("bool", ast);

  ast = testType("char");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("char", ast);

  ast = testType("int8");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("int8", ast);

  ast = testType("int16");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("int16", ast);

  ast = testType("int32");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("int32", ast);

  ast = testType("int64");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("int64", ast);

  ast = testType("uint8");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("uint8", ast);

  ast = testType("uint16");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("uint16", ast);

  ast = testType("uint32");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("uint32", ast);

  ast = testType("uint64");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("uint64", ast);

  ast = testType("float");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("float", ast);

  ast = testType("void");
  ASSERT_EQ(ASTNode::BuiltIn, ast->nodeType());
  EXPECT_AST_EQ("void", ast);

  // Derived types
  ast = testType("(int32, int32)");
  ASSERT_EQ(ASTNode::Tuple, ast->nodeType());
  EXPECT_AST_EQ("Tuple(int32, int32)", ast);

  ast = testType("String or int32");
  ASSERT_EQ(ASTNode::LogicalOr, ast->nodeType());
  EXPECT_AST_EQ("LogicalOr(String, int32)", ast);

  // Function type
  ast = testType("fn :int32 -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (:int32) -> int32", ast);

  ast = testType("fn (x) -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (x) -> int32", ast);

  ast = testType("fn (:int32) -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (:int32) -> int32", ast);

  ast = testType("fn (x:int32) -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (x:int32) -> int32", ast);

  ast = testType("fn (xx:int32, y:int32) -> int32");
  ASSERT_EQ(ASTNode::AnonFn, ast->nodeType());
  EXPECT_AST_EQ("fn (xx:int32, y:int32) -> int32", ast);
}

TEST_F(ASTSerializationTest, Terminals) {
  ASTNode * ast;

  // Integers
  ast = testExpression("10");
  ASSERT_EQ(ASTNode::LitInt, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "10", ast);

  ast = testExpression("0x10");
  ASSERT_EQ(ASTNode::LitInt, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "16", ast);

  ast = testExpression("0x100000000");
  ASSERT_EQ(ASTNode::LitInt, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "4294967296", ast);

  // Floats
#if 0
  ast = testExpression("1.0");
  ASSERT_EQ(ASTNode::LitDouble, ast->nodeType());
  ASSERT_FLOAT_EQ(1.0, dyn_cast<ASTDoubleLiteral>(ast)->value().convertToDouble());

  ast = testExpression("1.0f");
  ASSERT_EQ(ASTNode::LitFloat, ast->nodeType());
  ASSERT_FLOAT_EQ(1.0, dyn_cast<ASTFloatLiteral>(ast)->value().convertToFloat());

  ast = testExpression("5.0e3f");
  ASSERT_EQ(ASTNode::LitFloat, ast->nodeType());
  ASSERT_FLOAT_EQ(5.0e3f, dyn_cast<ASTFloatLiteral>(ast)->value().convertToFloat());
#endif

  // Character literals
  ast = testExpression("'c'");
  ASSERT_EQ(ASTNode::LitChar, ast->nodeType());
  ASSERT_EQ(uint32_t('c'), dyn_cast<ASTCharLiteral>(ast)->value());

  // String literals
  ast = testExpression("\"c\"");
  ASSERT_EQ(ASTNode::LitString, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "\"c\"", ast);

  // Boolean literals
  ast = testExpression("true");
  ASSERT_EQ(ASTNode::LitBool, ast->nodeType());
  ASSERT_EQ(true, dyn_cast<ASTBoolLiteral>(ast)->value());

  ast = testExpression("false");
  ASSERT_EQ(ASTNode::LitBool, ast->nodeType());
  ASSERT_FALSE(dyn_cast<ASTBoolLiteral>(ast)->value());

  // ASTIdent
  ast = testExpression("X");
  ASSERT_EQ(ASTNode::Id, ast->nodeType());
  ASSERT_PRED2(ASTCmp, "X", ast);

  // Other terminals
  ast = testExpression("null");
  ASSERT_EQ(ASTNode::Null, ast->nodeType());
  EXPECT_AST_EQ("Null", ast);

  ast = testExpression("super");
  ASSERT_EQ(ASTNode::Super, ast->nodeType());
  EXPECT_AST_EQ("Super()", ast);
}

TEST_F(ASTSerializationTest, SimpleExpressions) {
  ASTNode * ast;

  // TODO: Update these tests to conform to new AST style.
  ast = testExpression("1+1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixAdd(1, 1)", ast);

  ast = testExpression("1-1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixSubtract(1, 1)", ast);

  ast = testExpression("1*1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixMultiply(1, 1)", ast);

  ast = testExpression("1/1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixDivide(1, 1)", ast);

  ast = testExpression("1%1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixModulus(1, 1)", ast);

  ast = testExpression("1&1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixBitAnd(1, 1)", ast);

  ast = testExpression("1|1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixBitOr(1, 1)", ast);

  ast = testExpression("1^1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixBitXor(1, 1)", ast);

  ast = testExpression("1 and 1");
  ASSERT_EQ(ASTNode::LogicalAnd, ast->nodeType());
  EXPECT_AST_EQ("LogicalAnd(1, 1)", ast);

  ast = testExpression("1 or 1");
  ASSERT_EQ(ASTNode::LogicalOr, ast->nodeType());
  EXPECT_AST_EQ("LogicalOr(1, 1)", ast);

  ast = testExpression("1 << 1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixLShift(1, 1)", ast);

  ast = testExpression("1 >> 1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixRShift(1, 1)", ast);

#if 0
  ast = testExpression("1..1");
  ASSERT_EQ(ASTNode::Range, ast->nodeType());
  EXPECT_AST_EQ("Range(1, 1)", ast);
#endif

  ast = testExpression("1==1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixEqual(1, 1)", ast);

  ast = testExpression("1!=1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixNotEqual(1, 1)", ast);

  ast = testExpression("1<1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixLT(1, 1)", ast);

  ast = testExpression("1>1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixGT(1, 1)", ast);

  ast = testExpression("1<=1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixLE(1, 1)", ast);

  ast = testExpression("1>=1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixGE(1, 1)", ast);

  ast = testExpression("1<?1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixPossiblyLT(1, 1)", ast);

  ast = testExpression("1>?1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixPossiblyGT(1, 1)", ast);

  ast = testExpression("1<=?1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixPossiblyLE(1, 1)", ast);

  ast = testExpression("1>=?1");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixPossiblyGE(1, 1)", ast);

#if 0
  ast = testExpression("1 as Foo");
  ASSERT_EQ(ASTNode::AsType, ast->nodeType());
  EXPECT_AST_EQ("AsType(1, Foo)", ast);
#endif

  ast = testExpression("1 is 2");
  ASSERT_EQ(ASTNode::Is, ast->nodeType());
  EXPECT_AST_EQ("Is(1, 2)", ast);

  ast = testExpression("1 is not 2");
  ASSERT_EQ(ASTNode::IsNot, ast->nodeType());
  EXPECT_AST_EQ("IsNot(1, 2)", ast);

  ast = testExpression("1 in 2");
  ASSERT_EQ(ASTNode::In, ast->nodeType());
  EXPECT_AST_EQ("In(1, 2)", ast);

  ast = testExpression("1 not in 2");
  ASSERT_EQ(ASTNode::NotIn, ast->nodeType());
  EXPECT_AST_EQ("NotIn(1, 2)", ast);

  ast = testExpression("1 isa 2");
  ASSERT_EQ(ASTNode::IsInstanceOf, ast->nodeType());
  EXPECT_AST_EQ("IsInstanceOf(1, 2)", ast);

  ast = testExpression("X.Y");
  ASSERT_EQ(ASTNode::Member, ast->nodeType());
  EXPECT_PRED2(ASTCmp, "X.Y", ast);

  ast = testExpression("a[1]");
  ASSERT_EQ(ASTNode::GetElement, ast->nodeType());
  EXPECT_AST_EQ("GetElement(a, 1)", ast);

#if 0
  // TODO: Move lowering to infixSubtract out of parser.
  // Its parsed as a call to infixSubtract, but that's jumping the gun.
  ast = testExpression("-Y");
  ASSERT_EQ(ASTNode::Negate, ast->nodeType());
  EXPECT_AST_EQ("Negate(Y)", ast);
#endif

  ast = testExpression("not Y");
  ASSERT_EQ(ASTNode::LogicalNot, ast->nodeType());
  EXPECT_AST_EQ("LogicalNot(Y)", ast);

  ast = testExpression("~Y");
  ASSERT_EQ(ASTNode::Complement, ast->nodeType());
  EXPECT_AST_EQ("Complement(Y)", ast);
}

TEST_F(ASTSerializationTest, ComplexExpressions) {
  ASTNode * ast;

  // TODO: Update these tests to conform to new AST style.
  ast = testExpression("1+(1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixAdd(1, 1)", ast);

  ast = testExpression("(1-1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("infixSubtract(1, 1)", ast);

  ast = testExpression("[1, 2, 3]");
  ASSERT_EQ(ASTNode::ArrayLiteral, ast->nodeType());
  EXPECT_AST_EQ("ArrayLiteral(1, 2, 3)", ast);
}

TEST_F(ASTSerializationTest, Arguments) {
  ASTNode * ast;

  ast = testExpression("f()");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("f()", ast);

  ast = testExpression("f(1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("f(1)", ast);

  ast = testExpression("f(1, 1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("f(1, 1)", ast);

  ast = testExpression("f(1, n=1)");
  ASSERT_EQ(ASTNode::Call, ast->nodeType());
  EXPECT_AST_EQ("f(1, n=1)", ast);
}

TEST_F(ASTSerializationTest, Statements) {
  Stmt * ast;

  ast = testStatement("f();");
  ASSERT_EQ(Stmt::Expression, ast->nodeType());
  EXPECT_AST_EQ("f();", ast);

  ast = testStatement("a = b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("Assign(a, b);", ast);

  ast = testStatement("a += b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignAdd(a, b);", ast);

  ast = testStatement("a -= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignSub(a, b);", ast);

  ast = testStatement("a *= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignMul(a, b);", ast);

  ast = testStatement("a /= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignDiv(a, b);", ast);

  ast = testStatement("a %= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignMod(a, b);", ast);

  ast = testStatement("a &= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignBitAnd(a, b);", ast);

  ast = testStatement("a |= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignBitOr(a, b);", ast);

  ast = testStatement("a ^= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignBitXor(a, b);", ast);

  ast = testStatement("a >>= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignRSh(a, b);", ast);

  ast = testStatement("a <<= b;");
  ASSERT_EQ(ASTNode::Expression, ast->nodeType());
  EXPECT_AST_EQ("AssignLSh(a, b);", ast);

  ast = testStatement("return b;");
  ASSERT_EQ(ASTNode::Return, ast->nodeType());
  EXPECT_AST_EQ("return b;", ast);

  ast = testStatement("return;");
  ASSERT_EQ(ASTNode::Return, ast->nodeType());
  EXPECT_AST_EQ("return;", ast);

  ast = testStatement("yield b;");
  ASSERT_EQ(ASTNode::Yield, ast->nodeType());
  EXPECT_AST_EQ("yield b;", ast);

  ast = testStatement("throw b;");
  ASSERT_EQ(ASTNode::Throw, ast->nodeType());
  EXPECT_AST_EQ("throw b;", ast);

  ast = testStatement("break;");
  ASSERT_EQ(ASTNode::Break, ast->nodeType());
  EXPECT_AST_EQ("break;", ast);

  ast = testStatement("continue;");
  ASSERT_EQ(ASTNode::Continue, ast->nodeType());
  EXPECT_AST_EQ("continue;", ast);

  ast = testStatement("continue if a;");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (a, continue;)", ast);

  ast = testStatement("if a {}");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (a, {})", ast);

  ast = testStatement("if a {} else {}");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (a, {}, {})", ast);

  ast = testStatement("if (a) {} else {}");
  ASSERT_EQ(ASTNode::If, ast->nodeType());
  EXPECT_AST_EQ("If (a, {}, {})", ast);

  ast = testStatement("while a {}");
  ASSERT_EQ(ASTNode::While, ast->nodeType());
  EXPECT_AST_EQ("While (a, {})", ast);

  ast = testStatement("while (a) {}");
  ASSERT_EQ(ASTNode::While, ast->nodeType());
  EXPECT_AST_EQ("While (a, {})", ast);

  ast = testStatement("while let a = b {}");
  ASSERT_EQ(ASTNode::While, ast->nodeType());
  EXPECT_AST_EQ("While (let a = b, {})", ast);

  ast = testStatement("while let a, a = b {}");
  ASSERT_EQ(ASTNode::While, ast->nodeType());
  EXPECT_AST_EQ("While (varlist vlist(let a, let a) = b, {})", ast);

  ast = testStatement("do {} while a;");
  ASSERT_EQ(ASTNode::DoWhile, ast->nodeType());
  EXPECT_AST_EQ("Do {} While (a)", ast);

  ast = testStatement("for a; b; c {}");
  ASSERT_EQ(ASTNode::For, ast->nodeType());
  EXPECT_AST_EQ("For (var a; b; c; {})", ast);

#if 0
  ast = testStatement("for (a, b = 1; b; c) {}");
  ASSERT_EQ(ASTNode::For, ast->nodeType());
  EXPECT_AST_EQ("For (Assign(Tuple(var a, var b), 1); b; c; {})", ast);
#endif

  ast = testStatement("for a in b {}");
  ASSERT_EQ(ASTNode::ForEach, ast->nodeType());
  EXPECT_AST_EQ("ForEach (var a; b; {})", ast);

  ast = testStatement("try {} catch e:Ex {} else {} finally {}");
  ASSERT_EQ(ASTNode::Try, ast->nodeType());
  EXPECT_AST_EQ("Try ({} Catch (let e:Ex; {}); Else {} Finally {})", ast);

  ast = testStatement("switch n { case 1 {} case 2 {} case * {}}");
  ASSERT_TRUE(ast != NULL);
  ASSERT_EQ(ASTNode::Switch, ast->nodeType());
  ASSERT_AST_EQ("Switch (n,  case 1 {{}}; case 2 {{}}; {};)", ast);

  ast = testStatement("match n { as e:T {} as e:T {} else {}}");
  ASSERT_TRUE(ast != NULL);
  ASSERT_EQ(ASTNode::Match, ast->nodeType());
  ASSERT_AST_EQ("Match (n,  as (let e:T, {}); as (let e:T, {}); {};)", ast);

  ast = testStatement("let x = 0;");
  ASSERT_EQ(Stmt::LocalDecl, ast->nodeType());
  EXPECT_AST_EQ("let x = 0;", ast);

  ast = testStatement("let x, y = 1, 0;");
  ASSERT_EQ(Stmt::LocalDecl, ast->nodeType());
  EXPECT_EQ(ASTNode::VarList, cast<DeclStmt>(ast)->decl()->nodeType());
}

TEST_F(ASTSerializationTest, Decls) {
  ASTDecl * ast;

  // Parsing tests for declarations.
  ast = testDeclaration("namespace X {}");
  ASSERT_EQ(ASTDecl::Namespace, ast->nodeType());
  EXPECT_AST_EQ("namespace X", ast);

  ast = testDeclaration("class X {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_TRUE(ast->members().empty());
  EXPECT_TRUE(cast<ASTTypeDecl>(ast)->bases().empty());
  EXPECT_AST_EQ("class X", ast);

  ast = testDeclaration("struct X {}");
  ASSERT_EQ(ASTDecl::Struct, ast->nodeType());
  EXPECT_TRUE(ast->members().empty());
  EXPECT_TRUE(cast<ASTTypeDecl>(ast)->bases().empty());
  EXPECT_AST_EQ("struct X", ast);

  ast = testDeclaration("interface X {}");
  ASSERT_EQ(ASTDecl::Interface, ast->nodeType());
  EXPECT_TRUE(ast->members().empty());
  EXPECT_TRUE(cast<ASTTypeDecl>(ast)->bases().empty());
  EXPECT_AST_EQ("interface X", ast);

  ast = testDeclaration("class X : Foo {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_TRUE(ast->members().empty());
  EXPECT_EQ(1u, cast<ASTTypeDecl>(ast)->bases().size());
  EXPECT_EQ(ASTNode::Id, cast<ASTTypeDecl>(ast)->bases()[0]->nodeType());
  EXPECT_AST_EQ("class X", ast);

  ast = testDeclaration("class X : Foo, Bar {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_TRUE(ast->members().empty());
  EXPECT_EQ(2u, cast<ASTTypeDecl>(ast)->bases().size());
  EXPECT_EQ(ASTNode::Id, cast<ASTTypeDecl>(ast)->bases()[0]->nodeType());
  EXPECT_EQ(ASTNode::Id, cast<ASTTypeDecl>(ast)->bases()[1]->nodeType());
  EXPECT_AST_EQ("class X", ast);

  ast = testDeclaration("class X : Foo[Bar] {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_TRUE(ast->members().empty());
  EXPECT_EQ(1u, cast<ASTTypeDecl>(ast)->bases().size());
  EXPECT_EQ(ASTNode::Specialize, cast<ASTTypeDecl>(ast)->bases()[0]->nodeType());
  EXPECT_AST_EQ("class X", ast);

  ast = testDeclaration("class X : Foo[A, B] {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_TRUE(ast->members().empty());
  EXPECT_EQ(1u, cast<ASTTypeDecl>(ast)->bases().size());
  EXPECT_EQ(ASTNode::Specialize, cast<ASTTypeDecl>(ast)->bases()[0]->nodeType());
  EXPECT_EQ(2u, cast<ASTSpecialize>(cast<ASTTypeDecl>(ast)->bases()[0])->args().size());
  EXPECT_AST_EQ("class X", ast);

  ast = testDeclaration("enum X {}");
  ASSERT_EQ(ASTDecl::Enum, ast->nodeType());
  EXPECT_TRUE(ast->members().empty());
  EXPECT_AST_EQ("enum X", ast);

  ast = testDeclaration("let X:int32;");
  ASSERT_EQ(ASTNode::Let, ast->nodeType());
  EXPECT_AST_EQ("let X:int32", ast);

  ast = testDeclaration("var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(NULL, cast<ASTVarDecl>(ast)->value());
  EXPECT_AST_EQ("var X:int32", ast);

  ast = testDeclaration("var X:int32 = A;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(ASTNode::Id, cast<ASTVarDecl>(ast)->value()->nodeType());
  EXPECT_AST_EQ("var X:int32 = A", ast);

  ast = testDeclaration("def X:int32 {}");
  ASSERT_EQ(ASTNode::Prop, ast->nodeType());
  EXPECT_AST_EQ("def X:int32", ast);

  ast = testDeclaration("def X:int32 { get; }");
  ASSERT_EQ(ASTNode::Prop, ast->nodeType());
  EXPECT_TRUE(cast<ASTPropertyDecl>(ast)->getter() != NULL);
  EXPECT_TRUE(cast<ASTPropertyDecl>(ast)->setter() == NULL);
  EXPECT_EQ(0u, cast<ASTPropertyDecl>(ast)->params().size());
  EXPECT_AST_EQ("def X:int32", ast);

  ast = testDeclaration("def [x:int32]:int32;");
  ASSERT_EQ(ASTNode::Idx, ast->nodeType());
  EXPECT_EQ(1u, cast<ASTPropertyDecl>(ast)->params().size());
  EXPECT_AST_EQ("def $index(x:int32):int32", ast);

  ast = testDeclaration("def X();");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_EQ(0u, ast->modifiers().flags);
  EXPECT_AST_EQ("def X ()", ast);

  ast = testDeclaration("def X(x:int32);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32)", ast);

  ast = testDeclaration("def X(x:int32 = 3);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32 = 3)", ast);

  ast = testDeclaration("def X(x:int32, y:int32);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32, y:int32)", ast);

  ast = testDeclaration("def X(x:int32, y:int32) -> int32;");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32, y:int32) -> int32", ast);

  ast = testDeclaration("def X[%T](x:int32, y:int32) -> int32;");
  ASSERT_EQ(ASTNode::Template, ast->nodeType());
  EXPECT_AST_EQ("[%T] def X (x:int32, y:int32) -> int32", ast);

  ast = testDeclaration("class X[%T] {}");
  ASSERT_EQ(ASTDecl::Template, ast->nodeType());
  EXPECT_EQ(ASTNode::TypeVar, cast<ASTTemplate>(ast)->params()[0]->nodeType());
  EXPECT_FALSE(cast<ASTTypeVariable>(cast<ASTTemplate>(ast)->params()[0])->isVariadic());
  EXPECT_EQ(ASTTypeVariable::IS_INSTANCE,
      cast<ASTTypeVariable>(cast<ASTTemplate>(ast)->params()[0])->constraint());
  EXPECT_AST_EQ("[%T] class X", ast);

  ast = testDeclaration("class X[%T...] {}");
  ASSERT_EQ(ASTDecl::Template, ast->nodeType());
  EXPECT_EQ(ASTNode::TypeVar, cast<ASTTemplate>(ast)->params()[0]->nodeType());
  EXPECT_TRUE(cast<ASTTypeVariable>(cast<ASTTemplate>(ast)->params()[0])->isVariadic());
  EXPECT_EQ(ASTTypeVariable::IS_INSTANCE,
      cast<ASTTypeVariable>(cast<ASTTemplate>(ast)->params()[0])->constraint());
  EXPECT_AST_EQ("[%T] class X", ast);

  ast = testDeclaration("class X[%T <: Object] {}");
  ASSERT_EQ(ASTDecl::Template, ast->nodeType());
  EXPECT_EQ(ASTNode::TypeVar, cast<ASTTemplate>(ast)->params()[0]->nodeType());
  EXPECT_FALSE(cast<ASTTypeVariable>(cast<ASTTemplate>(ast)->params()[0])->isVariadic());
  EXPECT_EQ(ASTTypeVariable::IS_SUBTYPE,
      cast<ASTTypeVariable>(cast<ASTTemplate>(ast)->params()[0])->constraint());
  EXPECT_AST_EQ("[%T] class X", ast);

  ast = testDeclaration("def X(x:int32);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32)", ast);
  ASTParameter * param = cast<ASTFunctionDecl>(ast)->params()[0];
  EXPECT_EQ(0, param->flags());

  ast = testDeclaration("def X(x:int32...);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32)", ast);
  param = cast<ASTFunctionDecl>(ast)->params()[0];
  EXPECT_TRUE(param->flags() & Param_Variadic);

  ast = testDeclaration("def X(;x:int32);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32)", ast);
  param = cast<ASTFunctionDecl>(ast)->params()[0];
  EXPECT_TRUE(param->flags() & Param_KeywordOnly);

  ast = testDeclaration("def X(x:*int32);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32)", ast);
  param = cast<ASTFunctionDecl>(ast)->params()[0];
  EXPECT_TRUE(param->flags() & Param_Star);

  ast = testDeclaration("def X(x:int32=A);");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_AST_EQ("def X (x:int32 = A)", ast);
  param = cast<ASTFunctionDecl>(ast)->params()[0];
  ASSERT_EQ(ASTNode::Id, param->value()->nodeType());

  ast = testDeclaration("macro X() {}");
  ASSERT_EQ(ASTNode::Macro, ast->nodeType());
  EXPECT_AST_EQ("macro X (){}", ast);

  ast = testDeclaration("undef X();");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_EQ(Undef, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("def X ()", ast);

  ast = testDeclaration("override X();");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_EQ(Override, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("def X ()", ast);

  ast = testDeclaration("def X { x = 0; }");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_EQ(ASTNode::Block, cast<ASTFunctionDecl>(ast)->body()->nodeType());
}

TEST_F(ASTSerializationTest, DeclVisibility) {
  ASTDecl * ast;

  ast = testDeclaration("var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(Public, ast->modifiers().visibility);
  EXPECT_AST_EQ("var X:int32", ast);

  ast = testDeclaration("private var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(Private, ast->modifiers().visibility);
  EXPECT_AST_EQ("var X:int32", ast);

  ast = testDeclaration("protected var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(Protected, ast->modifiers().visibility);
  EXPECT_AST_EQ("var X:int32", ast);

#if 0
  ast = testDeclaration("internal var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(Internal, ast->modifiers().visibility);
  EXPECT_AST_EQ("var X:int32", ast);
#endif
}

TEST_F(ASTSerializationTest, DeclMods) {
  ASTDecl * ast;

  // Default case

  ast = testDeclaration("var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(0, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("var X:int32", ast);

  ast = testDeclaration("static var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(Static, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("var X:int32", ast);

  ast = testDeclaration("readonly var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(ReadOnly, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("var X:int32", ast);

  ast = testDeclaration("class X {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_EQ(0, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("class X", ast);

  ast = testDeclaration("abstract class X {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_EQ(Abstract, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("class X", ast);

  ast = testDeclaration("final class X {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_EQ(Final, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("class X", ast);

  ast = testDeclaration("final abstract class X {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_EQ(Abstract|Final, int32_t(ast->modifiers().flags));
  EXPECT_AST_EQ("class X", ast);
}

TEST_F(ASTSerializationTest, DeclAttrs) {
  ASTDecl * ast;

  // No attributes
  ast = testDeclaration("var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_TRUE(ast->attributes().empty());

  ast = testDeclaration("@Attr var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(1u, ast->attributes().size());
  ASSERT_EQ(ASTNode::Id, ast->attributes()[0]->nodeType());
  EXPECT_AST_EQ("Attr", ast->attributes()[0]);

  ast = testDeclaration("@Attr @Other var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(2u, ast->attributes().size());
  ASSERT_EQ(ASTNode::Id, ast->attributes()[0]->nodeType());
  EXPECT_AST_EQ("Attr", ast->attributes()[0]);
  ASSERT_EQ(ASTNode::Id, ast->attributes()[1]->nodeType());
  EXPECT_AST_EQ("Other", ast->attributes()[1]);

  ast = testDeclaration("@Attr(Foo) var X:int32;");
  ASSERT_EQ(ASTNode::Var, ast->nodeType());
  EXPECT_EQ(1u, ast->attributes().size());
  ASSERT_EQ(ASTNode::Call, ast->attributes()[0]->nodeType());
  EXPECT_AST_EQ("Attr(Foo)", ast->attributes()[0]);

  ast = testDeclaration("@Attr class X");
  ASSERT_EQ(ASTNode::Class, ast->nodeType());
  EXPECT_EQ(1u, ast->attributes().size());
  ASSERT_EQ(ASTNode::Id, ast->attributes()[0]->nodeType());
  EXPECT_AST_EQ("Attr", ast->attributes()[0]);

  ast = testDeclaration("@Attr def X {}");
  ASSERT_EQ(ASTNode::Function, ast->nodeType());
  EXPECT_EQ(1u, ast->attributes().size());
  ASSERT_EQ(ASTNode::Id, ast->attributes()[0]->nodeType());
  EXPECT_AST_EQ("Attr", ast->attributes()[0]);
}

TEST_F(ASTSerializationTest, DeclMembers) {
  ASTDecl * ast;

  ast = testDeclaration("class X { var a:int32; }");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_EQ(1u, ast->members().size());
  EXPECT_AST_EQ("var a:int32", ast->members()[0]);

  ast = testDeclaration("namespace X { var a:int32; }");
  ASSERT_EQ(ASTDecl::Namespace, ast->nodeType());
  EXPECT_EQ(1u, ast->members().size());
  EXPECT_AST_EQ("var a:int32", ast->members()[0]);

  ast = testDeclaration("enum X { FOO }");
  ASSERT_EQ(ASTDecl::Enum, ast->nodeType());
  EXPECT_EQ(1u, ast->members().size());
  EXPECT_AST_EQ("let FOO", ast->members()[0]);
}

TEST_F(ASTSerializationTest, DeclLocalImports) {
  ASTDecl * ast;

  ast = testDeclaration("class X {}");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_EQ(0u, ast->imports().size());

  ast = testDeclaration("class X { import x; }");
  ASSERT_EQ(ASTDecl::Class, ast->nodeType());
  EXPECT_EQ(1u, ast->imports().size());
  EXPECT_EQ(ASTNode::Import, ast->imports()[0]->nodeType());
  EXPECT_AST_EQ("import x", ast->imports()[0]);

  ast = testDeclaration("namespace X { import x.y; }");
  ASSERT_EQ(ASTDecl::Namespace, ast->nodeType());
  EXPECT_EQ(1u, ast->imports().size());
  EXPECT_EQ(ASTNode::Import, ast->imports()[0]->nodeType());
  EXPECT_FALSE(cast<ASTImport>(ast->imports()[0])->unpack());
  EXPECT_AST_EQ("import x.y", ast->imports()[0]);

  ast = testDeclaration("namespace X { import namespace x.y; }");
  ASSERT_EQ(ASTDecl::Namespace, ast->nodeType());
  EXPECT_EQ(1u, ast->imports().size());
  EXPECT_EQ(ASTNode::Import, ast->imports()[0]->nodeType());
  EXPECT_TRUE(cast<ASTImport>(ast->imports()[0])->unpack());
  EXPECT_STREQ("y", cast<ASTImport>(ast->imports()[0])->asName());
  EXPECT_AST_EQ("import x.y", ast->imports()[0]);

  ast = testDeclaration("namespace X { import x.y as z; }");
  ASSERT_EQ(ASTDecl::Namespace, ast->nodeType());
  EXPECT_EQ(1u, ast->imports().size());
  EXPECT_EQ(ASTNode::Import, ast->imports()[0]->nodeType());
  EXPECT_FALSE(cast<ASTImport>(ast->imports()[0])->unpack());
  EXPECT_STREQ("z", cast<ASTImport>(ast->imports()[0])->asName());
  EXPECT_AST_EQ("import x.y", ast->imports()[0]);
}

}  // namespace

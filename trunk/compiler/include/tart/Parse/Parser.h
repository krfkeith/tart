/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_PARSE_PARSER_H
#define TART_PARSE_PARSER_H

#ifndef TART_LEX_LEXER_H
#include "tart/Lex/Lexer.h"
#endif

#ifndef TART_AST_DECL_H
#include "tart/AST/ASTDecl.h"
#endif

namespace tart {

class Module;

/// -------------------------------------------------------------------
/// Parser class
class Parser {
private:
  Module          * module;         // The module we're parsing.
  Lexer             lexer;          // Lexer
  TokenType         token;          // Current token
  SourceLocation    matchLoc;       // Location of just-matched token
  int               templateNesting;// template nesting level
  bool              recover;        // In error recovery state.
  ASTFunctionDecl * function;       // Current function being parsed.

  /** Read the next token. */
  void next();

  /** Match a token. */
  bool match(TokenType tok);

  /** Match an identifier. */
  const char * matchIdent();

  /** Skip until we find an open brace or bracket */
  void skipToNextOpenDelim();

  /** Skip until we find an open brace or bracket, or the specified token */
  void skipToRParen();

  /** Error message functions. */
  void unexpectedToken();

  /** Error message that indicates we expected something else here. */
  void expected(const char * what);

  /** Various error messages. */
  void expectedImportPath();
  void expectedDeclaration();
  void expectedExpression();
  void expectedSemicolon();
  void expectedStatement();
  void expectedIdentifier();
  void expectedCloseParen();
  void expectedCloseBracket();

  /** Returns true if we matched a semicolon; Emits a fatal error and returns
      false otherwise. */
  bool needSemi();

public:
  Parser(ProgramSource * src, Module * module);

  /** True if we are finished parsing. */
  bool finished() const { return token == Token_End; }

  /** Parse the module, and fill in the given scope with the
      resuling definitions.
  */
  bool parse();

  /** Parse the import section. */
  bool parseImports();

  /** Parse a declaration list. */
  bool declarationList(ASTDeclList & dlist, DeclModifiers mods);

  /** Parse a declaration. */
  bool declaration(ASTDeclList & dlist, DeclModifiers mods);

  /** Parse a declaration (used for testing only). */
  ASTDecl * declaration();

  /** Parse an import statement. */
  bool importStmt(ASTNodeList & out);

  /** Parse a declarator. */
  ASTDecl * declarator(const DeclModifiers & mods);

  /** Parse a variable declaration. */
  ASTDecl * declareVariable(const DeclModifiers & mods, TokenType tok);

  /** Parse a function or property declaration. */
  ASTDecl * declareDef(const DeclModifiers & mods,
      TokenType tok);

  /** Parse a macro or intrinsic declaration. */
  ASTDecl * declareMacro(const DeclModifiers & mods,
      TokenType tok);

  /** Parse a type declaration. */
  ASTDecl * declareType(const DeclModifiers & mods, TokenType tok);

  /** Parse a namespace declaration. */
  ASTDecl * declareNamespace(DeclModifiers mods,
      TokenType tok);

  /** Parse an enumeration declaration. */
  ASTDecl * declareEnum(const DeclModifiers & mods);

  /** Parse a property accessor definition. */
  bool accessorMethodList(ASTPropertyDecl * property,
      ASTParamList & accessorParams, DeclModifiers mods);

  /** Parse list of attributes. */
  bool attributeList(ASTNodeList & attributes);

  /** Parse access types. */
  bool accessTypeModifiers(DeclModifiers & mods);

  /** Parse a type expression. */
  ASTNode * typeExpression();

  /** Parse a binary type expression. */
  ASTNode * typeExprBinary();

  /** Parse a binary type expression. */
  ASTNode * typeExprPrimary();

  /** Parse a type name - this includes compound names and template params. */
  ASTNode * typeName();

  /** Parse a built-in type name. */
  ASTNode * builtInTypeName(TokenType t);

  /** Parse function type declaration */
  ASTFunctionDecl * functionDeclaration(ASTNode::NodeType nt, const char * name,
    const DeclModifiers & mods);

  /** Parse function return type. */
  ASTNode * functionReturnType();

  /** Parse indexer declaration */
  ASTFunctionDecl * indexerDeclaration();

  /** Parse a list of type parameters. */
  void templateParamList(ASTNodeList & templateParams);

  /** Parse a type parameter. */
  bool templateParam(ASTNodeList & templateParams);

  /** Parse a list of template arguments. */
  bool templateArgList(ASTNodeList & typeArgs);

  /** Parse a template argument. */
  ASTNode * templateArg();

  /** Parse a list of template requirements. */
  void templateRequirements(ASTNodeList & requirements);

  /** Parse a format argument list. */
  bool formalArgumentList(ASTParamList & params, TokenType endDelim);

  /** Parse a format argument. */
  bool formalArgument(ASTParamList & params, int paramFlags);

  /** Create the 'self' argument. */
  //void createSelfParam(ParameterList & params, ASTDecl * selfType);

  /** Parse a Stmt */
  Stmt * statement();

  /** Parse a Stmt block */
  Stmt * blockStmt();

  /** Parse a return Stmt */
  Stmt * returnStmt();

  /** Parse a yield Stmt */
  Stmt * yieldStmt();

  /** Parse a break Stmt */
  Stmt * breakStmt();

  /** Parse a continue Stmt */
  Stmt * continueStmt();

  /** Parse a throw Stmt */
  Stmt * throwStmt();

  /** Parse a try Stmt */
  Stmt * tryStmt();

  /** Parse a declaration Stmt */
  Stmt * declStmt();

  /** Parse an if Stmt */
  Stmt * ifStmt();

  /** Parse a while Stmt */
  Stmt * whileStmt();

  /** Parse a for Stmt */
  Stmt * forStmt();

  /** Parse the body of a compound statement such as 'if' or 'while'. */
  Stmt * bodyStmt();

  /** Parse a condition after a Stmt. */
  Stmt * postCondition(Stmt * st);

  /** Parse either a declaration or an expression. */
  ASTNode * testOrDecl();

  /** Parse a list of local declations. Returns either a single declaration
      or a tuple of declarations. */
  ASTNode * localDeclList(ASTNode::NodeType nt);

  /** Either an expression, or a tuple of expressions. */
  ASTNode * expressionList();

  /** Parse an expression */
  ASTNode * expression();

  /** Parse an expression which may be a simple assignment. */
  ASTNode * assignmentExpression();

  /** Parse a unary operator expression. */
  ASTNode * unaryOperator();

  /** Parse a primary expression (integer, ident, etc.) */
  ASTNode * primaryExpression();

  /** Parse a list of arguments. */
  bool parseArgumentList(ASTNodeList & args);

  /** Parse a list of arguments. */
  ASTNode * arrayLiteral();

  /** Parse array arguments. */
  bool parseArrayIndices(ASTOper * arrayExpr);

  /** Operator-precedence parser for binary operators. */
  ASTNode * binaryOperator();

  /** Parse an integer constant. */
  ASTNode * parseIntegerLiteral();

  /** Parse a float constant. */
  ASTNode * parseFloatLiteral();

  /** Parse a string literal. */
  ASTNode * parseStringLiteral();

  /** Parse a character literal. */
  ASTNode * parseCharLiteral();

  /** Return the current doc comment string. */
  const std::string & docComment() const { return lexer.docComment(); }
  std::string & docComment() { return lexer.docComment(); }
};

}

#endif

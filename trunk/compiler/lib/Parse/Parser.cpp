/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Parse/Parser.h"
#include "tart/Parse/OperatorStack.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/AST/Stmt.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Module.h"
#include <errno.h>

namespace tart {

namespace {
  // Convenience function to get the suffix of a string
  inline char lastChar(const std::string & str) {
    std::string::const_iterator begin = str.begin();
    std::string::const_iterator end = str.end();
    if (end > begin) {
      return *(end - 1);
    }
    return 0;
  }

  // Operator precedence levels.
  enum Precedence {
    Prec_Lowest = 0,
    Prec_LogicalOr = 5,
    Prec_LogicalAnd = 6,
    Prec_Contains = 7,
    Prec_IsType = 8,

    Prec_Relational = 10,

    Prec_BitOr = 20,
    Prec_BitXor = 21,
    Prec_BitAnd = 22,

    Prec_Shift = 25,
    Prec_AddSub = 30,
    Prec_MulDiv = 32,

    Prec_Exponent = 40,
    Prec_Range = 50,

    //Prec_Attr = 60,

    Prec_Highest
  };

  ASTCall * callOperator(ASTNode * opFunc, const SourceLocation loc) {
    return new ASTCall(loc, opFunc, ASTNodeList());
  }

  ASTCall * callOperatorMethod(char * opname, const SourceLocation loc, ASTNode * base,
      ASTNode * arg) {

    ASTMemberRef * method = new ASTMemberRef(loc, base, opname);
    ASTCall * result = new ASTCall(loc, method, ASTNodeList());
    result->append(arg);
    return result;
  }

  ASTDecl * DECL_ERROR = (ASTDecl *) - 1;
}

Parser::Parser(ProgramSource * src, Module * m)
    : module(m)
    , lexer(src)
    , function(NULL) {
  token = lexer.next();
}

// -------------------------------------------------------------------
// Token matching functions
// -------------------------------------------------------------------

void Parser::next() {
  token = lexer.next();
}

inline bool Parser::match(TokenType tok) {
  if (token == tok) {
    token = lexer.next();
    return true;
  }
  return false;
}

const char * Parser::matchIdent() {
  if (token == Token_Ident) {
    // Save the token value as a string
    const char * value = istrings.intern(lexer.tokenValue().c_str());

    // Save the token location
    matchLoc = lexer.tokenLocation();

    // Get the next token
    token = lexer.next();
    return value;
  }
  return NULL;
}

void Parser::skipToNextOpenDelim() {
  for (;;) {
    if (token == Token_End || token == Token_LParen || token == Token_LBrace ||
        token == Token_LBracket) {
      return;
    }

    token = lexer.next();
  }
}

void Parser::skipToRParen() {
  for (;;) {
    if (token == Token_RParen) {
      lexer.next();
      return;
    } else if (token == Token_LParen) {
      token = lexer.next();
      skipToRParen();
      return;
    } else if (token == Token_End || token == Token_LBrace ||
        token == Token_LBracket) {
      return;
    }

    token = lexer.next();
  }
}

// -------------------------------------------------------------------
// High-level entry points
// -------------------------------------------------------------------

bool Parser::parse() {
  int errorCount = diag.getErrorCount();

  // Parse imports
  parseImports();

  bool foundDecl = declarationList(module->astMembers(), DeclModifiers());
  if (token != Token_End || !foundDecl) {
    expectedDeclaration();
    return false;
  }

  // Return false if parsing this module introduced any new errors.
  return diag.getErrorCount() == errorCount;
}

bool Parser::parseImports() {
  // Parse imports
  int errorCount = diag.getErrorCount();
  while (match(Token_Import)) {
    importStmt(module->imports());
  }
  return diag.getErrorCount() == errorCount;
}

// -------------------------------------------------------------------
// Declarations
// -------------------------------------------------------------------

bool Parser::declarationList(ASTDeclList & dlist, DeclModifiers mods) {
  bool foundDecl = false;
  while (declaration(dlist, mods)) {
    foundDecl = true;
  }
  return foundDecl;
}

ASTDecl * Parser::declaration() {
  // This method is used for unit tests
  ASTDeclList declList;
  if (declaration(declList, DeclModifiers())) {
    if (declList.size() == 1) {
      return declList[0];
    }
  }

  return NULL;
}

bool Parser::declaration(ASTDeclList & dlist, DeclModifiers mods) {

  // TODO: Declaration comments
  /*      comment = self.doc_comment
          self.doc_comment = ""
  */

  // Static 'if'
  if (match(Token_If)) {
    ASTNode * testExpr = expression();
    if (testExpr == NULL) {
      expectedExpression();
      return true;
    }

    DeclModifiers declMods(mods);
    if (mods.condition == NULL) {
      declMods.condition = testExpr;
    } else {
      ASTOper * combined = new ASTOper(ASTNode::LogicalAnd, testExpr->location());
      combined->append(mods.condition);
      combined->append(testExpr);
      declMods.condition = combined;
    }

    if (match(Token_LBrace)) {
      while (!match(Token_RBrace)) {
        if (!declaration(dlist, declMods)) {
          expectedDeclaration();
        }
      }
    } else if (match(Token_Colon)) {
      if (!declaration(dlist, declMods)) {
        expectedDeclaration();
      }
    }

    if (match(Token_Else)) {
      ASTNode * elseExpr = new ASTOper(ASTNode::LogicalNot, testExpr);
      if (mods.condition == NULL) {
        declMods.condition = elseExpr;
      } else {
        ASTOper * combined = new ASTOper(ASTNode::LogicalAnd, testExpr->location());
        combined->append(mods.condition);
        combined->append(elseExpr);
        declMods.condition = combined;
      }

      if (match(Token_LBrace)) {
        while (!match(Token_RBrace)) {
          if (!declaration(dlist, declMods)) {
            expectedDeclaration();
          }
        }
      } else {
        if (!declaration(dlist, declMods)) {
          expectedDeclaration();
        }
      }
    }

    return true;
  }

  // Parse attributes
  ASTNodeList attributes;
  if (!attributeList(attributes)) {
    return false;
  }

  bool accessType = accessTypeModifiers(mods);
  bool modifier = false;
  for (;;) {
    if (match(Token_Static)) {
      modifier = true;
      mods.storageClass = Storage_Static;
    } else if (match(Token_Final)) {
      mods.flags |= Final;
      modifier = true;
    } else if (match(Token_Readonly)) {
      mods.flags |= ReadOnly;
      modifier = true;
    } else if (match(Token_Abstract)) {
      mods.flags |= Abstract;
      modifier = true;
    } else {
      break;
    }
  }

  // Block beginning with 'private', etc.
  if ((accessType || modifier) && match(Token_LBrace)) {
    // TODO: No attributes...
    declarationList(dlist, mods);
    if (!match(Token_RBrace)) {
      expectedDeclaration();
    }
    return true;
  }

  // Grab the doc comment if there is one.
  std::string docComment;
  docComment.swap(lexer.docComment());

  ASTDecl * decl = declarator(mods);
  if (decl == NULL) {
    if (modifier) {
      diag.error(lexer.tokenLocation()) << "Syntax error";
    }
    return false;
  } else if (decl == DECL_ERROR) {
    return true;
  }

  docComment.swap(decl->docComment());
  decl->attributes().append(attributes.begin(), attributes.end());
  if (ASTTemplate * templ = dyn_cast<ASTTemplate>(decl)) {
    templ->body()->attributes().append(attributes.begin(),
        attributes.end());
  }

  dlist.push_back(decl);
  return true;
}

bool Parser::importStmt(ASTNodeList & out) {
  SourceLocation loc = lexer.tokenLocation();

  bool unpack = false;
  if (match(Token_Namespace)) {
    unpack = true;
  }

  // Parse imports
  const char * importName = matchIdent();
  if (!importName) {
    expectedImportPath();
    return false;
  }

  ASTNode * path = new ASTIdent(matchLoc, importName);
  while (match(Token_Dot)) {
    importName = matchIdent();
    if (!importName) {
      expectedImportPath();
      return false;
    }

    path = new ASTMemberRef(matchLoc, path, importName);
  }

  const char * asName = importName;
  if (match(Token_As)) {
    if (unpack) {
      diag.error(loc) << "Import statement cannot have both 'from' and 'as'";
    }

    asName = matchIdent();
    if (asName == NULL) {
      expectedIdentifier();
    }
  }

  if (!match(Token_Semi)) {
    expectedSemicolon();
  }

  out.push_back(new ASTImport(loc, path, asName, unpack));
  return true;
}

ASTDecl * Parser::declarator(const DeclModifiers & mods) {
  TokenType tok = token;
  if (match(Token_Var) || match(Token_Let)) {
    return declareVariable(mods, tok);
  } else if (match(Token_Def)) {
    return declareDef(mods, tok);
  } else if (match(Token_Macro)) {
    return declareMacro(mods, tok);
  } else if (match(Token_Class) || match(Token_Struct) ||
      match(Token_Interface) || match(Token_Protocol)) {
    return declareType(mods, tok);
  } else if (match(Token_Namespace)) {
    return declareNamespace(mods, tok);
  } else if (match(Token_Enum)) {
    return declareEnum(mods);
  } else {
    return NULL;
  }
}

ASTDecl * Parser::declareVariable(const DeclModifiers & mods, TokenType tok) {
  const char * declName = matchIdent();
  SourceLocation loc = matchLoc;
  ASTNode * declType = NULL;
  ASTNode * declValue = NULL;

  if (mods.flags & Final) {
    diag.warn(lexer.tokenLocation()) << "Values are always 'final'";
  }

  if (declName == NULL) {
    expectedIdentifier();
    return DECL_ERROR;
  }

  if (match(Token_Colon)) {
    declType = typeExpression();
  }

  if (match(Token_Assign)) {
    declValue = expression();
    if (declValue == NULL) {
      expectedExpression();
      return DECL_ERROR;
    }
  }

  if (!needSemi()) {
    return DECL_ERROR;
  }

  // TODO: Need to do this in analysis, because constructor could init it.
  //if (tok == Token_Let && declValue == NULL) {
  //    diag.fatal(lexer.tokenLocation(), "'let' requires an initializer");
  //    return DECL_ERROR;
  //}

  if (declType == NULL && declValue == NULL) {
    diag.fatal(lexer.tokenLocation()) << "Can't infer type for '" << declName << "'";
    return DECL_ERROR;
  }

  return new ASTVarDecl(
    tok == Token_Let ? ASTNode::Let : ASTNode::Var,
    matchLoc, declName, declType, declValue, mods);
}

ASTDecl * Parser::declareDef(const DeclModifiers & mods, TokenType tok) {
  if (match(Token_LBracket)) {
    SourceLocation loc = matchLoc;
    ASTNode * returnType = NULL;
    ASTParamList params;

    if (!formalArgumentList(params, Token_RBracket)) {
      return DECL_ERROR;
    }

    if (params.empty()) {
      diag.error(lexer.tokenLocation()) << "Indexer must have at least one argument";
      return DECL_ERROR;
    }

    // See if there's a return type declared
    if (match(Token_Colon)) {
      returnType = typeExpression();
    }

    // Function type.
    //FunctionType * funcType = new FunctionType(returnType, params, false);
    //return funcType;


    ASTPropertyDecl * indexer = new ASTPropertyDecl(
        ASTDecl::Idx, loc, istrings.idIndex, returnType, mods);
    indexer->params().append(params.begin(), params.end());
    if (match(Token_LBrace)) {
      // Parse accessors for indexer
      if (!accessorMethodList(indexer, params, mods)) {
        return DECL_ERROR;
      }
    }

    return indexer;
  }

  const char * declName = matchIdent();
  if (declName == NULL) {
    declName = istrings.idCall;
  }

  SourceLocation loc = matchLoc;
  if (match(Token_Colon)) {
    // It's a property
    ASTNode * declType = typeExpression();
    if (declType == NULL) {
      return DECL_ERROR;
    }

    ASTParamList params;
    ASTPropertyDecl * prop = new ASTPropertyDecl(loc, declName, declType, mods);
    if (match(Token_LBrace)) {
      // Parse accessors
      if (!accessorMethodList(prop, params, mods)) {
        return DECL_ERROR;
      }
    }

    return prop;
  } else {
    // Get template params.
    ASTNodeList templateParams;
    ASTNodeList requirements;
    templateParamList(templateParams);

    SourceLocation loc = matchLoc;
    ASTParamList params;
    if (match(Token_LParen)) {
      // Argument list
      if (!formalArgumentList(params, Token_RParen)) {
        return DECL_ERROR;
      }

      // Enable this if we want to allow parameterized properties.
#if 0
      // If there's a colon after the arg list, then it's a parameterized property rather
      // than a method.
      if (match(Token_Colon)) {
        ASTNode * declType = typeExpression();
        if (declType == NULL) {
          return DECL_ERROR;
        }

        ASTParamList params;
        ASTPropertyDecl * prop = new ASTPropertyDecl(loc, declName, declType, mods);
        prop->params().append(params.begin(), params.end());
        if (match(Token_LBrace)) {
          // Parse accessors
          if (!accessorMethodList(prop, params, mods)) {
            return DECL_ERROR;
          }
        }

        return prop;
      }
#endif
    } else {
      // Check for single argument (it's optional)
      formalArgument(params, 0);
    }

    // Function type.
    ASTNode * returnType = functionReturnType();
    ASTFunctionDecl * fd = new ASTFunctionDecl(
        ASTDecl::Function, loc, declName, params, returnType, mods);

    if (!templateParams.empty()) {
      if (match(Token_Requires)) {
        templateRequirements(requirements);
      }
    }

    Stmt * body = NULL;
    ASTFunctionDecl * saveFunction = function;
    function = fd;
    if (!match(Token_Semi)) {
      body = statement();
      if (body == NULL) {
        diag.error(loc) << "Function definition with no body";
        return DECL_ERROR;
      }
      fd->setBody(body);
    }

    function = saveFunction;

    // If there were type parameters, create a template
    if (!templateParams.empty()) {
      return new ASTTemplate(fd, templateParams, requirements);
    }

    return fd;
  }
}

ASTDecl * Parser::declareMacro(const DeclModifiers & mods, TokenType tok) {
  if (mods.flags & Final) {
    diag.error(lexer.tokenLocation()) << "Macros are always final";
  }

  const char * declName = matchIdent();
  if (declName == NULL) {
    expectedIdentifier();
    return DECL_ERROR;
  }

  ASTNodeList templateParams;
  ASTNodeList requirements;
  templateParamList(templateParams);

  SourceLocation loc = matchLoc;

  ASTFunctionDecl * fd = functionDeclaration(ASTNode::Macro, declName, mods);
#if 0
  if (tok == Token_Macro) {
#endif

  if (!templateParams.empty()) {
    if (match(Token_Requires)) {
      templateRequirements(requirements);
    }
  }

  Stmt * body = NULL;
  ASTFunctionDecl * saveFunction = function;
  function = fd;
  body = statement();
  if (body == NULL) {
    diag.error(loc) << "Macro definition requires a body";
    return DECL_ERROR;
  }
  function = saveFunction;
  fd->setBody(body);

#if 0
  } else {
    /*if (funcType->returnType() == NULL) {
      funcType->setReturnType(&VoidType::biDef);
    }*/

    int32_t funcId = intVal->getValue().asInt32();
    Stmt * body = Builtins::createIntrinsic(funcId);
    if (body == NULL) {
      diag.error(loc, "Invalid intrinsic function number '%d'", funcId);
      return DECL_ERROR;
    }

    fd->setBody(body);

    needSemi();
  }
#endif

  // If there were type parameters, create a template
  if (!templateParams.empty()) {
    return new ASTTemplate(fd, templateParams, requirements);
  }

  return fd;
}

ASTDecl * Parser::declareType(const DeclModifiers & mods, TokenType tok) {
  const char * declName = matchIdent();
  if (declName == NULL) {
    expectedIdentifier();
    skipToNextOpenDelim();
    declName = "#ERROR";
  }

  // Parse any type parameters
  ASTNodeList templateParams;
  ASTNodeList requirements;
  templateParamList(templateParams);

  ASTNodeList bases;
  if (match(Token_Colon)) {
    // Superclass list.
    for (;;) {
      ASTNode * baseType = typeExpression();
      if (baseType == NULL) {
        skipToNextOpenDelim();
      }

      bases.push_back(baseType);
      if (!match(Token_Comma)) {
        break;
      }
    }
  }

  ASTNode::NodeType kind = ASTNode::Class;
  if (tok == Token_Struct) {
    kind = ASTNode::Struct;
  } else if (tok == Token_Interface) {
    kind = ASTNode::Interface;
  } else if (tok == Token_Protocol) {
    kind = ASTNode::Protocol;
  }

  // TODO: If there were no errors

  ASTTypeDecl * typeDecl = new ASTTypeDecl(kind, matchLoc, declName, bases, mods);

  if (!templateParams.empty()) {
    if (match(Token_Requires)) {
      templateRequirements(requirements);
    }
  }

  if (!match(Token_LBrace)) {
    expected("class body");
    // Recovery: Look for a '{'
    // Recovery: Look for a '{'
    // Recovery: skip nested '{}' and look for a name...
    return typeDecl;
  }

  declarationList(typeDecl->members(), DeclModifiers(Storage_Instance));

  if (!match(Token_RBrace)) {
    expected("declaration or '}'");
    // Recovery: Look for a '}'
    // Recovery: Look for a '{'
  }

  // If there were type parameters, create a template
  if (!templateParams.empty()) {
    return new ASTTemplate(typeDecl, templateParams, requirements);
  }

  return typeDecl;
}

ASTDecl * Parser::declareNamespace(DeclModifiers mods, TokenType tok)
{
  const char * declName = matchIdent();
  if (declName == NULL) {
    expectedIdentifier();
    skipToNextOpenDelim();
    declName = "#ERROR";
  }

  mods.storageClass = Storage_Global; // Namespaces are always global
  ASTNamespace * nsDef = new ASTNamespace(matchLoc, declName);

  if (!match(Token_LBrace)) {
    expected("namespace body");
    // Recovery: Look for a '{'
    // Recovery: Look for a '{'
    // Recovery: skip nested '{}' and look for a name...
    return nsDef;
  }

  mods.visibility = Public;
  mods.storageClass = Storage_Global;
  //mods.flags = Available;
  declarationList(nsDef->members(), mods);

  if (!match(Token_RBrace)) {
    expectedDeclaration();
    // Recovery: Look for a '}'
    // Recovery: Look for a '{'
  }

  return nsDef;
}

ASTDecl * Parser::declareEnum(const DeclModifiers & mods) {
  const char * declName = matchIdent();
  if (declName == NULL) {
    expectedIdentifier();
    skipToNextOpenDelim();
    declName = "#ERROR";
  }

  ASTNodeList bases;
  //ASTNode * enumBase = NULL;
  if (match(Token_Colon)) {
    // Enum superclass.
    ASTNode * enumBase = typeExpression();
    if (enumBase != NULL) {
      bases.push_back(enumBase);
    }
  }

  //EnumType * enumType = new EnumType();
  //if (enumBase != NULL) {
  //  enumType->setSuper(enumBase);
  //}

  DeclModifiers enumMods(mods);
  enumMods.storageClass = Storage_Static;
  //enumMods.flags = Final | Available;
  ASTTypeDecl * enumDef = new ASTTypeDecl(ASTNode::Enum, matchLoc, declName,
      bases, enumMods);
  //enumType->setDeclaration(enumDef);

  if (!match(Token_LBrace)) {
    diag.error(lexer.tokenLocation()) << "Expecting enumeration constants";
    // Recovery: Look for a '{'
    // Recovery: Look for a '{'
    // Recovery: skip nested '{}' and look for a name...
    return enumDef;
  }

  enumMods.visibility = Public;
  for (;;) {
    // Parse attributes
    ASTNodeList attributes;
    if (!attributeList(attributes)) {
      return false;
    }

    const char * ecName = matchIdent();
    if (!ecName) {
      break;
    }

    //ASTDeclList prevDefs;
    //if (enumDef->lookupMember(ecName, prevDefs, false)) {
    //  diag.error(matchLoc, "Duplicate enum constant '%s'", ecName);
    //}

    ASTNode * ecValue = NULL;
    if (match(Token_Assign)) {
      ecValue = expression();
    }

    ASTDecl * ecDecl = new ASTVarDecl(ASTNode::Let, matchLoc, ecName, NULL, ecValue,
        enumMods);
    ecDecl->attributes().append(attributes.begin(), attributes.end());
    enumDef->addMember(ecDecl);

    // OK to have extra comma.
    if (!match(Token_Comma)) {
      break;
    }
  }

  if (!match(Token_RBrace)) {
    expected("'}'");
    // Recovery: Look for a '}'
    // Recovery: Look for a '{'
  }

  return enumDef;
}

bool Parser::accessorMethodList(ASTPropertyDecl * parent,
    ASTParamList & accessorParams, DeclModifiers mods) {
  for (;;) {
    if (match(Token_RBrace)) {
      return true;
    }

    // Parse attributes
    ASTNodeList attributes;
    if (!attributeList(attributes)) {
      return false;
    }

    for (;;) {
      if (match(Token_Final)) {
        mods.flags |= Final;
      } else if (match(Token_Abstract)) {
        mods.flags |= Abstract;
        //} else if (match(Token_Extern)) {
        //    mods.flags |= Extern;
      } else {
        break;
      }
    }

    TokenType tok = token;
    if (match(Token_Get) || match(Token_Set)) {
      std::string accessorName;
      if (tok == Token_Get) {
        accessorName.append("get");
      } else {
        accessorName.append("set");
      }

      SourceLocation loc = lexer.tokenLocation();

      ASTParamList params;
      if (tok == Token_Set) {
        ASTParamList setterParams;
        if (match(Token_LParen)) {
          // Argument list
          formalArgumentList(setterParams, Token_RParen);
        } else {
          // Check for single argument (it's optional)
          formalArgument(setterParams, 0);
        }

        if (setterParams.size() > 1) {
          diag.error(loc) << "Setter can have 0 or 1 parameters only.";
        }

        params.append(setterParams.begin(), setterParams.end());
      }

      ASTFunctionDecl * fc = new ASTFunctionDecl(ASTNode::Function, loc,
          istrings.intern(accessorName), params, (ASTNode *)NULL, mods);
      fc->attributes().append(attributes.begin(), attributes.end());

#if 0
      if (funcType->returnType() != NULL) {
        diag.error(loc, "Accessor shouldn't declare a return type.");
      } else if (tok == Token_Get) {
        funcType->setReturnType(propType /*->clone()*/);
      } else {
        funcType->setReturnType(&PrimitiveType::VoidType);
      }
#endif

      Stmt * body = NULL;
      ASTFunctionDecl * saveFunction = function;
      function = fc;
      if (!match(Token_Semi)) {
        body = statement();
        if (body == NULL) {
          diag.error(loc) << "Function body or ';' expected.";
          return true;
        }
        fc->setBody(body);
      }
      function = saveFunction;
      //fc->parentScope = parent;

      if (tok == Token_Get) {
        parent->setGetter(fc);
      } else {
        parent->setSetter(fc);
      }
    } else {
      diag.error(lexer.tokenLocation()) << "'get' or 'set' expected in property definition";
      return false;
    }
  }
}

bool Parser::attributeList(ASTNodeList & attributes) {
  // Parse attributes
  while (match(Token_AtSign)) {
    const char * ident = matchIdent();
    if (ident == NULL) {
      expectedIdentifier();
      return false;
    }

    ASTNode * attrExpr = new ASTIdent(matchLoc, ident);
    for (;;) {
      SourceLocation loc = lexer.tokenLocation();
      if (match(Token_LParen)) {
        ASTNodeList argList;
        if (!parseArgumentList(argList))
          return NULL;
        attrExpr = new ASTCall(loc, attrExpr, argList);
      } else if (match(Token_LBracket)) {
        // Template specialization
        ASTNodeList templateArgs;
        templateArgList(templateArgs);
        attrExpr = new ASTSpecialize(attrExpr->location(), attrExpr, templateArgs);
      } else if (match(Token_Dot)) {
        // Member dereference
        const char * ident = matchIdent();
        if (ident == NULL) {
          expectedIdentifier();
        }

        attrExpr = new ASTMemberRef(loc | attrExpr->location(), attrExpr, ident);
      } else {
        break;
      }
    }

    attributes.push_back(attrExpr);
  }

  return true;
}

bool Parser::accessTypeModifiers(DeclModifiers & mods) {
  if (match(Token_Public)) {
    mods.visibility = Public;
    return true;
  } else if (match(Token_Protected)) {
    mods.visibility = Protected;
    return true;
  } else if (match(Token_Private)) {
    mods.visibility = Private;
    return true;
  }
  return false;
}

ASTNode * Parser::typeExpression() {
  return typeExprBinary();
}

/*      // Tuple type
      case Token_Comma:
        opstack.pushOperator(
          new ASTOper(ASTNode::LogicalOr, lexer.tokenLocation()),
          Prec_Lowest, Left);
        next();
        break;*/

ASTNode * Parser::typeExprBinary() {
  ASTNode * e0 = typeExprPrimary();
  if (e0 == NULL)
    return NULL;

  OperatorStack opstack(e0);
  for (;;) {
    std::string tokenVal = lexer.tokenValue();

    switch (token) {
      /*case Token_LogicalAnd:
        opstack.pushOperator(
          new ASTOper(ASTNode::LogicalAnd, lexer.tokenLocation()),
          Prec_LogicalAnd, Left);
        next();
        break; */

      // Disjoint type
      case Token_LogicalOr:
        opstack.pushOperator(
          new ASTOper(ASTNode::LogicalOr, lexer.tokenLocation()),
          Prec_LogicalOr, Left);
        next();
        break;

      default:
        if (!opstack.reduceAll()) {
          return e0;
        }

        return opstack.getExpression();
    }

    ASTNode * e1 = typeExprPrimary();
    if (e1 == NULL) {
      diag.error(lexer.tokenLocation()) << "type expression expected after '"
          << tokenVal << "'";
      return NULL;
    }

    opstack.pushOperand(e1);
  }
}

ASTNode * Parser::typeExprPrimary() {
  ASTNode * result;
  SourceLocation loc = lexer.tokenLocation();
  if (match(Token_LParen)) {
    result = typeExpression();

    // Handle tuple type.
    if (match(Token_Comma)) {
      ASTOper * tuple = new ASTOper(ASTNode::Tuple, result);
      do {
        result = typeExpression();
        if (result == NULL) {
          return NULL;
        }

        tuple->append(result);
      } while (match(Token_Comma)) ;

      result = tuple;
    }

    if (!match(Token_RParen)) {
      expectedCloseParen();
      return NULL;
    }
  } else if (match(Token_Percent)) {
    // Pattern variable.
    ASTNode * declType = NULL;
    const char * pvarName = matchIdent();
    if (pvarName == NULL) {
      expectedIdentifier();
      return NULL;
    }

    if (match(Token_Colon)) {
      declType = typeExpression();
    }

    result = new ASTPatternVar(matchLoc, pvarName, declType);
  } else if (match(Token_Function)) {
    // Function type.
    result = functionDeclaration(ASTNode::AnonFn, "", DeclModifiers());
  } else {
    // Identifier or declaration
    result = typeName();
    if (result == NULL) {
      expected("type expression");
      return NULL;
    }
  }

  return result;
}

ASTNode * Parser::typeName() {
  ASTNode * result;
  SourceLocation loc = lexer.tokenLocation();
  if (token == Token_Ident) {
    const char * typeName = matchIdent();
    result = new ASTIdent(loc, typeName);
  } else if (token >= Token_BoolType && token <= Token_VoidType) {
    TokenType t = token;
    token = lexer.next();
    result = builtInTypeName(t);
  } else {
    return NULL;
  }

  if (match(Token_LBracket)) {
    ASTNodeList templateArgs;
    if (!templateArgList(templateArgs)) {
      return NULL;
    }

    if (templateArgs.empty()) {
      result = ASTUnaryOp::get(ASTNode::Array, result);
    } else {
      result = new ASTSpecialize(result->location(), result, templateArgs);
    }
  }

  while (match(Token_Dot)) {
    const char * typeName = matchIdent();
    if (typeName == NULL) {
      expected("type name after '.'");
      return NULL;
    }

    result = new ASTMemberRef(matchLoc, result, typeName);

    if (match(Token_LBracket)) {
      ASTNodeList templateArgs;
      if (!templateArgList(templateArgs)) {
        return NULL;
      }

      if (templateArgs.empty()) {
        result = ASTUnaryOp::get(ASTNode::Array, result);
      } else {
        result = new ASTSpecialize(result->location(), result, templateArgs);
      }
    }
  }

  while (match(Token_LBracket)) {
    // Array
    if (!match(Token_RBracket)) {
      expected("close bracket");
      return NULL;
    }

    result = ASTUnaryOp::get(ASTNode::Array, result);
  }

  return result;
}

ASTNode * Parser::builtInTypeName(TokenType t) {
  switch (t) {
    case Token_BoolType:
      return &BoolType::biDef;

    case Token_CharType:
      return &CharType::biDef;

    case Token_ByteType:
      return &ByteType::biDef;

    case Token_ShortType:
      return &ShortType::biDef;

    case Token_IntType:
      return &IntType::biDef;

    case Token_LongType:
      return &LongType::biDef;
      //if (match(Token_DoubleType))
      //  return &LongDoubleType::biDef;

    case Token_UByteType:
      return &UByteType::biDef;

    case Token_UShortType:
      return &UShortType::biDef;

    case Token_UIntType:
      return &UIntType::biDef;

    case Token_ULongType:
      return &ULongType::biDef;

    case Token_FloatType:
      return &FloatType::biDef;

    case Token_DoubleType:
      return &DoubleType::biDef;

    case Token_VoidType:
      return &VoidType::biDef;

    default:
      DASSERT(false);
  }
}

ASTFunctionDecl * Parser::functionDeclaration(ASTNode::NodeType nt, const char * name,
    const DeclModifiers & mods) {

  SourceLocation loc = matchLoc;

  ASTParamList params;
  if (match(Token_LParen)) {
    // Argument list
    formalArgumentList(params, Token_RParen);
  } else {
    // Check for single argument (it's optional)
    formalArgument(params, 0);
  }

  // See if there's a return type declared
  ASTNode * returnType = functionReturnType();

  // Function type.
  return new ASTFunctionDecl(nt, loc, name, params, returnType, mods);
}

ASTNode * Parser::functionReturnType() {
  if (match(Token_ReturnType)) {
    if (match(Token_LParen)) {
      // Check for multiple return values.
      ASTNodeList returnTypes;
      if (!match(Token_RParen)) {
        for (;;) {

          ASTNode * type = typeExpression();
          if (type != NULL)
            returnTypes.push_back(type);

          if (match(Token_RParen)) {
            break;
          } else if (!match(Token_Comma)) {
            expected("',' or ')'");
            break;
          }
        }
      }

      if (returnTypes.empty()) {
        return NULL;
      } else if (returnTypes.size() == 1) {
        return returnTypes[0];
      } else {
        return new ASTOper(ASTNode::Tuple, returnTypes);
      }
    } else {
      return typeExpression();
    }
  }

  return NULL;
}

void Parser::templateParamList(ASTNodeList & templateParams) {
  if (match(Token_LBracket)) {
    if (match(Token_RBracket)) {
      diag.error(lexer.tokenLocation()) << "Empty template parameter list";
      return;
    }

    for (;;) {
      if (!templateParam(templateParams)) {
        // TODO: Skip to RBracket
        return;
      }

      if (match(Token_RBracket)) {
        break;
      } else if (!match(Token_Comma)) {
        unexpectedToken();
        break;
      }
    }
  }
}

bool Parser::templateParam(ASTNodeList & templateParams) {
  ASTNode * param = typeExpression();
  if (param) {
    /*if (match(Token_Assign)) {
      ASTNode * paramDefault = expression();
      if (paramDefault == NULL) {
        expectedExpression();
        return false;
      }

      // TODO: Create an op representing the default value.
      DFAIL("Implement");
    }*/

    templateParams.push_back(param);
    return true;
  }

  return false;

/*
  const char * paramName = matchIdent();
  if (paramName == NULL) {
    expectedIdentifier();
    return false;
  }

  // TODO: Parse types, default values and varargs...

  ASTParameter * paramDef =
      new ASTParameter(matchLoc, paramName, NULL, NULL, 0);
  templateParams.push_back(paramDef);
  return true; */
}

bool Parser::templateArgList(ASTNodeList & templateArgs) {
  if (match(Token_RBracket)) {
    return true;
  }

  for (;;) {
    SourceLocation loc = lexer.tokenLocation();
    ASTNode * arg = templateArg();
    if (arg == NULL) {
      diag.error(loc) << "Template argument expected";
      skipToRParen();
      return false;
    } else {
      templateArgs.push_back(arg);
    }

    if (match(Token_RBracket)) {
      return true;
    } else if (!match(Token_Comma)) {
      unexpectedToken();
      return false;
    }
  }
}

ASTNode * Parser::templateArg() {
  // TODO: Also allow constants, expressions and such...
  // But don't require templates to be dotted and don't allow <> chars
  // Also, allow keywords.
  return expression();
}

void Parser::templateRequirements(ASTNodeList & requirements) {

}

bool Parser::formalArgumentList(ASTParamList & params, TokenType endDelim) {
  int paramFlags = 0;
  if (match(endDelim))
    return true;

  // Handle an initial semicolon if all params are keyword-only.
  if (match(Token_Semi)) {
    paramFlags |= Param_KeywordOnly;
  }

  if (!formalArgument(params, paramFlags)) {
    expected("formal argument");
    return false;
  }

  for (;;) {
    if (match(endDelim)) {
      return true;
    } else if (match(Token_Comma)) {
      // Fall through
    } else if (match(Token_Semi)) {
      if (paramFlags & Param_KeywordOnly) {
        diag.error(lexer.tokenLocation()) << "Only one ';' allowed in argument list";
      } else {
        paramFlags |= Param_KeywordOnly;
      }
      // Fall through
    } else {
      unexpectedToken();
      break;
    }

    if (!formalArgument(params, paramFlags)) {
      diag.error(lexer.tokenLocation()) << "Formal argument expected after ','";
      break;
    }

    // Check for duplicate argument names.
    ASTParameter * fa = params.back();
    for (ASTParamList::const_iterator it = params.begin();
        it != params.end() - 1; ++it) {
      ASTParameter * pp = *it;
      if (fa->name() && pp->name() && strcmp(fa->name(),
          pp->name()) == 0) {
        diag.error(lexer.tokenLocation()) << "Duplicate argument name '" << fa->name() << "'";
      }
    }
  }

  return false;
}

bool Parser::formalArgument(ASTParamList & params, int paramFlags) {
  // TODO: Check for attributes
  // TODO: Check for modifiers

  SourceLocation argLoc = lexer.tokenLocation();
  const char * argName = matchIdent();
  ASTNode * argType = NULL;
  if (match(Token_Colon)) {
    argType = typeExpression();
  }

  // If there's no name, and no argument, then there's no param
  if (argName == NULL && argType == NULL) {
    return false;
  }

  if (match(Token_Ellipsis)) {
    paramFlags = Param_Variadic;
  }

  ASTNode * defaultValue = NULL;
  if (match(Token_Assign)) {
    defaultValue = expression();
  }

  params.push_back(new ASTParameter(argLoc, argName, argType, defaultValue,
      paramFlags));
  return true;
}

// -------------------------------------------------------------------
// Statements
// -------------------------------------------------------------------

Stmt * Parser::statement() {
  switch (token) {
    case Token_LBrace:
      token = lexer.next();
      return blockStmt();

    case Token_If:
      token = lexer.next();
      return ifStmt();

    case Token_While:
      token = lexer.next();
      return whileStmt();

    case Token_For:
      token = lexer.next();
      return forStmt();

    case Token_Repeat:
      token = lexer.next();
      return repeatStmt();

    case Token_Switch:
      token = lexer.next();
      return switchStmt();

    case Token_Classify:
      token = lexer.next();
      return classifyStmt();

    case Token_Return:
      token = lexer.next();
      return returnStmt();

    case Token_Yield:
      token = lexer.next();
      return yieldStmt();

    case Token_Break:
      token = lexer.next();
      return breakStmt();

    case Token_Continue:
      token = lexer.next();
      return continueStmt();

    case Token_Throw:
      token = lexer.next();
      return throwStmt();

    case Token_Try:
      token = lexer.next();
      return tryStmt();

    case Token_Let:
    case Token_Var:
    case Token_Def:
    case Token_Class:
    case Token_Struct:
      return declStmt();

    default: {
      ASTNode * expr = assignmentExpression();
      if (!expr) {
        expectedStatement();
        return NULL;
      }

      needSemi();

      return new ExprStmt(Stmt::Expression,
          expr->location(), expr);
    }
  }

  return NULL;
}

Stmt * Parser::blockStmt() {
  BlockStmt * block = new BlockStmt(lexer.tokenLocation());
  while (!match(Token_RBrace)) {
    Stmt * st = statement();
    if (st == NULL) {
      expectedStatement();
      return NULL;
    }
    block->append(st);
  }

  block->setFinalLocation(lexer.tokenLocation());
  return block;
}

Stmt * Parser::returnStmt() {
  SourceLocation loc = lexer.tokenLocation();
  ASTNode * expr = expressionList();
  Stmt * st = new ReturnStmt(loc, expr);
  st = postCondition(st);
  needSemi();
  return st;
}

Stmt * Parser::yieldStmt() {
  ASTNode * expr = expression();
  if (expr == NULL) {
    expectedExpression();
  }

  Stmt * st = new YieldStmt(expr->location(), expr,
      function ? function->nextGeneratorIndex() : 0);
  st = postCondition(st);
  needSemi();
  return st;
}

Stmt * Parser::breakStmt() {
  Stmt * st = new Stmt(Stmt::Break, lexer.tokenLocation());
  st = postCondition(st);
  needSemi();
  return st;
}

Stmt * Parser::continueStmt() {
  Stmt * st = new Stmt(Stmt::Continue, lexer.tokenLocation());
  st = postCondition(st);
  needSemi();
  return st;
}

Stmt * Parser::throwStmt() {
  ASTNode * expr = expression();
  Stmt * st = new ThrowStmt(expr->location(), expr);
  st = postCondition(st);
  needSemi();
  return st;
}

Stmt * Parser::tryStmt() {
  Stmt * st = statement();
  if (st == NULL) {
    expectedStatement();
    return NULL;
  }

  TryStmt * tst = new TryStmt(lexer.tokenLocation(), st);
  while (match(Token_Catch)) {

    bool parens = match(Token_LParen); // Optional parens
    SourceLocation loc = lexer.tokenLocation();
    const char * exceptName = matchIdent();
    if (exceptName == NULL) {
      exceptName = "";
    }

    ASTNode * exceptType = NULL;
    if (match(Token_Colon)) {
      exceptType = typeExpression();
    } else {
      expected("exception type");
      return NULL;
    }

    if (parens && !match(Token_RParen)) {
      expectedCloseParen();
    }

    DeclModifiers mods;
    mods.storageClass = Storage_Local;
    mods.visibility = Public;
    ASTDecl * exceptDecl = new ASTVarDecl(ASTNode::Let, loc, exceptName, exceptType,
        NULL, mods);

    st = statement();
    if (st == NULL) {
      expectedStatement();
      return NULL;
    }

    tst->catchList().push_back(
      new CatchStmt(loc | st->location(), exceptDecl, st));
  }

  if (match(Token_Else)) {
    st = statement();
    if (st == NULL) {
      expectedStatement();
      return NULL;
    }
    tst->setElseSt(st);
  }

  if (match(Token_Finally)) {
    st = statement();
    if (st == NULL) {
      expectedStatement();
      return NULL;
    }
    tst->setFinallySt(st);
  }

  return tst;
}

Stmt * Parser::declStmt() {
  ASTDecl * decl = declarator(DeclModifiers(Storage_Local));
  if (decl == NULL || decl == DECL_ERROR) {
    return NULL;
  }

  // Rule for block scopes:
  // -- overloading not allowed.
  // -- variable hiding gives a warning.

  // Check for hiding similarly-named variable.
  // TODO: This is the only place in the code that uses scopeType,
  // if we can get rid of this then we can get rid of scopeType.
  // TODO: This no longer works since outerScope isn't set yet. We need
  // to move this.
#if 0
  for (Scope * s = scope->getOuterScope(); s != NULL; s = s->getOuterScope()) {
    if (s->getScopeType() != Scope::Block && s->getScopeType() != Scope::Params) {
      break;
    }

    if (s->find(decl->name())) {
      diag.warn(decl->location(),
          "'%s' hides definition in outer scope", decl->name());
      break;
    }
  }
#endif

  //scope->addMember(decl);

  return new DeclStmt(decl->location(), decl);
}

Stmt * Parser::ifStmt() {
  SourceLocation loc = matchLoc;
  bool parens = match(Token_LParen);
  ASTNode * testExpr = testOrDecl();
  if (testExpr == NULL)
    return NULL;

  if (parens && !match(Token_RParen)) {
    expectedCloseParen();
    return NULL;
  }

  Stmt * thenSt = bodyStmt();
  if (thenSt == NULL)
    return NULL;

  Stmt * elseSt = NULL;
  if (match(Token_Else)) {
    elseSt = statement();
  }

  return new IfStmt(matchLoc, testExpr, thenSt, elseSt);
}

Stmt * Parser::whileStmt() {
  SourceLocation loc = matchLoc;
  bool parens = match(Token_LParen);
  ASTNode * testExpr = testOrDecl();
  if (testExpr == NULL)
    return NULL;

  if (parens && !match(Token_RParen)) {
    expectedCloseParen();
    return NULL;
  }

  Stmt * bodySt = bodyStmt();
  if (bodySt == NULL)
    return NULL;

  return new WhileStmt(matchLoc, testExpr, bodySt);
}

Stmt * Parser::forStmt() {
  SourceLocation loc = matchLoc;
  ASTNodeList initVars;

  ASTNode * initExpr = NULL;
  if (!match(Token_Semi)) {
    // If it's a non-empty initialization clause, then it must declare
    // one or more iteration variables.
    ASTNode * loopVar = localDeclList(ASTNode::Var);
    if (match(Token_In)) {
      // It's a 'for-in' statement.
      ASTNode * iterable = expression();
      if (iterable == NULL) {
        diag.error(lexer.tokenLocation()) << "Expression expected after 'in'";
        return NULL;
      }

      Stmt * body = bodyStmt();
      return new ForEachStmt(loc, loopVar, iterable, body);
    }

    // It's a 'for' statement.
    initExpr = loopVar;
    if (loopVar && match(Token_Assign)) {
      ASTNode * rhs = expression();
      if (rhs == NULL) {
        diag.error(lexer.tokenLocation()) << "Expression expected after '='";
      }

      if (ASTVarDecl * vdef = dyn_cast<ASTVarDecl>(loopVar)) {
        vdef->setValue(rhs);
      } else {
        initExpr = new ASTOper(ASTNode::Assign, loopVar, rhs);
      }
    }

    if (!needSemi()) {
      return NULL;
    }
    // Fall through
  }

  // At this point, we've just eaten a semicolon, which succeeded either
  // an initialization expression or nothing.

  // It's OK for test to be NULL.
  ASTNode * test = expression();
  if (!needSemi()) {
    return NULL;
  }

  // It's OK for incr to be NULL
  ASTNode * incr = assignmentExpression();
  Stmt * body = bodyStmt();
  return new ForStmt(loc, initExpr, test, incr, body);
}

Stmt * Parser::repeatStmt() {
  SourceLocation loc = matchLoc;
  Stmt * bodySt = bodyStmt();
  if (bodySt == NULL)
    return NULL;

  return new WhileStmt(matchLoc, new ASTBoolLiteral(loc, true), bodySt);
}

Stmt * Parser::switchStmt() {
  SourceLocation loc = matchLoc;
  bool parens = match(Token_LParen);

  // It's a plain expression
  ASTNode * test = expressionList();
  if (test == NULL) {
    expectedExpression();
  }

  if (parens && !match(Token_RParen)) {
    expectedCloseParen();
    return NULL;
  }

  SwitchStmt * st = new SwitchStmt(loc, test);
  if (!match(Token_LBrace)) {
    expected("'{'");
    return NULL;
  }

  while (!match(Token_RBrace)) {
    if (match(Token_Case)) {
      Stmt * caseSt = caseStmt();
      if (caseSt != NULL) {
        st->caseList().push_back(caseSt);
      }
    } else if (match(Token_Else)) {
      Stmt * elseBody = bodyStmt();
      if (elseBody == NULL) {
        return NULL;
      }

      st->caseList().push_back(elseBody);
    } else {
      expected("'case' or 'else' statement");
      return NULL;
    }
  }

  return st;
}

Stmt * Parser::caseStmt() {
  SourceLocation loc = matchLoc;
  ASTNode * test = expression();
  if (test != NULL) {
    Stmt * body = bodyStmt();
    if (body != NULL) {
      return new CaseStmt(loc, test, body);
    }
  }

  return NULL;
}

Stmt * Parser::classifyStmt() {
  SourceLocation loc = matchLoc;
  bool parens = match(Token_LParen);

  // It's a plain expression
  ASTNode * test = expression();
  if (test == NULL) {
    expectedExpression();
  }

  if (parens && !match(Token_RParen)) {
    expectedCloseParen();
    return NULL;
  }

  ClassifyStmt * st = new ClassifyStmt(loc, test);

  if (match(Token_As)) {
    Stmt * asSt = asStmt();
    if (asSt == NULL) {
      return NULL;
    }

    st->caseList().push_back(asSt);
    if (match(Token_Else)) {
      Stmt * elseBody = bodyStmt();
      if (elseBody == NULL) {
        return NULL;
      }

      st->caseList().push_back(elseBody);
    }
  } else if (match(Token_LBrace)) {
    while (!match(Token_RBrace)) {
      if (match(Token_As)) {
        Stmt * asSt = asStmt();
        if (asSt != NULL) {
          st->caseList().push_back(asSt);
        }
      } else if (match(Token_Else)) {
        Stmt * elseBody = bodyStmt();
        if (elseBody == NULL) {
          return NULL;
        }

        st->caseList().push_back(elseBody);
      } else {
        expected("'as' or 'else' statement");
        return NULL;
      }
    }
  } else {
    expected("'{'");
    return NULL;
  }

  return st;
}

Stmt * Parser::asStmt() {
  const char * declName = matchIdent();
  SourceLocation loc = matchLoc;
  ASTNode * declType = NULL;

  if (declName == NULL) {
    expectedIdentifier();
    return NULL;
  }

  if (match(Token_Colon)) {
    declType = typeExpression();
  }

  if (declType == NULL) {
    expected("type expression");
  }

  ASTVarDecl * var = new ASTVarDecl(ASTNode::Let, matchLoc, declName, declType, NULL,
      DeclModifiers(Storage_Local));
  Stmt * body = bodyStmt();
  if (body == NULL) {
    return NULL;
  }

  return new CaseStmt(loc, var, body);
}

Stmt * Parser::bodyStmt() {
  if (token != Token_LBrace) {
    expected("'{'");
  }

  Stmt * st = statement();
  if (st == NULL) {
    expectedStatement();
  }

  return st;
}

Stmt * Parser::postCondition(Stmt * st) {
  if (match(Token_If)) {
    SourceLocation loc = matchLoc;
    ASTNode * testExpr = expression();
    if (testExpr == NULL) {
      expectedExpression();
      return NULL;
    }

    return new IfStmt(loc, testExpr, st, NULL);
  }
  return st;
}

ASTNode * Parser::testOrDecl() {
  TokenType tok = token;
  if (match(Token_Let) || match(Token_Var)) {
    // It's a declaration - for example "if let x = f() { ... }"
    ASTNode * decl = localDeclList(tok == Token_Let ? ASTNode::Let : ASTNode::Var);
    if (decl != DECL_ERROR) {

      // We require an initializer expression in this instance
      if (!match(Token_Assign)) {
        expected("'='");
        return NULL;
      }

      ASTNode * init = expression();
      if (init == NULL) {
        expectedExpression();
      } else {
        if (ASTVarDecl * var = dyn_cast<ASTVarDecl>(decl)) {
          var->setValue(init);
          return var;
        } else {
          return new ASTOper(ASTNode::Assign, decl, init);
        }
      }
    }

    return decl;
  } else {
    // It's a plain expression
    ASTNode * test = expression();
    if (test == NULL) {
      expectedExpression();
    }

    return test;
  }
}

ASTNode * Parser::localDeclList(ASTNode::NodeType nt) {
  DeclModifiers mods;
  mods.storageClass = Storage_Local;
  mods.visibility = Public;

  ASTNodeList decls;
  do {
    const char * declName = matchIdent();
    SourceLocation loc = matchLoc;
    ASTNode * declType = NULL;

    if (declName == NULL) {
      expectedIdentifier();
      return DECL_ERROR;
    }

    if (match(Token_Colon)) {
      declType = typeExpression();
    }

    decls.push_back(new ASTVarDecl(nt, matchLoc, declName, declType, NULL, mods));
  } while (match(Token_Comma));

  DASSERT(!decls.empty());
  if (decls.size() == 1) {
    return decls.front();
  } else {
    return new ASTOper(ASTNode::Tuple, decls);
  }
}

// -------------------------------------------------------------------
// Expressions
// -------------------------------------------------------------------

ASTNode * Parser::expressionList() {
  ASTNode * e = binaryOperator();
  if (e == NULL)
    return NULL;

  if (!match(Token_Comma)) {
    return e;
  }

  ASTNodeList elist;
  elist.push_back(e);

  for (;;) {
    e = binaryOperator();
    if (e == NULL) {
      expectedExpression();
      return NULL;
    }

    elist.push_back(e);
    if (!match(Token_Comma)) {
      break;
    }
  }

  return new ASTOper(ASTNode::Tuple, elist);
}

ASTNode * Parser::expression() {
  return binaryOperator();
}

ASTNode * Parser::assignmentExpression() {
  ASTNode * expr = binaryOperator();
  if (!expr) {
    return NULL;
  }

  TokenType tok = token;
  switch (tok) {
    // Normal assignment
    case Token_Assign: {
      next();
      ASTNode * rhs = assignmentExpression();
      if (rhs == NULL) {
        expectedExpression();
      }

      return new ASTOper(ASTNode::Assign, expr, rhs);
    }

    // Augmented assignment
    case Token_AssignPlus:
    case Token_AssignMinus:
    case Token_AssignStar:
    case Token_AssignSlash:
    case Token_AssignPercent:
    case Token_AssignAmpersand:
    case Token_AssignBar:
    case Token_AssignCaret:
    case Token_AssignRShift:
    case Token_AssignLShift: {
      SourceLocation loc = lexer.tokenLocation();
      next();
      ASTNode * rhs = binaryOperator();
      if (rhs == NULL) {
        expectedExpression();
      }

      ASTNode::NodeType opType;
      switch (int(tok)) {
        case Token_AssignPlus:      opType = ASTNode::AssignAdd; break;
        case Token_AssignMinus:     opType = ASTNode::AssignSub; break;
        case Token_AssignStar:      opType = ASTNode::AssignMul; break;
        case Token_AssignSlash:     opType = ASTNode::AssignDiv; break;
        case Token_AssignPercent:   opType = ASTNode::AssignMod; break;
        case Token_AssignAmpersand: opType = ASTNode::AssignBitAnd; break;
        case Token_AssignBar:       opType = ASTNode::AssignBitOr; break;
        case Token_AssignCaret:     opType = ASTNode::AssignBitXor; break;
        case Token_AssignRShift:    opType = ASTNode::AssignRSh; break;
        case Token_AssignLShift:    opType = ASTNode::AssignLSh; break;
      }

      return new ASTOper(opType, expr, rhs);
    }

    // Just an expression
    default:
      return expr;
  }
}

ASTNode * Parser::binaryOperator() {
  ASTNode * e0 = unaryOperator();
  if (e0 == NULL)
    return NULL;

  OperatorStack opstack(e0);
  for (;;) {
    TokenType operatorToken = token;

    switch (token) {
      case Token_Plus:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorAdd, lexer.tokenLocation()), Prec_AddSub, Left);
        next();
        break;

      case Token_Minus:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorSub, lexer.tokenLocation()), Prec_AddSub, Left);
        next();
        break;

      case Token_Star:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorMul, lexer.tokenLocation()), Prec_MulDiv, Left);
        next();
        break;

      case Token_Slash:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorDiv, lexer.tokenLocation()), Prec_MulDiv, Left);
        next();
        break;

      case Token_Percent:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorMod, lexer.tokenLocation()), Prec_MulDiv, Left);
        next();
        break;

      case Token_Ampersand:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorBitAnd, lexer.tokenLocation()), Prec_BitAnd, Left);
        next();
        break;

      case Token_Bar:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorBitOr, lexer.tokenLocation()), Prec_BitOr, Left);
        next();
        break;

      case Token_Caret:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorBitXor, lexer.tokenLocation()), Prec_BitXor, Left);
        next();
        break;

      case Token_LogicalAnd:
        opstack.pushOperator(
          new ASTOper(ASTNode::LogicalAnd, lexer.tokenLocation()),
          Prec_LogicalAnd, Left);
        next();
        break;

      case Token_LogicalOr:
        opstack.pushOperator(
          new ASTOper(ASTNode::LogicalOr, lexer.tokenLocation()),
          Prec_LogicalOr, Left);
        next();
        break;

      case Token_LShift:
        opstack.pushOperator(callOperator(
            &ASTIdent::operatorLSh, lexer.tokenLocation()), Prec_Shift, Left);
        next();
        break;

      case Token_RShift:
        opstack.pushOperator(callOperator(
            &ASTIdent::operatorRSh, lexer.tokenLocation()), Prec_Shift, Left);
        next();
        break;

      case Token_Range:
        opstack.pushOperator(
          new ASTOper(ASTNode::Range, lexer.tokenLocation()),
              Prec_Range, Left);
        next();
        break;

      case Token_Equal:
        opstack.pushOperator(callOperator(
            &ASTIdent::operatorEq, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      case Token_NotEqual:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorNe, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      /*case Token_RefEqual:
        opstack.pushOperator(
          new ASTOper(ASTNode::ReferenceEq, lexer.tokenLocation()),
          Prec_Relational, Left);
        next();
        break;*/

      case Token_Less:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorLT, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      case Token_Greater:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorGT, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      case Token_LessEqual:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorLE, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      case Token_GreaterEqual:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorGE, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      case Token_PossLess:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorPLT, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      case Token_PossGreater:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorPGT, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      case Token_PossLessEqual:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorPLE, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

      case Token_PossGreaterEqual:
        opstack.pushOperator(callOperator(
              &ASTIdent::operatorPGE, lexer.tokenLocation()), Prec_Relational, Left);
        next();
        break;

#if 0
      case Token_As: {
        TokenType tok = token;
        SourceLocation loc = lexer.tokenLocation();
        next();

        ASTOper * op = new ASTOper(ASTNode::AsType, lexer.tokenLocation());
        opstack.pushOperator(op, Prec_IsType, Left);

        // For the 'as' operator the right-hand side is a type literal,
        // not an expression.
        loc = lexer.tokenLocation();
        ASTNode * type = typeExpression();
        if (type == NULL) {
          return NULL;
        }
        opstack.pushOperand(type);
        continue;
      }
#endif

      case Token_Is: {
        TokenType tok = token;
        SourceLocation loc = lexer.tokenLocation();
        next();

        ASTOper * op;
        if (match(Token_LogicalNot)) {
          op = new ASTOper(ASTNode::IsNot, loc);
        } else {
          op = new ASTOper(ASTNode::Is, loc);
        }

        opstack.pushOperator(op, Prec_IsType, Left);
        break;
      }

      case Token_In:
        opstack.pushOperator(new ASTOper(ASTNode::In,
            lexer.tokenLocation()), Prec_Contains, Left);
        next();
        break;

      case Token_Isa:
        opstack.pushOperator(new ASTOper(ASTNode::IsInstanceOf,
            lexer.tokenLocation()), Prec_IsType, Left);
        next();
        break;

      case Token_LogicalNot: {
        // Negated operators
        next();
        SourceLocation loc = lexer.tokenLocation();
        if (match(Token_In)) {
          opstack.pushOperator(
            new ASTOper(ASTNode::NotIn, loc), Prec_Contains, Left);
        } else {
          diag.error(lexer.tokenLocation()) << "'in' expected after 'not'";
        }
        break;
      }

      //case Token_DoubleAmp:
      //case Token_DoubleBar:
      //    break;
      default:
        goto done;
        /*if (!opstack.reduceAll()) {
          return e0;
        }

        return opstack.getExpression();*/
    }

    ASTNode * e1 = unaryOperator();
    if (e1 == NULL) {
      diag.error(lexer.tokenLocation()) << "value expected after " << GetTokenName(operatorToken);
      return NULL;
    }
    opstack.pushOperand(e1);
  }

done:
  if (!opstack.reduceAll()) {
    return e0;
  }

  return opstack.getExpression();
}

ASTNode * Parser::unaryOperator() {
  switch (token) {
    case Token_LogicalNot: {
      // Negated operators
      next();
      SourceLocation loc = lexer.tokenLocation();
      ASTNode * e1 = unaryOperator();
      if (e1 == NULL)
        return NULL;
      ASTOper * result = new ASTOper(ASTNode::LogicalNot, loc);
      result->append(e1);
      return result;
    }

    case Token_Minus: {
      // Negated operators
      next();
      SourceLocation loc = lexer.tokenLocation();
      ASTNode * e1 = unaryOperator();
      if (e1 == NULL)
        return NULL;
      ASTCall * result = callOperator(&ASTIdent::operatorNegate, loc);
      result->append(e1);
      return result;
    }

    case Token_Increment:
    case Token_Decrement: {
      // Preincrement and predecrement
      TokenType tok = token;
      next();
      SourceLocation loc = lexer.tokenLocation();
      ASTNode * e1 = primaryExpression();
      if (e1 == NULL)
        return NULL;
      ASTCall * incDec = callOperator(
            tok == Token_Increment ? &ASTIdent::operatorSucc : &ASTIdent::operatorPred, loc);
      incDec->append(e1);
      return new ASTOper(ASTNode::Assign, e1, incDec);
    }

    default:
      return primaryExpression();
  }
}

ASTNode * Parser::primaryExpression() {
  ASTNode * result = NULL;

  switch (token) {
    case Token_LParen:
      next();
      result = expression();
      // Match generator expression here...
      if (!match(Token_RParen)) {
        expectedCloseParen();
        return NULL;
      }
      break;

    case Token_Integer:
      result = parseIntegerLiteral();
      break;

    case Token_Float:
      result = parseFloatLiteral();
      break;

    case Token_Ident: {
      const char * ident = matchIdent();
      result = new ASTIdent(matchLoc, ident);
      break;
    }

    case Token_Super:
      next();
      result = new ASTOper(ASTNode::Super, lexer.tokenLocation());
      break;

    case Token_String:
      result = parseStringLiteral();
      break;

    case Token_Char:
      result = parseCharLiteral();
      break;

    case Token_True: {
      SourceLocation loc = lexer.tokenLocation();
      next();
      return new ASTBoolLiteral(loc, true);
    }

    case Token_False: {
      SourceLocation loc = lexer.tokenLocation();
      next();
      return new ASTBoolLiteral(loc, false);
    }

    case Token_Null: {
      SourceLocation loc = lexer.tokenLocation();
      next();
      return new ASTNode(ASTNode::Null, loc);
    }

    case Token_Function: {
      next();
      result = functionDeclaration(ASTNode::AnonFn, "", DeclModifiers());
      break;
    }

      //return ConstantNullPtr::get(&PrimitiveType::NullType);

#if 0
    case Token_TypeOf: {
      next();
      SourceLocation loc = lexer.tokenLocation();

      if (!match(Token_LParen)) {
        expected("'('");
        return NULL;
      }

      ASTNode * arg = expression();
      if (arg == NULL) {
        expectedExpression();
        return NULL;
      }

      if (!match(Token_RParen)) {
        expectedCloseParen();
        return NULL;
      }

      /*Type * type = typeExpression(false);
      if (type == NULL) {
          return NULL;
      }*/
      result = new ASTOper(ASTNode::Typeof, arg);
      //result->setType(Builtins::typeType);
      break;
    }
#endif

    case Token_LBracket:
      next();
      result = arrayLiteral();
      break;

    default:
      if (token >= Token_BoolType && token <= Token_VoidType) {
        result = builtInTypeName(token);
        //result = new ASTIdent(lexer.tokenLocation(),
        //    istrings.intern(lexer.tokenValue().c_str()));
        token = lexer.next();
      }

      break;
  }

  // Suffix operators
  if (result) {
    for (;;) {
      SourceLocation loc = lexer.tokenLocation();
      if (match(Token_LParen)) {
        ASTNodeList argList;
        if (!parseArgumentList(argList))
          return NULL;
        result = new ASTCall(loc, result, argList);
      } else if (match(Token_LBracket)) {
        // Array dereference
        ASTOper * indexop = new ASTOper(ASTNode::GetElement, result->location());
        indexop->append(result);
        if (!parseArrayIndices(indexop))
          return NULL;
        result = indexop;
      //} else if (match(Token_BeginTmplArgs)) {
      //  // Template specialization
      //  ASTNodeList templateArgs;
      //  templateArgList(templateArgs);
      //  result = new ASTSpecialize(result->location(), result, templateArgs);
      } else if (match(Token_Dot)) {
        // Member dereference
        const char * ident = matchIdent();
        if (ident == NULL) {
          expectedIdentifier();
        }

        result = new ASTMemberRef(loc | result->location(), result, ident);
      } else {
        break;
      }
    }

    if (token == Token_Increment || token == Token_Decrement) {
      // Preincrement and predecrement
      TokenType tok = token;
      next();
      SourceLocation loc = lexer.tokenLocation();
      ASTCall * incDec = callOperator(
            tok == Token_Increment ? &ASTIdent::operatorSucc : &ASTIdent::operatorPred, loc);
      incDec->append(result);
      result = new ASTOper(ASTNode::PostAssign, result, incDec);
    }
  }

  return result;
}

bool Parser::parseArgumentList(ASTNodeList & args) {

  // Check for empty argument list
  if (match(Token_RParen))
    return true;

  // Parse individual arguments
  bool ok = true;
  for (;;) {
    ASTNode * arg = expression();
    if (arg == NULL) {
      expected("expression or closing ')'");
      return NULL;
    }

    // Check for keyword argument
    if (match(Token_Assign)) {
      // Keyword argument
      ASTNode * kwarg = expression();
      if (arg->nodeType() != ASTNode::Id) {
        diag.error(arg->location()) << "invalid keyword expression";
        ok = false;
      } else {
        const char * kwname = ((ASTIdent *)arg)->value();
        arg = new ASTKeywordArg(arg->location() | kwarg->location(), kwarg, kwname);
      }
    }

    args.push_back(arg);
    if (match(Token_RParen))
      break;

    if (!match(Token_Comma)) {
      expected("',' or ')'");
      return NULL;
    }
  }

  if (ok) {
    // Validate keyword arguments
    bool kwArg = false;
    for (ASTNodeList::const_iterator it = args.begin(); it != args.end();
        ++it) {
      const ASTNode * arg = *it;
      if (arg->nodeType() == ASTNode::Keyword) {
        kwArg = true;
      } else if (kwArg) {
        diag.error(arg->location()) << "positional arguments must come before all keyword args";
        return NULL;
      }
    }

    return true;
  }

  return false;
}

ASTNode * Parser::arrayLiteral() {
  ASTOper * arglist = new ASTOper(ASTNode::ArrayLiteral, lexer.tokenLocation());

  // Check for empty argument list
  if (match(Token_RBracket))
    return arglist;

  // Parse individual arguments
  bool ok = true;
  for (;;) {
    ASTNode * arg = expression();
    if (arg == NULL) {
      expectedCloseBracket();
      return NULL;
    }

    arglist->append(arg);
    if (match(Token_RBracket))
      break;

    if (!match(Token_Comma)) {
      expected("',' or ')'");
      return NULL;
    }
  }

  return arglist;
}

bool Parser::parseArrayIndices(ASTOper * arrayExpr) {
  if (match(Token_RBracket))
    return true;

  for (;;) {
    ASTNode * arg = expression();
    if (arg == NULL) {
      expectedCloseBracket();
      return NULL;
    }

    arrayExpr->append(arg);
    if (match(Token_RBracket))
      return true;

    if (!match(Token_Comma)) {
      expected("',' or ']'");
      return NULL;
    }
  }
  return false;
}

ASTNode * Parser::parseIntegerLiteral() {
  int numberBase = 10;

  // Copy to narrow string cause that's what LLVM uses.
  std::string tokenVal = lexer.tokenValue();
  SourceLocation loc = lexer.tokenLocation();
  next();

  // Check for hex number
  if (tokenVal.size() >= 2 && tokenVal[0] == '0' &&
      (tokenVal[1] == 'x' || tokenVal[1] == 'X')) {
    tokenVal.erase(tokenVal.begin(), tokenVal.begin() + 2);
    numberBase = 16;
  }

  // Figure out how many bits we need.
  uint32_t bits = llvm::APInt::getBitsNeeded(tokenVal, numberBase) + 1;
  if (bits <= 32) {
    bits = 32;
  } else if (bits <= 64) {
    bits = 64;
  } else if (bits <= 128) {
    bits = 128;
  } else {
    diag.error(loc) << "Integer constant > 128 bits: (" << bits << " bits)";
  }

  return new ASTIntegerLiteral(loc, llvm::APInt(bits, tokenVal, numberBase));
}

ASTNode * Parser::parseFloatLiteral() {
  // TODO: Handle long doubles.
  std::string tokenVal = lexer.tokenValue();
  SourceLocation loc = lexer.tokenLocation();
  next();

  char lastCh = lastChar(tokenVal);
  bool isSingle = false;
  if (lastCh == 'f' || lastCh == 'F') {
    isSingle = true;
    tokenVal.erase(tokenVal.end() - 1, tokenVal.end());
  }

  llvm::APFloat value(
      isSingle ? llvm::APFloat::IEEEsingle : llvm::APFloat::IEEEdouble,
      llvm::APFloat::fcZero, false);
  llvm::APFloat::opStatus status = value.convertFromString(tokenVal.c_str(),
      llvm::APFloat::rmNearestTiesToEven);

  if (status != llvm::APFloat::opOK) {
    diag.warn(lexer.tokenLocation()) << "conversion error";
  }

  return new ASTFloatLiteral(loc, value);
}

ASTNode * Parser::parseStringLiteral() {
  ASTNode * result = new ASTStringLiteral(lexer.tokenLocation(),
      lexer.tokenValue());
  next();
  return result;
}

ASTNode * Parser::parseCharLiteral() {
  if (lexer.tokenValue().size() > 1) {
    diag.error(lexer.tokenLocation()) << "multi-character constant";
  }

  ASTNode * result = new ASTCharLiteral(lexer.tokenLocation(), (int)lexer.tokenValue()[0]);
  next();
  return result;
}

void Parser::unexpectedToken() {
  diag.error(lexer.tokenLocation()) << "Unexpected token " << GetTokenName(token);
}

bool Parser::needSemi() {
  if (!match(Token_Semi)) {
    expectedSemicolon();
    return false;
  }
  return true;
}

void Parser::expectedImportPath() {
  expected("import path");
}

void Parser::expectedDeclaration() {
  expected("declaration");
}

void Parser::expectedExpression() {
  expected("expression");
}

void Parser::expectedSemicolon() {
  expected("semicolon");
}

void Parser::expectedStatement() {
  expected("statement");
}

void Parser::expectedIdentifier() {
  expected("identifier");
}

void Parser::expectedCloseParen() {
  expected("closing ')'");
}

void Parser::expectedCloseBracket() {
  expected("closing ']'");
}

void Parser::expected(const char * what) {
  diag.error(lexer.tokenLocation()) << "Expected " << what << ", not " << GetTokenName(token);
}

}

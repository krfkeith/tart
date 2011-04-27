/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"

#include "tart/AST/ASTNode.h"
#include "tart/AST/ASTDecl.h"
#include "tart/AST/Stmt.h"

#include "tart/Defn/TypeDefn.h"

#include "tart/Meta/ASTReader.h"
#include "tart/Meta/VarInt.h"

#include "tart/Type/PrimitiveType.h"

#define READER_DEBUG 0

namespace tart {

using namespace llvm;

#define ENSURE_VALID(n) if ((n) == NULL || (n)->isInvalid()) return &ASTNode::INVALID
#define ENSURE_VALID_OR_NULL(n) if (n != NULL && n->isInvalid()) return &ASTNode::INVALID
#define VERIFY_VALID(n) if (n == NULL || n->isInvalid()) return false
#define VERIFY_VALID_OR_NULL(n) if (n != NULL && n->isInvalid()) return false

// -------------------------------------------------------------------
// DeclContent

struct DeclContent {
  DeclModifiers mods;
  ASTParamList params;
  ASTNodeList templateParams;
  ASTNodeList baseTypes;
  ASTNodeList attributes;
  ASTNodeList constraints;
  ASTNodeList imports;
  ASTDeclList members;
  ASTNode * body;
  ASTNode * init;
  uint32_t paramFlags;

  DeclContent() : body(NULL), init(NULL), paramFlags(0) {}
};

// -------------------------------------------------------------------
// ASTReader

bool ASTReader::readAll(ASTNodeList & out) {
  while (pos_ < end_) {
    out.push_back(read());
  }
  return true;
}

ASTNode * ASTReader::read() {
  meta::AST::Tag tag = readTag();
#if READER_DEBUG
  AutoIndent ai;
  diag.debug() << "tag: " << tag;
#endif
  switch (tag) {
    case meta::AST::NONE:
      return NULL;

    case meta::AST::END:
      break;

    case meta::AST::ID_DEF: {
      const char * id = readIdDef();
      return new ASTIdent(ASTNode::Id, loc_, id);
    }

    case meta::AST::ID_REF: {
      const char * id = readIdRef();
      return new ASTIdent(ASTNode::Id, loc_, id);
    }

    case meta::AST::QID_DEF: {
      const char * id = readIdDef();
      return new ASTIdent(ASTNode::QName, loc_, id);
    }

    case meta::AST::QID_REF: {
      const char * id = readIdRef();
      return new ASTIdent(ASTNode::QName, loc_, id);
    }

    // Constants

    case meta::AST::CONST_NULL:
      return new ASTNode(ASTNode::Null, loc_);

    case meta::AST::CONST_INT: {
      llvm::StringRef str = readStringRef();
      uint32_t bits = llvm::APInt::getBitsNeeded(str, 16) + 1;
      if (bits <= 32) {
        bits = 32;
      } else if (bits <= 64) {
        bits = 64;
      } else if (bits <= 128) {
        bits = 128;
      } else {
        diag.error(loc_) << "Integer constant > 128 bits: (" << bits << " bits)";
      }
      return new ASTIntegerLiteral(loc_, APInt(bits, str, 16));
    }

    case meta::AST::CONST_FLOAT:
      break;

    case meta::AST::CONST_DOUBLE:
      break;

    case meta::AST::CONST_STRING: {
      StringRef str = readStringRef();
      return new ASTStringLiteral(loc_, str);
    }

    case meta::AST::CONST_CHAR: {
      uint64_t charVal = readVarInt();
      return new ASTCharLiteral(loc_, uint32_t(charVal));
    }

    case meta::AST::CONST_BOOL_FALSE:
      return new ASTBoolLiteral(loc_, false);

    case meta::AST::CONST_BOOL_TRUE:
      return new ASTBoolLiteral(loc_, true);

    // Special forms

    case meta::AST::CALL: {
      ASTNode * fn = read();
      ENSURE_VALID(fn);
      ASTNodeList args;
      if (!readNodeList(args)) {
        return &ASTNode::INVALID;
      }
      return new ASTCall(loc_, fn, args);
    }

    case meta::AST::MEMBER_REF: {
      const char * name = readId();
      ASTNode * baseExpr = read();
      ENSURE_VALID(baseExpr);
      return new ASTMemberRef(loc_, baseExpr, name);
    }

    case meta::AST::GET_ELEMENT: {
      ASTNodeList args;
      if (!readNodeList(args)) {
        return &ASTNode::INVALID;
      }
      return new ASTOper(ASTNode::GetElement, loc_, args);
    }

    case meta::AST::SPECIALIZE: {
      ASTNode * templateExpr = read();
      ENSURE_VALID(templateExpr);
      ASTNodeList args;
      if (!readNodeList(args)) {
        return &ASTNode::INVALID;
      }
      return new ASTSpecialize(loc_, templateExpr, args);
    }

    case meta::AST::ARRAY_LITERAL: {
      ASTNodeList args;
      if (!readNodeList(args)) {
        return &ASTNode::INVALID;
      }
      return new ASTOper(ASTNode::ArrayLiteral, loc_, args);
    }

    case meta::AST::KEYWORD_ARG: {
      const char * kw = readId();
      ASTNode * arg = read();
      ENSURE_VALID(arg);
      return new ASTKeywordArg(loc_, arg, kw);
    }

    case meta::AST::SUPER:
      return new ASTOper(ASTNode::Super, loc_);

    case meta::AST::ANONFN: {
      ASTNode * returnType = read();
      ENSURE_VALID_OR_NULL(returnType);
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.init == NULL);
      DASSERT(content.baseTypes.empty());
      DASSERT(content.constraints.empty());
      DASSERT(content.imports.empty());
      DASSERT(content.members.empty());
      DASSERT(content.templateParams.empty());

      ASTFunctionDecl * result = new ASTFunctionDecl(
          ASTNode::AnonFn, loc_, "", content.params, returnType, content.mods);
      result->setBody(cast_or_null<Stmt>(content.body));
      return result;
    }

    // Unary ops

    case meta::AST::NEGATE:
    case meta::AST::LOGICAL_NOT:
    case meta::AST::COMPLEMENT: {
      ASTNode * arg = read();
      ENSURE_VALID(arg);
      ASTNode::NodeType nt = ASTNode::Invalid;
      switch (int(tag)) {
        case meta::AST::NEGATE: nt = ASTNode::Negate; break;
        case meta::AST::LOGICAL_NOT: nt = ASTNode::LogicalNot; break;
        case meta::AST::COMPLEMENT: nt = ASTNode::Complement; break;
      }
      return new ASTOper(nt, loc_, arg);
    }

    // Binary ops
    case meta::AST::ASSIGN:
    case meta::AST::ASSIGN_ADD:
    case meta::AST::ASSIGN_SUB:
    case meta::AST::ASSIGN_MUL:
    case meta::AST::ASSIGN_DIV:
    case meta::AST::ASSIGN_MOD:
    case meta::AST::ASSIGN_BITAND:
    case meta::AST::ASSIGN_BITOR:
    case meta::AST::ASSIGN_BITXOR:
    case meta::AST::ASSIGN_RSH:
    case meta::AST::ASSIGN_LSH:
    case meta::AST::POST_ASSIGN:
    case meta::AST::LOGICAL_OR:
    case meta::AST::LOGICAL_AND:
    case meta::AST::IS:
    case meta::AST::IS_NOT:
    case meta::AST::IN:
    case meta::AST::NOT_IN:
    case meta::AST::ISA: {
      ASTNode::NodeType nt = ASTNode::Invalid;
      switch (int(tag)) {
        case meta::AST::ASSIGN: nt = ASTNode::Assign; break;
        case meta::AST::ASSIGN_ADD: nt = ASTNode::AssignAdd; break;
        case meta::AST::ASSIGN_SUB: nt = ASTNode::AssignSub; break;
        case meta::AST::ASSIGN_MUL: nt = ASTNode::AssignMul; break;
        case meta::AST::ASSIGN_DIV: nt = ASTNode::AssignDiv; break;
        case meta::AST::ASSIGN_MOD: nt = ASTNode::AssignMod; break;
        case meta::AST::ASSIGN_BITAND: nt = ASTNode::AssignBitAnd; break;
        case meta::AST::ASSIGN_BITOR: nt = ASTNode::AssignBitOr; break;
        case meta::AST::ASSIGN_BITXOR: nt = ASTNode::AssignBitXor; break;
        case meta::AST::ASSIGN_RSH: nt = ASTNode::AssignRSh; break;
        case meta::AST::ASSIGN_LSH: nt = ASTNode::AssignLSh; break;
        case meta::AST::POST_ASSIGN: nt = ASTNode::PostAssign; break;
        case meta::AST::LOGICAL_OR: nt = ASTNode::LogicalOr; break;
        case meta::AST::LOGICAL_AND: nt = ASTNode::LogicalAnd; break;
        case meta::AST::IS: nt = ASTNode::Is; break;
        case meta::AST::IS_NOT: nt = ASTNode::IsNot; break;
        case meta::AST::IN: nt = ASTNode::In; break;
        case meta::AST::NOT_IN: nt = ASTNode::NotIn; break;
        case meta::AST::ISA: nt = ASTNode::IsInstanceOf; break;
      }

      ASTNode * a0 = read();
      ENSURE_VALID(a0);
      ASTNode * a1 = read();
      ENSURE_VALID(a1);
      return new ASTOper(nt, a0, a1);
    }

    // Built-in type definitions

    case meta::AST::VOID:
      return &VoidType::biDef;

    case meta::AST::NIL:
      return &NullType::biDef;

    case meta::AST::BOOL:
      return &BoolType::biDef;

    case meta::AST::CHAR:
      return &CharType::biDef;

    case meta::AST::INT8:
      return &Int8Type::biDef;

    case meta::AST::INT16:
      return &Int16Type::biDef;

    case meta::AST::INT32:
      return &Int32Type::biDef;

    case meta::AST::INT64:
      return &Int64Type::biDef;

    case meta::AST::UINT8:
      return &UInt8Type::biDef;

    case meta::AST::UINT16:
      return &UInt16Type::biDef;

    case meta::AST::UINT32:
      return &UInt32Type::biDef;

    case meta::AST::UINT64:
      return &UInt64Type::biDef;

    case meta::AST::FLOAT:
      return &FloatType::biDef;

    case meta::AST::DOUBLE:
      return &DoubleType::biDef;

    // Derived types

    case meta::AST::ARRAY: {
      ASTNode * ty = read();
      ENSURE_VALID(ty);
      return new ASTOper(ASTNode::Array, loc_, ty);
    }

    case meta::AST::ADDRESS: {
      ASTNode * ty = read();
      ENSURE_VALID(ty);
      return new ASTOper(ASTNode::Address, loc_, ty);
    }

    case meta::AST::NARRAY: {
      ASTNode * ty = read();
      ENSURE_VALID(ty);
      ASTNode * size = read();
      ENSURE_VALID(size);
      return new ASTOper(ASTNode::NativeArray, ty, size);
    }

    case meta::AST::FLEXARRAY: {
      ASTNode * ty = read();
      ENSURE_VALID(ty);
      return new ASTOper(ASTNode::FlexArray, loc_, ty);
    }

    case meta::AST::TUPLE: {
      ASTNodeList nodes;
      if (!readNodeList(nodes)) {
        return &ASTNode::INVALID;
      }
      return new ASTOper(ASTNode::Tuple, loc_, nodes);
    }

    case meta::AST::UNION: {
      ASTNodeList nodes;
      if (!readNodeList(nodes)) {
        return &ASTNode::INVALID;
      }
      return new ASTOper(ASTNode::LogicalOr, loc_, nodes);
    }

    case meta::AST::FNTYPE: {
      ASTNode * returnType = read();
      ENSURE_VALID(returnType);
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.init == NULL);
      DASSERT(content.baseTypes.empty());
      DASSERT(content.constraints.empty());
      DASSERT(content.imports.empty());
      DASSERT(content.members.empty());
      DASSERT(content.templateParams.empty());

      ASTFunctionDecl * result = new ASTFunctionDecl(
          ASTNode::AnonFn, loc_, "", content.params, returnType, content.mods);
      return result;
    }

    case meta::AST::TYPEVAR: {
      const char * name = readId();
      ASTTypeVariable::ConstraintType constraint = ASTTypeVariable::IS_INSTANCE;
      ASTNode * constraintValue = NULL;
      bool isVariadic = false;
      while (pos_ < end_) {
        meta::AST::Tag tag = meta::AST::Tag(*pos_);
        if (tag == meta::AST::END) {
          pos_++;
          return new ASTTypeVariable(loc_, name, constraintValue, constraint, isVariadic);
        } else if (tag == meta::AST::VARIADIC) {
          pos_++;
          isVariadic = true;
        } else if (tag == meta::AST::ISA) {
          pos_++;
          constraint = ASTTypeVariable::IS_INSTANCE;
          constraintValue = read();
        } else if (tag == meta::AST::IS_SUBTYPE) {
          pos_++;
          constraint = ASTTypeVariable::IS_SUBTYPE;
          constraintValue = read();
        } else if (tag == meta::AST::IS_SUPERTYPE) {
          pos_++;
          constraint = ASTTypeVariable::IS_SUPERTYPE;
          constraintValue = read();
        } else {
          diag.fatal(loc_) << "Invalid tag for type variable: " << tag;
          return &ASTNode::INVALID;
        }
      }

      diag.fatal(loc_) << "Premature end of stream";
      return &ASTNode::INVALID;
    }

    case meta::AST::TYPELITERAL:
      break;

    // Declarations
    case meta::AST::CLASS:
    case meta::AST::STRUCT:
    case meta::AST::INTERFACE:
    case meta::AST::PROTOCOL:
    case meta::AST::ENUM:
    {
      const char * name = readId();
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.body == NULL);
      DASSERT(content.init == NULL);
      DASSERT(content.params.empty());
      DASSERT(content.templateParams.empty());

      ASTNode::NodeType nt;
      switch (int(tag)) {
        case meta::AST::CLASS: nt = ASTNode::Class; break;
        case meta::AST::STRUCT: nt = ASTNode::Struct; break;
        case meta::AST::INTERFACE: nt = ASTNode::Interface; break;
        case meta::AST::PROTOCOL: nt = ASTNode::Protocol; break;
        case meta::AST::ENUM: nt = ASTNode::Enum; break;
      }

      ASTTypeDecl * result = new ASTTypeDecl(nt, loc_, name, content.baseTypes, content.mods);
      result->attributes().append(content.attributes.begin(), content.attributes.end());
      result->imports().append(content.imports.begin(), content.imports.end());
      result->members().append(content.members.begin(), content.members.end());
      return result;
    }

    case meta::AST::TYPEALIAS: {
//      ASTNode * type = read();
//      ENSURE_VALID(type);
//      ASTNodeList bases;
//      bases.push_back(type);
//      return new ASTTypeDecl(ASTNode::TypeAlias, loc, declName, bases, mods);
      DFAIL("Implement");
      break;
    }

    case meta::AST::FUNCTION:
    case meta::AST::UNDEF:
    case meta::AST::OVERRIDE:
    case meta::AST::MACRO: {
      const char * name = readId();
      ASTNode * returnType = read();
      if (returnType == &ASTNode::INVALID) {
        return &ASTNode::INVALID;
      }
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.init == NULL);
      DASSERT(content.baseTypes.empty());
      DASSERT(content.constraints.empty());
      DASSERT(content.imports.empty());
      DASSERT(content.members.empty());
      DASSERT(content.templateParams.empty());

      ASTNode::NodeType nt = ASTNode::Function;
      if (tag == meta::AST::MACRO) {
        nt = ASTNode::Macro;
      }
      if (tag == meta::AST::UNDEF) {
        content.mods.flags |= Undef;
      }
      if (tag == meta::AST::OVERRIDE) {
        content.mods.flags |= Override;
      }

      ASTFunctionDecl * result = new ASTFunctionDecl(nt, loc_, name, content.params, returnType,
          content.mods);
      result->attributes().append(content.attributes.begin(), content.attributes.end());
      result->setBody(cast_or_null<Stmt>(content.body));
      return result;
    }

    case meta::AST::PARAM: {
      const char * name = readId();
      ASTNode * ty = read();
      if (ty == &ASTNode::INVALID) {
        return &ASTNode::INVALID;
      }
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.body == NULL);
      DASSERT(content.params.empty());
      DASSERT(content.templateParams.empty());
      DASSERT(content.baseTypes.empty());
      DASSERT(content.constraints.empty());
      DASSERT(content.imports.empty());
      DASSERT(content.members.empty());

      ASTParameter * result = new ASTParameter(loc_, name, ty, content.init, content.paramFlags);
      result->attributes().append(content.attributes.begin(), content.attributes.end());
      return result;
    }

    case meta::AST::PROP:
    case meta::AST::IDX: {
      const char * name = readId();
      ASTNode * ty = read();
      if (ty == &ASTNode::INVALID) {
        return &ASTNode::INVALID;
      }
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.body == NULL);
      DASSERT(content.init == NULL);
      if (tag == meta::AST::IDX) {
        DASSERT(!content.params.empty());
      } else {
        DASSERT(content.params.empty());
      }
      DASSERT(content.templateParams.empty());
      DASSERT(content.baseTypes.empty());
      DASSERT(content.constraints.empty());
      DASSERT(content.imports.empty());

      ASTNode::NodeType nt = (tag == meta::AST::PROP) ? ASTNode::Prop : ASTNode::Idx;
      ASTPropertyDecl * result = new ASTPropertyDecl(nt, loc_, name, ty, content.mods);
      result->attributes().append(content.attributes.begin(), content.attributes.end());
      result->params().append(content.params.begin(), content.params.end());

      // Accessors
      for (ASTDeclList::const_iterator it = content.members.begin();
          it != content.members.end(); ++it) {
        ASTFunctionDecl * accessor = cast<ASTFunctionDecl>(*it);
        if (accessor->name() == istrings.idGet) {
          DASSERT(result->getter() == NULL);
          result->setGetter(accessor);
        } else if (accessor->name() == istrings.idSet) {
          DASSERT(result->setter() == NULL);
          result->setSetter(accessor);
        } else {
          diag.error(loc_) << "Invalid accessor name: " << accessor->name();
        }
      }

      return result;
    }

    case meta::AST::LET:
    case meta::AST::VAR: {
      const char * name = readId();
      ASTNode * ty = read();
      if (ty == &ASTNode::INVALID) {
        return &ASTNode::INVALID;
      }
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.body == NULL);
      DASSERT(content.params.empty());
      DASSERT(content.templateParams.empty());
      DASSERT(content.baseTypes.empty());
      DASSERT(content.constraints.empty());
      DASSERT(content.imports.empty());
      DASSERT(content.members.empty());

      ASTNode::NodeType nt = (tag == meta::AST::LET) ? ASTNode::Let : ASTNode::Var;
      ASTVarDecl * result = new ASTVarDecl(nt, loc_, name, ty, content.init, content.mods);
      result->attributes().append(content.attributes.begin(), content.attributes.end());
      return result;
    }

    case meta::AST::VARLIST: {
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.params.empty());
      DASSERT(content.baseTypes.empty());
      DASSERT(content.constraints.empty());
      DASSERT(content.imports.empty());
      DASSERT(content.templateParams.empty());
      DASSERT(content.body == NULL);
      DASSERT(content.paramFlags == 0);

      ASTVarDecl * result = new ASTVarDecl(ASTNode::VarList, loc_, "vlist", NULL,
          content.init, content.mods);
      result->members().append(content.members.begin(), content.members.end());
      result->attributes().append(content.attributes.begin(), content.attributes.end());
      return result;
    }

    case meta::AST::TEMPLATE: {
      const char * name = readId();
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      if (content.body == NULL) {
        diag.error(loc_) << "Missing template body for " << name;
        return &ASTNode::INVALID;
      }

      DASSERT(content.init == NULL);
      DASSERT(content.baseTypes.empty());
      DASSERT(content.imports.empty());
      DASSERT(content.members.empty());
      DASSERT(content.params.empty());

      ASTTemplate * result = new ASTTemplate(cast<ASTDecl>(content.body), content.templateParams,
          content.constraints);
      return result;
    }

    case meta::AST::NAMESPACE: {
      const char * name = readId();
      DeclContent content;
      if (!readDecl(content)) {
        return &ASTNode::INVALID;
      }

      DASSERT(content.body == NULL);
      DASSERT(content.init == NULL);
      DASSERT(content.params.empty());
      DASSERT(content.templateParams.empty());
      DASSERT(content.baseTypes.empty());
      DASSERT(content.constraints.empty());
      DASSERT(content.attributes.empty());

      ASTNamespace * result = new ASTNamespace(loc_, name);
      result->imports().append(content.imports.begin(), content.imports.end());
      result->members().append(content.members.begin(), content.members.end());
      return result;
    }

    case meta::AST::IMPORT:
    case meta::AST::IMPORT_NS: {
      ASTNode * path = read();
      ENSURE_VALID(path);
      const char * name = readId();
      DASSERT(name != NULL);
      return new ASTImport(loc_, path, name, tag == meta::AST::IMPORT_NS);
    }

    // Statements

    case meta::AST::BLOCK: {
      BlockStmt * result = new BlockStmt(loc_);
      if (!readStmtList(result->stmts())) {
        return &ASTNode::INVALID;
      }
      return result;
    }

    case meta::AST::IF: {
      ASTNode * test = read();
      ENSURE_VALID(test);
      Stmt * thenSt = readStmt();
      ENSURE_VALID(thenSt);
      Stmt * elseSt = readOptionalStmt();
      ENSURE_VALID_OR_NULL(elseSt);
      return new IfStmt(loc_, test, thenSt, elseSt);
    }

    case meta::AST::WHILE: {
      ASTNode * test = read();
      Stmt * body = readStmt();
      return new WhileStmt(loc_, test, body);
    }

    case meta::AST::DO_WHILE: {
      ASTNode * test = read();
      Stmt * body = readStmt();
      return new DoWhileStmt(loc_, test, body);
    }

    case meta::AST::FOR: {
      ASTNode * init = read();
      ENSURE_VALID_OR_NULL(init);
      ASTNode * test = read();
      ENSURE_VALID_OR_NULL(test);
      ASTNode * incr = read();
      ENSURE_VALID_OR_NULL(incr);
      Stmt * body = readStmt();
      ENSURE_VALID_OR_NULL(body);
      return new ForStmt(loc_, init, test, incr, body);
    }

    case meta::AST::FOR_EACH: {
      ASTNode * lvars = read();
      ENSURE_VALID(lvars);
      ASTNode * iter = read();
      ENSURE_VALID(iter);
      Stmt * body = readStmt();
      ENSURE_VALID_OR_NULL(body);
      if (ASTVarDecl * vars = dyn_cast<ASTVarDecl>(lvars)) {
        return new ForEachStmt(loc_, vars, iter, body);
      } else {
        diag.error(loc_) << "Invalid node type for iteration vars: " << lvars;
        return &ASTNode::INVALID;
      }
    }

    case meta::AST::SWITCH: {
      ASTNode * test = read();
      ENSURE_VALID(test);
      SwitchStmt * result = new SwitchStmt(loc_, test);
      readStmtList(result->caseList());
      return result;
    }

    case meta::AST::CASE: {
      ASTNodeList caseVals;
      Stmt * body = readStmt();
      ENSURE_VALID(body);
      if (!readNodeList(caseVals)) {
        return &ASTNode::INVALID;
      }
      return new CaseStmt(loc_, caseVals, body);
    }

    case meta::AST::MATCH: {
      ASTNode * test = read();
      ENSURE_VALID(test);
      MatchStmt * result = new MatchStmt(loc_, test);
      readStmtList(result->caseList());
      return result;
    }

    case meta::AST::MATCH_AS: {
      ASTNode * ast = read();
      ENSURE_VALID(ast);
      if (ASTDecl * decl = dyn_cast<ASTDecl>(ast)) {
        Stmt * body = readStmt();
        ENSURE_VALID(body);
        return new MatchAsStmt(loc_, decl, body);
      } else {
        diag.error(loc_) << "Expected a declaration: " << ast;
        return &ASTNode::INVALID;
      }
    }

    case meta::AST::THROW: {
      ASTNode * ex = read();
      ENSURE_VALID_OR_NULL(ex);
      return new ThrowStmt(loc_, ex);
    }

    case meta::AST::RETURN: {
      ASTNode * retval = read();
      ENSURE_VALID_OR_NULL(retval);
      return new ReturnStmt(loc_, retval);
    }

    case meta::AST::YIELD: {
      ASTNode * retval = read();
      ENSURE_VALID_OR_NULL(retval);
      return new YieldStmt(loc_, retval);
    }

    case meta::AST::TRY: {
      Stmt * body = readStmt();
      ENSURE_VALID_OR_NULL(body);
      TryStmt * result =  new TryStmt(loc_, body);
      while (pos_ < end_) {
        meta::AST::Tag tag = meta::AST::Tag(*pos_);
        if (tag == meta::AST::END) {
          pos_++;
          return result;
        } else if (tag == meta::AST::CATCH) {
          Stmt * st = readStmt();
          ENSURE_VALID(st);
          result->catchList().push_back(st);
        } else if (tag == meta::AST::ELSE) {
          pos_++;
          Stmt * st = readStmt();
          ENSURE_VALID(st);
          DASSERT(result->elseSt() == NULL);
          result->setElseSt(st);
        } else if (tag == meta::AST::FINALLY) {
          pos_++;
          Stmt * st = readStmt();
          ENSURE_VALID(st);
          DASSERT(result->finallySt() == NULL);
          result->setFinallySt(st);
        } else {
          break;
        }
      }

      diag.error() << "Invalid tag for 'try' block: " << tag;
      return &ASTNode::INVALID;
    }

    case meta::AST::CATCH: {
      ASTNode * ast = read();
      ENSURE_VALID(ast);
      if (ASTDecl * decl = dyn_cast<ASTDecl>(ast)) {
        Stmt * body = readStmt();
        ENSURE_VALID(body);
        return new CatchStmt(loc_, decl, body);
      } else {
        diag.error(loc_) << "Expected an exception declaration: " << ast;
        return &ASTNode::INVALID;
      }
    }

    case meta::AST::BREAK:
      return new Stmt(ASTNode::Break, loc_);

    case meta::AST::CONTINUE:
      return new Stmt(ASTNode::Continue, loc_);

    case meta::AST::EXPR: {
      ASTNode * exp = read();
      ENSURE_VALID(exp);
      return new ExprStmt(ASTNode::Expression, loc_, exp);
    }

    case meta::AST::LOCAL_DECL: {
      ASTNode * ast = read();
      ENSURE_VALID(ast);
      if (ASTDecl * decl = dyn_cast<ASTDecl>(ast)) {
        return new DeclStmt(loc_, decl);
      } else {
        diag.error(loc_) << "Expected a declaration: " << ast;
        return &ASTNode::INVALID;
      }
    }

    // Visibility

    case meta::AST::PRIVATE:
    case meta::AST::PROTECTED:
    case meta::AST::INTERNAL:
      diag.error() << "Visibility outside of declaration: " << tag;
      return &ASTNode::INVALID;

    // Modifiers

    case meta::AST::STATIC:
    case meta::AST::ABSTRACT:
    case meta::AST::FINAL:
    case meta::AST::READONLY:
    case meta::AST::VARIADIC:
    case meta::AST::KEYWORDONLY:
    case meta::AST::EXPANDED:
      diag.error() << "Modifier outside of declaration: " << tag;
      return &ASTNode::INVALID;

    // Misc

    case meta::AST::ATTRIBUTE:
      diag.error() << "Attribute outside of declaration: " << tag;
      return &ASTNode::INVALID;

    case meta::AST::ELSE:
      diag.error() << "Invalid tag for this context: " << tag;
      return &ASTNode::INVALID;

    default:
      break;
  }

  diag.fatal() << "Implement deserialization of " << tag;
  return &ASTNode::INVALID;
}

bool ASTReader::readDecl(DeclContent & dc) {
#if READER_DEBUG
  AutoIndent ai;
#endif
  while (pos_ < end_) {
    meta::AST::Tag tag = meta::AST::Tag(*pos_);
#if READER_DEBUG
    diag.debug() << "declTag: " << tag;
#endif

    switch (tag) {
      case meta::AST::END:
        pos_++;
        return true;

      case meta::AST::PRIVATE:
        pos_++;
        dc.mods.visibility = Private;
        break;

      case meta::AST::PROTECTED:
        pos_++;
        dc.mods.visibility = Protected;
        break;

      case meta::AST::INTERNAL:
        pos_++;
        dc.mods.visibility = Internal;
        break;

      case meta::AST::STATIC:
        pos_++;
        dc.mods.flags |= Static;
        break;

      case meta::AST::FINAL:
        pos_++;
        dc.mods.flags |= Final;
        break;

      case meta::AST::ABSTRACT:
        pos_++;
        dc.mods.flags |= Abstract;
        break;

      case meta::AST::READONLY:
        pos_++;
        dc.mods.flags |= ReadOnly;
        break;

      case meta::AST::VARIADIC:
        pos_++;
        dc.paramFlags |= Param_Variadic;
        break;

      case meta::AST::KEYWORDONLY:
        pos_++;
        dc.paramFlags |= Param_KeywordOnly;
        break;

      case meta::AST::EXPANDED:
        pos_++;
        dc.paramFlags |= Param_Star;
        break;

      case meta::AST::PARAM: {
        ASTNode * param = read();
        VERIFY_VALID(param);
        dc.params.push_back(cast<ASTParameter>(param));
        break;
      }

      case meta::AST::TPARAM: {
        pos_++;
        ASTNode * param = read();
        VERIFY_VALID(param);
        dc.templateParams.push_back(param);
        break;
      }

      case meta::AST::BASETYPE: {
        pos_++;
        ASTNode * base = read();
        VERIFY_VALID(base);
        dc.baseTypes.push_back(base);
        break;
      }

      case meta::AST::ATTRIBUTE: {
        pos_++;
        ASTNode * attr = read();
        VERIFY_VALID(attr);
        dc.attributes.push_back(attr);
        break;
      }

      case meta::AST::CONSTRAINT: {
        pos_++;
        ASTNode * cstr = read();
        VERIFY_VALID(cstr);
        dc.constraints.push_back(cstr);
        break;
      }

      case meta::AST::IMPORT:
      case meta::AST::IMPORT_NS: {
        ASTNode * imp = read();
        VERIFY_VALID(imp);
        dc.imports.push_back(imp);
        break;
      }

      case meta::AST::BODY: {
        pos_++;
        ASTNode * body = read();
        VERIFY_VALID(body);
        if (dc.body != NULL) {
          diag.error(loc_) << "Deserialization error: Multiple BODY tags";
        }
        dc.body = body;
        break;
      }

      case meta::AST::INIT: {
        pos_++;
        ASTNode * init = read();
        VERIFY_VALID(init);
        if (dc.init != NULL) {
          diag.error(loc_) << "Deserialization error: Multiple INIT tags";
        }
        dc.init = init;
        break;
      }

      case meta::AST::CLASS:
      case meta::AST::STRUCT:
      case meta::AST::INTERFACE:
      case meta::AST::PROTOCOL:
      case meta::AST::ENUM:
      case meta::AST::FUNCTION:
      case meta::AST::MACRO:
      case meta::AST::UNDEF:
      case meta::AST::OVERRIDE:
      case meta::AST::PROP:
      case meta::AST::IDX:
      case meta::AST::LET:
      case meta::AST::VAR:
      case meta::AST::NAMESPACE:
      case meta::AST::TEMPLATE:
      case meta::AST::TYPEALIAS: {
        ASTNode * member = read();
        VERIFY_VALID(member);
        dc.members.push_back(cast<ASTDecl>(member));
        break;
      }

      default:
        diag.fatal() << "Unexpected tag in declaration: " << tag;
        return false;
    }
  }

  return false;
}

bool ASTReader::readStmtList(llvm::SmallVectorImpl<Stmt *> & out) {
  while (pos_ < end_) {
    if (*pos_ == meta::AST::END) {
      pos_++;
      return true;
    }

    Stmt * st = readStmt();
    DASSERT(st != NULL);
    if (st->isInvalid()) {
      return false;
    }

    out.push_back(st);
  }

  return false;
}

Stmt * ASTReader::readOptionalStmt() {
  ASTNode * node = read();
  if (node == NULL) {
    return NULL;
  }

  if (node->isInvalid()) {
    return static_cast<Stmt *>(&ASTNode::INVALID);
  }

  if (Stmt * st = dyn_cast<Stmt>(node)) {
    return st;
  }

  diag.error(loc_) << "Expected statement, not: " << node;
  return static_cast<Stmt *>(&ASTNode::INVALID);
}

Stmt * ASTReader::readStmt() {
  ASTNode * node = read();
  if (node == NULL || node->isInvalid()) {
    return static_cast<Stmt *>(&ASTNode::INVALID);
  }

  if (Stmt * st = dyn_cast<Stmt>(node)) {
    return st;
  }

  diag.error(loc_) << "Expected statement, not: " << node;
  return static_cast<Stmt *>(&ASTNode::INVALID);
}

bool ASTReader::readNodeList(ASTNodeList & out) {
  while (pos_ < end_) {
    if (*pos_ == meta::AST::END) {
      pos_++;
      return true;
    }

    ASTNode * node = read();
    if (node != NULL && node->isInvalid()) {
      return false;
    }

    out.push_back(node);
  }

  return false;
}

meta::AST::Tag ASTReader::readTag() {
  if (pos_ < end_) {
    return meta::AST::Tag(*pos_++);
  }

  return meta::AST::NONE;
}

uint64_t ASTReader::readVarInt() {
  DASSERT(pos_ < end_);
  uint64_t value = *pos_++;
  if (value < VARINT_2BYTE_PREFIX) {
    return value;
  } else if (value < VARINT_3BYTE_PREFIX) {
    DASSERT(pos_ < end_);
    value = (value & ~VARINT_2BYTE_PREFIX) << 8;
    value |= *pos_++;
    return value;
  } else {
    DASSERT(pos_ + 1 < end_);
    value = (value & ~VARINT_3BYTE_PREFIX) << 16;
    value |= *pos_++ << 8;
    value |= *pos_++;
    return value;
  }
}

const char * ASTReader::readId() {
  meta::AST::Tag tag = readTag();
  AutoIndent ai;
  if (tag == meta::AST::ID_DEF) {
    const char * name = readString();
#if READER_DEBUG
    diag.debug() << "id: " << tag << " : " << name;
#endif
    idTable_.push_back(name);
    return name;
  } else if (tag == meta::AST::ID_REF) {
    uint64_t index = readVarInt();
#if READER_DEBUG
    diag.debug() << "id: " << tag << " : " << idTable_[index];
#endif
    return idTable_[index];
  } else {
    diag.error(loc_) << "Expected an identifier, found " << tag;
    return NULL;
  }
}

const char * ASTReader::readIdDef() {
  const char * name = readString();
  idTable_.push_back(name);
  return name;
}

const char * ASTReader::readIdRef() {
  uint64_t index = readVarInt();
  return idTable_[index];
}

const char * ASTReader::readString() {
  uint64_t size = readVarInt();
  llvm::StringRef ident(pos_, size);
  pos_ += size;
  if (size == 0) {
    return "";
  }
  return istrings.intern(ident);
}

llvm::StringRef ASTReader::readStringRef() {
  uint64_t size = readVarInt();
  llvm::StringRef str(pos_, size);
  pos_ += size;
  return str;
}

} // namespace tart

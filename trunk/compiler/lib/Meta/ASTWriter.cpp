/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"

#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/Module.h"

#include "tart/Meta/ASTWriter.h"
#include "tart/Meta/VarInt.h"

#include "tart/Type/PrimitiveType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/TupleType.h"
#include "tart/Type/TypeAlias.h"

#include "tart/AST/Stmt.h"

namespace tart {

using namespace llvm;

// -------------------------------------------------------------------
// ASTWriter

/** Write a type expression. */
ASTWriter & ASTWriter::write(const Type * ty) {
  if (ty == NULL) {
    write(meta::AST::NONE);
    return *this;
  }

  switch (ty->typeClass()) {
    case Type::Primitive: {
      const PrimitiveType * pt = static_cast<const PrimitiveType *>(ty);
      meta::AST::Tag tag;
      switch (int(pt->typeId())) {
        case TypeId_Void:       tag = meta::AST::VOID; break;
        case TypeId_Bool:       tag = meta::AST::BOOL; break;
        case TypeId_Char:       tag = meta::AST::CHAR; break;
        case TypeId_SInt8:      tag = meta::AST::INT8; break;
        case TypeId_SInt16:     tag = meta::AST::INT16; break;
        case TypeId_SInt32:     tag = meta::AST::INT32; break;
        case TypeId_SInt64:     tag = meta::AST::INT64; break;
        case TypeId_UInt8:      tag = meta::AST::UINT8; break;
        case TypeId_UInt16:     tag = meta::AST::UINT16; break;
        case TypeId_UInt32:     tag = meta::AST::UINT32; break;
        case TypeId_UInt64:     tag = meta::AST::UINT64; break;
        case TypeId_Float:      tag = meta::AST::FLOAT; break;
        case TypeId_Double:     tag = meta::AST::DOUBLE; break;
        case TypeId_Null:       tag = meta::AST::NIL; break;
        //case TypeId_UnsizedInt: tag = meta::AST::UNSIZED_INT; break;
        default:
          diag.fatal() << "Implement ASTWriter::typeRef: " << pt;
      }
      write(tag);
      break;
    }

    case Type::Class:
    case Type::Struct:
    case Type::Interface:
    case Type::Protocol:
    case Type::Enum: {
      const TypeDefn * td = ty->typeDefn();
      if (td->isTemplateInstance()) {
        const TemplateInstance * tinst = td->templateInstance();
        write(meta::AST::SPECIALIZE);
        writeQName(td);
        for (size_t i = 0; i < tinst->typeArgs()->size(); ++i) {
          write(tinst->typeArg(i));
        }
        write(meta::AST::END);
      } else {
        writeQName(td);
      }
      break;
    }

    case Type::Function: {
      const FunctionType * fnType = static_cast<const FunctionType *>(ty);
      write(meta::AST::FNTYPE);
      write(fnType->returnType());
      if (fnType->isStatic()) {
        write(meta::AST::STATIC);
      }
      for (ParameterList::const_iterator it = fnType->params().begin();
          it != fnType->params().end(); ++it) {
        const ParameterDefn * p = *it;
        write(meta::AST::PARAM);
        writeId(p->name() ? p->name() : "");
        writeAttributes(p);
        write(p->type());
        if (p->initValue() != NULL) {
          write(meta::AST::INITVAL);
          write(p->initValue());
        }
        write(meta::AST::END);
      }
      write(meta::AST::END);
      break;
    }

    case Type::Tuple: {
      write(meta::AST::TUPLE);
      for (size_t i = 0; i < ty->numTypeParams(); ++i) {
        write(ty->typeParam(i));
      }
      write(meta::AST::END);
      break;
    }

    case Type::Union: {
      write(meta::AST::UNION);
      for (size_t i = 0; i < ty->numTypeParams(); ++i) {
        write(ty->typeParam(i));
      }
      write(meta::AST::END);
      break;
    }

    case Type::NAddress: {
      write(meta::AST::ADDRESS);
      write(ty->typeParam(0));
      break;
    }

    case Type::NArray: {
      write(meta::AST::NARRAY);
      write(ty->typeParam(0));
      stream_ << VarInt(static_cast<const NativeArrayType *>(ty)->size());
      break;
    }

    case Type::FlexibleArray: {
      write(meta::AST::FLEXARRAY);
      write(ty->typeParam(0));
      break;
    }

    case Type::TypeLiteral: {
      write(meta::AST::TYPELITERAL);
      write(ty->typeParam(0));
      break;
    }

    case Type::Alias: {
      const TypeAlias * alias = static_cast<const TypeAlias *>(ty);
      if (alias->typeDefn() != NULL) {
        writeQName(alias->typeDefn());
      } else {
        write(alias->value());
      }
      break;
    }

    case Type::TypeVar: {
      const TypeVariable * tv = static_cast<const TypeVariable *>(ty);
      write(meta::AST::TYPEVAR);
      writeId(tv->name());
      if (tv->isVariadic()) {
        write(meta::AST::VARIADIC);
      }
      write(meta::AST::END);
      break;
    }

    default:
      diag.fatal() << "Implement serialize typeref: " << ty;
      break;
  }

  return *this;
}

ASTWriter & ASTWriter::write(const Expr * in) {
  switch (in->exprType()) {
  default:
    diag.error(in) << "Implement serialization for: " << exprTypeName(in->exprType());
  }
  return *this;
}

ASTWriter & ASTWriter::write(const ASTNode * ast) {
  if (ast == NULL) {
    write(meta::AST::NONE);
    return *this;
  }

  switch (ast->nodeType()) {
    case ASTNode::Null:
      write(meta::AST::CONST_NULL);
      break;

    case ASTNode::Id: {
      const ASTIdent * id = static_cast<const ASTIdent *>(ast);
      writeId(id->value());
      break;
    }

    case ASTNode::Super: {
      write(meta::AST::SUPER);
      break;
    }

    case ASTNode::LitBool: {
      const ASTBoolLiteral * lit = static_cast<const ASTBoolLiteral *>(ast);
      if (lit->value()) {
        write(meta::AST::CONST_BOOL_TRUE);
      } else {
        write(meta::AST::CONST_BOOL_FALSE);
      }
      break;
    }

    case ASTNode::LitInt: {
      const ASTIntegerLiteral * lit = static_cast<const ASTIntegerLiteral *>(ast);
      write(meta::AST::CONST_INT);
      llvm::SmallVector<char, 20> str;
      lit->value().toString(str, 16, false);
      writeStr(llvm::StringRef(str.data(), str.size()));
      break;
    }

    case ASTNode::LitFloat: {
      const ASTFloatLiteral * lit = static_cast<const ASTFloatLiteral *>(ast);
      const APFloat & fp = lit->value();
      write(meta::AST::CONST_FLOAT);
      // TODO: Serialize value
      //DFAIL("Implement");
      break;
    }

    case ASTNode::LitDouble: {
      const ASTDoubleLiteral * lit = static_cast<const ASTDoubleLiteral *>(ast);
      const APFloat & fp = lit->value();
      write(meta::AST::CONST_DOUBLE);
      // TODO: Serialize value
      //DFAIL("Implement");
      break;
    }

    case ASTNode::LitString: {
      const ASTStringLiteral * lit = static_cast<const ASTStringLiteral *>(ast);
      write(meta::AST::CONST_STRING);
      stream_ << VarInt(lit->value().size());
      stream_ << lit->value();
      break;
    }

    case ASTNode::LitChar: {
      const ASTCharLiteral * lit = static_cast<const ASTCharLiteral *>(ast);
      write(meta::AST::CONST_CHAR);
      stream_ << VarInt(lit->value());
      break;
    }

    case ASTNode::ArrayLiteral: {
      const ASTOper * op = static_cast<const ASTOper *>(ast);
      write(meta::AST::ARRAY_LITERAL);
      writeNodeList(op->args());
      write(meta::AST::END);
      break;
    }

    // Declarations

    case ASTNode::Class:
    case ASTNode::Struct:
    case ASTNode::Interface:
    case ASTNode::Protocol:
    case ASTNode::Enum: {
      const ASTTypeDecl * decl = static_cast<const ASTTypeDecl *>(ast);
      meta::AST::Tag tag;
      switch (int(ast->nodeType())) {
        case ASTNode::Class: tag = meta::AST::CLASS; break;
        case ASTNode::Struct: tag = meta::AST::STRUCT; break;
        case ASTNode::Interface: tag = meta::AST::INTERFACE; break;
        case ASTNode::Protocol: tag = meta::AST::PROTOCOL; break;
        case ASTNode::Enum: tag = meta::AST::ENUM; break;
      }

      write(tag);
      writeId(decl->name());
      writeDeclMods(decl);
      writeAttributes(decl);
      writeNodeList(meta::AST::BASETYPE, decl->bases());
      writeNodeList(decl->imports());
      writeNodeList(decl->members().begin(), decl->members().end());
      write(meta::AST::END);
      break;
    }

    case ASTNode::TypeAlias: {
      DFAIL("Implement");
    }

    case ASTNode::Function:
    case ASTNode::Macro: {
      const ASTFunctionDecl * decl = static_cast<const ASTFunctionDecl *>(ast);
      meta::AST::Tag tag = meta::AST::FUNCTION;
      if (ast->nodeType() == ASTNode::Macro) {
        tag = meta::AST::MACRO;
      } else if (decl->modifiers().flags & Undef) {
        tag = meta::AST::UNDEF;
      } else if (decl->modifiers().flags & Override) {
        tag = meta::AST::OVERRIDE;
      }
      write(tag);
      writeId(decl->name());
      write(decl->returnType());
      writeDeclMods(decl);
      writeAttributes(decl);

      // Param declarations
      writeNodeList(decl->params().begin(), decl->params().end());

      // The function body
      if (decl->body() != NULL) {
        write(meta::AST::BODY);
        write(decl->body());
      }

      write(meta::AST::END);
      break;
    }

    case ASTNode::Param: {
      const ASTParameter * param = static_cast<const ASTParameter *>(ast);
      write(meta::AST::PARAM);
      writeId(param->name() ? param->name() : "");
      write(param->type());
      if (param->flags() & Param_Variadic) {
        write(meta::AST::VARIADIC);
      }
      if (param->flags() & Param_KeywordOnly) {
        write(meta::AST::KEYWORDONLY);
      }
      if (param->flags() & Param_Star) {
        write(meta::AST::EXPANDED);
      }
      writeAttributes(param);
      if (param->value() != NULL) {
        write(meta::AST::INIT);
        write(param->value());
      }
      write(meta::AST::END);
      break;
    }

    case ASTNode::Prop:
    case ASTNode::Idx: {
      const ASTPropertyDecl * decl = static_cast<const ASTPropertyDecl *>(ast);
      write(ast->nodeType() == ASTNode::Idx ? meta::AST::IDX : meta::AST::PROP);
      writeId(decl->name());
      write(decl->type());
      writeDeclMods(decl);
      writeAttributes(decl);
      writeNodeList(decl->params().begin(), decl->params().end());
      if (decl->getter()) {
        write(decl->getter());
      }
      if (decl->setter()) {
        write(decl->setter());
      }
      write(meta::AST::END);
      break;
    }

    case ASTNode::Var:
    case ASTNode::Let: {
      const ASTVarDecl * decl = static_cast<const ASTVarDecl *>(ast);
      if (ast->nodeType() == ASTNode::Var) {
        write(meta::AST::VAR);
      } else {
        write(meta::AST::LET);
      }
      writeId(decl->name());
      write(decl->type());
      writeDeclMods(decl);
      writeAttributes(decl);
      if (decl->value() != NULL) {
        write(meta::AST::INIT);
        write(decl->value());
      }
      write(meta::AST::END);
      break;
    }

    case ASTNode::Template: {
      const ASTTemplate * decl = static_cast<const ASTTemplate *>(ast);
      write(meta::AST::TEMPLATE);
      writeId(decl->name());
      writeDeclMods(decl);
      writeNodeList(meta::AST::TPARAM, decl->params());
      writeNodeList(meta::AST::CONSTRAINT, decl->attributes());
      if (decl->body() != NULL) {
        write(meta::AST::BODY);
        write(decl->body());
      }
      write(meta::AST::END);
      break;
    }

    case ASTNode::Namespace: {
      const ASTNamespace * decl = static_cast<const ASTNamespace *>(ast);
      write(meta::AST::NAMESPACE);
      writeId(decl->name());
      writeDeclMods(decl);
      writeAttributes(decl);
      writeNodeList(decl->imports());
      writeNodeList(decl->members().begin(), decl->members().end());
      write(meta::AST::END);
      break;
    }

    case ASTNode::Import: {
      const ASTImport * ai = static_cast<const ASTImport *>(ast);
      DASSERT(ai->asName() != NULL);
      write(ai->unpack() ? meta::AST::IMPORT_NS : meta::AST::IMPORT);
      write(ai->path());
      writeId(ai->asName());
      break;
    }

    // Derived types

    case ASTNode::Array: {
      const ASTOper * oper = static_cast<const ASTOper *>(ast);
      write(meta::AST::ARRAY);
      write(oper->arg(0));
      break;
    }

    case ASTNode::Tuple: {
      const ASTOper * oper = static_cast<const ASTOper *>(ast);
      write(meta::AST::TUPLE);
      writeNodeList(oper->args());
      write(meta::AST::END);
      break;
    }

    case ASTNode::Alias: {
      DFAIL("Implement");
      break;
    }

    case ASTNode::TParam: {
      DFAIL("Implement");
      break;
    }

    case ASTNode::TypeVar: {
      const ASTTypeVariable * tv = static_cast<const ASTTypeVariable *>(ast);
      write(meta::AST::TYPEVAR);
      writeId(tv->name());
      if (tv->isVariadic()) {
        write(meta::AST::VARIADIC);
      }
      if (tv->type() != NULL) {
        switch (tv->constraint()) {
          case ASTTypeVariable::IS_INSTANCE:
            write(meta::AST::ISA);
            break;
          case ASTTypeVariable::IS_SUBTYPE:
            write(meta::AST::IS_SUBTYPE);
            break;
          case ASTTypeVariable::IS_SUPERTYPE:
            write(meta::AST::IS_SUPERTYPE);
            break;
        }
        write(tv->type());
      }
      write(meta::AST::END);
      break;
    }

    case ASTNode::VarList: {
      const ASTVarDecl * decl = static_cast<const ASTVarDecl *>(ast);
      write(meta::AST::VARLIST);
      writeDeclMods(decl);
      writeNodeList(decl->members().begin(), decl->members().end());
      if (decl->value() != NULL) {
        write(meta::AST::INIT);
        write(decl->value());
      }
      write(meta::AST::END);
      break;
    }

    case ASTNode::AnonClass: {
      DFAIL("Implement");
    }

    case ASTNode::BuiltIn: {
      const ASTBuiltIn * builtIn = static_cast<const ASTBuiltIn *>(ast);
      TypeDefn * td = cast<TypeDefn>(builtIn->value());
      if (const PrimitiveType * pt = dyn_cast<PrimitiveType>(td->typeValue())) {
        meta::AST::Tag tag;
        switch (int(pt->typeId())) {
          case TypeId_Void: tag = meta::AST::VOID; break;
          case TypeId_Bool: tag = meta::AST::BOOL; break;
          case TypeId_Char: tag = meta::AST::CHAR; break;
          case TypeId_SInt8: tag = meta::AST::INT8; break;
          case TypeId_SInt16: tag = meta::AST::INT16; break;
          case TypeId_SInt32: tag = meta::AST::INT32; break;
          case TypeId_SInt64: tag = meta::AST::INT64; break;
          case TypeId_UInt8: tag = meta::AST::UINT8; break;
          case TypeId_UInt16: tag = meta::AST::UINT16; break;
          case TypeId_UInt32: tag = meta::AST::UINT32; break;
          case TypeId_UInt64: tag = meta::AST::UINT64; break;
          case TypeId_Float: tag = meta::AST::FLOAT; break;
          case TypeId_Double: tag = meta::AST::DOUBLE; break;
          case TypeId_Null: tag = meta::AST::NIL; break;
          default:
            diag.fatal() << "Implement serial: " << pt;
        }
        write(tag);
      } else if (const NativeArrayType * nat = dyn_cast<NativeArrayType>(td->typeValue())) {
        DFAIL("Implement");
      } else {
        DFAIL("Not a builtin type");
      }
      break;
    }

    case ASTNode::Member: {
      const ASTMemberRef * m = static_cast<const ASTMemberRef *>(ast);
      write(meta::AST::MEMBER_REF);
      writeId(m->memberName());
      write(m->qualifier());
      break;
    }

    case ASTNode::Call: {
      const ASTCall * call = static_cast<const ASTCall *>(ast);
      write(meta::AST::CALL);
      write(call->func());
      writeNodeList(call->args());
      write(meta::AST::END);
      break;
    }

    // N-ary ops
    case ASTNode::GetElement: {
      const ASTOper * oper = static_cast<const ASTOper *>(ast);
      write(meta::AST::GET_ELEMENT);
      writeNodeList(oper->args());
      write(meta::AST::END);
      break;
    }

    case ASTNode::Specialize: {
      const ASTSpecialize * spec = static_cast<const ASTSpecialize *>(ast);
      write(meta::AST::SPECIALIZE);
      write(spec->templateExpr());
      writeNodeList(spec->args());
      write(meta::AST::END);
      break;
    }

    // Unary ops
    case ASTNode::Negate:
    case ASTNode::LogicalNot:
    case ASTNode::Complement:
    {
      const ASTOper * oper = static_cast<const ASTOper *>(ast);
      meta::AST::Tag tag = meta::AST::NONE;
      switch (int(ast->nodeType())) {
        case ASTNode::Negate:     tag = meta::AST::NEGATE; break;
        case ASTNode::LogicalNot: tag = meta::AST::LOGICAL_NOT; break;
        case ASTNode::Complement: tag = meta::AST::COMPLEMENT; break;
      }

      write(tag);
      write(oper->arg(0));
      break;
    }

    // Binary ops
    case ASTNode::Assign:
    case ASTNode::AssignAdd:
    case ASTNode::AssignSub:
    case ASTNode::AssignMul:
    case ASTNode::AssignDiv:
    case ASTNode::AssignMod:
    case ASTNode::AssignBitAnd:
    case ASTNode::AssignBitOr:
    case ASTNode::AssignBitXor:
    case ASTNode::AssignRSh:
    case ASTNode::AssignLSh:
    case ASTNode::PostAssign:
    case ASTNode::LogicalAnd:
    case ASTNode::LogicalOr:
    case ASTNode::Is:
    case ASTNode::IsNot:
    case ASTNode::In:
    case ASTNode::NotIn:
    case ASTNode::IsInstanceOf:
    {
      const ASTOper * oper = static_cast<const ASTOper *>(ast);
      meta::AST::Tag tag = meta::AST::NONE;
      switch (int(ast->nodeType())) {
        case ASTNode::Assign:       tag = meta::AST::ASSIGN; break;
        case ASTNode::AssignAdd:    tag = meta::AST::ASSIGN_ADD; break;
        case ASTNode::AssignSub:    tag = meta::AST::ASSIGN_SUB; break;
        case ASTNode::AssignMul:    tag = meta::AST::ASSIGN_MUL; break;
        case ASTNode::AssignDiv:    tag = meta::AST::ASSIGN_DIV; break;
        case ASTNode::AssignMod:    tag = meta::AST::ASSIGN_MOD; break;
        case ASTNode::AssignBitAnd: tag = meta::AST::ASSIGN_BITAND; break;
        case ASTNode::AssignBitOr:  tag = meta::AST::ASSIGN_BITOR; break;
        case ASTNode::AssignBitXor: tag = meta::AST::ASSIGN_BITXOR; break;
        case ASTNode::AssignRSh:    tag = meta::AST::ASSIGN_RSH; break;
        case ASTNode::AssignLSh:    tag = meta::AST::ASSIGN_LSH; break;
        case ASTNode::PostAssign:   tag = meta::AST::POST_ASSIGN; break;
        case ASTNode::LogicalAnd:   tag = meta::AST::LOGICAL_AND; break;
        case ASTNode::LogicalOr:    tag = meta::AST::LOGICAL_OR; break;
        case ASTNode::Is:           tag = meta::AST::IS; break;
        case ASTNode::IsNot:        tag = meta::AST::IS_NOT; break;
        case ASTNode::In:           tag = meta::AST::IN; break;
        case ASTNode::NotIn:        tag = meta::AST::NOT_IN; break;
        case ASTNode::IsInstanceOf: tag = meta::AST::ISA; break;
      }

      write(tag);
      write(oper->arg(0));
      write(oper->arg(1));
      break;
    }

    case ASTNode::AnonFn: {
      const ASTFunctionDecl * fn = static_cast<const ASTFunctionDecl *>(ast);
      write(meta::AST::ANONFN);
      write(fn->returnType());
      writeNodeList(fn->params().begin(), fn->params().end());
      if (fn->body() != NULL) {
        write(meta::AST::BODY);
        write(fn->body());
      }

      write(meta::AST::END);
      break;
    }

    case ASTNode::Keyword: {
      const ASTKeywordArg * kw = static_cast<const ASTKeywordArg *>(ast);
      write(meta::AST::KEYWORD_ARG);
      writeId(kw->keyword());
      write(kw->arg());
      break;
    }

    case ASTNode::Block: {
      const BlockStmt * block = static_cast<const BlockStmt *>(ast);
      write(meta::AST::BLOCK);
      writeNodeList(block->stmts().begin(), block->stmts().end());
      write(meta::AST::END);
      break;
    }

    case ASTNode::If: {
      const IfStmt * st = static_cast<const IfStmt *>(ast);
      write(meta::AST::IF);
      write(st->testExpr());
      write(st->thenSt());
      write(st->elseSt());
      break;
    }

    case ASTNode::While: {
      const WhileStmt * st = static_cast<const WhileStmt *>(ast);
      write(meta::AST::WHILE);
      write(st->testExpr());
      write(st->body());
      break;
    }

    case ASTNode::DoWhile: {
      const DoWhileStmt * st = static_cast<const DoWhileStmt *>(ast);
      write(meta::AST::DO_WHILE);
      write(st->testExpr());
      write(st->body());
      break;
    }

    case ASTNode::For: {
      const ForStmt * st = static_cast<const ForStmt *>(ast);
      write(meta::AST::FOR);
      write(st->initExpr());
      write(st->testExpr());
      write(st->incrExpr());
      write(st->body());
      break;
    }

    case ASTNode::ForEach: {
      const ForEachStmt * st = static_cast<const ForEachStmt *>(ast);
      write(meta::AST::FOR_EACH);
      write(st->loopVars());
      write(st->iterExpr());
      write(st->body());
      break;
    }

    case ASTNode::Switch: {
      const SwitchStmt * st = static_cast<const SwitchStmt *>(ast);
      write(meta::AST::SWITCH);
      write(st->testExpr());
      writeNodeList(st->caseList().begin(), st->caseList().end());
      write(meta::AST::END);
      break;
    }

    case ASTNode::Case: {
      const CaseStmt * st = static_cast<const CaseStmt *>(ast);
      write(meta::AST::CASE);
      write(st->body());
      writeNodeList(st->caseExprs());
      write(meta::AST::END);
      break;
    }

    case ASTNode::Match: {
      const MatchStmt * st = static_cast<const MatchStmt *>(ast);
      write(meta::AST::MATCH);
      write(st->testExpr());
      writeNodeList(st->caseList().begin(), st->caseList().end());
      write(meta::AST::END);
      break;
    }

    case ASTNode::MatchAs: {
      const MatchAsStmt * st = static_cast<const MatchAsStmt *>(ast);
      write(meta::AST::MATCH_AS);
      write(st->asDecl());
      write(st->body());
      break;
    }

    case ASTNode::Throw: {
      const ThrowStmt * st = static_cast<const ThrowStmt *>(ast);
      write(meta::AST::THROW);
      write(st->value());
      break;
    }

    case ASTNode::Return: {
      const ReturnStmt * st = static_cast<const ReturnStmt *>(ast);
      write(meta::AST::RETURN);
      write(st->value());
      break;
    }

    case ASTNode::Yield: {
      const ReturnStmt * st = static_cast<const ReturnStmt *>(ast);
      write(meta::AST::YIELD);
      write(st->value());
      break;
    }

    case ASTNode::Try: {
      const TryStmt * st = static_cast<const TryStmt *>(ast);
      write(meta::AST::TRY);
      write(st->body());
      writeNodeList(st->catchList().begin(), st->catchList().end());
      if (st->elseSt()) {
        write(meta::AST::ELSE);
        write(st->elseSt());
      }
      if (st->finallySt()) {
        write(meta::AST::FINALLY);
        write(st->elseSt());
      }
      write(meta::AST::END);
      break;
    }

    case ASTNode::Catch: {
      const CatchStmt * st = static_cast<const CatchStmt *>(ast);
      write(meta::AST::CATCH);
      write(st->exceptDecl());
      write(st->body());
      break;
    }

    case ASTNode::Break: {
      write(meta::AST::BREAK);
      break;
    }

    case ASTNode::Continue: {
      write(meta::AST::CONTINUE);
      break;
    }

    case ASTNode::Expression: {
      const ExprStmt * st = static_cast<const ExprStmt *>(ast);
      write(meta::AST::EXPR);
      write(st->value());
      break;
    }

    case ASTNode::LocalDecl: {
      const DeclStmt * ds = static_cast<const DeclStmt *>(ast);
      write(meta::AST::LOCAL_DECL);
      write(ds->decl());
      break;
    }

    default:
      diag.fatal(ast) << "Implement serialize ASTNode: '" << nodeTypeName(ast->nodeType()) << "'";
      break;
  }

  return *this;
}

ASTWriter & ASTWriter::writeInt(uint32_t i) {
  stream_ << VarInt(i);
  return *this;
}

/** Write an AST declaration header. */
ASTWriter & ASTWriter::writeDeclMods(const ASTDecl * decl) {
  if (decl->modifiers().visibility != Public) {
    write(visibility(decl->modifiers().visibility));
  }
  if (decl->modifiers().flags & Static) {
    write(meta::AST::STATIC);
  }
  if (decl->modifiers().flags & Final) {
    write(meta::AST::FINAL);
  }
  if (decl->modifiers().flags & Abstract) {
    write(meta::AST::ABSTRACT);
  }
  if (decl->modifiers().flags & ReadOnly) {
    write(meta::AST::READONLY);
  }

  return *this;
}

void ASTWriter::writeAttributes(const ASTDecl * decl) {
  writeNodeList(meta::AST::ATTRIBUTE, decl->attributes());
}

void ASTWriter::writeAttributes(const Defn * de) {
  for (ExprList::const_iterator it = de->attrs().begin();
      it != de->attrs().end(); ++it) {
    write(meta::AST::ATTRIBUTE);
    write(*it);
  }
}

void ASTWriter::writeNodeList(const ASTNodeList & nodes) {
  for (ASTNodeList::const_iterator it = nodes.begin(); it != nodes.end(); ++it) {
    write(*it);
  }
}

void ASTWriter::writeNodeList(meta::AST::Tag tag, const ASTNodeList & nodes) {
  for (ASTNodeList::const_iterator it = nodes.begin(); it != nodes.end(); ++it) {
    write(tag);
    write(*it);
  }
}

ASTWriter & ASTWriter::write(meta::AST::Tag tag) {
  stream_ << uint8_t(tag);
  return *this;
}

meta::AST::Tag ASTWriter::visibility(Visibility v) {
  switch (int(v)) {
    case Private: return meta::AST::PRIVATE;
    case Protected: return meta::AST::PROTECTED;
    case Internal: return meta::AST::INTERNAL;
  }
  return meta::AST::NONE;
}

void ASTWriter::writeQName(const Defn * de) {
  const Module * m = de->sourceModule();
  const Defn * topLevel = de->moduleLevelParent();
  if (topLevel->name() != m->name()) {
    llvm::SmallString<128> qname(m->qualifiedName());
    qname += ':';
    writeRelativeName(qname, de);
    writeQualId(qname);
  } else {
    writeQualId(de->qualifiedName());
  }
}

void ASTWriter::writeRelativeName(llvm::SmallVectorImpl<char> & out, const Defn * de) {
  Defn * parent = de->parentDefn();
  if (parent != NULL && parent->defnType() != Defn::Mod) {
    writeRelativeName(out, parent);
    out.push_back('.');
  }
  StringRef name = de->name();
  out.append(name.begin(), name.end());
}

void ASTWriter::writeId(llvm::StringRef id) {
  StringMap<uint32_t>::const_iterator it = idMap_.find(id);
  if (it != idMap_.end()) {
    write(meta::AST::ID_REF);
    stream_ << VarInt(it->second);
  } else {
    write(meta::AST::ID_DEF);
    writeStr(id);
    idMap_[id] = nextIndex_++;
  }
}

void ASTWriter::writeQualId(llvm::StringRef id) {
  StringMap<uint32_t>::const_iterator it = idMap_.find(id);
  if (it != idMap_.end()) {
    write(meta::AST::QID_REF);
    stream_ << VarInt(it->second);
  } else {
    write(meta::AST::QID_DEF);
    stream_ << VarInt(id.size());
    stream_ << id;
    idMap_[id] = nextIndex_++;
  }
}

void ASTWriter::writeStr(llvm::StringRef str) {
  stream_ << VarInt(str.size());
  stream_ << str;
}

llvm::StringRef ASTWriter::str() {
  return stream_.str();
}

} // namespace tart

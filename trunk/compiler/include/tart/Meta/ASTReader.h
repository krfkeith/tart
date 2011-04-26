/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_ASTREADER_H
#define TART_META_ASTREADER_H

#ifndef TART_META_TAGS_H
#include "tart/Meta/Tags.h"
#endif

#ifndef LLVM_ADT_SMALLSTRING_H
#include "llvm/ADT/SmallString.h"
#endif

namespace tart {

class ASTNode;
class ASTDecl;
class Stmt;
class CodeGenerator;
typedef llvm::SmallVector<ASTNode *, 4> ASTNodeList;

struct DeclContent;

/// -------------------------------------------------------------------
/// AST deserializer.

class ASTReader {
public:
  ASTReader(SourceLocation loc, const char * buffer, size_t size)
    : loc_(loc)
    , pos_(buffer)
    , end_(buffer + size)
    , nextIndex_(0)
  {}

  ASTReader(SourceLocation loc, llvm::StringRef str)
    : loc_(loc)
    , pos_(str.data())
    , end_(str.data() + str.size())
    , nextIndex_(0)
  {}

  ASTNode * read();
  bool readAll(ASTNodeList & out);

  bool atEnd() const;

private:
  bool readDecl(DeclContent & dc);
  bool readStmtList(llvm::SmallVectorImpl<Stmt *> & out);
  bool readNodeList(ASTNodeList & out);

  Stmt * readOptionalStmt();
  Stmt * readStmt();

  meta::AST::Tag readTag();
  uint64_t readVarInt();
  const char * readId();
  const char * readIdDef();
  const char * readIdRef();
  const char * readString();
  llvm::StringRef readStringRef();

  SourceLocation loc_;
  const char * pos_;
  const char * end_;
  llvm::SmallVector<const char *, 16> idTable_;
  uint32_t nextIndex_;
};

} // namespace tart

#endif // TART_META_ASTREADER_H

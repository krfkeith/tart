/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_ASTREADER_H
#define TART_META_ASTREADER_H

#ifndef TART_META_TAGS_H
#include "tart/Meta/Tags.h"
#endif

#ifndef TART_COMMON_STRINGTABLE_H
#include "tart/Common/StringTable.h"
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
  ASTReader(SourceLocation loc, StringTable & stringTable, const char * buffer, size_t size)
    : loc_(loc)
    , stringTable_(stringTable)
    , pos_(reinterpret_cast<const unsigned char *>(buffer))
    , end_(reinterpret_cast<const unsigned char *>(buffer) + size)
    , nextIndex_(0)
  {}

  ASTReader(SourceLocation loc, StringTable & stringTable, StringRef str)
    : loc_(loc)
    , stringTable_(stringTable)
    , pos_(reinterpret_cast<const unsigned char *>(str.data()))
    , end_(reinterpret_cast<const unsigned char *>(str.data()) + str.size())
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
  StringRef readId();
  StringRef readIdDef();
  StringRef readIdRef();
  StringRef readString();
  StringRef readStringRef();

  SourceLocation loc_;
  StringTable & stringTable_;
  const unsigned char * pos_;
  const unsigned char * end_;
  llvm::SmallVector<StringRef, 16> idTable_;
  uint32_t nextIndex_;
};

} // namespace tart

#endif // TART_META_ASTREADER_H

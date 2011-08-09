/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_ASTWRITER_H
#define TART_META_ASTWRITER_H

#ifndef TART_META_TAGS_H
#include "tart/Meta/Tags.h"
#endif

#ifndef LLVM_ADT_SMALLSTRING_H
#include "llvm/ADT/SmallString.h"
#endif

#ifndef LLVM_ADT_STRINGMAP_H
#include "llvm/ADT/StringMap.h"
#endif

#ifndef LLVM_SUPPORT_RAW_OSTREAM_H
#include "llvm/Support/raw_ostream.h"
#endif

namespace tart {

class Type;
class Expr;
class ASTNode;
class ASTDecl;
class CodeGenerator;

/// -------------------------------------------------------------------
/// AST serializer.

class ASTWriter {
public:
  ASTWriter() : stream_(str_), nextIndex_(0) {}

  /** Write a type expression. */
  ASTWriter & write(const Type * ty);

  /** Write an expression. */
  ASTWriter & write(const Expr * ex);

  /** Write an AST node. */
  ASTWriter & write(const ASTNode * ast);

  /** Write an AST node tag. */
  ASTWriter & write(meta::AST::Tag tag);

  /** Write an integer. */
  ASTWriter & writeInt(uint32_t i);

  /** Write an AST declaration header. */
  ASTWriter & writeDeclMods(const ASTDecl * ast);

  /** Return the serialized AST. */
  StringRef str();

  /** Write all of the AST nodes in the list. */
  void writeNodeList(const ASTNodeList & nodes);

private:
  template<class Iterator>
  void writeNodeList(Iterator first, Iterator last);

  void writeNodeList(meta::AST::Tag tag, const ASTNodeList & nodes);
  void writeAttributes(const ASTDecl * decl);
  void writeAttributes(const Defn * de);
  void writeQName(const Defn * de);
  void writeRelativeName(llvm::SmallVectorImpl<char> & out, const Defn * de);
  void writeId(StringRef id);
  void writeQualId(StringRef qid);
  void writeStr(StringRef str);
  meta::AST::Tag visibility(Visibility v);

  llvm::SmallString<128> str_;
  llvm::raw_svector_ostream stream_;
  llvm::StringMap<uint32_t> idMap_;
  uint32_t nextIndex_;
};

template<class Iterator>
inline void ASTWriter::writeNodeList(Iterator first, Iterator last) {
  for (Iterator it = first; it != last; ++it) {
    write(*it);
  }
}

} // namespace tart

#endif

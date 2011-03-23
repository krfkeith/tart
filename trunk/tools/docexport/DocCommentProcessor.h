/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DOCCOMMENTPROCESSOR_H
#define TART_DOCCOMMENTPROCESSOR_H

#ifndef TART_AST_DOCCOMMENT_H
#include "tart/AST/DocComment.h"
#endif

#ifndef TART_DOCNODE_H
#include "DocNode.h"
#endif

#ifndef LLVM_ADT_SMALLVECTOR_H
#include "llvm/ADT/SmallVector.h"
#endif

namespace tart {

struct DocTag;

/// -------------------------------------------------------------------
/// Writes out the contents of a module as XML.
class DocCommentProcessor {
public:
  struct Line {
    llvm::StringRef text;   // The text of the line
    size_t indent;          // How many columns this line is indented
  };
  typedef llvm::SmallVector<Line, 16> LineList;

  DocCommentProcessor(const DocComment & docComment) : docComment_(docComment) {}

  Doc::Node * process();

  // Parsers for various doc tags.
  void authors(Doc::Node * parent);
  void example(Doc::Node * parent);
  void note(Doc::Node * parent);
  void parameters(Doc::Node * parent);
  void returns(Doc::Node * parent);
  void seeAlso(Doc::Node * parent);
  void since(Doc::Node * parent);
  void summary(Doc::Node * parent);
  void throws(Doc::Node * parent);
  void todo(Doc::Node * parent);
  void warning(Doc::Node * parent);
  void genericSection(Doc::Node * parent);

  // Parsers for other markup constructs.
  void unorderedList(Doc::Node * parent);
  void orderedList(Doc::Node * parent);
  void codeBlock(Doc::Node * parent);
  void blockQuote(Doc::Node * parent);
  bool definition(Doc::Node * parent, Doc::NodeType nt);

private:
  void splitLines();
  size_t breakLine(Line & line, llvm::StringRef text, size_t pos, int column);
  void removeBanners();
  void trimEmptyLines();
  void parseLines(Doc::Node * parent, size_t parentIndent);
  void finishParagraph(Doc::Node * parent);
  void createParagraph(Doc::Node * parent);
  void nextLine();
  size_t parseWord();
  size_t skipWS();
  int charAt(size_t pos);
  size_t nextChar(size_t pos);
  DocTag * matchDocTag();
  SourceLocation location();

  const DocComment & docComment_;
  LineList lines_;
  LineList::iterator currentLine_;
  size_t currentPos_;
  Doc::Node * currentSection_;
  llvm::SmallString<256> paragraphText_;
};

} // namespace tart

#endif // DOCEXPORTER

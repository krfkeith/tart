/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DOCNODE_H
#define TART_DOCNODE_H

#ifndef TART_AST_DOCCOMMENT_H
#include "tart/AST/DocComment.h"
#endif

#include "llvm/ADT/SmallVector.h"

namespace tart {
namespace Doc {

class Node;

enum NodeType {
  ROOT,
  TEXT,
  PARAGRAPH,
  SECTION,
  SECTION_HEADING,
  UNORDERED_LIST,
  ORDERED_LIST,
  LIST_ITEM,
  CODE,
  STYLE,
};

enum SectionType {
  GENERIC,
  PARAMETERS,
  RETURNTYPE,
  EXCEPTIONS,
  ATTRIBUTES,
  SEE_ALSO,
};

enum Style {
  STRONG,
  EMPHATIC,
};

/// -------------------------------------------------------------------
/// A document node list
typedef llvm::SmallVector<Node *, 0> NodeList;

/// -------------------------------------------------------------------
/// A document node
class Node {
public:
  Node(NodeType type) : type_(type) {}
  ~Node();

  typedef NodeList::iterator iterator;
  typedef NodeList::const_iterator const_iterator;

  /** The type of this node. */
  NodeType type() { return nodeType_; }

  /** The list of child nodes. */
  const NodeList & children() const { return children_; }
  NodeList & children() { return children_; }

  /** Child node iterators. */
  const_iterator begin() const { return children_.begin(); }
  iterator begin() { return children_.begin(); }
  const_iterator end() const { return children_.end(); }
  iterator end() { return children_.end(); }

  /** True if this node has no child nodes. */
  bool empty() const { return children_.empty(); }

  /** Returns the number of child nodes. */
  size_t size() { return children_.size(); }

private:
  NodeType type_;
  NodeList children_;
};

/// -------------------------------------------------------------------
/// A text node
class TextNode : public Node {
public:
  TextNode(llvm::StringRef text) : text_(text) {}

  /** The text of this node. */
  llvm::StringRef text() const { return text_; }
private:
  llvm::SmallString<128> text_;
};

/// -------------------------------------------------------------------
/// A section of a doc comment
class SectionNode : public Node {
public:
  SectionNode(SectionType sectionType) : sectionType_(sectionType) {}

  /** What kind of section this is. */
  SectionType sectionType() const { return sectionType_; }

private:
  SectionType sectionType_;
};

} // namespace doc
} // namespace tart

#endif // DOCEXPORTER

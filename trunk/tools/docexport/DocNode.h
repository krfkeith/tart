/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DOCNODE_H
#define TART_DOCNODE_H

#ifndef TART_AST_DOCCOMMENT_H
#include "tart/AST/DocComment.h"
#endif

#ifndef LLVM_ADT_SMALLVECTOR_H
#include "llvm/ADT/SmallVector.h"
#endif

#ifndef LLVM_SUPPORT_CASTING_H
#include "llvm/Support/Casting.h"
#endif

namespace tart {
namespace Doc {

class Node;

enum NodeType {
  ROOT,
  TEXT,
  PARAGRAPH,
  BLOCKQUOTE,
  SECTION,
  SECTION_HEADING,
  PROPERTY,
  PARAMETER,
  EXCEPTION,
  RETURNS,
  UNORDERED_LIST,
  ORDERED_LIST,
  LIST_ITEM,
  CODE,
  STYLE,
};

enum SectionType {
  GENERIC,
  DESCRIPTION,
  ATTRIBUTES,
  SEE_ALSO,
};

enum Style {
  STYLE_STRONG,
  STYLE_EMPHATIC,
  STYLE_SYMBOL,
  STYLE_CODE,
};

/// -------------------------------------------------------------------
/// A document node list
typedef llvm::SmallVector<Node *, 0> NodeList;

/// -------------------------------------------------------------------
/// A document node
class Node {
public:
  Node(NodeType type) : type_(type) {}
  virtual ~Node();

  typedef NodeList::iterator iterator;
  typedef NodeList::const_iterator const_iterator;

  /** The type of this node. */
  NodeType type() const { return type_; }

  /** The list of child nodes. */
  const NodeList & children() const { return children_; }
  NodeList & children() { return children_; }

  /** Append a node to this node's list of children. */
  void append(Node * child) { children_.push_back(child); }

  /** Child node iterators. */
  const_iterator begin() const { return children_.begin(); }
  iterator begin() { return children_.begin(); }
  const_iterator end() const { return children_.end(); }
  iterator end() { return children_.end(); }

  /** True if this node has no child nodes. */
  bool empty() const { return children_.empty(); }

  /** Returns the number of child nodes. */
  size_t size() const { return children_.size(); }

  /** Dynamic casting support. */
  static inline bool classof(const Node *) { return true; }

private:
  NodeType type_;
  NodeList children_;
};

/// -------------------------------------------------------------------
/// A text node
class TextNode : public Node {
public:
  TextNode(llvm::StringRef text) : Node(TEXT), text_(text) {}

  /** The text of this node. */
  llvm::StringRef text() const { return text_; }

  static inline bool classof(const TextNode *) { return true; }
  static inline bool classof(const Node * n) {
    return n->type() == TEXT;
  }
private:
  llvm::SmallString<128> text_;
};

/// -------------------------------------------------------------------
/// A section of a doc comment
class SectionNode : public Node {
public:
  SectionNode(SectionType sectionType) : Node(SECTION), sectionType_(sectionType) {}

  /** What kind of section this is. */
  SectionType sectionType() const { return sectionType_; }

  static inline bool classof(const SectionNode *) { return true; }
  static inline bool classof(const Node * n) {
    return n->type() == SECTION;
  }

private:
  SectionType sectionType_;
};

/// -------------------------------------------------------------------
/// A styled span of text
class StyleNode : public Node {
public:
  StyleNode(Style style) : Node(STYLE), style_(style) {}

  /** What kind of section this is. */
  Style style() const { return style_; }

  static inline bool classof(const StyleNode *) { return true; }
  static inline bool classof(const Node * n) {
    return n->type() == STYLE;
  }

private:
  Style style_;
};

/// -------------------------------------------------------------------
/// A node which represents information about a named item.
class DefinitionNode : public Node {
public:
  DefinitionNode(NodeType type, llvm::StringRef name)
    : Node(type)
    , name_(name)
  {}

  /** The name of the param. */
  llvm::StringRef name() const { return name_; }

  static inline bool classof(const DefinitionNode *) { return true; }
  static inline bool classof(const Node * n) {
    return n->type() == PARAMETER || n->type() == EXCEPTION;
  }

private:
  llvm::SmallString<32> name_;
};

/// -------------------------------------------------------------------
/// A node which represents the value of some property, such as author,
/// date, etc.
class PropertyNode : public Node {
public:
  PropertyNode(llvm::StringRef propertyName, llvm::StringRef propertyValue)
    : Node(PROPERTY)
    , propertyName_(propertyName)
    , propertyValue_(propertyValue)
  {}

  /** The name of the property. */
  llvm::StringRef propertyName() const { return propertyName_; }

  /** The value of the property. */
  llvm::StringRef propertyValue() const { return propertyValue_; }

  static inline bool classof(const PropertyNode *) { return true; }
  static inline bool classof(const Node * n) {
    return n->type() == PROPERTY;
  }

private:
  llvm::SmallString<32> propertyName_;
  llvm::SmallString<32> propertyValue_;
};

} // namespace doc
} // namespace tart

#endif // DOCEXPORTER

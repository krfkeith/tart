/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_AST_DOCCOMMENT_H
#define TART_AST_DOCCOMMENT_H

#ifndef TART_COMMON_SOURCELOCATION_H
#include "tart/Common/SourceLocation.h"
#endif

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallString.h>

namespace tart {

/// -------------------------------------------------------------------
/// Tracks documentation comments associated with a declaration.
class DocComment {
public:
  /** For each text entry, we need to know the starting column,
      so that we can correctly piece together multiple comments. */
  class Entry {
  public:
    Entry(SourceLocation location, llvm::SmallVectorImpl<char> & text)
      : location_(location)
    {
      text_.swap(text);
    }

    /** The location of this doc comment. */
    const SourceLocation & location() { return location_; }

    /** The text of this doc comment entry. */
    llvm::StringRef text() const { return text_; }

  private:
    SourceLocation location_;
    llvm::SmallString<0> text_;
  };

  typedef llvm::SmallVector<Entry *, 0> EntryList;
  typedef EntryList::iterator iterator;
  typedef EntryList::const_iterator const_iterator;

  /** The list of comment entries. */
  const EntryList & entries() const { return entries_; }
  EntryList & entries() { return entries_; }

  // Vector-like accessors

  const_iterator begin() const { return entries_.begin(); }
  iterator begin() { return entries_.begin(); }
  const_iterator end() const { return entries_.end(); }
  iterator end() { return entries_.end(); }
  size_t size() const { return entries_.size(); }
  bool empty() const { return entries_.empty(); }
  void clear() { entries_.clear(); }

  /** Transfer comment entries from another list. */
  void take(DocComment & from) {
    entries_.swap(from.entries_);
  }

  /** Convert into a flat string. */
  void toString(llvm::SmallVectorImpl<char> & buffer) const {
    for (const_iterator it = begin(); it != end(); ++it) {
      buffer.append((*it)->text().begin(), (*it)->text().end());
    }
  }

  void toString(std::string & buffer) const {
    for (const_iterator it = begin(); it != end(); ++it) {
      buffer.append((*it)->text().begin(), (*it)->text().end());
    }
  }

private:
  EntryList entries_;
};

} // namespace tart

#endif // TART_AST_DOCCOMMENT_H

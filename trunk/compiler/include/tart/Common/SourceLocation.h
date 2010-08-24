/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_SOURCELOCATION_H
#define TART_COMMON_SOURCELOCATION_H

#include <string>
#include <llvm/System/DataTypes.h>
#include <sys/types.h>

namespace tart {

class ProgramSource;
class Formattable;

/// -------------------------------------------------------------------
/// Location of a token within a source file, in terms of character
/// offsets from the start of the file.
struct SourceLocation {
  ProgramSource * file;           // Pointer to file
  uint32_t        begin;          // Start position in file
  uint32_t        end;            // End position in file

  SourceLocation() {
    file = NULL;
    begin = end = 0;
  }

  SourceLocation & operator=(const SourceLocation & in) {
    file = in.file;
    begin = in.begin;
    end = in.end;
    return *this;
  }

  friend SourceLocation operator|(const SourceLocation & a, const SourceLocation & b) {
    SourceLocation result;
    if (a.file == b.file) {
      result.file = a.file;
      result.begin = a.begin < b.begin ? a.begin : b.begin;
      result.end = a.end > b.end ? a.end : b.end;
    } else if (a.file == NULL) {
      result = b;
    } else {
      result = a;
    }

    return result;
  }

  SourceLocation operator|=(const SourceLocation & a) {
    if (a.file == file) {
      if (a.begin < begin) begin = a.begin;
      if (a.end > end) end = a.end;
    } else if (file == NULL) {
      file = a.file;
      begin = a.begin;
      end = a.end;
    }

    return *this;
  }

  bool operator==(const SourceLocation & in) const {
    return (file == in.file && begin == in.begin && end == in.end);
  }

  bool operator!=(const SourceLocation & in) const {
    return (file != in.file || begin != in.begin || end != in.end);
  }

  void trace() const;

  void dump() const __attribute__ ((noinline));
};

// Because 'const SourceLocation' occurs so much, an abbreviation is useful
typedef const SourceLocation SLC;

/// -------------------------------------------------------------------
/// Position of a token within a source file in terms of line number
/// and column number.
struct TokenPosition {
  uint32_t        beginLine;
  uint32_t        beginCol;
  uint32_t        endLine;
  uint32_t        endCol;
};

/// -------------------------------------------------------------------
/// Base class for classes that have a source location.
class Locatable {
public:
  virtual const SourceLocation & location() const = 0;
  virtual ~Locatable() {};

  operator const SourceLocation &() { return location(); }
};

/// -------------------------------------------------------------------
/// A source context contains a source location and a pointer to a
/// context (used to represent nested template instantiations.)
class SourceContext : public Locatable {
public:
  /** Constructor that takes a location. */
  SourceContext(const SourceLocation & location, SourceContext * parent = NULL,
      Formattable * expression = NULL, int formatOptions = 0)
    : location_(location)
    , parent_(parent)
    , expression_(expression)
    , formatOptions_(formatOptions)
  {}

  /** Constructor that takes a locatable. */
  SourceContext(const Locatable * loc, SourceContext * parent = NULL,
      Formattable * expression = NULL, int formatOptions = 0)
    : location_(loc->location())
    , parent_(parent)
    , expression_(expression)
    , formatOptions_(formatOptions)
  {}

  /** The location of the source line. */
  const SourceLocation & location() const { return location_; }

  /** The parent context of this context. This will usually
      be used to represent the source location of the calling
      template. */
  SourceContext * parent() const { return parent_; }

  /** A formattable object representing the calling expression or type. */
  Formattable * expression() const { return expression_; }

  /** The set of format option flags to use. */
  int formatOptions() const { return formatOptions_; }

private:
  SourceLocation location_;
  SourceContext * parent_;
  Formattable * expression_;
  int formatOptions_;
};

}

#endif

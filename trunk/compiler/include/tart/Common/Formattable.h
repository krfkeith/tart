/* ================================================================ *
  TART - A Sweet Programming Language.
* ================================================================ */

#ifndef TART_COMMON_FORMATTABLE_H
#define TART_COMMON_FORMATTABLE_H

#include <llvm/Support/Streams.h>
#include <stdarg.h>
#include <stdio.h>
#include <string>
//#include <ostream>

namespace tart {
  
class Formattable;
class FormatStream;
  
/// -------------------------------------------------------------------
/// Options for the format() method
enum FormatOptions {
  Format_QualifiedName = (1 << 0),    // Include fully qualified names
  Format_Type = (1 << 1),             // Include types
  Format_Initializer = (1 << 2),      // Include initializers
  Format_Verbose = (1 << 3),          // Include full debugging info
  Format_Dealias = (1 << 4),          // Show underlying definitions.
  
  Format_Default = 0,
};

/// -------------------------------------------------------------------
/// Interface for objects that can be converted to a string
/// representation.
class Formattable {
public:
  /** Produce a textual representation of this object. */
  virtual void format(FormatStream & out) const = 0;

  /** Virtual dtor to make the compiler happy. */
  virtual ~Formattable() {}

  /** Print debugging information to output console (for use in debugger). */
  virtual void dump() const;
};

/// -------------------------------------------------------------------
/// Stream class that also holds format options.
class FormatStream : public llvm::OStream {
private:
  int formatOptions_;
  
public:
  FormatStream(std::ostream &S)
    : llvm::OStream(&S)
    , formatOptions_(Format_Default)
  {}

  FormatStream(std::ostream *S)
    : llvm::OStream(S)
    , formatOptions_(Format_Default)
  {}

  /** Current set of format options. */
  int formatOptions() const { return formatOptions_; }
  void setFormatOptions(int formatOptions) { formatOptions_ = formatOptions; }

  /** Whether names should be shown as fully qualified. */
  bool getShowQualifiedName() const {
    return (formatOptions_ & Format_QualifiedName) != 0;
  }

  /** Whether type information should be shown when printing a variable. */
  bool getShowType() const {
    return (formatOptions_ & Format_Type) != 0;
  }

  /** Whether initializer information should be shown when printing a
      variable. */
  bool getShowInitializer() const {
    return (formatOptions_ & Format_Initializer) != 0;
  }

  /** Whether to show underlying definitions of aliases. */
  bool getDealias() const {
    return (formatOptions_ & Format_Dealias) != 0;
  }

  /** Whether type information should be shown when printing a variable. */
  bool isVerbose() const {
    return (formatOptions_ & Format_Verbose) != 0;
  }

  inline FormatStream & operator<<(const Formattable * obj) {
    obj->format(*this);
    return *this;
  }

  inline FormatStream & operator<<(Formattable * obj) {
    obj->format(*this);
    return *this;
  }

  // These are non-inline in order to avoid including <ostream> everywhere.
  // We don't use a template because we want to carefully control how
  // every type is streamed.
  
  FormatStream & operator<<(const char * str);
  FormatStream & operator<<(const std::string & str);
  FormatStream & operator<<(int value);
  FormatStream & operator<<(void (*func)(FormatStream &)) {
    (*func)(*this);
    return *this;
  }
  
  // Enable format option
  FormatStream & operator<<(FormatOptions f);
};

}

#endif

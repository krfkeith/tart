/* ================================================================ *
  TART - A Sweet Programming Language.
* ================================================================ */

#ifndef TART_COMMON_FORMATTABLE_H
#define TART_COMMON_FORMATTABLE_H

#ifndef LLVM_SUPPORT_RAW_OS_OSTREAM_H
#include "llvm/Support/raw_os_ostream.h"
#endif

#ifndef LLVM_ADT_SMALLSTRING_H
#include "llvm/ADT/SmallString.h"
#endif

namespace llvm {
class Twine;
}

namespace tart {

class Formattable;
class FormatStream;

/// -------------------------------------------------------------------
/// Options for the format() method
enum FormatOptions {
  Format_QualifiedName = (1 << 0),    // Include fully qualified names
  Format_Type = (1 << 1),             // Include types
  Format_Initializer = (1 << 2),      // Include initializers
  Format_Dealias = (1 << 3),          // Show underlying definitions.

  Format_Verbose = Format_QualifiedName | Format_Type,
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

  /** Return debugging information as a string (for use in debugger). */
  virtual const char * str() const;
};

/// -------------------------------------------------------------------
/// Stream interface that also holds format options.
class FormatStream : public llvm::raw_ostream {
public:
  FormatStream(llvm::raw_ostream & baseStrm)
    : formatOptions_(Format_Default)
    , baseStrm_(baseStrm)
  {
  }

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
    if (obj != NULL) {
      obj->format(*this);
    } else {
      *this << "<null_ptr>";
    }
    return *this;
  }

  inline FormatStream & operator<<(Formattable * obj) {
    if (obj != NULL) {
      obj->format(*this);
    } else {
      *this << "<null_ptr>";
    }
    return *this;
  }

  // These are non-inline in order to avoid including <ostream> everywhere.
  // We don't use a template because we want to carefully control how
  // every type is streamed.

  FormatStream & operator<<(const char * str);
  FormatStream & operator<<(const std::string & str);
  FormatStream & operator<<(llvm::StringRef str);
  FormatStream & operator<<(const llvm::Twine & str);
  FormatStream & operator<<(int value);
  FormatStream & operator<<(void (*func)(FormatStream &)) {
    (*func)(*this);
    return *this;
  }

  // Enable format option
  FormatStream & operator<<(FormatOptions f);

  void flush() {
    llvm::raw_ostream::flush();
    baseStrm_.flush();
  }

private:
  int formatOptions_;
  llvm::raw_ostream & baseStrm_;

  uint64_t current_pos() const {
    return baseStrm_.tell();
  }

  void write_impl(const char * ptr, size_t size) {
    baseStrm_.write(ptr, size);
  }
};

class StrFormatStream : public FormatStream {
public:
  StrFormatStream() : FormatStream(strm_), strm_(str_) {}

  llvm::StringRef str() { flush(); return str_.str(); }

private:
  llvm::SmallString<128> str_;
  llvm::raw_svector_ostream strm_;
};

class OsFormatStream : public FormatStream {
public:
  OsFormatStream(std::ostream & os) : FormatStream(strm_), strm_(os) {}
  OsFormatStream(std::ostream * os) : FormatStream(strm_), strm_(*os) {}

private:
  llvm::raw_os_ostream strm_;
};

} // namespace tart

#endif // TART_COMMON_FORMATTABLE_H

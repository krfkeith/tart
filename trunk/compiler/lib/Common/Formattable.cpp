/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Formattable.h"

#include "llvm/ADT/Twine.h"

namespace tart {

// -------------------------------------------------------------------
// Formattable

void Formattable::dump() const {
  FormatStream stream(llvm::errs());
  format(stream);
}

const char * Formattable::str() const {
  static llvm::SmallString<256> temp;
  StrFormatStream stream;
  stream.setFormatOptions(Format_Verbose);
  format(stream);
  stream.flush();

  temp.clear();
  temp += stream.str();
  return temp.c_str();
}

// -------------------------------------------------------------------
// FormatStream

FormatStream & FormatStream::operator<<(const char * str) {
  llvm::raw_ostream::operator<<(str);
  return *this;
}

FormatStream & FormatStream::operator<<(const std::string & str) {
  llvm::raw_ostream::operator<<(str);
  return *this;
}

FormatStream & FormatStream::operator<<(int value) {
  llvm::raw_ostream::operator<<(value);
  return *this;
}

FormatStream & FormatStream::operator<<(StringRef str) {
  llvm::raw_ostream::operator<<(str);
  return *this;
}

FormatStream & FormatStream::operator<<(const llvm::Twine & str) {
  str.print(*this);
  return *this;
}

FormatStream & FormatStream::operator<<(FormatOptions f) {
  formatOptions_ |= f;
  return *this;
}

}

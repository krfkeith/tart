/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Formattable.h"
#include <ostream>
#include <iostream>
#include <sstream>

namespace tart {

// -------------------------------------------------------------------
// Formattable

void Formattable::dump() const {
  FormatStream stream(std::cerr);
  format(stream);
}

const char * Formattable::asString() const {
  static std::string temp;
  std::stringstream ss;
  FormatStream stream(ss);
  stream.setFormatOptions(Format_Verbose);
  format(stream);
  temp = ss.str();
  return temp.c_str();
}

// -------------------------------------------------------------------
// FormatStream

FormatStream & FormatStream::operator<<(const char * str) {
  *stream() << str;
  return *this;
}

FormatStream & FormatStream::operator<<(const std::string & str) {
  *stream() << str;
  return *this;
}

FormatStream & FormatStream::operator<<(int value) {
  *stream() << value;
  return *this;
}

FormatStream & FormatStream::operator<<(FormatOptions f) {
  formatOptions_ |= f;
  return *this;
}

}

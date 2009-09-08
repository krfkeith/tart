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

std::string Formattable::tostr() const {
  std::stringstream ss;
  FormatStream stream(ss);
  format(stream);
  return ss.str();
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

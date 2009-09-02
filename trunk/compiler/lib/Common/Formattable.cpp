/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Formattable.h"
#include <ostream>
#include <iostream>

namespace tart {
    
// -------------------------------------------------------------------
// Formattable

void Formattable::dump() const {
  FormatStream stream(std::cerr);
  format(stream);
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

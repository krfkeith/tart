/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"
#include "tart/Meta/VarInt.h"

namespace tart {

void VarInt::write(llvm::raw_ostream & out, uint32_t value) {
  if (value < VARINT_MIN_2BYTE_VALUE) {
    out << char(value);
  } else if (value < VARINT_MIN_3BYTE_VALUE) {
    out << char(VARINT_2BYTE_PREFIX | (value >> 8));
    out << char(value);
  } else if (value <= VARINT_MAX_3BYTE_VALUE) {
    out << char(VARINT_3BYTE_PREFIX | (value >> 16));
    out << char(value >> 8);
    out << char(value);
  } else {
    diag.error() << "Integer value '" << value << "' too large to be encoded.";
  }
}

//uint32_t readVarInt();

} // namespace tart

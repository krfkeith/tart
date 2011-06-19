/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"
#include "tart/Meta/VarInt.h"

namespace tart {

void VarInt32::write(llvm::raw_ostream & out, uint32_t value) {
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
    out << char(VARINT_INT32_PREFIX);
    out << char(value >> 24);
    out << char(value >> 16);
    out << char(value >> 8);
    out << char(value);
  }
}

void VarInt64::write(llvm::raw_ostream & out, uint64_t value) {
  if (value < VARINT_MIN_2BYTE_VALUE) {
    out << char(value);
  } else if (value < VARINT_MIN_3BYTE_VALUE) {
    out << char(VARINT_2BYTE_PREFIX | (value >> 8));
    out << char(value);
  } else if (value <= VARINT_MAX_3BYTE_VALUE) {
    out << char(VARINT_3BYTE_PREFIX | (value >> 16));
    out << char(value >> 8);
    out << char(value);
  } else if (value <= 0x0ffff) {
    out << char(VARINT_INT32_PREFIX);
    out << char(value >> 24);
    out << char(value >> 16);
    out << char(value >> 8);
    out << char(value);
  } else {
    out << char(VARINT_INT64_PREFIX);
    out << char(value >> 56);
    out << char(value >> 48);
    out << char(value >> 40);
    out << char(value >> 32);
    out << char(value >> 24);
    out << char(value >> 16);
    out << char(value >> 8);
    out << char(value);
  }
}

} // namespace tart

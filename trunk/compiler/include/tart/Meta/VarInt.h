/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_VARINT_H
#define TART_META_VARINT_H

#include "llvm/Support/raw_ostream.h"

namespace tart {

/// -------------------------------------------------------------------
/// Variable-length integers.
///
/// Variable-length integers are encoded using the following patterns:
///
///   00nnnnnn
///   01nnnnnn
///   10nnnnnn
///   110nnnnn nnnnnnnn
///   1110nnnn nnnnnnnn nnnnnnnn
///   11111110 nnnnnnnn nnnnnnnn nnnnnnnn nnnnnnnn
///   11111111 nnnnnnnn nnnnnnnn nnnnnnnn nnnnnnnn nnnnnnnn nnnnnnnn nnnnnnnn nnnnnnnn
///
///   1-byte sequence header:    00000000 - 10111111 (0x00 - 0xAF)
///   2-byte sequence header:    11000000 - 11011111 (0xC0 - 0xDF)
///   3-byte sequence header:    11100000 - 11101111 (0xE0 - 0xEF)
///   uncompressed int32 header: 11111110 - 11111110 (0xFE)
///   uncompressed int64 header: 11111111 - 11111111 (0xFF)

enum VarIntConstants {
  VARINT_MIN_2BYTE_VALUE = 0xC0,
  VARINT_MAX_2BYTE_VALUE = 0x1fff,
  VARINT_MIN_3BYTE_VALUE = 0x2000,
  VARINT_MAX_3BYTE_VALUE = 0x0fffff,

  VARINT_2BYTE_PREFIX = 0xC0,
  VARINT_3BYTE_PREFIX = 0xE0,
  VARINT_INT32_PREFIX = 0xFE,
  VARINT_INT64_PREFIX = 0xFF,

  VARINT_2BYTE_MASK = ~VARINT_2BYTE_PREFIX,
  VARINT_3BYTE_MASK = ~VARINT_3BYTE_PREFIX,
};

/// -------------------------------------------------------------------
/// Wrapper for varints used in serialization.

class VarInt32 {
public:
  VarInt32(uint32_t value) : value_(value) {}

  uint32_t value() const { return value_; }

  static void write(llvm::raw_ostream & out, uint32_t value);

private:
  uint32_t value_;
};

class VarInt64 {
public:
  VarInt64(uint64_t value) : value_(value) {}

  uint64_t value() const { return value_; }

  static void write(llvm::raw_ostream & out, uint64_t value);

private:
  uint64_t value_;
};

inline VarInt32 VarInt( int32_t value) { return VarInt32(uint32_t(value)); }
inline VarInt32 VarInt(uint32_t value) { return VarInt32(value); }
inline VarInt64 VarInt(uint64_t value) { return VarInt64(uint64_t(value)); }
inline VarInt64 VarInt( int64_t value) { return VarInt64(value); }

} // namespace tart

namespace llvm {
  inline raw_ostream & operator<<(raw_ostream & out, const tart::VarInt32 & varint) {
    varint.write(out, varint.value());
    return out;
  }
  inline raw_ostream & operator<<(raw_ostream & out, const tart::VarInt64 & varint) {
    varint.write(out, varint.value());
    return out;
  }
}

#endif // TART_META_TAGS_H

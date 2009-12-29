/** String formatting functions for primitive types. */

#include "tart_object.h"
#include "tart_string.h"

#if HAVE_STRING_H
#include <string.h>
#endif

#if HAVE_STDIO_H
#include <stdio.h>
#endif

#if HAVE_STDBOOL_H
#include <stdbool.h>
#endif

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if WIN32
  #define bool int
  #define snprintf _snprintf
#endif

const String * bool_toString(bool value) {
  if (value) {
    return String_create("True", 4);
  } else {
    return String_create("False", 5);
  }

  return NULL;
}

const String * char_toString(uint32_t value) {
  char data[16];
  size_t length;
  if (value == 0) {
    length = snprintf(data, sizeof(data), "\\0");
  } else if (value < 0x32) {
    length = snprintf(data, sizeof(data), "\\x%x", value);
  } else {
    length = snprintf(data, sizeof(data), "%lc", value);
  }

  return String_create(data, length);
}

const String * byte_toString(int8_t value) {
  char data[16];
  size_t length = snprintf(data, sizeof(data), "%d", value);
  return String_create(data, length);
}

const String * short_toString(int16_t value) {
  char data[16];
  int length = snprintf(data, sizeof(data), "%d", value);
  return String_create(data, length);
}

const String * int_toString(int32_t value) {
  char data[16];
  int length = snprintf(data, sizeof(data), "%d", value);
  return String_create(data, length);
}

const String * long_toString(int64_t value) {
  char data[32];
#if SIZEOF_LONG_LONG == 64
  int length = snprintf(data, sizeof(data), "%lld", value);
#elif SIZEOF_LONG == 64
  int length = snprintf(data, sizeof(data), "%ld", (long)value);
#else
  #error Unsupported size for int64_t
#endif
  return String_create(data, length);
}

const String * ubyte_toString(uint8_t value) {
  char data[16];
  int length = snprintf(data, sizeof(data), "%u", value);
  return String_create(data, length);
}

const String * ushort_toString(uint16_t value) {
  char data[16];
  int length = snprintf(data, sizeof(data), "%u", value);
  return String_create(data, length);
}

const String * uint_toString(uint32_t value) {
  char data[16];
  int length = snprintf(data, sizeof(data), "%u", value);
  return String_create(data, length);
}

const String * ulong_toString(uint64_t value) {
  char data[32];
#if SIZEOF_LONG_LONG == 64
  int length = snprintf(data, sizeof(data), "%llu", value);
#elif SIZEOF_LONG == 64
  int length = snprintf(data, sizeof(data), "%lu", (unsigned long)value);
#else
  #error Unsupported size for int64_t
#endif
  return String_create(data, length);
}

const String * float_toString(float value) {
  char data[16];
  int length = snprintf(data, sizeof(data), "%f", value);
  return String_create(data, length);
}

const String * double_toString(double value) {
  char data[32];
  int length = snprintf(data, sizeof(data), "%lf", value);
  return String_create(data, length);
}

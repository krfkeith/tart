/** String formatting functions for primitive types. */

#include "tart_object.h"
#include "tart_string.h"
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

const String * bool_toString(bool value, const String * format) {
  if (value) {
    return String_create("True", 4);
  } else {
    return String_create("False", 5);
  }

  return NULL;
}

const String * char_toString(uint32_t value, const String * format) {
  if (format == NULL) {
    char data[4];
    int length;
    if (value == 0) {
      length = snprintf(data, sizeof(data), "\\0");
    } else if (value < 0x32) {
      length = snprintf(data, sizeof(data), "\\x%x", value);
    } else {
      length = snprintf(data, sizeof(data), "%lc", value);
    }
    return String_create(data, length);
  }

  exit(-1);
}

const String * byte_toString(int8_t value, const String * format) {
  if (format == NULL) {
    char data[16];
    int length = snprintf(data, sizeof(data), "%d", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * short_toString(int16_t value, const String * format) {
  if (format == NULL) {
    char data[16];
    int length = snprintf(data, sizeof(data), "%d", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * int_toString(int32_t value, const String * format) {
  if (format == NULL) {
    char data[16];
    int length = snprintf(data, sizeof(data), "%d", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * long_toString(int64_t value, const String * format) {
  if (format == NULL) {
    char data[32];
    int length = snprintf(data, sizeof(data), "%lld", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * ubyte_toString(uint8_t value, const String * format) {
  if (format == NULL) {
    char data[16];
    int length = snprintf(data, sizeof(data), "%u", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * ushort_toString(uint16_t value, const String * format) {
  if (format == NULL) {
    char data[16];
    int length = snprintf(data, sizeof(data), "%u", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * uint_toString(uint32_t value, const String * format) {
  if (format == NULL) {
    char data[16];
    int length = snprintf(data, sizeof(data), "%u", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * ulong_toString(uint64_t value, const String * format) {
  if (format == NULL) {
    char data[32];
    int length = snprintf(data, sizeof(data), "%llu", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * float_toString(float value, const String * format) {
  if (format == NULL) {
    char data[16];
    int length = snprintf(data, sizeof(data), "%f", value);
    return String_create(data, length);
  }

  exit(-1);
}

const String * double_toString(double value, const String * format) {
  if (format == NULL) {
    char data[32];
    int length = snprintf(data, sizeof(data), "%lf", value);
    return String_create(data, length);
  }

  exit(-1);
}

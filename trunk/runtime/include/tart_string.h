/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_RUNTIME_TART_STRING_H
#define TART_RUNTIME_TART_STRING_H

#ifndef TART_RUNTIME_TART_OBJECT_H
#include "tart_object.h"
#endif

#include "llvm/System/DataTypes.h"

/** Tart string class. */
typedef struct TartString {
  struct TartObject object;
  intptr_t length;
  struct TartString * source;
  char * start;
  char chars[1];
} String;

/** Tart function to create a new String object from a UTF-8 encoded
    character array.
 */
extern const String * String_create(char * data, int32_t length);

#endif

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/** Object header for Tart runtime objects. */

#ifndef TART_RUNTIME_TART_OBJECT_H
#define TART_RUNTIME_TART_OBJECT_H

#include "config.h"

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

struct TypeInfoBlock;

typedef struct TartObject {
  struct TypeInfoBlock * tib;
  size_t gcinfo;
} Object;

#endif

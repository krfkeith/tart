/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
/** Object header for Tart runtime objects. */

#ifndef TART_RUNTIME_TART_OBJECT_H
#define TART_RUNTIME_TART_OBJECT_H

struct TypeInfoBlock;

typedef struct TartObject {
  struct TypeInfoBlock * tib;
  /** Garbage collection data. */
} Object;

#endif

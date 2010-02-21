#include "config.h"

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if HAVE_STDIO_H
#include <stdio.h>
#endif

extern "C" {
  void * GC_alloc(size_t size, void * localAlloc);
}

struct AllocationBlock {
  AllocationBlock * next;
  uintptr_t start;
  uintptr_t end;
  uintptr_t pos;
};

//__thread AllocationBlock * localAlloc;

void * GC_alloc(size_t size) {
  // First thing to do: Align.

  //fprintf(stderr, "Alloc size: %d, %p\n", int(size), localAlloc);
  return NULL;
}

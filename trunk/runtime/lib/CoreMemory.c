/** Low-level memory allocation functions. */

#include "config.h"

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

void * PageAllocator_allocImpl(int pageSize, int numPages) {
#ifdef HAVE_POSIX_MEMALIGN
  void * memptr;
  if (posix_memalign(&memptr, pageSize, pageSize * numPages) == 0) {
    return memptr;
  }

  // TODO: Throw an exception instead.
  return NULL;
#elif HAVE_VALLOC
  return valloc(pageSize * numPages);
#else
  #error("Missing aligned memory allocator for platform.")
  (void)pageSize;
  (void)numPages;
  return NULL;
#endif
}

void PageAllocator_freeImpl(void * mem, int size) {
#ifdef HAVE_POSIX_MEMALIGN
  free(mem);
  (void)size;
#elif HAVE_VALLOC
  free(mem);
  (void)size;
#else
  #error("Missing aligned memory allocator for platform.")
  (void)pageSize;
  (void)numPages;
  return NULL;
#endif
}

/* ================================================================== *
 * Local (per-thread) heap.
 * ================================================================== */
 
#ifndef COLLECTOR_LOCAL_H
#define COLLECTOR_LOCAL_H

#ifndef COLLECTOR_ALLOCATOR_H
#include "collector/Allocator.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif  

/* ==================================================================
   Various constants.
   ================================================================== */

#define GC_LOCAL_HEAP_SIZE (GC_CHUNK_SIZE-sizeof(gcheader_t))   /* 32K */
#define GC_MAX_LOCAL_ALLOC_SIZE 0x1000                          /* 4K */

/* ==================================================================
   A young generation heap, consisting of two semi-spaces.
   ================================================================== */

typedef struct _gc_local_heap {
  gcptr_t             begin;   /** Beginning of young generation heap */
  gcptr_t             end;     /** End of current young generation heap */
  gcptr_t             alloc;   /** Allocation pointer of yg heap */
  gcptr_t             prev;    /** Beginning of previous young generation heap */
} gc_local_heap;

/** Initialize a local heap. This involves allocating the semispace
    buffers from the global allocator. */
gc_local_init(gc_local_heap *);

/** Tear down a local heap. Return the semispaces to the global heap. */
gc_local_uninit(gc_local_heap *);

/** Allocate a block from the local heap. Note that the local heap
    does not support aligned allocations. */
void * gc_local_alloc(gc_local_heap *, gcsize_t size, void * tag);

/** Do a collection cycle on the local heap. */
void gc_local_collect(gc_local_heap *);

#ifdef __cplusplus
}
#endif  

#endif

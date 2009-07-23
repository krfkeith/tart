/* ================================================================== *
 * Global heap structures.
 * ================================================================== */
 
#ifndef COLLECTOR_ALLOCATOR_H
#define COLLECTOR_ALLOCATOR_H

#ifndef COLLECTOR_CONFIG_H
#include "collector/GCConfig.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif  

/* ==================================================================
   Various constants.
   ================================================================== */

#define GC_CHUNK_SIZE 0x8000                /* 32K */
#define GC_CHUNK_MASK (GC_CHUNK_SIZE-1)

#define GC_ALIGN_SIZE 8                     /* 8 bytes */
#define GC_ALIGN_MASK (~(GC_ALIGN_SIZE - 1))

#define GC_NUM_SMALL_BINS 32                /* 32 small bins = 32 bits */
#define GC_MAX_SMALL_SIZE (GC_ALIGN_SIZE * GC_NUM_SMALL_BINS) /* 256 bytes */
#define GC_MAX_SMALL_SIZE_LOG_2 8           /* 256 = 2^8 */

#define GC_NUM_LARGE_BINS 64                /* 64 large bins */

/* Meta field flags, stored in low 2 bits of the pointer. */
#define GC_FLAG_ALLOCATED 1                 /* Indicates an allocated block (global heap only) */
#define GC_FLAG_PREV_ALLOCATED 2            /* Indicates an allocated block (global heap only) */
#define GC_META_MASK ~3                     /* Meta field mask */

//#define GC_FLAG_FORWARDED 4                 /* Indicates a forwarded block (new heap only) */

/* Size field flags, stored in high bits of the size. */
#define GC_COLL_FLAG_1 ((SIZE_MAX>>1)+1)    /* Reserved for collector use. */
#define GC_COLL_FLAG_2 ((SIZE_MAX>>2)+1)    /* Reserved for collector use. */
#define GC_SIZE_MASK (SIZE_MAX>>2)          /* Size field mask */

#define GC_HEADER_SIZE (sizeof(gcheader_t) + GC_GUARD_BYTES)
#define GC_FOOTER_SIZE GC_GUARD_BYTES

#define GC_VALIDATE_IS_EMPTY 0x01           /* Verify that the heap is actually empty. */

/* ==================================================================
   Allocation header blocks.
   ================================================================== */

/** Allocation header block. Each allocation is preceded in memory
    by this structure. */
typedef struct _gcheader_t {
  /** High 29 bits are the metadata from the allocation. Low 2 bits
     are for allocator use. */
  uintptr_t meta;

  /** Exact (unaligned) size of the allocation. High bits are for
      collector use */
  gcsize_t size;
} gcheader_t;

/* ==================================================================
   Accessor macros.
   ================================================================== */

#define GC_IS_ALLOCATED(p) ((p)->meta & GC_FLAG_ALLOCATED)
#define GC_SET_ALLOCATED(p) ((p)->meta |= GC_FLAG_ALLOCATED)
#define GC_CLEAR_ALLOCATED(p) ((p)->meta &= ~GC_FLAG_ALLOCATED)

#define GC_IS_PREV_ALLOCATED(p) ((p)->meta & GC_FLAG_PREV_ALLOCATED)
#define GC_SET_PREV_ALLOCATED(p) ((p)->meta |= GC_FLAG_PREV_ALLOCATED)
#define GC_CLEAR_PREV_ALLOCATED(p) ((p)->meta &= ~GC_FLAG_PREV_ALLOCATED)

#define GC_TEST_FLAG(p, flag) (((p)->size & flag) != 0)
#define GC_SET_FLAG(p, flag) ((p)->size |= flag)
#define GC_CLEAR_FLAG(p, flag) ((p)->size &= ~flag)

#define GC_ALLOC_SIZE(p) ((p)->size & GC_SIZE_MASK)
#define GC_METADATA(p) ((void *)((p)->meta & GC_META_MASK))

//#define GC_IS_FORWARDED(p) ((uintptr_t)(p)->size & GC_FLAG_FORWARDED)
//#define GC_FORWARD_PTR(p) ((gcheader_ex_t*)p)->forward

/* ==================================================================
   Debugging functions.
   ================================================================== */
   
/* Macro to deal with result codes */
#define CHECK_RESULT( x ) { \
  int result = x;           \
  if (result != 0) {        \
    fprintf( stderr, "Operation " #x " failed with code %d: %s (%s: %d)\n", \
        result, strerror( result ), __FILE__, __LINE__ ); \
    exit( result );         \
  }                         \
}

/* ==================================================================
   Entry points to the allocator, not intended to be called directly
   from application code.
   ================================================================== */
   
/** Initialize global data. */
void gc_heap_init();
    
/** Global cleanup. */
void gc_heap_uninit();

/** Allocate a block of memory from the global heap. */
void * gc_heap_alloc(gcsize_t size, void * meta);

/** Allocate a block of memory from the global heap, with alignment and
    offset.
*/
void * gc_heap_alloc_aligned(gcsize_t size, gcsize_t alignment,
    gcoffset_t offset, void * meta);

/** Free a block of memory from the global heap. */
void gc_heap_free(void * addr);

/** Given the start of an allocated block, return the size of the
    allocation.
*/
gcsize_t gc_alloc_size(void * addr);

/** Given the start of an allocated block, return the application-
    specific metadata for that block.
*/
void * gc_alloc_metadata(void * addr);

/** Get the total size of the heap */
gcsize_t gc_heap_total_size();

/** Get the free size of the heap */
gcsize_t gc_heap_total_free();

/** Set a hook function which is called when the heap is about to
    expand. The hook function takes a single parameter which is
    the size of the allocation which triggered the heap growth.
    The hook is not required to take any action; However, if it returns
    true, then that indicates that the hook was able to scavenge enough
    free memory to satisfy the allocation, in which case the heap
    will attempt to check again if the allocation can be satisfied from
    available free memory. The hook function is called with the heap
    unlocked.
*/
void gc_set_expansion_hook(bool (*hook_func)(gcsize_t));

/** Return true if this is a valid heap address. (Note: Expensive). */
//bool gc_heap_is_valid_ptr(void * addr);

/** Validate the entire heap. */
bool gc_heap_validate(int flags);

/** Dump the heap via trace function. */
bool gc_heap_dump();
    
/** Walk all allocated blocks and free them if the given callback returns
    true. The heap will be locked while 'callback' is called.
    
    For small heaps, the heap will be locked during the entire traversal.
    For large heaps, the heap will be locked during traversal, but will
    periodically be unlocked.
    
    This function should only be called from a single thread, which will
    generally be the collector thread.
*/
void gc_heap_free_if(
    bool (*callback)(void * alloc, void * ctx),
    void * ctx);

#ifdef __cplusplus
}
#endif  

#endif

/* ================================================================== *
 * Global heap implementation.
 * ================================================================== */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>
#include <stdarg.h>
#include "collector/Allocator.h"

/* ==================================================================
   Free blocks.
   ================================================================== */
   
/** For the general heap, the structure of a free block. Note that for
    free blocks of 8 bytes, this entire structure will not fit, but
    that's OK because we don't use this structure in that case.
 */
typedef struct _gcfreeblk_t {
  gcsize_t                size;       /** Size + flags, stored at start of block. (Flags are always 0) */
  struct _gcfreeblk_t   * next;       /** Pointer to next block in free list */
  struct _gcfreeblk_t   * prev;       /** Pointer to prev block in free list */
} gcfreeblk_t;

/** Macro to remove a block from a block list */
#define GC_REMOVE_BLK(blk) {\
    blk->next->prev = blk->prev; \
    blk->prev->next = blk->next; \
}

/** Macro to insert a block after another one */
#define GC_INSERT_AFTER(prevblk, blk) {\
    (blk)->prev = (prevblk); \
    (blk)->next = (prevblk)->next; \
    (prevblk)->next = (blk); \
    (blk)->next->prev = (blk); \
}

#define GC_INSERT_BEFORE(nextblk, blk) {\
    (blk)->next = nextblk; \
    (blk)->prev = (nextblk)->prev; \
    (nextblk)->prev = (blk); \
    (blk)->prev->next = (blk); \
}

/** Tests if a block list is empty */
#define GC_EMPTY(blk) ((blk).next == &(blk))

/** Size of a free block */
#define GC_FREE_SIZE(p) ((p)->size)

/** An extended free block, for free blocks larger than 256 bytes. */
typedef struct _gcfreeblk_ex_t {
  gcsize_t                size;       /** Size + flags, stored at start of block */
  struct _gcfreeblk_ex_t* next;       /** Pointer to next (larger) block, or NULL if same-size block */
  struct _gcfreeblk_ex_t* prev;       /** Pointer to prev (smaller) block */
  struct _gcfreeblk_ex_t* ss_next;    /** Pointer to next same-size block in bin */
  struct _gcfreeblk_ex_t* ss_prev;    /** Pointer to prev same-size block in bin */
} gcfreeblk_ex_t;

/** Macro to remove a block from an extended block list */
#define GC_REMOVE_SS_BLK(blk) {\
    blk->ss_next->ss_prev = blk->ss_prev; \
    blk->ss_prev->ss_next = blk->ss_next; \
}

/** Macro to insert a block into an extended block list */
#define GC_INSERT_SS_AFTER(prevblk, blk) {\
    (blk)->ss_prev = prevblk; \
    (blk)->ss_next = (prevblk)->ss_next; \
    (prevblk)->ss_next = (blk); \
    (blk)->ss_next->ss_prev = (blk); \
}

/* ==================================================================
   Core segment blocks.
   ================================================================== */

/** A core segment, used to describe the available heap locations for
    allocations. The segment structure is located at the *end* of the
    segment itself. The segment structure serves a dual role:

    *  It is the list node in the linked list of segments.
    *  It serves as a sentinel value at the end of the segment memory.

    The segment structure "appears" to be the start of an allocated block,
    and has the same structure layout as gcheader_t - including the
    same placement of flag bits.

    To the allocator, a segment initially appears to be a large free
    block surrounded by allocated blocks at the start and end of the
    segment. This prevents the allocator from attempting to merge
    free blocks outside of the range of the segment.

    This illusion is maintained by setting GC_FLAG_ALLOCATED
    bits - one at the beginning of the segment, in the free block header,
    and one at the end of the segment, in the segment structure. Both
    bits are preserved regardless of the allocations/deallocations that
    occur within the segment.
 */
typedef struct _gccoresegment_t {
  uintptr_t               segnext;    /** Pointer to next core segment + flags */
  gcsize_t                segsize;    /** Size of this segment. */
} gccoresegment_t;

/* Macro to get the next segment. */
#define GC_NEXT_SEGMENT(seg) (gccoresegment_t *)((seg)->segnext & GC_META_MASK)

/* Set the footer size of a free block */
#define GC_BLK_FOOTER_SIZE( blk ) \
    ((gcsize_t *)((gcptr_t)(blk) + (blk)->size))[-1]

/* Fatal message */
#define GC_FATAL(x) __FILE__, __LINE__, "error", x
#define GC_WARN(x) __FILE__, __LINE__, "WARNING", x

/* ==================================================================
   Allocator globals.
   ================================================================== */

static gcsize_t         gHeapExtent;            /* Size of the heap in chunks, from 0 to highest addr. */
static gcsize_t         gHeapSize = 0;          /* Total size of the heap, in bytes. */
static gcsize_t         gHeapFreeSize = 0;      /* Total number of free bytes. */
static pthread_mutex_t  gFreeListLock;          /* Lock on free lists, lower priority than runlock. */
static gccoresegment_t  gCoreList;              /* List of core segments */
static gcfreeblk_t      gSmallBins[GC_NUM_SMALL_BINS];
static gcfreeblk_t      gLargeBins[GC_NUM_LARGE_BINS];
static uint32_t         gNonEmptySmallBins;     /* Bitfield indicating which small bins have stuff in them. */
static gcfreeblk_t    * gLastSplit;             /* Last free-block that was split */
static bool          (* gExpansionHook)(gcsize_t); /* Heap Expansion hook. */

/** Align a size to multiple of kAlignSize. */
inline gcsize_t AlignUp(gcsize_t size) {
  return (size + GC_ALIGN_SIZE - 1) & GC_ALIGN_MASK;
}

/** Calculate how much padding needs to be added to an address to align it. */
inline gcoffset_t AlignmentPadding(gcoffset_t address, gcsize_t alignment, gcoffset_t offset) {
  return (address + alignment - 1 - offset) / alignment * alignment + offset - address;
}

/** Return the bit index of the highest set bit. */
inline int HighestBit(gcsize_t n) {
  int b = 0;
  if (n >= (1<<16)) { n >>= 16; b += 16; }
  if (n >= (1<< 8)) { n >>=  8; b +=  8; }
  if (n >= (1<< 4)) { n >>=  4; b +=  4; }
  if (n >= (1<< 2)) { n >>=  2; b +=  2; }
  if (n >= (1<< 1)) { n >>=  1; b +=  1; }
  return b;
}

/* Given a size, calculate the bin that objects of that size go into.
   The bin number is computed as a kind of floating-point value, with
   5 bits of exponent and 1 bit of mantissa, giving a 6 bit number.
 */
inline uint32_t GCLargeBinIndex(gcsize_t size) {
  if (size < GC_MAX_SMALL_SIZE + GC_MAX_SMALL_SIZE / 2)
    return 0;
  int highbit = HighestBit(size);
  assert(highbit > 0);
  int nextbit = (size >> (highbit - 1)) & 1;
  return ((highbit - GC_MAX_SMALL_SIZE_LOG_2) << 1) | nextbit;
};

inline gcsize_t GCLargeBinSize(uint32_t index) {
  int exp = (index >> 1) + GC_MAX_SMALL_SIZE_LOG_2;
  return (gcsize_t)(1 << exp) + ((index & 1) << (exp - 1));
};

#if GC_ENABLE_TRACE
void gctracef(char * format, ...) {
  va_list ap;
  va_start(ap, format);
  vfprintf(stderr, format, ap);
  va_end(ap);
}

#else
inline void gctracef(char * format, ...) {}
#endif

#if GC_ENABLE_ERR
void gcerrorf(char * file, int32_t line, char * severity, char * format, ...) {
  char buffer[256];
  va_list ap;
  va_start(ap, format);
  vsnprintf(buffer, 256, format, ap);
  va_end(ap);
  fprintf(stderr, "%s:%d: %s: %s\n", file, line, severity, buffer);
}
#else
inline void gcerrorf(char * file, int32_t line, char * severity, char * format, ...) {}
#endif

/* Guard bytes functions */
#if GC_GUARD_BYTES != 0

static uint8_t guard_pattern[64] = {
  0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
  0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
  0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
  0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
  0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
  0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
  0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
  0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef,
};

static void fill(void * addr, uint32_t pattern, gcsize_t size) {
  uint32_t * ptr = (uint32_t *)addr;
  size >>= 2;
  while (size-- > 0) {
    *ptr++ = pattern;
  }
}

static void gc_guard_fill(gcheader_t * alloc, gcsize_t qsize) {
  fill((gcptr_t)alloc + sizeof(gcheader_t) + GC_GUARD_BYTES,
      0xbbadf00d, qsize - sizeof(gcheader_t) - GC_GUARD_BYTES * 2);
  memcpy((gcptr_t)alloc + sizeof(gcheader_t), guard_pattern,
      GC_GUARD_BYTES);
  memcpy((gcptr_t)alloc + qsize - GC_GUARD_BYTES, guard_pattern,
      GC_GUARD_BYTES);
}

static bool gc_guard_check(gcheader_t * alloc, gcsize_t qsize) {
  return !memcmp((gcptr_t)alloc + sizeof(gcheader_t), guard_pattern,
              GC_GUARD_BYTES) &&
         !memcmp((gcptr_t)alloc + qsize - GC_GUARD_BYTES, guard_pattern,
              GC_GUARD_BYTES);
}

static void gc_guard_verify(gcheader_t * alloc, gcsize_t qsize) {
  if (!gc_guard_check(alloc, qsize)) {
    GC_ERR((GC_FATAL("Failed guard bytes check around block %p, length %ld"),
        (gcptr_t)alloc + sizeof(gcheader_t) + GC_GUARD_BYTES,
        qsize - sizeof(gcheader_t) - 2 * GC_GUARD_BYTES));
  }
}

static void gc_free_fill(gcptr_t start, gcsize_t size) {
  fill(start, 0xdeadc0de, size);
}

#else

inline void gc_guard_fill(gcheader_t * alloc, gcsize_t len) {}
inline bool gc_guard_check(gcheader_t * alloc, gcsize_t qsize) { return true; }
inline void gc_guard_verify(gcheader_t * alloc, gcsize_t qsize) {}
static void gc_free_fill(gcptr_t start, gcsize_t size) {}

#endif

/* Forward declarations. */
void gc_remove_free_blk(gcfreeblk_t * blk);

/** Initialize global data. */
void gc_heap_init() {
  /* create a key for storing our thread-local data */
  CHECK_RESULT(pthread_mutex_init(&gFreeListLock, NULL))

  /* Initialize the list headers for the various bins. */
  int i;
  for (i = 0; i < GC_NUM_SMALL_BINS; i++) {
    gcfreeblk_t * bin = &gSmallBins[i];
    bin->next = bin->prev = bin;
  }
  for (i = 0; i < GC_NUM_LARGE_BINS; i++) {
    gcfreeblk_t * bin = &gLargeBins[i];
    bin->next = bin->prev = bin;
  }

  gCoreList.segnext = (uintptr_t)NULL;
  gLastSplit = NULL;
  gHeapExtent = 0;
  gHeapSize = 0;
  gHeapFreeSize = 0;
  gNonEmptySmallBins = 0;
}

/** Global cleanup. */
void gc_heap_uninit() {
  CHECK_RESULT(pthread_mutex_destroy(&gFreeListLock))
  gccoresegment_t * pseg = GC_NEXT_SEGMENT(&gCoreList);
  while (pseg != NULL) {
    gcptr_t segment_end = (gcptr_t)pseg + sizeof(gccoresegment_t);
    gcptr_t segment_begin = segment_end - pseg->segsize;
    pseg = GC_NEXT_SEGMENT(pseg);
    GC_RAW_FREE(segment_begin);
  }
}

/** Expand the heap. Must be called while heap lock is held. */
gcfreeblk_t * gc_grow_heap(gcsize_t size)
{
  /* Make sure we have enough room for the new allocation and
     a segment structure, rounded to the size of a chunk. */
  size = (size + sizeof(gccoresegment_t) + GC_CHUNK_SIZE - 1) & ~GC_CHUNK_MASK;
  gcptr_t new_core_begin = (gcptr_t)GC_RAW_ALLOC(size);
  if (new_core_begin == NULL) {
    GC_ERR((GC_FATAL("Attempt to expand heap by %ld bytes failed"), size));
    return NULL;
  }

  gcptr_t new_core_end = new_core_begin + size;

  /* Initially assume that the new free block is going to take up
     the entire new segment, minus the segment structure at the
     end. */
  gcptr_t freeblk_begin = new_core_begin;
  gcptr_t freeblk_end = new_core_end - sizeof(gccoresegment_t);
  gcsize_t size_change = size - sizeof(gccoresegment_t);

  GC_TRACE(("ALLOC:  * Expanded heap by %ld bytes, new address in range %p-%p\n", size, new_core_begin, new_core_end));

  /* Add to the list of segments */
  gccoresegment_t * pprev = &gCoreList;

  for (;;) {
    /* If we reached the end of the list */
    if (GC_NEXT_SEGMENT(pprev) == NULL) {
      break;
    }

    /* Get the address range of the next segment in the list */
    gccoresegment_t * pnext = GC_NEXT_SEGMENT(pprev);
    gcptr_t segment_end = (gcptr_t)pnext + sizeof(gccoresegment_t);
    gcptr_t segment_begin = segment_end - pnext->segsize;
    assert(segment_begin < segment_end);

    if (segment_end == new_core_begin) {
      /* Merge the two segments, with the new segment coming after the old one. */
      GC_TRACE(("ALLOC:     * Merging with previous segment %p-%p\n", segment_begin, segment_end));
      pprev->segnext = pnext->segnext;
      new_core_begin = segment_begin;

      /* We need to discover whether there is a free block at
         the end of the previous segment. If there is, then unlink
         it and merge it with the free block we're going to create. */
      if (!(pnext->segnext & GC_FLAG_PREV_ALLOCATED)) {
        gcsize_t prev_blk_size = ((gcsize_t *)pnext)[-1];
        gcfreeblk_t * prev_blk = (gcfreeblk_t *)((gcptr_t)pnext - prev_blk_size);
        GC_TRACE(("ALLOC:     * Merging with previous free block %p size %ld\n", prev_blk, prev_blk->size));
        gc_remove_free_blk(prev_blk);
        freeblk_begin = (gcptr_t)prev_blk;
      } else {
        freeblk_begin = (gcptr_t)pnext;
      }
      size_change += sizeof(gccoresegment_t);
      break;
    } else if (segment_end < new_core_begin) {
      /* The segment ends before our new core, so go to next seg. */
      pprev = pnext;
    } else if (segment_begin >= new_core_end) {
      break;
    } else {
      /* The only way we could get here is if there is an overlap. */
      fprintf(stderr, "Overlap between core segments %p-%p and %p-%p\n",
          new_core_begin, new_core_end,
          segment_begin, segment_end);
      exit(-1);
    }
  }

  if (GC_NEXT_SEGMENT(pprev) != NULL) {
    /* Check for merge with next segment. */
    gccoresegment_t * pnext = GC_NEXT_SEGMENT(pprev);
    gcptr_t segment_end = (gcptr_t)pnext + sizeof(gccoresegment_t);
    gcptr_t segment_begin = segment_end - pnext->segsize;

    if (new_core_end == segment_begin) {
      /* Merge the two segments. The simplest way to do this, for now,
         is to remove the old segment and add it's memory to the segment
         we are about to insert. */
      GC_TRACE(("ALLOC:     * Merging with next segment %p-%p\n", segment_begin, segment_end));
      pprev->segnext = pnext->segnext;
      pprev = pnext;
      new_core_end = segment_end;

      /* Discover whether the block at the beginning of this segment
         is allocated, and if so, remove it and add to the free block
         we're building. */
      if (!GC_IS_ALLOCATED((gcheader_t *)pnext)) {
        gcfreeblk_t * next_blk = (gcfreeblk_t *)pnext;
        GC_TRACE(("ALLOC:     * Merging with next free block %p size %ld\n", next_blk, next_blk->size));
        gc_remove_free_blk(next_blk);
        freeblk_end = (gcptr_t)next_blk + next_blk->size;
        size_change += sizeof(gccoresegment_t);
      }
    }
  }

  gccoresegment_t * pnewseg = (gccoresegment_t *)(new_core_end - sizeof(gccoresegment_t));
  if (GC_NEXT_SEGMENT(pprev) == NULL) {
    /* Add a new segment before the beginning of pnext */
    GC_TRACE(("ALLOC:     * Adding new segment %p-%p\n", new_core_begin, new_core_end));
    pnewseg->segnext = (uintptr_t)NULL | GC_FLAG_ALLOCATED;
  } else {
    /* If our address comes before that, then
       add a new segment before the beginning of pnext */
    pnewseg->segnext = pprev->segnext | GC_FLAG_ALLOCATED;
  }
  pnewseg->segsize = (new_core_end - new_core_begin);
  pprev->segnext = (uintptr_t)pnewseg | GC_FLAG_ALLOCATED;

  /* Keep track of the overall heap size. */
  gcsize_t extent = ((uintptr_t)new_core_end + GC_CHUNK_SIZE - 1) / GC_CHUNK_SIZE;
  if (extent > gHeapExtent) {
    extent += extent / 4;   /* Add a little extra so we don't have to do this often. */
    /* TODO: Implement heap chunk table. But lazily. */
    GC_TRACE(("ALLOC:     * Would have expanding heap chunk table to size %ld\n", extent));
    gHeapExtent = extent;
  }

  gHeapSize += size_change;
  gHeapFreeSize += size_change;

  /* Construct a new free block, not on any list, since the caller
     expects us to return an unlinked block.
  */
  gcfreeblk_t * freeblk = (gcfreeblk_t *)freeblk_begin;
  freeblk->size = freeblk_end - freeblk_begin;

  return freeblk;
}

/** Allocate a block of memory from the global heap. */
gcfreeblk_t * gc_get_free_blk(gcsize_t size)
{
  gcfreeblk_t * freeblk;
  bool bUseLastSplit = false;

  /* If there is a 'last split' block available, then we'll use it
     unless we can find an exact match. Don't use last split for aligned
     allocations (for now anyway.) */
  if (gLastSplit != NULL && gLastSplit->size >= size) {
    if (gLastSplit->size == size) {
      GC_TRACE(("ALLOC:  * Using last split block of exact size %ld (%p-%p)\n",
          gLastSplit->size,
          gLastSplit,
          (gcptr_t)gLastSplit + gLastSplit->size));
      return gLastSplit;
    }
    bUseLastSplit = true;
  }

  if (size < GC_MAX_SMALL_SIZE) {

    /* Compute which bin we're going to look in for free blocks. */
    int bin = size / GC_ALIGN_SIZE;

    /* Shift off bits representing bins smaller than the one we
       need, and see if there are any 1-bits left. */
    uint32_t binmask = (gNonEmptySmallBins >> bin);
    if (binmask != 0) {

      /* If there's a 'last split' block, and the size isn't
         an exact match, then return the last split block instead. */
      if (bUseLastSplit && (binmask & 1) == 0) {
        GC_TRACE(("ALLOC:  * Using last split block of size %ld (%p-%p)\n",
            gLastSplit->size,
            gLastSplit,
            (gcptr_t)gLastSplit + gLastSplit->size));
        return gLastSplit;
      }

      if ((binmask & 0x0000ffff) == 0) {
        binmask >>= 16;
        bin += 16;
      }
      if ((binmask & 0x000000ff) == 0) {
        binmask >>=  8;
        bin +=  8;
      }
      if ((binmask & 0x0000000f) == 0) {
        binmask >>=  4;
        bin +=  4;
      }
      if ((binmask & 0x00000003) == 0) {
        binmask >>=  2;
        bin +=  2;
      }
      if ((binmask & 0x00000001) == 0) {
        binmask >>=  1;
        bin +=  1;
      }
      /* At this point, we should now have the bin number of the
         first non-empty bin that is larger than or equal to
         the bin size that we need. */

      /* Get the address of the free list. */
      freeblk = gSmallBins[bin].next;
      assert(freeblk);
      GC_REMOVE_BLK(freeblk)

      /* If this caused the bin to become empty, then reset
         the bit for it. */
      if (GC_EMPTY(gSmallBins[bin])) {
        gNonEmptySmallBins &= ~(1 << bin);
      }

      GC_TRACE(("ALLOC:  * Found free block of size %ld (%p-%p) in small bin\n",
          freeblk->size,
          freeblk,
          (gcptr_t)freeblk + freeblk->size));
      return freeblk;
    }
  }

  /* Either the size was too big for the small bins, or we didn't
     find a small block large enough. */

  int largebin_index = GCLargeBinIndex(size);
  gcfreeblk_t * largebin = &gLargeBins[largebin_index];
  gcfreeblk_t * blk;

  /* Skip over any blocks smaller than what we need. */
  for (blk = largebin->next; blk != largebin && blk->size < size; blk = blk->next) {}

  /* If nothing was found: */
  if (blk == largebin) {
    /* Use the last split if available*/
    if (bUseLastSplit) {
      GC_TRACE(("ALLOC:  * Using last split block of size %ld (%p-%p) [empty bin]\n",
          gLastSplit->size,
          gLastSplit,
          (gcptr_t)gLastSplit + gLastSplit->size));
      return gLastSplit;
    }

    /* None of the blocks in that bin were large enough. Try the next bin.
       In this case, we only need to test the first block in the bin, since
       they are in sorted order, we will always get the smallest block. */
    blk = NULL;
    while (++largebin_index < GC_NUM_LARGE_BINS) {
      largebin = &gLargeBins[largebin_index];
      if (largebin->next != largebin) {
        blk = largebin->next;
        break;
      }
    }

    /* If we didn't find anything, return NULL. */
    if (blk == NULL)
      return NULL;

  } else if (blk->size > size && bUseLastSplit) {
    /* If the fit isn't an exact match, then use the 'last split'
       block. */
    GC_TRACE(("ALLOC:  * Using last split block of size %ld (%p-%p) [no exact size]\n",
        gLastSplit->size,
        gLastSplit,
        (gcptr_t)gLastSplit + gLastSplit->size));
    return gLastSplit;
  }

  /* Check to see if there are any duplicate-sized blocks here. If there
     are, then use one of those instead, since that will make delinking
     simpler. */
  gcfreeblk_ex_t * eblk = (gcfreeblk_ex_t *)blk;
  if (/*size >= GC_MAX_SMALL_SIZE &&*/ eblk->ss_next != eblk) {
    eblk = eblk->ss_next;
    GC_REMOVE_SS_BLK(eblk);
    blk = (gcfreeblk_t *)eblk;
  } else {
    GC_REMOVE_BLK(eblk);
  }

  /* Unlink the block we found, from both the bin list and the same
     size block list. */
  GC_TRACE(("ALLOC:  * Found free block of size %ld (%p-%p)\n",
      eblk->size,
      eblk,
      (gcptr_t)eblk + eblk->size));
  return blk;
}

/** Allocate a block of memory from the global heap, taking alignment considerations
    into account. This version is rather less efficient than the non-aligned version,
    since we can't always use the first block we find - it may have a large enough
    size to hold the allocation size, but will be of the wrong alignment. If an
    appropriate same-sized block cannot be found, we'll have to use a larger block to
    accomodate the alignment padding, and split it on the aligment boundary.

    This version is also less consistent about using the 'last-split' block when
    there's not an exact fit. Right now, we check small bins first (in size order),
    then the last split, and then the large bins in size order. We take the
    first fit we can find. All of this is due to the fact that 'fit-testing' is
    more expensive when alignment constraints are in play.
    */
gcfreeblk_t * gc_get_free_blk_aligned(gcsize_t size, gcsize_t alignment, gcsize_t offset)
{
  gcfreeblk_t * freeblk;

  if (size < GC_MAX_SMALL_SIZE) {

    /* Compute which bin we're going to look in for free blocks. */
    int bin = size / GC_ALIGN_SIZE;

    /* Shift off bits representing bins smaller than the one we
       need, and see if there are any 1-bits left. */
    uint32_t binmask = (gNonEmptySmallBins >> bin);
    while (binmask != 0) {

      /* Mechanism for having more than 32 small bins.
      while (binmask == 0) {
          bin = (bin + 32) & ~31
          binmask = gNonEmptySmallBins[bin >> 5];
      }*/

      // Skip over any empty bins.
      if ((binmask & 0x0000ffff) == 0) {
        binmask >>= 16;
        bin += 16;
      }
      if ((binmask & 0x000000ff) == 0) {
        binmask >>=  8;
        bin +=  8;
      }
      if ((binmask & 0x0000000f) == 0) {
        binmask >>=  4;
        bin +=  4;
      }
      if ((binmask & 0x00000003) == 0) {
        binmask >>=  2;
        bin +=  2;
      }
      if ((binmask & 0x00000001) == 0) {
        binmask >>=  1;
        bin +=  1;
      }

      /* At this point, we should now have the bin number of the
         first non-empty bin that is larger than or equal to
         the bin size that we need. */

      /* Get the address of the free list. */
      gcfreeblk_t * smallbin = &gSmallBins[bin];
      assert(smallbin);

      /* See if we can find a block in this bin that matches our
         alignment requirements.
       */
      for (freeblk = smallbin->next; freeblk != smallbin; freeblk = freeblk->next) {
        gcoffset_t padding = AlignmentPadding((gcoffset_t)freeblk, alignment, offset);
        if (freeblk->size + padding >= size) {
          GC_REMOVE_BLK(freeblk)
          /* If this caused the bin to become empty, then reset
             the bit for it. */
          if (GC_EMPTY(gSmallBins[bin])) {
            gNonEmptySmallBins &= ~(1 << bin);
          }

          GC_TRACE(("ALLOC:  * Found aligned free block of size %ld (%p-%p) in small bin\n",
              freeblk->size,
              freeblk,
              (gcptr_t)freeblk + freeblk->size));
          return freeblk;
        }
      }

      // Skip this bin; Try the next non-empty bin.
      binmask >>= 1;
      bin += 1;
    }
  }

  /* If there is a 'last split' block available, and it can contain
     the aligned request, then go ahead and use it. */
  if (gLastSplit != NULL) {
    /* Calculate the padding that would be needed to align this. */
    gcoffset_t padding = AlignmentPadding((gcoffset_t)gLastSplit, alignment, offset);
    GC_TRACE(("ALLOC:  * Checking if aligned alloc will fit in last split block %ld (%p-%p)\n",
        gLastSplit->size,
        gLastSplit,
        (gcptr_t)gLastSplit + gLastSplit->size));
    if (gLastSplit->size + padding >= size) {
      GC_TRACE(("ALLOC:  * Using last split block of size %ld (%p-%p)\n",
          gLastSplit->size,
          gLastSplit,
          (gcptr_t)gLastSplit + gLastSplit->size));
      return gLastSplit;
    }
  }

  /* Either the size was too big for the small bins, or we didn't
     find a small block large enough. */

  int largebin_index = GCLargeBinIndex(size);
  gcfreeblk_t * blk;

  /* Skip over any bins that are empty. */
  while (largebin_index < GC_NUM_LARGE_BINS) {
    /* Skip over any blocks smaller than what we need. */
    gcfreeblk_t * largebin = &gLargeBins[largebin_index];
    for (blk = largebin->next; blk != largebin; blk = blk->next) {
      /* If block is too small, skip it. */
      if (blk->size < size) {
        continue;
      }
      
      /* If block is large enough, it still might not fit because
         of padding. Check all linked blocks to see if we can find
         one that we can fit into. We start by checking the first
         block on the 'same-size' list to avoid having to unlink
         the 'by-size' list node, which will be checked last.
      */
      gcfreeblk_ex_t * eblk = (gcfreeblk_ex_t *)blk;
      gcfreeblk_ex_t * nblk = eblk->ss_next;
      do {
        /* This is a ring list, meaning that there's no list head,
           just a circle of nodes. In order to check every node, we
           have to do the test at the end of the loop (otherwise it will
           always skip one list node.) */
        gcoffset_t padding = AlignmentPadding((gcoffset_t)eblk, alignment, offset);
        if (nblk->size + padding >= size) {
          /* Unlink the block we found, from both the bin list and the same
             size block list. */
          GC_TRACE(("ALLOC:  * Found free block of size %ld (%p-%p), padding=%ld\n",
              nblk->size,
              nblk,
              (gcptr_t)nblk + nblk->size,
              padding));
          GC_REMOVE_BLK(nblk);
          GC_REMOVE_SS_BLK(nblk);
          return (gcfreeblk_t *)nblk;
        }
        nblk = nblk->ss_next;
      } while (nblk != eblk->ss_next) ;
    }

    largebin_index++;
  }

  return NULL;
}

/** Unlink a free block from the heap. */
void gc_remove_free_blk(gcfreeblk_t * blk) {
  if (blk == gLastSplit) {
    /* We 'remove' the last split block by simply clearing its pointer. */
    gLastSplit = NULL;
    /* GC_TRACE(("ALLOC:  * (Removed last split)\n")); */
  } else {
    gcsize_t size = blk->size;
    if (size < GC_MAX_SMALL_SIZE) {
      /* Fragments of size 8 or less are never added to a bin */
      if (size <= 8) {
        return;
      }

      /* Compute which bin we're going to use. */
      int bin = size / GC_ALIGN_SIZE;

      /* Remove this block from the small bin list. */
      GC_REMOVE_BLK(blk)
      /* GC_TRACE(("ALLOC:  * (Removed from small bin %d)\n", bin)); */

      /* If this caused the bin to become empty, then reset
         the bit for it. */
      if (GC_EMPTY(gSmallBins[bin])) {
        gNonEmptySmallBins &= ~(1 << bin);
      }
    } else {
      gcfreeblk_ex_t * eblk = (gcfreeblk_ex_t *)blk;
      if (eblk->next == NULL) {
        /* If the 'next' pointer is NULL, that means we're on a
           'same size' block list - that is, we're part of a
           linked list of blocks that are all the same size. So
           just remove this from that list. */
        GC_REMOVE_SS_BLK(eblk);
        /* GC_TRACE(("ALLOC:  * (Removed from same-size block list)\n")); */
      } else {
        /* Otherwise, if the 'next' pointer is non-null, that
           means that this block is a head block of a same-size
           list. This block is both a node in a large-bin list,
           and it also contains a list of same-size blocks.
            
           We need to take one of our same-size siblings, and
           make it the new head of the same-size list, adding
           it to the large-bin list in this block's place.
        */

        /* The next larger block in the large bin list. */
        gcfreeblk_ex_t * next_larger = eblk->next;

        /* The first same-size sibling. If the same-size
           list is empty, then it will point to eblk. */
        gcfreeblk_ex_t * first_same = eblk->ss_next;

        /* Remove from large bin and same-size lists. */
        GC_REMOVE_BLK(eblk);
        GC_REMOVE_SS_BLK(eblk);

        /* If same-size list was non-empty, pick one from the
           same-size list and add it to the bin. */
        if (first_same != eblk) {
          GC_INSERT_BEFORE(next_larger, first_same);
        }
        /* GC_TRACE(("ALLOC:  * (Removed from large bin list)\n")); */
      }
    }
  }
}

/** Put a block of free memory back on the heap. This does not do
    any merging of blocks. It will always be the case that the previous
    and next blocks are allocated. (If they were free blocks, we would
    have expanded them rather than calling this function.) */
void gc_insert_free_blk(gcfreeblk_t * blk) {
  gcsize_t size = blk->size;
  if (size < GC_MAX_SMALL_SIZE) {
    if (size > 8) {    /* Don't queue free blocks of size 8 */

      /* Compute which bin we're going to use. */
      int bin = size / GC_ALIGN_SIZE;

      GC_INSERT_AFTER(&gSmallBins[bin], blk);
      gNonEmptySmallBins |= (1 << bin);
    }
  } else {

    /* First thing we want to know is which bin it goes in.
       Each bin is a power of two larger than the last, so
       calculate the bin number as the highest bit set in the size. */
    int bin = GCLargeBinIndex(size);
    gcfreeblk_t * largebin = &gLargeBins[bin];
    gcfreeblk_t * fblk;

    /* Find the first block in the free list that is equal or greater
       in size than the block to be inserted. */
    for (fblk = largebin->next; fblk != largebin && fblk->size < size; fblk = fblk->next) {}

    if (fblk == largebin || blk->size != fblk->size) {
      /* Insert our new block immediately before the block we found.
         It's ok if fblk is treeblk. */
      //GC_TRACE(("ALLOC:    * Inserting before block of size %ld\n", fblk->size));
      GC_INSERT_BEFORE(fblk, blk);
      gcfreeblk_ex_t * eblk = (gcfreeblk_ex_t *)blk;

      /* Initialize the 'same-size block' list. */
      eblk->ss_next = eblk->ss_prev = eblk;
    } else {
      /* Add this block to the 'same-size block' list of fblk. */
      //GC_TRACE(("ALLOC:    * Inserting on same-size block list of block %p\n", fblk));
      blk->next = blk->prev = NULL;
      GC_INSERT_SS_AFTER((gcfreeblk_ex_t *)fblk, (gcfreeblk_ex_t *)blk);
    }
  }
}

/** Allocate a block of memory from the global heap. */
void * gc_heap_alloc_aligned(gcsize_t size, gcsize_t alignment, gcoffset_t offset, void * meta)
{
  gcsize_t qsize = AlignUp(size) + GC_HEADER_SIZE + GC_FOOTER_SIZE;

  /* Alignment can't be smaller than our default alignment size.
     Also, it makes no sense for offset to be greater than alignment. */
  if (alignment < GC_ALIGN_SIZE)
    alignment = GC_ALIGN_SIZE;
  if (offset < 0 || offset >= alignment || (offset & GC_ALIGN_MASK) != 0) {
    GC_TRACE(("ALLOC: Offset must be non-negative, less than alignment, and a multiple of %d\n", GC_ALIGN_SIZE));
  }

  GC_TRACE(("ALLOC: Allocating global block of exact size %ld, alloc size %ld, alignment %ld+%ld\n", size, qsize, alignment, offset));

  /* Adjust alignment to take into account the header. */
  offset = AlignmentPadding(GC_HEADER_SIZE, alignment, offset);
  if (offset != 0) {
    GC_TRACE(("ALLOC:  * Using actual offset of %ld\n", offset));
  }
  bool use_aligned = (alignment > GC_ALIGN_SIZE || offset != 0);

  CHECK_RESULT(pthread_mutex_lock(&gFreeListLock))

  gcfreeblk_t * newblk;
  if (use_aligned)
    newblk = gc_get_free_blk_aligned(qsize, alignment, offset);
  else
    newblk = gc_get_free_blk(qsize);
  if (newblk == NULL) {
    bool retry = false;
    /* No free space found, we need to finish whatever collection
       is in progress. */
    if (gExpansionHook != NULL) {
      CHECK_RESULT(pthread_mutex_unlock(&gFreeListLock))
      retry = (*gExpansionHook)(qsize);
      CHECK_RESULT(pthread_mutex_lock(&gFreeListLock))
    }

    if (retry) {
      if (use_aligned)
        newblk = gc_get_free_blk_aligned(qsize, alignment, offset);
      else
        newblk = gc_get_free_blk(qsize);
    }

    if (newblk == NULL) {
      /* No free space found, we need to grow the heap. Since we don't know
         on what address boundary the allocation will occur, make sure that it's
         large enough so that any alignment of the allocated block will fit within it.
      */
      newblk = gc_grow_heap(qsize + alignment);
      if (!newblk) {
        CHECK_RESULT(pthread_mutex_unlock(&gFreeListLock))
        return NULL;
      }
    }
  }

  /* At this point, newblk will have been unlinked from the heap. */

  /* If aligning the address produces a different address: */
  gcoffset_t padding = AlignmentPadding((gcoffset_t)newblk, alignment, offset);
  if (padding > 0) {
    /* We need to carve out an aligned block within the new block. */
    gcptr_t blk_end = (gcptr_t)newblk + newblk->size;

    /* Create a new free block representing the part before the alignment. */
    newblk->size = padding;
    GC_BLK_FOOTER_SIZE(newblk) = padding;
    if (newblk != gLastSplit)
      gc_insert_free_blk(newblk);

    GC_TRACE(("ALLOC:  * Splitting block for alignment, padding block is size %ld (%p-%p)\n",
        padding, newblk, (gcptr_t)newblk + padding));

    /* Reduce the size of the new block.*/
    newblk = (gcfreeblk_t *)((gcptr_t)newblk + padding);
    newblk->size = blk_end - (gcptr_t)newblk;
  }

  /* newblk is already naturally aligned. */
  gcsize_t blk_size = newblk->size;
  assert(blk_size >= qsize);
  if (blk_size > qsize) {

    //gcoffset_t align_adjust = AlignAddressAdjust
    gcfreeblk_t * split = (gcfreeblk_t *)((gcptr_t)newblk + qsize);
    split->size = blk_size - qsize;
    GC_BLK_FOOTER_SIZE(split) = split->size;
    GC_TRACE(("ALLOC:  * Splitting free block, new free block is size %ld (%p-%p)\n",
        split->size, split, (gcptr_t)split + split->size));
    /* Add 'split' back */
    if (gLastSplit != NULL && gLastSplit != newblk) {
      /* Put last split block back on free list */
      gc_insert_free_blk(gLastSplit);
    }
    gLastSplit = split;
  } else {
    /* The free block is an exact match.
       Update the following block (which must be an allocated block) to
       set the flag indicating that the previous block is also allocated. */
    gcheader_t * next_blk = (gcheader_t *)((gcptr_t)newblk + newblk->size);
    assert(GC_IS_ALLOCATED(next_blk));
    assert(!GC_IS_PREV_ALLOCATED(next_blk));
    GC_SET_PREV_ALLOCATED(next_blk);

    /* If this was the last split block, then set last split to NULL,
       since it is now taken. */
    if (gLastSplit == newblk)
      gLastSplit = NULL;
  }

  /* Return the allocated block. */
  CHECK_RESULT(pthread_mutex_unlock(&gFreeListLock))

  gHeapFreeSize -= qsize;

  /* Set up the header. Always set the allocated and previous allocated flag.
     (we know that the previous allocated flag can always be set because we
     always take from the start of a freeblock, meaning that the region
     before the free block must be allocated.) */
  //if (type_info == NULL) type_info = &gDefaultType;
  gcheader_t * header = (gcheader_t *)newblk;
  header->size = size;
  header->meta = (uintptr_t)meta | GC_FLAG_ALLOCATED | (padding == 0 ? GC_FLAG_PREV_ALLOCATED : 0);
  gc_guard_fill(header, qsize);

  return (gcptr_t)header + GC_HEADER_SIZE;
}

/** Allocate a block of memory from the global heap. */
void * gc_heap_alloc(gcsize_t size, void * meta) {
  return gc_heap_alloc_aligned(size, 0, 0, meta);
}

/** Free a block of memory from the global heap. */
void gc_heap_dealloc(void * addr) {
  gcptr_t blk_begin = (gcptr_t)addr - GC_HEADER_SIZE;
  gcheader_t * header = (gcheader_t *)(blk_begin);
  gcsize_t qsize = AlignUp(GC_ALLOC_SIZE(header)) + GC_HEADER_SIZE + GC_FOOTER_SIZE;
  gcptr_t blk_end = (gcptr_t)header + qsize;
  bool was_last_split = false;    /* True if we just merged with 'last split' */

  gc_guard_verify(header, qsize);
  gc_free_fill((gcptr_t)addr, GC_ALLOC_SIZE(header));

  GC_TRACE(("ALLOC: Freeing global allocation of size %ld (%p-%p)\n",
      (gcsize_t)(blk_end - blk_begin), blk_begin, blk_end));

  gHeapFreeSize += (blk_end - blk_begin);

  /** Merge with previous block if free */
  if (!GC_IS_PREV_ALLOCATED(header)) {
    gcsize_t prev_size = ((gcsize_t *)blk_begin)[-1];
    assert(prev_size >= 8);
    assert((prev_size & 7) == 0);
    GC_TRACE(("ALLOC:  * Merging with previous free block of size %ld (%p-%p)\n",
        prev_size, blk_begin - prev_size, blk_begin));
    blk_begin -= prev_size;
    assert(((gcfreeblk_t *)blk_begin)->size == prev_size);
    if ((gcfreeblk_t *)blk_begin == gLastSplit)
      was_last_split = true;
    gc_remove_free_blk((gcfreeblk_t *)blk_begin);
  }

  /** Merge with next block if free */
  if (!GC_IS_ALLOCATED((gcheader_t *)blk_end)) {
    gcfreeblk_t * next_blk = (gcfreeblk_t *)blk_end;
    GC_TRACE(("ALLOC:  * Merging with next free block of size %ld (%p-%p)\n",
        GC_FREE_SIZE(next_blk), blk_end, blk_end + next_blk->size));
    assert(GC_FREE_SIZE(next_blk) >= 8);
    assert((GC_FREE_SIZE(next_blk) & 7) == 0);
    blk_end += next_blk->size;
    if ((gcfreeblk_t *)next_blk == gLastSplit)
      was_last_split = true;
    gc_remove_free_blk(next_blk);
  }

  /* Clear the previous allocation bit */
  GC_CLEAR_PREV_ALLOCATED((gcheader_t *)blk_end);

  gcfreeblk_t * freeblk = (gcfreeblk_t *)blk_begin;

  /* Set the block size, which has the effect of clearing the allocated bits. */
  freeblk->size = blk_end - blk_begin;

  /* Clear the previous allocation bit and set size */
  GC_BLK_FOOTER_SIZE(freeblk) = freeblk->size;
  if (was_last_split) {
    gLastSplit = freeblk;
    GC_TRACE(("ALLOC:  * Merged free block %ld (%p-%p) is now last split block\n",
        freeblk->size, freeblk, (gcptr_t)freeblk + freeblk->size));
  } else {
    /* Link the block to the free list */
    gc_insert_free_blk(freeblk);
  }
}

/** Free a block of memory from the global heap. */
void gc_heap_free(void * addr) {
  CHECK_RESULT(pthread_mutex_lock(&gFreeListLock))
  gc_heap_dealloc(addr);
  CHECK_RESULT(pthread_mutex_unlock(&gFreeListLock))
}

void gc_heap_free_if(
    bool (*callback)(void * alloc, void * ctx),
    void * ctx)
{
  gccoresegment_t * segment;
  CHECK_RESULT(pthread_mutex_lock(&gFreeListLock))

  /* Walk each segment. */
  for (segment = GC_NEXT_SEGMENT(&gCoreList);
      segment != NULL;
      segment = GC_NEXT_SEGMENT(segment)) {
    gcptr_t seg_begin = (gcptr_t)segment + sizeof(gccoresegment_t) - segment->segsize;
    gcptr_t seg_end = (gcptr_t)segment;
    gcptr_t seg_addr = seg_begin;
    gcptr_t to_free = NULL;

    /* Theory of operation: Because the act of freeing an allocated
        block will disrupt iteration, we don't free the block
        immediately, but instead wait until we have iterated past it.
        This is done by storing the address to be freed in 'to_free'.
        
        In a future version of this function, we can even release
        the free list lock momentarily during iteration to allow
        other threads to run. 'to_free' should not be disturbed by
        this, since technically its garbage which means no one should
        be touching it. We can therefore use 'to_free' as a safe
        point for resuming the iteration. (Although the segment list
        may have changed - not sure how to deal with that yet.)
    */
    while (seg_addr < seg_end) {
      if (GC_IS_ALLOCATED((gcheader_t *)seg_addr)) {
        if (to_free != NULL) {
          gc_heap_dealloc(to_free);
        }
        gcheader_t * header = (gcheader_t *)seg_addr;
        to_free = seg_addr + GC_HEADER_SIZE;
        if (!callback(to_free, ctx)) {
          to_free = NULL;
        }
        seg_addr += AlignUp(GC_ALLOC_SIZE(header)) + GC_HEADER_SIZE + GC_FOOTER_SIZE;
      } else {
        gcfreeblk_t * freeblk = (gcfreeblk_t *)seg_addr;
        seg_addr += freeblk->size;
      }
    }

    if (to_free != NULL) {
      gc_heap_dealloc(to_free);
    }
  }

  CHECK_RESULT(pthread_mutex_unlock(&gFreeListLock))
}

gcsize_t gc_alloc_size(void * addr) {
  return GC_ALLOC_SIZE((gcheader_t *)addr);
}

void * gc_alloc_metadata(void * addr) {
  return (void *)GC_METADATA((gcheader_t *)addr);
}

gcsize_t gc_heap_total_size() {
  return gHeapSize;
}

gcsize_t gc_heap_total_free() {
  return gHeapFreeSize;
}

void gc_set_expansion_hook(bool (*hook_func)(gcsize_t)) {
  gExpansionHook = hook_func;
}

bool gc_heap_validate(int flags) {
  gccoresegment_t * segment;
  bool result = true;
  CHECK_RESULT(pthread_mutex_lock(&gFreeListLock))
  int i;
  int free_count = 0;
  int linked_free_count = 0;
  gcsize_t heap_size = 0;
  gcsize_t free_size = 0;
  bool foundLastSplit = false;

  /* Walk each segment. */
  for (segment = GC_NEXT_SEGMENT(&gCoreList);
      segment != NULL && result;
      segment = GC_NEXT_SEGMENT(segment)) {
    if (!GC_IS_ALLOCATED((gcheader_t *)segment)) {
      GC_ERR((GC_FATAL(
            "Segment node at %p is not marked as allocated."),
          segment));
    }

    gcptr_t seg_begin = (gcptr_t)segment + sizeof(gccoresegment_t) - segment->segsize;
    gcptr_t seg_end = (gcptr_t)segment;
    bool prev_allocated = true;
    gcptr_t seg_addr = seg_begin;

    heap_size += (seg_end - seg_begin);
    while (seg_addr < seg_end) {
      if (GC_IS_ALLOCATED((gcheader_t *)seg_addr)) {
        gcheader_t * header = (gcheader_t *)seg_addr;
        if (flags & GC_VALIDATE_IS_EMPTY) {
          GC_ERR((GC_FATAL(
                "Allocated block at %p size %ld - heap should be empty."),
              header, GC_ALLOC_SIZE(header)));
          result = false;
          break;
        }
        if (GC_ALLOC_SIZE(header) > seg_end - seg_addr) {
          GC_ERR((GC_FATAL(
                "Allocated block at %p has invalid size %ld"),
              header, GC_ALLOC_SIZE(header)));
          result = false;
          break;
        }
        if (GC_IS_PREV_ALLOCATED(header) && !prev_allocated) {
          GC_ERR((GC_FATAL(
                "Allocated block at %p claims that its predecessor is allocated, but it isn't"),
              header));
          result = false;
          break;
        } else if (!GC_IS_PREV_ALLOCATED(header) && prev_allocated) {
          GC_ERR((GC_FATAL(
                "Allocated block at %p claims that its predecessor is not allocated, but it is"),
              header));
          result = false;
          break;
        }
        prev_allocated = true;
        seg_addr += AlignUp(GC_ALLOC_SIZE(header)) + GC_HEADER_SIZE + GC_FOOTER_SIZE;
      } else {
        gcfreeblk_t * freeblk = (gcfreeblk_t *)seg_addr;
        if (!prev_allocated) {
          GC_ERR((GC_FATAL(
                "Free block at %p succeeds another free block - should have been merged"),
              freeblk));
          result = false;
          break;
        }

        if (freeblk->size > seg_end - seg_addr || freeblk->size < 8 || (freeblk->size & 7)) {
          GC_ERR((GC_FATAL(
                "Free block at %p has invalid size %ld"),
              freeblk, freeblk->size));
          result = false;
          break;
        }

        if (GC_BLK_FOOTER_SIZE(freeblk) != freeblk->size) {
          GC_ERR((GC_FATAL(
                "Free block at %p has invalid footer size %ld"),
              freeblk, GC_BLK_FOOTER_SIZE(freeblk)));
          result = false;
          break;
        }

        if (freeblk == gLastSplit) {
          // It's the last split block.
          foundLastSplit = true;
        } else if (freeblk->size < 8) {
          GC_ERR((GC_FATAL(
                "Free block at %p has bad size size %ld"),
              freeblk, freeblk->size));
        } else if (freeblk->size == 8) {
          free_count -= 1;
        } else if (freeblk->size < GC_MAX_SMALL_SIZE) {
          gcfreeblk_t * sblk;
          gcfreeblk_t * bin = &gSmallBins[freeblk->size / GC_ALIGN_SIZE];
          for (sblk = bin->next; sblk != freeblk && sblk != bin; sblk = sblk->next) {}
          if (sblk == bin) {
            GC_ERR((GC_FATAL(
                  "Free block at %p not found in bin %d"),
                freeblk, (int)(freeblk->size / GC_ALIGN_SIZE)));
            result = false;
            break;
          }
        } else {
          // TODO: Search large bins for this block.
          // TODO: It could also be the 'last split' block.
        }

        free_size += freeblk->size;
        prev_allocated = false;
        free_count += 1;
        seg_addr += freeblk->size;
      }
    }
  }

  /* Walk each free list */
  for (i = 0; i < GC_NUM_SMALL_BINS; i++) {
    gcfreeblk_t * bin = &gSmallBins[i];
    gcfreeblk_t * blk;
    if (gNonEmptySmallBins & (1 << i)) {
      if (bin->next == bin || bin->prev == bin) {
        GC_ERR((GC_FATAL(
              "Small bin %d is inconsistent with status bit"), i));
        result = false;
      }
    } else {
      if (bin->next != bin || bin->prev != bin) {
        GC_ERR((GC_FATAL(
              "Small bin %d is inconsistent with status bit"),
            i));
        result = false;
      }
    }

    gcsize_t min_bin_size = i * GC_ALIGN_SIZE;
    gcsize_t max_bin_size = (i + 1) * GC_ALIGN_SIZE - 1;
    for (blk = bin->next; blk != bin; blk = blk->next) {
      assert(blk->next->prev == blk);
      assert(blk->prev->next == blk);

      linked_free_count += 1;
      if (blk->size < min_bin_size) {
        GC_ERR((GC_FATAL(
              "Small bin %d contains a free block [%p] of size %ld, which is smaller than it's minimum size %ld"),
            i, blk, blk->size, min_bin_size));
        result = false;
        break;
      }

      if (blk->size > max_bin_size) {
        GC_ERR((GC_FATAL(
              "Small bin %d contains a free block [%p] of size %ld, which is large than it's maximum size %ld"),
            i, blk, blk->size, max_bin_size));
        result = false;
        break;
      }

      if (blk == gLastSplit) {
        GC_ERR((GC_FATAL(
              "Small bin %d contains the 'last split' block [%p]"),
            i, blk));
        result = false;
        break;
      }
    }
  }

  for (i = 0; i < GC_NUM_LARGE_BINS; i++) {
    gcfreeblk_t * bin = &gLargeBins[i];
    gcfreeblk_t * blk;
    gcsize_t min_bin_size = GCLargeBinSize(i);
    gcsize_t max_bin_size = GCLargeBinSize(i + 1) - 1;
    for (blk = bin->next; blk != bin; blk = blk->next) {
      assert(blk->next->prev == blk);
      assert(blk->prev->next == blk);

      gcfreeblk_ex_t * exblk = (gcfreeblk_ex_t *)blk;
      gcfreeblk_ex_t * e;

      linked_free_count += 1;
      if (blk->size < min_bin_size) {
        GC_ERR((GC_FATAL(
              "Large bin %d contains a free block [%p] of size %ld, which is smaller than it's minimum size %ld"),
            i, blk, blk->size, min_bin_size));
        result = false;
        break;
      }

      if (blk->size > max_bin_size) {
        GC_ERR((GC_FATAL(
              "Large bin %d contains a free block [%p] of size %ld, which is large than it's maximum size %ld"),
            i, blk, blk->size, max_bin_size));
        result = false;
        break;
      }

      if (blk == gLastSplit) {
        GC_ERR((GC_FATAL(
              "Large bin %d contains the 'last split' block [%p]"),
            i, blk));
        result = false;
        break;
      }

      assert((exblk->ss_next == NULL) == (exblk->ss_prev == NULL));

      for (e = exblk->ss_next; e != exblk; e = e->ss_next) {
        linked_free_count += 1;

        assert(e->ss_next->ss_prev == e);
        assert(e->ss_prev->ss_next == e);

        if (e->size < min_bin_size) {
          GC_ERR((GC_FATAL(
                "Large bin %d contains a free block [%p] of size %ld, which is smaller than it's minimum size %ld"),
              i, e, e->size, min_bin_size));
          result = false;
          break;
        }

        if (e->size > max_bin_size) {
          GC_ERR((GC_FATAL(
                "Large bin %d contains a free block [%p] of size %ld, which is large than it's maximum size %ld"),
              i, e, e->size, max_bin_size));
          result = false;
          break;
        }

        if ((gcfreeblk_t *)e == gLastSplit) {
          GC_ERR((GC_FATAL(
                "Large bin %d contains the 'last split' block [%p]"),
              i, e));
          result = false;
          break;
        }
      }
    }
  }

  if (gLastSplit != NULL) {
    if (gLastSplit->size < 8 || (gLastSplit->size & 7)) {
      GC_ERR((GC_FATAL(
            "Last split block at %p has invalid size %ld"),
          gLastSplit, gLastSplit->size));
      result = false;
    }

    if (!foundLastSplit) {
      GC_ERR((GC_FATAL(
            "Last split block [%p] of size %ld, was not found during heap walk"),
          gLastSplit, gLastSplit->size));
      result = false;
    }

    linked_free_count += 1;
  }

  if (heap_size != gHeapSize) {
    GC_ERR((GC_FATAL(
          "heap size reports %ld but is actually %ld"),
        gHeapSize, heap_size));
    result = false;
  }

  if (free_size != gHeapFreeSize) {
    GC_ERR((GC_FATAL(
          "heap free size reports %ld but is actually %ld"),
        gHeapFreeSize, free_size));
    result = false;
  }

  if (result && free_count != linked_free_count) {
    GC_ERR((GC_FATAL(
          "The number of free blocks (%d) does not match the number of blocks in the bin lists (%d)"),
        free_count, linked_free_count));

    /* Walk each free list */
    for (i = 0; i < GC_NUM_SMALL_BINS; i++) {
      gcfreeblk_t * bin = &gSmallBins[i];
      gcfreeblk_t * blk;
      if (bin->next != bin) {
        GC_TRACE(("Small bin %d:\n", i));
        for (blk = bin->next; blk != bin; blk = blk->next) {
          GC_TRACE(("   Block at %p size %ld.\n", blk, blk->size));
        }
      }
    }

    for (i = 0; i < GC_NUM_LARGE_BINS; i++) {
      gcfreeblk_t * bin = &gLargeBins[i];
      gcfreeblk_t * blk;
      if (bin->next != bin) {
        GC_TRACE(("Large bin %d:\n", i));
        for (blk = bin->next; blk != bin; blk = blk->next) {
          gcfreeblk_ex_t * eblk = (gcfreeblk_ex_t *)blk;
          gcfreeblk_ex_t * ss;
          GC_TRACE(("   Block at %p size %ld.\n", blk, blk->size));
          for (ss = eblk->ss_next; ss != eblk; ss = ss->ss_next) {
            GC_TRACE(("     Same-size block at %p size %ld.\n", ss, ss->size));
          }
        }
      }
    }

    if (gLastSplit != NULL) {
      GC_TRACE(("Last Split Block at %p size %ld.\n", gLastSplit, gLastSplit->size));
    }

    result = false;
  }

  CHECK_RESULT(pthread_mutex_unlock(&gFreeListLock))
  if (!result) {
    gc_heap_dump();
    assert(false);
  }
  return result;
}

bool gc_heap_dump() {
  gccoresegment_t * segment;
  bool result = true;
  int i;
  CHECK_RESULT(pthread_mutex_lock(&gFreeListLock))

  /* Walk each segment. */
  fprintf(stderr, "==== Heap Blocks ====\n");
  for (segment = GC_NEXT_SEGMENT(&gCoreList);
      segment != NULL && result;
      segment = GC_NEXT_SEGMENT(segment)) {
    gcptr_t seg_begin = (gcptr_t)segment + sizeof(gccoresegment_t) - segment->segsize;
    gcptr_t seg_end = (gcptr_t)segment;
    fprintf(stderr, "Heap Core Segment [A:%p-%p L:%ld (%ldK)]\n",
        seg_begin, seg_begin + segment->segsize, segment->segsize, (segment->segsize >> 10));

    gcptr_t seg_addr = seg_begin;
    while (seg_addr < seg_end) {
      gcptr_t prev_seg_addr = seg_addr;
      if (GC_IS_ALLOCATED((gcheader_t *)seg_addr)) {
        gcheader_t * header = (gcheader_t *)seg_addr;
        fprintf(stderr, "= Allocated [A: %p (%+ld in segment) L: %ld:%ld]\n",
            header,
            (gcsize_t)(seg_addr - seg_begin),
            GC_ALLOC_SIZE(header),
            AlignUp(GC_ALLOC_SIZE(header)) + GC_HEADER_SIZE + GC_FOOTER_SIZE);
        if (GC_ALLOC_SIZE(header) > seg_end - seg_addr) {
          fprintf(stderr, "ERROR: Allocated block at %p has invalid size %ld\n", header, GC_ALLOC_SIZE(header));
          break;
        }
        seg_addr += AlignUp(GC_ALLOC_SIZE(header)) + GC_HEADER_SIZE + GC_FOOTER_SIZE;
      } else {
        gcfreeblk_t * freeblk = (gcfreeblk_t *)seg_addr;
        fprintf(stderr, "= Free [A: %p (%+ld in seg) L:%ld]%s\n",
            freeblk,
            (gcsize_t)(seg_addr - seg_begin),
            freeblk->size,
            (freeblk == gLastSplit ? " (last split)" : ""));
        if (freeblk->size > seg_end - seg_addr || freeblk->size < 8 || (freeblk->size & 7)) {
          fprintf(stderr, "ERROR: Free block at %p has invalid size %ld\n", freeblk, freeblk->size);
          result = false;
          break;
        }
        seg_addr += freeblk->size;
      }
      if (seg_addr <= prev_seg_addr) {
        fprintf(stderr, "ERROR: Error walking heap\n");
        break;
      }
    }
  }

  fprintf(stderr, "==== Small Bins ====\n");
  for (i = 0; i < GC_NUM_SMALL_BINS; i++) {
    gcfreeblk_t * bin = &gSmallBins[i];
    gcfreeblk_t * blk;
    int count = 0;
    for (blk = bin->next; blk != bin; blk = blk->next) {
      count += 1;
    }
    
    if (count != 0) {
      fprintf(stderr, "Bin %d: %d blocks of size %d\n",
          i, count, i * GC_ALIGN_SIZE);
    }
  }

  fprintf(stderr, "==== Large Bins ====\n");
  for (i = 0; i < GC_NUM_LARGE_BINS; i++) {
    gcfreeblk_t * bin = &gLargeBins[i];
    gcfreeblk_t * blk;

    if (bin->next != bin) {
      fprintf(stderr, "Bin %d: ", i);
      for (blk = bin->next; blk != bin; blk = blk->next) {
        gcfreeblk_ex_t * exblk = (gcfreeblk_ex_t *)blk;
        gcfreeblk_ex_t * e;

        int count = 1;
        for (e = exblk->ss_next; e != exblk; e = e->ss_next) {
          count += 1;
        }

        fprintf(stderr, "%dx%ld ", count, blk->size);
      }

      fprintf(stderr, "\n");
    }
  }

#if 0
  if (gLastSplit != NULL) {
    if (gLastSplit->size < 8 || (gLastSplit->size & 7)) {
      GC_ERR((GC_FATAL(
            "Last split block at %p has invalid size %ld"),
          gLastSplit, gLastSplit->size));
      result = false;
    }

    if (!foundLastSplit) {
      GC_ERR((GC_FATAL(
            "Last split block [%p] of size %ld, was not found during heap walk"),
          gLastSplit, gLastSplit->size));
      result = false;
    }

    linked_free_count += 1;
  }
#endif

  CHECK_RESULT(pthread_mutex_unlock(&gFreeListLock))
  return result;
}

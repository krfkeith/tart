/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/* Tart Garbage collection functions. */

#include "gc_common.h"
#include "tart_object.h"

#if 0
#if HAVE_UNISTD_H
  #include <unistd.h>
#endif

#define USE_PTHREAD_THREAD_LOCAL 0
#if !HAVE_GCC_THREAD_LOCAL && HAVE_PTHREADS
  #include <pthread.h>
  #undef USE_PTHREAD_THREAD_LOCAL
  #define USE_PTHREAD_THREAD_LOCAL 1
#endif

#if HAVE_ALIGNED_MALLOC
  #include <malloc.h>
#endif

extern "C" {
  void GC_init(size_t *);
  LocalAllocState * GC_getLocalAllocState();
  char * GC_alloc(size_t size);
  void GC_sync();
  void GC_syncStart();
  void GC_syncEnd();
  void GC_collect();
  void GC_traceStack(tart_object * action);
  extern void TraceAction_traceDescriptors(tart_object * action,
      void * baseAddr, TraceDescriptor * traceTable);
}

static size_t pageSize;
static Segment * edenSegments = NULL;
static Segment * nextLasSegment = NULL;
static char * nextLasChar = NULL;
static SurvivorSpace ss[2];
static size_t numSafePoints;
static SafePointEntry * safePointTable;

//static SurvivorSpace * ssTo = &ss[0];
//static SurvivorSpace * ssFrom = &ss[1];

#if HAVE_GCC_THREAD_LOCAL
  __thread LocalAllocState * tldLas;
#endif

#if USE_PTHREAD_THREAD_LOCAL
  pthread_key_t lasKey;
#endif

#if HAVE_MSVC_THREAD_LOCAL
  __declspec(thread) LocalAllocState * tldLas;
#endif

inline size_t GC_align(size_t size) {
  return (size + (MEM_ALIGN_SIZE - 1)) & ~(MEM_ALIGN_SIZE - 1);
}

void * GC_getPages(size_t size) {
  #ifdef HAVE_POSIX_MEMALIGN
    void * memptr;
    if (posix_memalign(&memptr, pageSize, size) == 0) {
      return memptr;
    }

    fprintf(stderr, "posix_memalign failed\n");
    abort();
    return NULL;
  #elif HAVE_VALLOC
    return valloc(size);
  #elif HAVE_ALIGNED_MALLOC
    return _aligned_malloc(size, pageSize);
  #else
    #error("Missing aligned memory allocator for platform.")
    (void)pageSize;
    (void)numPages;
    return NULL;
  #endif
}

void GC_addSegments(size_t size, Segment ** seglist) {
  AddressRange newRange;
  newRange.first = (char *) GC_getPages(size);
  newRange.last = newRange.first + size;

  for (Segment ** sptr = seglist;;) {
    Segment * s = *sptr;
    if (s == NULL || s->range.first > newRange.last) {
      // Insert new segment before current segment
      Segment * newSeg = (Segment *) malloc(sizeof (Segment));
      newSeg->range.first = newRange.first;
      newSeg->range.last = newRange.last;
      newSeg->next = s;
      *sptr = newSeg;
      break;
    } else if (s->range.first == newRange.last) {
      // Expand current segment to include new segment
      s->range.first = newRange.first;
      break;
    } else if (s->range.last == newRange.first) {
      // Expand current segment to include new segment
      s->range.last = newRange.last;

      // And next segment if the new segment fills the hole between them.
      if (s->next != NULL && s->next->range.first == newRange.first) {
        Segment * next = s->next;
        s->range.last = next->range.last;
        s->next = next->next;
        free(next);
      }

      break;
    } else if (s->range.last < newRange.first) {
      // Go to next segment
      sptr = &(*sptr)->next;
    } else {
      fprintf(stderr, "Invalid segment overlap: %p-%p, %p-%p\n",
          newRange.first, newRange.last,
          s->range.first, s->range.last);
      abort();
    }
  }
}

void GC_init(size_t * safepointMap) {
  #ifdef HAVE_POSIX_MEMALIGN
    pageSize = sysconf(_SC_PAGESIZE);
  #elif HAVE_VALLOC
    pageSize = sysconf(_SC_PAGESIZE);
  #elif HAVE_ALIGNED_MALLOC
    pageSize = 4096;
  #endif

  #if USE_PTHREAD_THREAD_LOCAL
    pthread_key_create(&lasKey, NULL);
  #endif

  // Alloc survivor spaces
  ss[0].range.first = ss[0].pos = (char *) GC_getPages(0x8000);
  ss[0].range.last = ss[0].range.first + 0x8000;
  ss[1].range.first = ss[1].pos = (char *) GC_getPages(0x8000);
  ss[1].range.last = ss[1].range.first + 0x8000;

  // Alloc eden
  GC_addSegments(0x10000, &edenSegments);
  nextLasSegment = edenSegments;
  nextLasChar = edenSegments->range.first;

  // get list of safe points
  numSafePoints = *safepointMap;
  safePointTable = (SafePointEntry *)(safepointMap + 1);

  printf("Num safe points: %ld\n", numSafePoints);

//  GC_alloc(4);
//  GC_alloc(5);
  GC_alloc(6);
}

LocalAllocState * GC_getLocalAllocState() {
  #if HAVE_GCC_THREAD_LOCAL || HAVE_MSVC_THREAD_LOCAL
    // TODO: Get rid of this check, instead create LocalAllocState on thread startup.
    if (tldLas == NULL) {
      tldLas = (LocalAllocState *)malloc(sizeof(LocalAllocState));
      tldLas->pos = tldLas->end = NULL;
    }
    return tldLas;
  #elif USE_PTHREAD_THREAD_LOCAL
    LocalAllocState * las = (LocalAllocState *)pthread_getspecific(lasKey);
    // TODO: Get rid of this check, instead create LocalAllocState on thread startup.
    if (las == NULL) {
      las = (LocalAllocState *)malloc(sizeof(LocalAllocState));
      las->pos = las->end = NULL;
      pthread_setspecific(lasKey, las);
    }
    return las;
  #else
    #error No thread-local support for platform.
    return NULL;
  #endif
}

void GC_syncImpl(LocalAllocState * las) {
  CallFrame * framePtr;
  #if _MSC_VER
    #if SIZEOF_VOID_PTR == 4
      __asm {
        mov framePtr, ebp
      }
    #else
      __asm {
        mov framePtr, rbp
      }
    #endif
  #else
    #if SIZEOF_VOID_PTR == 4
      __asm__("movl %%ebp, %0" :"=r"(framePtr));
    #else
      __asm__("movq %%rbp, %0" :"=r"(framePtr));
    #endif
  #endif

  while (framePtr != NULL) {
    //void * fp = _Unwind_FindEnclosingFunction(framePtr->returnAddr);
    fprintf(stderr, "RBP: %p, %p\n", (void *)framePtr, framePtr->returnAddr);
    framePtr = framePtr->prevFrame;
  }

  fprintf(stderr, "\n");
  (void)las;
}

void GC_syncStartImpl(LocalAllocState * las) {
  (void)las;
}

void GC_syncEndImpl(LocalAllocState * las) {
  (void)las;
}

char * GC_alloc2(size_t size, LocalAllocState * las) {
  GC_syncImpl(las);

  size_t sizeAligned = GC_align(size);
  if (sizeAligned > MAX_EDEN_SIZE) {
    fprintf(stderr, "Alloc too large: %d\n", int(size));
    abort();
  }

  // See if it will fit in the current local block.
  if (las->pos + sizeAligned <= las->end) {
    char * result = las->pos;
    las->pos += sizeAligned;
    return result;
  }

  // If not, see if we can allocate another local block.
  // TODO: Need locking here.
  if (nextLasSegment != NULL) {
    size_t lasSize = nextLasSegment->range.last - nextLasChar;
    if (lasSize > MAX_LAS_SIZE) {
      lasSize = MAX_LAS_SIZE;
    }

    las->pos = nextLasChar;
    nextLasChar += lasSize;
    las->end = nextLasChar;

    // TODO: Unlock.

    if (las->pos + sizeAligned <= las->end) {
      char * result = las->pos;
      las->pos += sizeAligned;
      return result;
    }
  }

  // TODO: Run a collection.
  // Stop the world.

  fprintf(stderr, "Not enough room for alloc: %d\n", int(size));
  abort();
}

char * GC_alloc(size_t size) {
  return GC_alloc2(size, GC_getLocalAllocState());
}

void GC_sync() {
  GC_syncImpl(GC_getLocalAllocState());
}

void GC_syncStart() {
  GC_syncStartImpl(GC_getLocalAllocState());
}

void GC_syncEnd() {
  GC_syncEndImpl(GC_getLocalAllocState());
}

void GC_threadEnter() {
}

void GC_threadExit() {
}

class ObjectPrinter {
public:
  void operator()(tart_object ** obj) {
    (void)obj;
  }
};

void GC_collect() {
  printf("Checking for safe points\n");
  //tart::Tracer<ObjectPrinter> tracer;

  CallFrame * framePtr;
  #if _MSC_VER
    #if SIZEOF_VOID_PTR == 4
      __asm {
        mov framePtr, ebp
      }
    #else
      __asm {
        mov framePtr, rbp
      }
    #endif
  #else
    #if SIZEOF_VOID_PTR == 4
      __asm__("movl %%ebp, %0" :"=r"(framePtr));
    #else
      __asm__("movq %%rbp, %0" :"=r"(framePtr));
    #endif
  #endif

  SafePointEntry * spEnd = &safePointTable[numSafePoints];
  while (framePtr != NULL) {
    void * returnAddr = framePtr->returnAddr;
    framePtr = framePtr->prevFrame;

    printf("\nFound frame %p, return %p\n", framePtr, returnAddr);
    SafePointEntry * sp = safePointTable;
    while (sp < spEnd) {
      if (sp->safePoint == returnAddr) {
        uintptr_t baseAddr = uintptr_t(framePtr);
        if (sp->traceTable != NULL) {
          TraceDescriptor * desc = sp->traceTable;
          for (;; desc++) {
            if (desc->fieldCount != 0) {
              for (int i = 0; i < desc->fieldCount; ++i) {
                intptr_t offset = desc->offset + desc->fieldOffsets[i];
                uint8_t ** addr = (uint8_t **)(baseAddr + offset);
                if (*addr != NULL) {
                  printf("Found var in frame %p, offset %04ld addr %p value %p\n",
                      framePtr, offset, addr, *addr);
                }
              }
            } else {
              //uint8_t ** addr = (uint8_t **)(baseAddr + desc->offset);
              //(*desc->traceMethod)(addr, action);
              //assert(false && "trace methods not supported just yet");
            }
            if (desc->last) {
              break;
            }
          }
        }
        break;
      }

      sp++;
    }

    // given return address, find instruction list.
    // Call tracer.execute() on this list.
  }

  fprintf(stderr, "Finished checking for safe points\n");
  //(void)las;
}

void GC_traceStack(tart_object * traceAction) {
  CallFrame * framePtr;
  #if _MSC_VER
    #if SIZEOF_VOID_PTR == 4
      __asm {
        mov framePtr, ebp
      }
    #else
      __asm {
        mov framePtr, rbp
      }
    #endif
  #else
    #if SIZEOF_VOID_PTR == 4
      __asm__("movl %%ebp, %0" :"=r"(framePtr));
    #else
      __asm__("movq %%rbp, %0" :"=r"(framePtr));
    #endif
  #endif

  SafePointEntry * spEnd = &safePointTable[numSafePoints];
  while (framePtr != NULL) {
    void * returnAddr = framePtr->returnAddr;
    framePtr = framePtr->prevFrame;

    // TODO: Something better than a linear search.
    SafePointEntry * sp = safePointTable;
    while (sp < spEnd) {
      if (sp->safePoint == returnAddr) {
        TraceAction_traceDescriptors(traceAction, (void *)framePtr, sp->traceTable);
        break;
      }

      sp++;
    }
  }
}

void GC_trace(uint8_t * basePtr, TraceDescriptor * traceTable, tart_object * traceAction) {
  (void)basePtr;
  (void)traceTable;
  (void)traceAction;
//  tart::TracerBase * tpBase = static_cast<tart::TracerBase *>(ctx);
//  tpBase->execute(basePtr, traceTable);
}
#endif

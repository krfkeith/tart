/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/* Tart Garbage collection common functions. */

#include "gc_common.h"
#include "tart_object.h"

#if HAVE_UNISTD_H
  #include <unistd.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#if HAVE_STDIO_H
  #include <stdio.h>
#endif

#if HAVE_ASSERT_H
  #include <assert.h>
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
  void GC_initStackFrameDescMap(size_t * initData);
  void GC_traceStack(tart_object * action);
  extern void TraceAction_traceDescriptors(tart_object * action,
      void * baseAddr, TraceDescriptor * traceTable);
  void GC_initThreadLocalData();
  tart_object * GC_getThreadLocalData();
  void GC_setThreadLocalData(tart_object * obj);
  void * GC_allocAligned(size_t size);
}

namespace {
  size_t stackFrameDescMapSize;
  size_t stackFrameDescMapMask;
  StackFrameDescMapEntry * stackFrameDescMap;
  StaticRootsTableEntry * staticRootsTable;

  #if HAVE_GCC_THREAD_LOCAL
    __thread tart_object * tld;
  #endif

  #if USE_PTHREAD_THREAD_LOCAL
    pthread_key_t tldKey;
  #endif

  #if HAVE_MSVC_THREAD_LOCAL
    __declspec(thread) tart_object * tld;
  #endif
}

  /** Given an address, compute a hash of that address. */
static size_t GC_hashAddress(void * addr) {
  return (uintptr_t(addr) ^ (uintptr_t(addr) << 7)) * 2654435769u;
}

size_t GC_getPageSize() {
  #ifdef HAVE_POSIX_MEMALIGN
    return sysconf(_SC_PAGESIZE);
  #elif HAVE_VALLOC
    return sysconf(_SC_PAGESIZE);
  #elif HAVE_ALIGNED_MALLOC
    return 4096;
  #endif
}

void * GC_allocAligned(size_t size) {
  #ifdef HAVE_POSIX_MEMALIGN
    size_t pageSize = sysconf(_SC_PAGESIZE);
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

void GC_initStackFrameDescMap(size_t * initData) {
  // get list of stack frame descriptors
  size_t numEntries = *initData;
  StackFrameDescMapEntry * entries = (StackFrameDescMapEntry *)(initData + 1);

  // Find the nearest power of two larger than numEntries.
  size_t tableSize = 64;
  while (tableSize < numEntries) {
    tableSize <<= 1;
  }

  // Multiply by two to give room for collisions.
  stackFrameDescMapSize = tableSize * 2;
  stackFrameDescMapMask = stackFrameDescMapSize - 1;
  stackFrameDescMap = new StackFrameDescMapEntry[stackFrameDescMapSize];
  memset(stackFrameDescMap, 0, sizeof(StackFrameDescMapEntry) * stackFrameDescMapSize);

  for (size_t i = 0; i < numEntries; ++i, ++entries) {
    size_t index = GC_hashAddress(entries->instructionAddr) & stackFrameDescMapMask;
    size_t probeCount = 0;
    while (stackFrameDescMap[index].instructionAddr != NULL) {
      index = (index + 1) & stackFrameDescMapMask;
      #if HAVE_ASSERT_H
        if (probeCount >= 10) {
          fprintf(stderr, "Collision for table size %lu, index %lu, entry %lu\n",
              stackFrameDescMapSize, index, i);
          abort();
        }
        assert(probeCount < 5);
      #endif
    }
    #if HAVE_ASSERT_H
      assert(index < stackFrameDescMapSize);
    #endif
    stackFrameDescMap[index].instructionAddr = entries->instructionAddr;
    stackFrameDescMap[index].traceTable = entries->traceTable;
  }
}

static TraceDescriptor * GC_lookupStackFrameDesc(void * addr) {
  size_t index = GC_hashAddress(addr) & stackFrameDescMapMask;
  while (stackFrameDescMap[index].instructionAddr != NULL) {
    if (stackFrameDescMap[index].instructionAddr == addr) {
      return stackFrameDescMap[index].traceTable;
    }
    index = (index + 1) & stackFrameDescMapMask;
  }
  return NULL;
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

  while (framePtr != NULL) {
    void * returnAddr = framePtr->returnAddr;
    framePtr = framePtr->prevFrame;
    TraceDescriptor * tdesc = GC_lookupStackFrameDesc(returnAddr);
    if (tdesc != NULL) {
      TraceAction_traceDescriptors(traceAction, (void *)framePtr, tdesc);
    }
  }
}

void GC_traceStaticRoots(tart_object * traceAction) {
  for (StaticRootsTableEntry * root = staticRootsTable; root->rootAddr != NULL; ++root) {
    TraceDescriptor * tdesc = root->traceTable;
    TraceAction_traceDescriptors(traceAction, root->rootAddr, tdesc);
  }
}

void GC_initThreadLocalData() {
  #if USE_PTHREAD_THREAD_LOCAL
    pthread_key_create(&tldKey, NULL);
  #endif
}

tart_object * GC_getThreadLocalData() {
#if HAVE_GCC_THREAD_LOCAL || HAVE_MSVC_THREAD_LOCAL
  // TODO: Get rid of this check, instead create LocalAllocState on thread startup.
  return tld;
#elif USE_PTHREAD_THREAD_LOCAL
  return (tart_object *)pthread_getspecific(tldKey);
#else
  #error No thread-local support for platform.
  return NULL;
#endif
}

void GC_setThreadLocalData(tart_object * obj) {
#if HAVE_GCC_THREAD_LOCAL || HAVE_MSVC_THREAD_LOCAL
  tld = obj;
#elif USE_PTHREAD_THREAD_LOCAL
  pthread_setspecific(tldKey, obj);
#else
  #error No thread-local support for platform.
  return NULL;
#endif
}

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/** Tart Garbage collection functions. */

#include "config.h"

#if HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if HAVE_STDIO_H
#include <stdio.h>
#endif

#if HAVE_STDINT_H
#include <stdint.h>
#endif

struct tart_object;

enum GCInfoBits {
  GCInfo_Relocated = (1<<0),     // Object has moved to a new location
  GCInfo_Marked = (1<<1),        // Object is live
  GCInfo_Finalizable = (1<<2),   // Object has a finalizer
  // Pinned?
  // Color?
};

struct AddressRange {
  char * first;
  char * last;

  AddressRange() : first(NULL), last(NULL) {}
  AddressRange(char * f, char * l) : first(f), last(l) {}
  AddressRange(const AddressRange & rg) : first(rg.first), last(rg.last) {}

  size_t size() const { return last - first; }
};

struct SafePointEntry {
  void * safePoint;
  intptr_t * stackDescriptor;
};

struct Segment {
  Segment * next;
  AddressRange range;
};

struct LocalAllocState {
  void * framePtr;
  char * pos;
  char * end;
};

struct SurvivorSpace {
  AddressRange range;
  char * pos;
};

struct CallFrame {
  CallFrame * prevFrame;
  void * returnAddr;
};

static const size_t MEM_ALIGN_SIZE = (16 * sizeof(void *));
static const size_t MAX_EDEN_SIZE = 4096;
static const size_t MAX_LAS_SIZE = 0x8000;

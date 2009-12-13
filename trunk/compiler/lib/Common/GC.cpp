/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/GC.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceLocation.h"
#include <malloc.h>

namespace tart {

size_t GC::reclaimed;
size_t GC::total;
int GC::debugLevel;
GC * GC::allocList_ = NULL;

static bool initialized = false;

void * GC::operator new(size_t size) {
  DASSERT(initialized);
  GC * gc = reinterpret_cast<GC *>(malloc(size));
  gc->next_ = allocList_;
  allocList_ = gc;
  return gc;
}

void GC::operator delete(void * mem) {}

void GC::init() {
  DASSERT(!initialized);
  initialized = true;
  //gc_heap_init();
}

void GC::uninit() {
  DASSERT(initialized);
  initialized = false;
  //gc_heap_uninit();
}

bool GC::sweepCallback(void * alloc, void * ctx) {
  ++total;
  GC * gc = static_cast<GC *>(alloc);
  if (gc->marked_) {
    gc->marked_ = false;
    return false;
  } else {
    ++reclaimed;
    gc->~GC();
    return true;
  }
}

void GC::sweep() {
  reclaimed = 0;
  total = 0;
  //gc_heap_validate(0);
  //gc_heap_free_if(&sweepCallback, NULL);
  GC ** ptr = &allocList_;
  while (GC * gc = *ptr) {
    if (gc->marked_) {
      ptr = &gc->next_;
    } else {
      *ptr = gc->next_;
      free(gc);
    }
  }

  if (debugLevel) {
    diag.info(SourceLocation()) << "GC: " << reclaimed <<
        " objects reclaimed, " << (total - reclaimed) << " in use";
  }
  //gc_heap_validate(0);
}

}

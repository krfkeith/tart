/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Common/GC.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceLocation.h"

namespace tart {
    
size_t GC::reclaimed;
size_t GC::total;
int GC::debugLevel;
    
void * GC::operator new(size_t size) {
  return gc_heap_alloc(size, NULL);
}

void GC::operator delete(void * mem) {}

void GC::init() {
  gc_heap_init();
}

void GC::uninit() {
  gc_heap_uninit();
}

bool GC::sweepCallback(void * alloc, void * ctx) {
  ++total;
  GC * gc = static_cast<GC *>(alloc);
  if (gc->marked) {
    gc->marked = false;
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
  gc_heap_validate(0);
  gc_heap_free_if(&sweepCallback, NULL);
  if (debugLevel) {
    diag.info(SourceLocation()) << "GC: " << reclaimed <<
        " objects reclaimed, " << (total - reclaimed) << " in use";
  }
  gc_heap_validate(0);
}

}

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "config.h"
#include "tart/Common/GC.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceLocation.h"

#if HAVE_MALLOC_H
  #include <malloc.h>
#endif

#if HAVE_STDLIB_H
  #include <stdlib.h>
#endif

#include <llvm/ADT/SmallVector.h>

#define GC_DEBUG 0

namespace tart {

namespace {
  size_t reclaimed;
  size_t total;
  int debugLevel;
}

static bool initialized = false;

// -------------------------------------------------------------------
// GC

GC * GC::allocList_ = NULL;
GCRootBase * GC::roots_ = NULL;
GC::CallbackList GC::uninitCallbacks_;
GC::WeakPtrList GC::weakPtrs_;
GC::ObjectList GC::toTrace_;

unsigned char GC::cycleIndex_ = 0;

void * GC::operator new(size_t size) {
  DASSERT(initialized);
  GC * gc = reinterpret_cast<GC *>(malloc(size));
  #if GC_DEBUG
    memset(gc, 0xDB, size);
  #endif
  gc->next_ = allocList_;
  gc->cycle_ = cycleIndex_;
  allocList_ = gc;
  return gc;
}

void GC::operator delete(void * mem) {}

void GC::init() {
  DASSERT(!initialized);
  initialized = true;
}

void GC::uninit() {
  DASSERT(initialized);
  initialized = false;
  for (llvm::SmallVector<GC::Callback *, 8>::iterator it = uninitCallbacks_.begin();
      it != uninitCallbacks_.end(); ++it) {
    (*it)->call();
  }

  uninitCallbacks_.clear();
}

void GC::registerUninitCallback(Callback * cb) {
  uninitCallbacks_.push_back(cb);
}

void GC::sweep() {
  reclaimed = 0;
  total = 0;

  // Increment the collection cycle index
  ++cycleIndex_;

  // Trace all roots.
  for (GCRootBase * root = roots_; root != NULL; root = root->next_) {
    root->trace();
  }

  // Trace all deferred objects. This prevents tracing from using too much stack.
  while (!toTrace_.empty()) {
    const GC * obj = toTrace_.back();
    toTrace_.pop_back();
    obj->trace();
  }

  // Update all of the weak pointers we found during the sweep.
  for (WeakPtrList::const_iterator it = weakPtrs_.begin(), itEnd = weakPtrs_.end();
      it != itEnd; ++it) {
    GCWeakPtrBase * wp = *it;
    if (wp->ptr_->cycle_ != cycleIndex_) {
      wp->finalize();
      wp->ptr_ = NULL;
    }
  }
  weakPtrs_.clear();

  // Delete any allocated object not marked.
  GC ** ptr = &allocList_;
  while (GC * gc = *ptr) {
    if (gc->cycle_ == cycleIndex_) {
      ptr = &gc->next_;
    } else {
      *ptr = gc->next_;
      gc->~GC();
      #if GC_DEBUG
        size_t sz = malloc_usable_size(gc);
        memset(gc, 0xDF, sz);
      #endif
      free(gc);
    }
  }

  if (debugLevel) {
    diag.info(SourceLocation()) << "GC: " << reclaimed <<
        " objects reclaimed, " << (total - reclaimed) << " in use";
  }
}

void GC::setDebugLevel(int level) {
  debugLevel = level;
}

// -------------------------------------------------------------------
// GCRootBase

GCRootBase::GCRootBase() {
  next_ = GC::roots_;
  GC::roots_ = this;
}

// -------------------------------------------------------------------
// GCWeakPtr

void GCWeakPtrBase::trace() const {
  GC::weakPtrs_.push_back(const_cast<GCWeakPtrBase *>(this));
}

}

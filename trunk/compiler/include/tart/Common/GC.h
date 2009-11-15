/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_GC_H
#define TART_COMMON_GC_H

#include <stddef.h>

//#include "collector/Allocator.h"

namespace tart {

/// -------------------------------------------------------------------
/// Base class of garbage-collectable objects
class GC {
public:
  void * operator new(size_t size);
  void operator delete(void * mem);

  /** Construct a new GC object. */
  GC() : marked_(false) {}

  /** Mark an object as in-use. */
  void mark() const {
    if (!marked_) {
      marked_ = true;
      trace();
    }
  }

  virtual ~GC() {}

  /** Trace all references in this object. */
  virtual void trace() const = 0;

  /** Initialize the GC heap. */
  static void init();

  /** Tear down the GC heap. */
  static void uninit();

  /** Delete all unmarked objects. */
  static void sweep();

  /** Set the verbosity level. */
  static void setDebugLevel(int level) { debugLevel = level; }

  /** A version of mark which handles null pointers. */
  template <class T>
  static void safeMark(T const * const ptr) {
    if (ptr != NULL) {
      ptr->mark();
    }
  }

  template <class T>
  static void markList(T * const * first, T * const * last) {
    while (first < last) {
      (*first)->mark();
      first++;
    }
  }

  template <class T>
  static void safeMarkList(T * const * first, T * const * last) {
    while (first < last) {
      if (*first) {
        (*first)->mark();
      }

      first++;
    }
  }

private:
  mutable bool marked_;
  GC * next_;

  static size_t reclaimed;
  static size_t total;
  static int debugLevel;
  static GC * allocList_;

  static bool sweepCallback(void * alloc, void * ctx);
};

}

#endif

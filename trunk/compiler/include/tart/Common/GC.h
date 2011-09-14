/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_GC_H
#define TART_COMMON_GC_H

#ifndef LLVM_ADT_SMALLVECTOR_H
#include "llvm/ADT/SmallVector.h"
#endif

#ifndef LLVM_ADT_ARRAYREF_H
#include "llvm/ADT/ArrayRef.h"
#endif

#include <stddef.h>

namespace tart {

class GCRootBase;
class GCWeakPtrBase;

/// -------------------------------------------------------------------
/// Base class of garbage-collectable objects
class GC {
public:
  void * operator new(size_t size);
  void operator delete(void * mem);

  class Callback {
  public:
    virtual void call() = 0;
    virtual ~Callback() {}
  };

  /** Construct a new GC object. */
  GC() : cycle_(0) {}

  /** Mark an object as in-use. */
  void mark() const {
    if (cycle_ != cycleIndex_) {
      cycle_ = cycleIndex_;
      trace();
    }
  }

  /** Mark an object as in-use and trace it later. */
  void markDeferred() {
    if (cycle_ != cycleIndex_) {
      cycle_ = cycleIndex_;
      toTrace_.push_back(this);
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
  static void setDebugLevel(int level);

  /** Register a callback to be called when we shut down the collector. */
  static void registerUninitCallback(Callback * cb);

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
  static void markArray(llvm::ArrayRef<T> array) {
    for (typename llvm::ArrayRef<T>::const_iterator it = array.begin(); it != array.end(); ++it) {
      (*it)->mark();
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
  friend class GCRootBase;
  friend class GCWeakPtrBase;

  typedef llvm::SmallVector<GC::Callback *, 8> CallbackList;
  typedef llvm::SmallVector<GCWeakPtrBase *, 128> WeakPtrList;
  typedef llvm::SmallVector<const GC *, 128> ObjectList;

  mutable unsigned char cycle_;
  GC * next_;

  static unsigned char cycleIndex_;
  static GC * allocList_;
  static GCRootBase * roots_;
  static CallbackList uninitCallbacks_;
  static WeakPtrList weakPtrs_;
  static ObjectList toTrace_;
};

/// -------------------------------------------------------------------
/// Base class for garbage collection roots.
class GCRootBase {
public:
  GCRootBase();
  virtual ~GCRootBase() {}

  /** Trace this root. */
  virtual void trace() const = 0;

private:
  friend class GC;
  GCRootBase * next_;
};

/// -------------------------------------------------------------------
/// Base class for garbage collection roots.
class GCPointerRoot : public GCRootBase {
public:
  GCPointerRoot(GC * ptr) : ptr_(ptr) {}
  virtual void trace() const { GC::safeMark(ptr_); }
private:
  GC * ptr_;
};

/// -------------------------------------------------------------------
/// Base class for weak pointers.
class GCWeakPtrBase {
public:
  GCWeakPtrBase() : ptr_(NULL) {}
  GCWeakPtrBase(GC * ptr) : ptr_(ptr) {}
  virtual ~GCWeakPtrBase() {}

  virtual void finalize() const = 0;
  void trace() const;

protected:
  friend class GC;
  GC * ptr_;
};

/// -------------------------------------------------------------------
/// Weak pointer class.
template <class T>
class GCWeakPtr : public GCWeakPtrBase {
public:
  GCWeakPtr() {}
  GCWeakPtr(T * ptr) : GCWeakPtrBase(ptr) {}

  T * get() const { return static_cast<T *>(ptr_); }
  void set(T * ptr) { ptr_ = ptr; }
  T * operator->() const { return get(); }
  operator T *() const { return get(); }

  GCWeakPtr & operator=(T * ptr) { ptr_ = ptr; return *this; }
  GCWeakPtr & operator=(const GCWeakPtr & wp) { ptr_ = wp.ptr_; return *this; }
};

}

#endif

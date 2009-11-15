/* ================================================================== *
 * General GC definitions and public interface.
 * ================================================================== */
 
#ifndef COLLECTOR_CONFIG_H
#define COLLECTOR_CONFIG_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#include "config.h"

#if HAVE_LIBKERN_OSATOMIC_H
#include <libkern/OSAtomic.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif  

/* ==================================================================
   Collector algorithm selection.
   ================================================================== */
   
#define COLLECTOR_SG0 1
#define COLLECTOR_SG1 0

/* ==================================================================
   Configuration options.
   ================================================================== */

/** Set to one to enable debugging features in the collector, primarily
    the abilty to associate a string label with each allocation. */
#ifndef GC_DEBUG
  #define GC_DEBUG 0
#endif

#ifndef GC_ENABLE_TRACE
  #define GC_ENABLE_TRACE GC_DEBUG
#endif

#ifndef GC_ENABLE_ERR
  #define GC_ENABLE_ERR 1
#endif

/** Tracing macro */
#ifndef GC_TRACE
  #if GC_ENABLE_TRACE
    #define GC_TRACE(x) gctracef x
  #else
    #define GC_TRACE(x) if (false)
  #endif
#endif

/** Error message macro */
#ifndef GC_ERR
  #if GC_ENABLE_ERR
    #define GC_ERR(x) gcerrorf x
  #else
    #define GC_ERR(x) if (false)
  #endif
#endif

/** The function to allocate raw blocks of memory. */
#ifndef GC_RAW_ALLOC
  #define GC_RAW_ALLOC(size) valloc(size)
#endif

/** The function to free raw blocks of memory. */
#ifndef GC_RAW_FREE
  #define GC_RAW_FREE(ptr) free(ptr)
#endif

/** Number of guard bytes to put around each global allocation. */
#ifndef GC_GUARD_BYTES
  #define GC_GUARD_BYTES 8
#endif

#if HAVE_LIBKERN_OSATOMIC_H
  #if SIZEOF_CHAR_P == 8
  static inline bool gc_compare_and_swap_ptr(
      void * oldValue, void * newValue, void ** theValue) {
    return OSAtomicCompareAndSwap64(
        (int64_t)oldValue, (int64_t)newValue, (int64_t *)theValue);
  }
  #else
  static inline bool gc_compare_and_swap_ptr(
      void * oldValue, void * newValue, void ** theValue) {
    return OSAtomicCompareAndSwap32(
        (int32_t)oldValue, (int32_t)newValue, (int32_t *)theValue);
  }
  #endif
#elif HAVE_GCC_ATOMICS
  #if SIZEOF_CHAR_P == 8
  static inline bool gc_compare_and_swap_ptr(
      void * oldValue, void * newValue, void ** theValue) {
    return __sync_bool_compare_and_swap(theValue, oldValue, newValue);
  }
  #else
  static inline bool gc_compare_and_swap_ptr(
      void * oldValue, void * newValue, void ** theValue) {
    return __sync_bool_compare_and_swap(theValue, oldValue, newValue);
  }
  #endif
#else
  #error "gc_compare_and_swap_ptr not defined!"
#endif

/* ==================================================================
   Basic types.
   ================================================================== */

// We may need to control the size of a size_t exactly.
typedef size_t gcsize_t;
typedef ssize_t gcoffset_t;

// It's convenient to have a pointer type with which we can calculate
// memory addresses.
typedef uint8_t * gcptr_t;

/* ==================================================================
   Public API.
   ================================================================== */


/** Trace function */
#if GC_DEBUG
  void gctracef(char * format, ...);
#endif

#ifdef __cplusplus
}
#endif  

#endif

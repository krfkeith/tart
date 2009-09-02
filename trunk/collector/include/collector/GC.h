/* ================================================================== *
 * Collector API.
 * ================================================================== */
 
#ifndef COLLECTOR_GC_H
#define COLLECTOR_GC_H

#ifndef COLLECTOR_CONFIG_H
#include "collector/GCConfig.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/** Defines a set of callback operations for tracing an object.
    This structure is maintained by the collector and passed to
    an object's tracing function.
    
    From the collector's standpoint, there are three kinds of pointers:
    Strong pointers, weak pointers, and internal pointers. The short
    definitions of these are as follows:
    
    Strong pointers are strong references to type-tagged objects.
    Weak pointers are weak references to type-tagged objects.
    Internal pointers are strong references to untagged objects.

    "Tagged" objects are objects containing a type tag or some other
    means to identify the type of an object by inspecting it. The
    collector doesn't know or care about how this type information is
    represented, as long as the application-supplied tracer callback
    can get to it.

    Strong and Weak pointers can only point to tagged objects.

    The explanation of Internal pointers is as follows:

    In some cases a single "object" will consist of multiple allocations.
    An example is an Array, which has a fixed-length "head" part, and
    a private, variable-length "body" part. The head part contains the
    type tag, the length field, and a pointer to the body part.

    This private body part is not directly visible to the outside world -
    all access must go through the methods of the head part. Because of
    this, and because of the need to simplify the address calculations for
    accessing array elements, it is permissible for the private body part
    to not have a type tag. This allows the address of the first array
    element to start at the base address of the allocation.

    This pointer to the private buffer is called an 'Internal' pointer.

    Because the private part has no type tag, it cannot directly
    be traced by the collector (since tracing requires the type of an
    object to be discoverable by looking at it.) Instead the tracer
    function for the head part is responsible for tracing both itself
    and its private buffer.

    So the tracer function of an object containing an internal pointer
    must do two things: It must declare the internal pointer to the
    collector (so that the collector doesn't prematurely collect the
    private buffer), and it must declare any pointers contained within
    the private buffer, as well as any pointers within itself.
*/
struct GCTraceContext {
  /** Add a strong pointer field to the list of objects to be traced. */
  void (*declare_strong)(void **);

  /** Add a weak pointer field to the list of objects to be traced. */
  void (*declare_weak)(void **);

  /** Tell the collector this is an internal pointer, and should be
      marked as in use. */
  void (*declare_internal)(void **);
};

/** Declare the type of callback for tracing objects with type tags.

    The application must supply a callback for tracing tagged objects.
    When called, it should enumerate all pointers, whether strong,
    weak, or internal, and identify them to the collector.
*/
typedef void gc_trace_callback_t(void * object, struct GCTraceContext * ctx);
typedef void gc_root_callback_t(void ** pptr, void * ctx);

/** Initialize the collector data structures, and start the collector
    thread (which will initially be in a paused state.
*/
void gc_init(gc_trace_callback_t * trace,
    void (*getroots)(gc_root_callback_t *, void * ctx));

/** Stop the collector thread and shut down the collector. */
void gc_shutdown();

/** Called at the start of each thread. */
void gc_thread_init();

/** Called before a thread finishes. */
void gc_thread_shutdown();

/** A synchronization point. */
void gc_thread_sync();

/** Call this before any blocking operation. This lets the collector know
    that it can run while we are waiting.
*/
void gc_begin_wait();

/** Call this after the blocking operation finishes. */
void gc_end_wait();

/** Allocate a block of memory from the global heap. */
void * gc_alloc(gcsize_t size, void * meta);

/** Allocate a block of memory from the global heap, with alignment and
    offset.
*/
void * gc_alloc_aligned(gcsize_t size, gcsize_t alignment,
    gcoffset_t offset, void * meta);

/** Politely request a collection. If a collection is already in progress
    this does nothing.
*/
void gc_collect();

#ifdef __cplusplus
}
#endif

#endif

/* ================================================================== *
 * Work queues - lists of pointers to dirty/unmarked objects
 * ================================================================== */
 
#ifndef COLLECTOR_WORK_QUEUE_H
#define COLLECTOR_WORK_QUEUE_H

#ifndef COLLECTOR_CONFIG_H
#include "collector/GCConfig.h"
#endif

#include <pthread.h>

#ifdef __cplusplus
extern "C" {
#endif  

/* ==================================================================
   Various constants.
   ================================================================== */

#define GC_PTR_QUEUE_BLOCK_SIZE 4096        /* 4K pointers */

/* ==================================================================
   Work Queue Block.
   ================================================================== */

typedef struct _gcqueueblock_t {
  struct _gcqueueblock_t * volatile prev; /** Previous block in queue. */
  struct _gcqueueblock_t * next; /** Next block in queue */
  void     ** free_ptr;       /** Pointer to next free slot */
  void     * slots[GC_PTR_QUEUE_BLOCK_SIZE];
} gcqueueblock_t;

/* ==================================================================
   Work Queue.
   ================================================================== */
   
/** A work queue is a data structure that stores pointers to objects
    that need to be processed later. The typical use case is for recording
    dirty objects that need to be re-scanned by a garbage collector.
    
    The queue allows for fast, mostly-lock-free insertion of pointers
    by multiple threads. "Mostly lock free" in this case means that a lock
    will be taken on average once every 4096 insertions.
    
    The data structure does not support simultaneous reading and writing.
    Instead, queues should be double-buffered - that is you have two
    queues, one for reading and another for writing, and when the read
    queue is empty do an atomic swap on the queues.
 */

typedef struct _gcwork_queue_t {
  pthread_mutex_t  lock;   /** Lock used when adding a new block. */
  gcqueueblock_t * first;   /** Least recent block in the list. */
  gcqueueblock_t * volatile last;   /** Most recent block in the list. */
} gcwork_queue_t;

/* ==================================================================
   Work Queue API.
   ================================================================== */

/** Initialized a work queue. Does no locking. */
void gc_queue_init(gcwork_queue_t * queue);
    
/** Uninitialized a work queue. This frees any blocks that are in the
    block list. (Normally this won't happen, since it is expected that
    the consumer of the queue data will simply reset the queue back
    to empty at the end of each collection cycle.) */
void gc_queue_uninit(gcwork_queue_t * queue);
    
/** Append a pointer to a queue in a thread-safe manner. Only takes a
    lock when a new block needs to be allocated, otherwise it uses
    lock-free techniques for filling the most recent block. */
void gc_queue_append(gcwork_queue_t * queue, void * ptr);
    
/** Remove the oldest block from the queue.
    This function does no locking - it is assumed that whatever process
    normally inserts new data into the queue is suspended while the queue
    is being processed.
*/
gcqueueblock_t * gc_queue_rmfirst(gcwork_queue_t * queue);
    
#ifdef __cplusplus
}
#endif  

#endif

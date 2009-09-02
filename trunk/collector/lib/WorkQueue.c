/* ================================================================== *
 * Implementation of Work Queues
 * ================================================================== */
 
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include "collector/Allocator.h"
#include "collector/WorkQueue.h"

inline gcqueueblock_t * gc_alloc_block(gcwork_queue_t * queue) {
  gcqueueblock_t * blk =
    (gcqueueblock_t *)gc_heap_alloc(sizeof(gcqueueblock_t), NULL);
  blk->prev = NULL;
  blk->next = NULL;
  blk->free_ptr = blk->slots;
  return blk;
}

void gc_queue_init(gcwork_queue_t * queue) {
  /* Create the lock for this queue */
  CHECK_RESULT(pthread_mutex_init(&queue->lock, NULL))
  queue->first = NULL;
  queue->last = NULL;
}

/** Uninitialized a work queue. */
void gc_queue_uninit(gcwork_queue_t * queue) {
  CHECK_RESULT(pthread_mutex_lock(&queue->lock))
  while (queue->last) {
    gcqueueblock_t * blk = queue->last;
    queue->last = blk->prev;
    gc_heap_free(blk);
  }
  queue->first = NULL;
  CHECK_RESULT(pthread_mutex_unlock(&queue->lock))
  CHECK_RESULT(pthread_mutex_destroy(&queue->lock))
}

void gc_queue_append(gcwork_queue_t * queue, void * ptr) {
  /* Get the pointer to the last block. Note that this pointer may be
     stale if another thread has recently appended a block; That's ok,
     as it will be detected later on in this function. (Because it will
     mean that the pointer is pointing to a full block.)
  */
  gcqueueblock_t * blk = queue->last;

  /* If the queue is empty. */
  if (blk == NULL) {
    /* Lock the list. */
    CHECK_RESULT(pthread_mutex_lock(&queue->lock))

    /* Re-check to insure that we need a new block. Even though other
       threads may change the value of blk->prev_ptr, they cannot
       change the fact that we need a new block. That's because
       (a) queue->last can only change during a lock, which means
       that no new blocks can be added while we are holding this lock,
       and (b) blk->prev_ptr is only incremented, never decremented,
       meaning that once it gets to the end its stuck there. Thus.
       once blk->free_ptr >= end is true its always true until a
       new empty block is inserted - which, again, can only happen
       under lock.
    */
    blk = queue->last;
    if (blk == NULL) {
      blk = gc_alloc_block(queue);
      queue->last = queue->first = blk;
    }
    CHECK_RESULT(pthread_mutex_unlock(&queue->lock))
  }

  /* Mostly lock-free insertion into a linear buffer. */
  for (;;) {

    /* Calculate the address of where we are going to store the
       pointer. */
    void ** next = blk->free_ptr;

    /* If the block is full, then we need to do a new block. Note that
       if another thread came along and inserted a new last block (so
       that 'blk' isn't pointing at the last any more) then we need
       refresh the value of 'blk'.
    */
    if (next >= &blk->slots[GC_PTR_QUEUE_BLOCK_SIZE]) {
      CHECK_RESULT(pthread_mutex_lock(&queue->lock))
      blk = queue->last;
      /* See above for description. */
      if (blk->free_ptr >= &blk->slots[GC_PTR_QUEUE_BLOCK_SIZE]) {
        blk = gc_alloc_block(queue);
        queue->last->next = blk;
        blk->prev = queue->last;
        queue->last = blk;
      }
      CHECK_RESULT(pthread_mutex_unlock(&queue->lock))
    } else if (gc_compare_and_swap_ptr(next, next + 1, (void **)&blk->free_ptr)) {
      // Store the pointer.
      *next = ptr;
      break;
    }
  }
}

gcqueueblock_t * gc_queue_rmfirst(gcwork_queue_t * queue) {
  gcqueueblock_t * result = queue->first;
  if (result) {
    queue->first = result->next;
    if (queue->last == result)
      queue->last = NULL;
  }
  return result;
}

#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include "collector/Allocator.h"
#include "collector/WorkQueue.h"
#include "UnitTest.h"

/* Single-threaded insertion case. */
void TestSimpleInsertion() {
  gcwork_queue_t q;

  gc_queue_init(&q);
  for (int i = 0; i < 10000; i++) {
    gc_queue_append(&q, (void *)i);
  }

  int count = 0;
  gcqueueblock_t * blk;
  while ((blk = gc_queue_rmfirst(&q)) != NULL) {
    count += (void **)blk->free_ptr - blk->slots;
    gc_heap_free(blk);
  }

  TEST_ASSERT_EQUAL(10000, count);

  gc_queue_uninit(&q);
  TEST_ASSERT(gc_heap_validate(GC_VALIDATE_IS_EMPTY));
}

namespace {
const int NUM_THREADS = 10;

struct thread_data {
  gcwork_queue_t * queue;
  int base_index;
};

void * insert_into_queue(void *arg) {
  thread_data * td = (thread_data *)arg;

  fprintf(stderr, " ** Thread started\n");

  for (int i = 0; i < 10000; i++) {
    gc_queue_append(td->queue, (void *)td->base_index);
  }

  fprintf(stderr, " ** Thread finished\n");
  return NULL;
}
};

void TestThreadedInsertion() {
  gcwork_queue_t q;
  pthread_t threads[NUM_THREADS];
  thread_data td[NUM_THREADS];

  // Initialize the queue
  gc_queue_init(&q);

  // Create the threads to add items to the queue
  // Create the threads to add items to the queue
  for (int i = 0; i < NUM_THREADS; i++) {
    td[i].queue = &q;
    td[i].base_index = i + 1;
  }

  for (int i = 0; i < NUM_THREADS; i++) {
    TEST_ASSERT_EQUAL(0,
        pthread_create(&threads[i], NULL, &insert_into_queue, &td[i]));
  }

  // Now wait for the threads to finish
  for (int i = 0; i < NUM_THREADS; i++) {
    TEST_ASSERT_EQUAL(pthread_join(threads[i], NULL), 0);
  }

  fprintf(stderr, "All threads finished\n");
  TEST_ASSERT(gc_heap_validate(0));

  int count = 0;
  gcqueueblock_t * blk;
  while ((blk = gc_queue_rmfirst(&q)) != NULL) {
    TEST_ASSERT(blk->free_ptr >= blk->slots);
    TEST_ASSERT(blk->free_ptr <= &blk->slots[GC_PTR_QUEUE_BLOCK_SIZE]);
    for (void ** ptr = blk->slots; ptr < blk->free_ptr; ptr++) {
      //TEST_ASSERT((int)*ptr >= 1 && (int)*ptr < NUM_THREADS + 1);
    }
    count += (void **)blk->free_ptr - blk->slots;
    gc_heap_free(blk);
  }

  TEST_ASSERT_EQUAL(10000 * NUM_THREADS, count);

  gc_queue_uninit(&q);
  TEST_ASSERT(gc_heap_validate(GC_VALIDATE_IS_EMPTY));
}

void TestWorkQueue() {
  fprintf(stderr, "* Work Queue Tests\n");

  gc_heap_init();
  TestSimpleInsertion();
  TestThreadedInsertion();
  //TestBasic();

  // TODO: Need to verify that the heap is empty

  gc_heap_uninit();
}

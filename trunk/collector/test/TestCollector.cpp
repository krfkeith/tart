#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include "collector/GC.h"
#include "collector/Allocator.h"
#include "UnitTest.h"

static void trace_func(void * object, struct GCTraceContext * ctx) {
}

void * gRoot = NULL;

static void trace_roots(gc_root_callback_t *cb, void * ctx) {
  (*cb)(&gRoot, ctx);
}

static void TestNoRoots() {
  // Set up the collector
  gc_init(&trace_func, &trace_roots);
  gc_thread_init();

  TEST_ASSERT_EQUAL(0u, gc_heap_total_size() - gc_heap_total_free());
  void * ptr = gc_alloc(21, NULL);
  TEST_ASSERT(gc_heap_total_size() - gc_heap_total_free() > 0u);
  TEST_ASSERT(gc_heap_validate(0));
  gc_collect();
  TEST_ASSERT(gc_heap_validate(0));
  TEST_ASSERT_EQUAL(0u, gc_heap_total_size() - gc_heap_total_free());

  // Tear down the collector
  gc_thread_shutdown();
  gc_shutdown();
}

static void TestSingularRoot() {
  // Set up the collector
  gc_init(&trace_func, &trace_roots);
  gc_thread_init();

  TEST_ASSERT_EQUAL(0u, gc_heap_total_size() - gc_heap_total_free());
  void * ptr = gc_alloc(21, NULL);
  TEST_ASSERT(gc_heap_total_size() - gc_heap_total_free() > 0u);
  gRoot = ptr;
  TEST_ASSERT(gc_heap_validate(0));
  gc_collect();
  TEST_ASSERT(gc_heap_validate(0));
  TEST_ASSERT(gc_heap_total_size() - gc_heap_total_free() > 0u);

  // Tear down the collector
  gc_thread_shutdown();
  gc_shutdown();
}

void TestCollector() {
  fprintf(stderr, "* Collector Tests\n");

  gc_heap_init();
  TestNoRoots();
  TestSingularRoot();
  gc_heap_uninit();
}

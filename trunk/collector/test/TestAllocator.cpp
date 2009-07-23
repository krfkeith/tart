#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include "collector/Allocator.h"
#include "UnitTest.h"

void TestBasic() {
  void * ptr = gc_heap_alloc(21, NULL);
  TEST_ASSERT(gc_heap_validate(0));
  gc_heap_free(ptr);
  TEST_ASSERT(gc_heap_validate(GC_VALIDATE_IS_EMPTY));

  for (int size = 21; size < 50000; size = size * 3 / 2) {
    void * ptr0 = gc_heap_alloc(size, NULL);
    void * ptr1 = gc_heap_alloc(size, NULL);
    void * ptr2 = gc_heap_alloc(size, NULL);
    //gc_heap_dump();
    TEST_ASSERT(gc_heap_validate(0));
    gc_heap_free(ptr1);
    TEST_ASSERT(gc_heap_validate(0));
    gc_heap_free(ptr0);
    TEST_ASSERT(gc_heap_validate(0));
    gc_heap_free(ptr2);
    TEST_ASSERT(gc_heap_validate(GC_VALIDATE_IS_EMPTY));
  }

}

void TestThreaded() {
}

void TestLarge() {
  void * ptr = gc_heap_alloc(21, NULL);
  TEST_ASSERT(gc_heap_validate(0));
  gc_heap_free(ptr);
  TEST_ASSERT(gc_heap_validate(GC_VALIDATE_IS_EMPTY));

  for (int i = 0; i < 2; ++i) {
    int size = 500000;
    void * ptr0 = gc_heap_alloc(size, NULL);
    void * ptr1 = gc_heap_alloc(size, NULL);
    void * ptr2 = gc_heap_alloc(size, NULL);
    //gc_heap_dump();
    TEST_ASSERT(gc_heap_validate(0));
    gc_heap_free(ptr1);
    TEST_ASSERT(gc_heap_validate(0));
    gc_heap_free(ptr0);
    TEST_ASSERT(gc_heap_validate(0));
    gc_heap_free(ptr2);
    TEST_ASSERT(gc_heap_validate(GC_VALIDATE_IS_EMPTY));
  }
}

void TestAllocator() {
  gcsize_t heap_size;
  gcsize_t free_size;

  fprintf(stderr, "* Allocator Tests\n");

  gc_heap_init();
  TestBasic();
  TestLarge();

  heap_size = gc_heap_total_size();
  free_size = gc_heap_total_free();
  //TEST_ASSERT(gc_heap_size(&heap_size, &free_size));
  fprintf(stderr, "Total heap size = %ld, available = %ld\n",
      heap_size, free_size);

  gc_heap_uninit();
}

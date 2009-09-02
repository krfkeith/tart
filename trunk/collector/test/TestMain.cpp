#include "UnitTest.h"

TestRunner TestRunner::runner;

void TestAllocator();
void TestWorkQueue();
void TestCollector();

int main() {
  fprintf(stderr, "\n");
  fprintf(stderr, "Running Unit Tests\n");
  fprintf(stderr, "------------------\n");
  TestAllocator();
  TestWorkQueue();
  TestCollector();
  fprintf(stderr, "------------------\n");
  int32_t failCount = TestRunner::GetFailureCount();
  if (failCount) {
    fprintf(stderr, "Number of test failures: %d.\n\n", failCount);
  } else {
    fprintf(stderr, "All tests succeeded.\n\n");
  }
  return failCount;
}

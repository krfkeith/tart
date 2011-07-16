/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/GC.h"
#include "tart/Objects/Builtins.h"

#include <gmock/gmock.h>

int main(int argc, char **argv) {
  using namespace tart;

  // Initialize the garbage collector.
  GC::init();
  Builtins::init();

  // Initialize and run tests.
  testing::InitGoogleMock(&argc, argv);
  int result = RUN_ALL_TESTS();

  // Clean up memory
  GC::sweep();
  GC::uninit();
  return result;
}

/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Common/GC.h"
#include <gtest/gtest.h>

int main(int argc, char **argv) {
  using namespace tart;
  
  // Initialize the garbage collector.
  GC::init();

  // Initialize and run tests.
  testing::InitGoogleTest(&argc, argv);
  int result = RUN_ALL_TESTS();

  // Clean up memory
  GC::sweep();
  GC::uninit();
  return result;
}

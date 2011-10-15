/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/GC.h"
#include "tart/Common/PackageMgr.h"
#include "tart/Objects/Builtins.h"
#include "libpaths.h"

#include <gmock/gmock.h>

int main(int argc, char **argv) {
  using namespace tart;

  // Initialize the garbage collector.
  GC::init();
  Builtins::init();

  // Add the standard library to the import path
  PackageMgr::get().addImportPath(STDLIB_BUILD_PATH);

  // Initialize and run tests.
  testing::InitGoogleMock(&argc, argv);
  int result = RUN_ALL_TESTS();

  // Clean up memory
  GC::sweep();
  GC::uninit();
  return result;
}

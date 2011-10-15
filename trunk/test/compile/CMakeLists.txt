# CMake build file for tart/test/compile

file(GLOB COMPILETEST_SRC *.cpp *.h)

# Extra flags for GCC (C++ only)
if (CMAKE_COMPILER_IS_GNUCXX)
  add_definitions(
      -Woverloaded-virtual
      -fno-operator-names -ffor-scope
      )
endif (CMAKE_COMPILER_IS_GNUCXX)

if (CMAKE_COMPILER_IS_CLANG)
  add_definitions(
      -Woverloaded-virtual
      -fno-rtti
      -DGTEST_HAS_RTTI=0
      )
endif (CMAKE_COMPILER_IS_CLANG)

# Directory paths for standard library so that the unit tests can locate it.
file(TO_NATIVE_PATH ${PROJECT_BINARY_DIR}/lib/std/libstd.bc STDLIB_BUILD_PATH)

# Generate the libpaths.h file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/libpaths.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/libpaths.h)

execute_process(
  COMMAND ${LLVM_CONFIG} --libs bitwriter bitreader asmparser ${LLVM_TARGETS}
  OUTPUT_VARIABLE LLVM_TESTRUNNER_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.6.0/include)
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.6.0/gtest/include)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Unit test executable. We specify the sources explicitly because we want
# the tests to run in that order.
add_executable(compiletest ${COMPILETEST_SRC})
target_link_libraries(compiletest
    gtest gmock compiler
    ${LLVM_TESTRUNNER_LIBS}
    )
if (LIB_DL)
  target_link_libraries(compiletest dl)
endif (LIB_DL)
set_target_properties(compiletest PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

add_custom_target(compiletest.run DEPENDS compiletest COMMAND compiletest)
add_dependencies(check compiletest.run)

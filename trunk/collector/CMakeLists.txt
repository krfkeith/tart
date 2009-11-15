# CMake build file for Tart/collector

#if (CMAKE_COMPILER_IS_GNUCC)
#  add_definitions(
#      -Wall
#      -Wcast-align
#      -Wpointer-arith)
#endif(CMAKE_COMPILER_IS_GNUCC)

add_library(collector lib/Allocator.c)

#include_directories(${TART_SOURCE_DIR}/third-party/gtest-1.3.0/include)
#include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.1.0/include)

# Unit tests for collector
add_executable(collectortest
  test/TestMain.cpp
  test/TestAllocator.cpp
#  test/TestCollector.cpp
  )
target_link_libraries(collectortest
    collector
    )
#    gtest gmock

add_custom_target(collectortest.run DEPENDS collectortest COMMAND collectortest)
#add_dependencies(check unittest.run)

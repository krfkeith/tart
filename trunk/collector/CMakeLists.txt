# CMake build file for Tart/collector

if (CMAKE_COMPILER_IS_GNUCC)
  add_definitions(
      -Wall
      -Wcast-align
      -Wpointer-arith)
endif(CMAKE_COMPILER_IS_GNUCC)

add_library(collector lib/Allocator.c)

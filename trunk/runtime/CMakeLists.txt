# CMake build file for Tart/runtime

if (CMAKE_COMPILER_IS_GNUCC)
  add_definitions(
      -g
      -Wall
      -Wcast-align
      -Wpointer-arith)
endif(CMAKE_COMPILER_IS_GNUCC)

include_directories(include)

file(GLOB sources lib/*.c)
file(GLOB headers include/*.h)
add_library(runtime STATIC ${sources} ${headers})

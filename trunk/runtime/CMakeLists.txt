# CMake build file for Tart/runtime

if (CMAKE_COMPILER_IS_GNUCC)
  add_definitions(
      -g
      -Wall
      -Wcast-align
      -Wpointer-arith
      -fno-exceptions)
endif(CMAKE_COMPILER_IS_GNUCC)

if (CMAKE_COMPILER_IS_GNUCXX)
  remove_definitions(
      -Wno-deprecated -Wno-unused
      )
endif (CMAKE_COMPILER_IS_GNUCXX)

include_directories(include)

file(GLOB sources lib/*.c lib/*.cpp)
file(GLOB headers include/*.h)
add_library(runtime STATIC ${sources} ${headers})

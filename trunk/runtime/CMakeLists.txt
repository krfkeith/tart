# CMake build file for Tart/runtime

include_directories(include)

file(GLOB sources lib/*.c )
file(GLOB sources_cpp lib/*.cpp)
file(GLOB headers include/*.h)

if (CMAKE_COMPILER_IS_GNUCC)
  add_definitions(
      -g
      -fno-exceptions)
endif(CMAKE_COMPILER_IS_GNUCC)

if (CMAKE_COMPILER_IS_GNUCXX)
  remove_definitions(-Wno-deprecated -Wno-unused)
  set_source_files_properties(lib/GC.cpp PROPERTIES COMPILE_FLAGS "${COMPILE_FLAGS} -fno-rtti")
endif (CMAKE_COMPILER_IS_GNUCXX)

if (CMAKE_COMPILER_IS_CLANG)
  remove_definitions(-Wno-deprecated -Wno-unused)
  set_source_files_properties(${sources_cpp} PROPERTIES COMPILE_FLAGS "-fno-rtti -fno-exceptions")
  remove_definitions(-fno-rtti)
endif (CMAKE_COMPILER_IS_CLANG)

add_library(runtime STATIC ${sources} ${sources_cpp} ${headers})

install(TARGETS runtime ARCHIVE DESTINATION lib/tart/static)

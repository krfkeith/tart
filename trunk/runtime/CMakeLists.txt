# CMake build file for Tart/runtime

if (CMAKE_COMPILER_IS_GNUCC)
  add_definitions(
      -g
      -fno-exceptions)
endif(CMAKE_COMPILER_IS_GNUCC)

if (CMAKE_COMPILER_IS_GNUCXX)
  remove_definitions(
      -Wno-deprecated -Wno-unused
      )
  set_source_files_properties(lib/GC.cpp PROPERTIES COMPILE_FLAGS "${COMPILE_FLAGS} -fno-rtti")
endif (CMAKE_COMPILER_IS_GNUCXX)

if (CMAKE_COMPILER_IS_CLANG)
  add_definitions(
#      -Woverloaded-virtual
      -fno-rtti
      -fno-exceptions
      )
endif (CMAKE_COMPILER_IS_CLANG)

include_directories(include)

file(GLOB sources lib/*.c lib/*.cpp)
file(GLOB headers include/*.h)
add_library(runtime STATIC ${sources} ${headers})

install(TARGETS runtime ARCHIVE DESTINATION lib/tart/static)

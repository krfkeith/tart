# CMake build file for Tart/linker

# Extra flags for GCC (C++ only)
if (CMAKE_COMPILER_IS_GNUCXX)
  add_definitions(
      -Woverloaded-virtual
      -fno-operator-names -ffor-scope
      )
   set(LINK_FLAGS -pg)
endif (CMAKE_COMPILER_IS_GNUCXX)

file(GLOB GC_SOURCES lib/GC/*.cpp)
file(GLOB GC_HEADERS include/GC/*.h)

source_group(GC REGULAR_EXPRESSION lib/GC/*)

add_library(gcstrategy STATIC ${GC_SOURCES} ${GC_HEADERS})

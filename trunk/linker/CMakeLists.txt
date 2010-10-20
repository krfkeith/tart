# CMake build file for Tart/linker

# Extra flags for GCC (C++ only)
if (CMAKE_COMPILER_IS_GNUCXX)
  add_definitions(
      -Woverloaded-virtual
      -fno-operator-names -ffor-scope
      )
   if ($PROFILE_TARTLN)
     add_definitions(-pg)
     set(CMAKE_EXE_LINKER_FLAGS -pg)
   endif ($PROFILE_TARTLN)
endif (CMAKE_COMPILER_IS_GNUCXX)

file(GLOB COMMON_SOURCES lib/Common/*.cpp)
file(GLOB COMMON_HEADERS include/Common/*.h)

file(GLOB GC_SOURCES lib/GC/*.cpp)
file(GLOB GC_HEADERS include/GC/*.h)

file(GLOB REFLECT_SOURCES lib/Reflect/*.cpp)
file(GLOB REFLECT_HEADERS include/Reflect/*.h)

source_group(GC REGULAR_EXPRESSION lib/GC/*)

add_library(linker_common STATIC ${COMMON_SOURCES} ${COMMON_HEADERS})
add_library(gcstrategy STATIC ${GC_SOURCES} ${GC_HEADERS})
add_library(linker_reflect STATIC ${REFLECT_SOURCES} ${COMMON_HEADERS} ${REFLECT_HEADERS})

add_library(tartln_plugin SHARED lib/plugin.cpp)
target_link_libraries(tartln_plugin
    linker_reflect
    linker_common
    gcstrategy)

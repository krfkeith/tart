# CMake build file for lib/gc1

include(AddTartLibrary)

#set(TART_OPTIONS
#    -debug-errors
#)

set(SRC_ROOT ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(TARTC_OPTIONS -debug-errors)
set(TARTLN_OPTIONS -disable-fp-elim -internalize -O0)

# Module search path
set(TART_MODULE_PATH
    -i ${TART_SOURCE_DIR}/lib/std
    -i ${SRC_ROOT})

# Input files
file(GLOB LIB_GC1_SRC RELATIVE ${SRC_ROOT} ${SRC_ROOT}/tart/gc1/*.tart)

add_tart_library(gc1 "${SRC_ROOT}" "${LIB_GC1_SRC}")
add_custom_target(libgc1 ALL DEPENDS libgc1.bc libgc1.deps)
add_dependencies(libgc1 libstd)

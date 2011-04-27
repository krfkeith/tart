# CMake build file for lib/testing

include(AddTartLibrary)

set(SRC_ROOT ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(TARTC_OPTIONS -g -debug-errors)
set(TARTLN_OPTIONS -disable-fp-elim -internalize -O0)

# Module search path
set(TART_MODULE_PATH
    -i ${TART_SOURCE_DIR}/lib/std
    -i ${SRC_ROOT})

# Input files
file(GLOB LIB_TESTING_SRC RELATIVE ${SRC_ROOT} ${SRC_ROOT}/tart/testing/*.tart)

add_tart_library(testing "${SRC_ROOT}" "${LIB_TESTING_SRC}")
add_custom_target(libtesting ALL DEPENDS libtesting.bc libtesting.deps)
add_dependencies(libtesting libstd)

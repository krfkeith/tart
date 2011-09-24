# CMake build file for lib/opts

include(AddTartLibrary)

set(SRC_ROOT ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(TARTC_OPTIONS -g -debug-errors)
set(TARTLN_OPTIONS -disable-fp-elim -internalize -O0)

# Module search path
set(TART_MODULE_PATH
    -i ${TART_SOURCE_DIR}/lib/std
    -i ${TART_SOURCE_DIR}/lib/opts
    -i ${SRC_ROOT})

# Input files
file(GLOB LIB_OPTS_SRC RELATIVE ${SRC_ROOT} ${SRC_ROOT}/tart/opts/*.tart)

add_tart_library(opts "${SRC_ROOT}" "${LIB_OPTS_SRC}")
add_custom_target(libopts ALL DEPENDS libopts.bc libopts.deps)
add_dependencies(libopts libstd)

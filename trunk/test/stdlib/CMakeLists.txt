# CMake build file for tart/test/stdlib

include(AddTartTest)

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TEST_SRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tart)
source_group(LibStdTests FILES ${TEST_SRC})

# Module search path
set(MODPATH
  -i ${TART_SOURCE_DIR}/lib/std
  -i ${TART_SOURCE_DIR}/lib/testing)

# Input libraries
set(BC_LIBS
  "${PROJECT_BINARY_DIR}/lib/std/libstd.bc"
  "${PROJECT_BINARY_DIR}/lib/testing/libtesting.bc"
  "${PROJECT_BINARY_DIR}/lib/gc1/libgc1.bc"
  )

# Library dependencies
set(LIB_DEPS libstd libtesting libgc1)

# Test data
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/iotest.txt DESTINATION .)

#set(GCC_OPTIONS -lruntime)
#set(TEST_LIBS runtime)

#if (LIB_DL)
#  set(GCC_OPTIONS ${GCC_OPTIONS} -ldl)
#  set(TEST_LIBS ${TEST_LIBS} dl)
#endif (LIB_DL)

#set(TEST_BC_FILES)

set(PUBLIC_SYMBOLS "main,String_create,TraceAction_traceDescriptors,GC_static_roots_array")

# Flags for 'opt' for unit test debug builds.
set(TEST_OPT_FLAGS_DEBUG
      -disable-inlining
      -load="${REFLECTOR_PLUGIN}"
      -internalize-public-api-list=${PUBLIC_SYMBOLS}
      -globaldce
      -staticroots
      -reflector
  )

# Flags for 'opt' for unit test optimized builds.
set(TEST_OPT_FLAGS_NDEBUG
      -O2
      -strip-debug
      -load="${REFLECTOR_PLUGIN}"
      -internalize-public-api-list=${PUBLIC_SYMBOLS}
#     -mergefunc
#      -std-compile-opts
      -reflector
#      -std-link-opts
      -instcombine
      -simplifycfg
      -adce
      -globaldce
      -globalopt
      -staticroots
#      -globaldce
  )

set(GENERATE_DEBUG_INFO 0)

if (GENERATE_DEBUG_INFO)
  set(TART_OPTIONS ${TART_OPTIONS} -g)
  set(TARTLN_OPTIONS -O2 -disable-inlining)
  set(OPT_FLAGS ${TEST_OPT_FLAGS_DEBUG})
else (GENERATE_DEBUG_INFO)
  set(TARTLN_OPTIONS -internalize -O2)
  set(OPT_FLAGS ${TEST_OPT_FLAGS_NDEBUG})
endif (GENERATE_DEBUG_INFO)

add_tart_test(LibStdTests TEST_SRC)

# CMake build file for tartx/lexgen

include(AddTartLibrary)

set(SRC_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/src) # Source file root
set(TARTC_OPTIONS -g -debug-errors)
#set(TARTC_OPTIONS -debug-errors)
set(TARTLN_OPTIONS -disable-fp-elim -disable-inlining -internalize -O2)

set(RUNTIME_LIBS runtime)
if (LIB_DL)
  set(RUNTIME_LIBS runtime dl)
endif (LIB_DL)

# Module search path
set(TART_MODULE_PATH
    -i ${TART_SOURCE_DIR}/lib/std
    -i ${TART_SOURCE_DIR}/lib/testing
    -i ${TART_SOURCE_DIR}/lib/opts
    -i ${SRC_ROOT})

# Input files
file(GLOB LIB_LEXGEN_COMMON_SRC RELATIVE ${SRC_ROOT} ${SRC_ROOT}/tartx/lexgen/shared/*.tart)
file(GLOB LIB_LEXGEN_GEN_SRC    RELATIVE ${SRC_ROOT} ${SRC_ROOT}/tartx/lexgen/gen/*.tart)
file(GLOB LIB_LEXGEN_APP_SRC    RELATIVE ${SRC_ROOT} ${SRC_ROOT}/tartx/lexgen/app/*.tart)
file(GLOB LIB_LEXGEN_TEST_SRC   RELATIVE ${SRC_ROOT}
    ${SRC_ROOT}/tartx/lexgenTest/*.tart
    ${SRC_ROOT}/tartx/lexgenTest/*/*.tart)

add_tart_library(LexGenShared "${SRC_ROOT}" "${LIB_LEXGEN_COMMON_SRC}")
add_tart_library(LexGenGen    "${SRC_ROOT}" "${LIB_LEXGEN_GEN_SRC}")
add_tart_library(LexGenApp    "${SRC_ROOT}" "${LIB_LEXGEN_APP_SRC}")
add_tart_library(LexGenTest   "${SRC_ROOT}" "${LIB_LEXGEN_TEST_SRC}")

function(add_tart_executable Name)
  set(OBJ_FILE ${Name}${CMAKE_CXX_OUTPUT_EXTENSION})
  set(EXE_FILE ${Name}${CMAKE_EXECUTABLE_SUFFIX})
  set(LIBS ${ARGN})

  # Generate the list of .deps files.
  set(DEPFILES)
  foreach(LIB ${LIBS})
    string(REGEX REPLACE ".bc\$" ".deps" DEPFILE "${LIB}")
    set(DEPFILES ${DEPFILES} "${DEPFILE}")
  endforeach(LIB)

  # Link
  add_custom_command(OUTPUT ${OBJ_FILE}
      COMMAND tartln -filetype=obj -o ${OBJ_FILE} ${TARTLN_OPTIONS} ${LIBS}
      DEPENDS ${LIBS} ${DEPFILES}
      COMMENT "Linking Tart bitcode file ${OBJ_FILE}")

  add_executable(${EXE_FILE} EXCLUDE_FROM_ALL ${OBJ_FILE})
  add_dependencies(${EXE_FILE} libstd libtesting)
  target_link_libraries(${EXE_FILE} ${RUNTIME_LIBS})
endfunction(add_tart_executable)

add_tart_executable(lexgenTest${CMAKE_EXECUTABLE_SUFFIX}
  "${PROJECT_BINARY_DIR}/lib/std/libstd.bc"
  "${PROJECT_BINARY_DIR}/lib/testing/libtesting.bc"
  "${PROJECT_BINARY_DIR}/lib/gc1/libgc1.bc"
  libLexGenShared.bc
  libLexGenGen.bc
  libLexGenTest.bc
)
add_dependencies(lexgenTest libstd libtesting libLexGenShared libLexGenGen libLexGenTest)
add_custom_target(lexgenTest.run COMMAND ./lexgenTest)
add_dependencies(lexgenTest.run lexgenTest)

add_tart_executable(lexgen${CMAKE_EXECUTABLE_SUFFIX}
  "${PROJECT_BINARY_DIR}/lib/std/libstd.bc"
  "${PROJECT_BINARY_DIR}/lib/opts/libopts.bc"
  "${PROJECT_BINARY_DIR}/lib/gc1/libgc1.bc"
  libLexGenApp.bc
  libLexGenShared.bc
  libLexGenGen.bc
)
add_dependencies(lexgen libstd libopts libLexGenShared libLexGenGen libLexGenApp)

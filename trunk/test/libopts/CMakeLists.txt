# CMake build file for tart/test/libopts

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TART_SRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tart)
source_group(LibOptsTests FILES ${TART_SRC})
set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root

# Module search path
set(MODPATH
  -i ${TART_SOURCE_DIR}/lib/std
  -i ${TART_SOURCE_DIR}/lib/opts
  -i ${TART_SOURCE_DIR}/lib/testing)

# Input libraries
set(BC_LIBS
  "${PROJECT_BINARY_DIR}/lib/std/libstd.bc"
  "${PROJECT_BINARY_DIR}/lib/testing/libtesting.bc"
  "${PROJECT_BINARY_DIR}/lib/opts/libopts.bc"
  "${PROJECT_BINARY_DIR}/lib/gc1/libgc1.bc"
  )

set(TART_OPTIONS
  -debug-errors
  -nostdlib
)
#set(TART_OPTIONS -debug-errors -show-generated -show-size)
#set(TARTLN_OPTIONS -O2)
#set(TARTLN_OPTIONS -internalize -O0)

set(GCC_OPTIONS -lruntime)
set(TEST_LIBS runtime)

if (LIB_DL)
  set(GCC_OPTIONS ${GCC_OPTIONS} -ldl)
  set(TEST_LIBS ${TEST_LIBS} dl)
endif (LIB_DL)

set(TEST_BC_FILES)

set(GENERATE_ASM 0)
set(USE_LLVM_BINARIES 1)
set(GENERATE_DEBUG_INFO 0)

include(${CMAKE_CURRENT_BINARY_DIR}/test.deps OPTIONAL)

set(GC_PLUGIN "${PROJECT_BINARY_DIR}/linker/libgc${CMAKE_SHARED_LIBRARY_SUFFIX}")
set(REFLECTOR_PLUGIN "${PROJECT_BINARY_DIR}/linker/libreflector${CMAKE_SHARED_LIBRARY_SUFFIX}")
set(PUBLIC_SYMBOLS "main,String_create,TraceAction_traceDescriptors,GC_static_roots,GC_static_roots_array")

if (GENERATE_DEBUG_INFO)
  set(TART_OPTIONS ${TART_OPTIONS} -g)
  set(TARTLN_OPTIONS -O2 -disable-inlining)
  set(OPT_FLAGS
      -disable-inlining
      -load="${REFLECTOR_PLUGIN}"
      -internalize-public-api-list=${PUBLIC_SYMBOLS}
#      -std-compile-opts
#      -std-link-opts
#      -instcombine
#      -simplifycfg
#      -adce
#      -globaldce
#      -globalopt
      -globaldce
      -staticroots
      -reflector
  )
else (GENERATE_DEBUG_INFO)
  set(TARTLN_OPTIONS -internalize -O2)
  set(OPT_FLAGS
      -O2
      -strip-debug
      -load="${REFLECTOR_PLUGIN}"
      -internalize-public-api-list=${PUBLIC_SYMBOLS}
#     -mergefunc
      -std-compile-opts
      -std-link-opts
      -instcombine
      -simplifycfg
      -adce
      -globaldce
      -globalopt
      -staticroots
      -reflector
      -globaldce
  )
endif (GENERATE_DEBUG_INFO)

# Create and run a test executable for each source file
foreach(SRC_FILE ${TART_SRC})
  # Source file name
  string(REGEX REPLACE ".tart\$" ".bc" BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".out.bc" OUT_BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".opt.bc" OPT_BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" "${CMAKE_CXX_OUTPUT_EXTENSION}" OBJ_FILE "${SRC_FILE}")  
  string(REGEX REPLACE ".tart\$" "${CMAKE_EXECUTABLE_SUFFIX}" EXE_FILE "${SRC_FILE}")

  # Generate the deps variable name
  string(REGEX REPLACE ".tart\$" "" TEST_NAME "${SRC_FILE}")
  string(TOUPPER "${TEST_NAME}" DEPS_NAME)
  string(REGEX REPLACE "[^a-zA-Z0-9]" "_" DEPS_NAME "${DEPS_NAME}")
  
  # Compile tart source
  add_custom_command(OUTPUT ${BC_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} ${MODPATH} ${SRC_FILE}
      MAIN_DEPENDENCY "${SRC_FILE}" 
      DEPENDS "${PROJECT_BINARY_DIR}/lib/opts/libopts.bc" ${${DEPS_NAME}_DEPS}
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Remember the list of output files.
  set(TEST_BC_FILES ${TEST_BC_FILES} "${BC_FILE}")
endforeach(SRC_FILE)

# Link bitcode files
add_custom_command(OUTPUT ${OUT_BC_FILE}
    COMMAND ${LLVM_LD}
        -disable-opt
        -link-as-library
        -o ${OUT_BC_FILE}
        ${TEST_BC_FILES} ${BC_LIBS}
    MAIN_DEPENDENCY "${TEST_BC_FILES}"
    DEPENDS ${BC_LIBS}
    COMMENT "Linking Tart bitcode file ${TEST_BC_FILES}")

# Optimize object files
add_custom_command(OUTPUT ${OPT_BC_FILE}
    COMMAND ${LLVM_OPT} ${OPT_FLAGS} -o ${OPT_BC_FILE} ${OUT_BC_FILE}
    MAIN_DEPENDENCY "${OUT_BC_FILE}" 
    DEPENDS reflector
    COMMENT "Generating optimized bitcode file ${OPT_BC_FILE}")

# Link object files
add_custom_command(OUTPUT ${OBJ_FILE}
    COMMAND ${LLVM_LLC}
        -load="${GC_PLUGIN}"
        -disable-fp-elim
        -filetype=obj
        -o ${OBJ_FILE}
        ${OPT_BC_FILE}
    MAIN_DEPENDENCY "${OPT_BC_FILE}" 
    DEPENDS gc
    COMMENT "Generating object file ${OBJ_FILE}")

add_executable(${EXE_FILE} EXCLUDE_FROM_ALL ${OBJ_FILE})
add_dependencies(${EXE_FILE} libstd libtesting libgc1 libopts)
target_link_libraries(${EXE_FILE} ${TEST_LIBS})

add_custom_target(LibOptsTests.run COMMAND ./${EXE_FILE} DEPENDS ${EXE_FILE})
add_dependencies(LibOptsTests.run libstd libtesting libopts libgc1)
add_dependencies(check LibOptsTests.run)

# Generate dependency info
add_custom_target(libopts.test.deps
    COMMAND gendeps -o test.deps ${TEST_BC_FILES}
    DEPENDS ${TEST_BC_FILES} gendeps
    COMMENT "Generating dependencies for tests")

add_dependencies(check libopts.test.deps)

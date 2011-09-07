# CMake build file for tart/test/simple - very basic tests with minimal dependencies.

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TEST_SRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tart)
source_group(SimpleTests FILES ${TEST_SRC})
set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(MODPATH -i ${TART_SOURCE_DIR}/lib/std) # Module search path

set(BC_LIBS
  "${PROJECT_BINARY_DIR}/lib/std/libstd.bc"
  "${PROJECT_BINARY_DIR}/lib/gc1/libgc1.bc"
  )

set(TART_OPTIONS
  -debug-errors
  -nostdlib
)
#set(TART_OPTIONS -show-generated -show-size)

set(TEST_LIBS runtime)
if (LIB_DL)
  set(TEST_LIBS ${TEST_LIBS} dl)
endif (LIB_DL)

set(GENERATE_DEBUG_INFO 0)

include(${CMAKE_CURRENT_BINARY_DIR}/test.deps OPTIONAL)

set(GC_PLUGIN "${PROJECT_BINARY_DIR}/linker/libgc${CMAKE_SHARED_LIBRARY_SUFFIX}")
set(REFLECTOR_PLUGIN "${PROJECT_BINARY_DIR}/linker/libreflector${CMAKE_SHARED_LIBRARY_SUFFIX}")
set(PUBLIC_SYMBOLS "main,String_create,TraceAction_traceDescriptors,GC_static_roots")

if (GENERATE_DEBUG_INFO)
  set(TART_OPTIONS ${TART_OPTIONS} -g)
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

set_source_files_properties("${PROJECT_BINARY_DIR}/lib/std/libstd.bc" PROPERTIES GENERATED 1)

# Dummy target needed to depend on a top-level target in another directory
add_custom_target(test-simple.libdeps DEPENDS libstd)

# Create and run a test executable for each source file
set(TEST_BC_FILES)
foreach(SRC_FILE ${TEST_SRC})
  # Source file name
  string(REGEX REPLACE ".tart\$" ".bc" BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".lnk.bc" LNK_BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".opt.bc" OPT_BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" "${CMAKE_CXX_OUTPUT_EXTENSION}" OBJ_FILE "${SRC_FILE}")  
  string(REGEX REPLACE ".tart\$" "${CMAKE_EXECUTABLE_SUFFIX}" EXE_FILE "${SRC_FILE}")

  # Generate the deps variable name
  string(REGEX REPLACE ".tart\$" "" DEPS_NAME "${SRC_FILE}")
  string(TOUPPER "${DEPS_NAME}" DEPS_NAME)
  string(REGEX REPLACE "[^a-zA-Z0-9]" "_" DEPS_NAME "${DEPS_NAME}")
  
  # Compile test source
  add_custom_command(OUTPUT ${BC_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} ${MODPATH} ${SRC_FILE}
      MAIN_DEPENDENCY "${SRC_FILE}" 
      DEPENDS "${PROJECT_BINARY_DIR}/lib/std/libstd.bc" ${${DEPS_NAME}_DEPS}
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Remember the list of output files.
  set(TEST_BC_FILES ${TEST_BC_FILES} "${BC_FILE}")
  
  # Link bitcode files
  add_custom_command(OUTPUT ${LNK_BC_FILE}
      COMMAND ${LLVM_LD}
          -disable-opt
          -link-as-library
          -o ${LNK_BC_FILE}
          ${BC_FILE} ${BC_LIBS}
      MAIN_DEPENDENCY "${BC_FILE}" 
      DEPENDS test-simple.libdeps ${BC_LIBS}
      COMMENT "Linking Tart bitcode file ${BC_FILE}")

  # Optimize object files
  add_custom_command(OUTPUT ${OPT_BC_FILE}
      COMMAND ${LLVM_OPT} ${OPT_FLAGS}
          -o ${OPT_BC_FILE}
          ${LNK_BC_FILE}
      MAIN_DEPENDENCY "${LNK_BC_FILE}" 
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

  # Link executable
  add_executable(${EXE_FILE} EXCLUDE_FROM_ALL ${OBJ_FILE})
  target_link_libraries(${EXE_FILE} ${TEST_LIBS})

  add_custom_target("${EXE_FILE}.run" COMMAND ./${EXE_FILE} DEPENDS ${EXE_FILE})
  add_dependencies(check "${EXE_FILE}.run")
endforeach(SRC_FILE)

# Generate dependency info
add_custom_target(test-simple.deps
    COMMAND gendeps -o test.deps ${TEST_BC_FILES}
    DEPENDS ${TEST_BC_FILES} gendeps
    COMMENT "Generating dependencies for tests")

add_dependencies(check test-simple.deps)

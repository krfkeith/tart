# CMake build file for tart/test/stdlib

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TART_SRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tart)
set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(MODPATH  # Module search path
  -i ${TART_SOURCE_DIR}/lib/core
  -i ${TART_SOURCE_DIR}/lib/testing)

set(TART_OPTIONS
  -g
  -debug-errors
#  -trace-def=InterfaceTest
)
#set(TART_OPTIONS -debug-errors -show-generated -show-size)
set(TARTLN_OPTIONS -internalize -O0)

set(TEST_BC_FILES)

include(${CMAKE_CURRENT_BINARY_DIR}/test.deps OPTIONAL)

# Create and run a test executable for each source file
foreach(SRC_FILE ${TART_SRC})
  # Source file name
  string(REGEX REPLACE ".tart\$" ".bc" BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".out.bc" OUT_BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".s" ASM_FILE "${SRC_FILE}")

  # Generate the deps variable name
  string(REGEX REPLACE ".tart\$" "" DEPS_NAME "${SRC_FILE}")
  string(TOUPPER "${DEPS_NAME}" DEPS_NAME)
  string(REGEX REPLACE "[^a-zA-Z0-9]" "_" DEPS_NAME "${DEPS_NAME}")

  # Compile tart source
  add_custom_command(OUTPUT ${BC_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} ${MODPATH} ${SRC_FILE}
      MAIN_DEPENDENCY "${SRC_FILE}" 
      DEPENDS tartc ${${DEPS_NAME}_DEPS}
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Remember the list of output files.
  set(TEST_BC_FILES ${TEST_BC_FILES} "${BC_FILE}")

  # Link with libcore
  add_custom_command(OUTPUT ${OUT_BC_FILE}
      COMMAND tartln -disable-fp-elim -filetype=bc -o ${OUT_BC_FILE} ${TARTLN_OPTIONS} ${BC_FILE}
          "${PROJECT_BINARY_DIR}/lib/core/libcore.bc"
          "${PROJECT_BINARY_DIR}/lib/testing/libtesting.bc"
      MAIN_DEPENDENCY "${BC_FILE}" 
      DEPENDS
          "${PROJECT_BINARY_DIR}/lib/core/libcore.bc"
          "${PROJECT_BINARY_DIR}/lib/testing/libtesting.bc"
      COMMENT "Linking Tart bitcode file ${BC_FILE}")
      
  # Convert to assembly
  add_custom_command(OUTPUT ${ASM_FILE}
      COMMAND ${LLVM_BIN_DIR}/llc -o ${ASM_FILE} ${OUT_BC_FILE}
      DEPENDS "${OUT_BC_FILE}"
      COMMENT "Generating assembly for file ${BC_FILE}")

  # Assemble and run
  string(REGEX REPLACE ".tart\$" "" EXE_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${EXE_FILE}
      COMMAND gcc -g -fno-omit-framepointer -c -O0 -x assembler ${ASM_FILE} -o ${EXE_FILE}.o
      COMMAND gcc -o ${EXE_FILE} -L${TART_BINARY_DIR}/runtime ${EXE_FILE}.o -lruntime -ldl
      MAIN_DEPENDENCY "${ASM_FILE}"
      DEPENDS runtime
      COMMENT "Assembling ${ASM_FILE}")

  # Generate debugging symbols
  # TODO(talin): This doesn't work and I have no idea why
  if ($DSYMUTIL)
    add_custom_command(OUTPUT "${EXE_FILE}.dSYM/Contents/Resources/DWARF/${EXE_FILE}"
        COMMAND ${DSYMUTIL} ${EXE_FILE}
        MAIN_DEPENDENCY "${EXE_FILE}"
        COMMENT "Generating debug symbols for ${EXE_FILE}")
  endif ($DSYMUTIL)

  add_custom_target("${EXE_FILE}.run" COMMAND ./${EXE_FILE} DEPENDS ${EXE_FILE})
  add_dependencies("${EXE_FILE}.run" libcore libtesting)
  add_dependencies(check "${EXE_FILE}.run")
endforeach(SRC_FILE)

# Generate dependency info
add_custom_target(test.deps
    COMMAND gendeps -o test.deps ${TEST_BC_FILES}
    DEPENDS ${TEST_BC_FILES} gendeps
    COMMENT "Generating dependencies for tests")

add_dependencies(check test.deps)

# CMake build file for tart/test/stdlib

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TART_SRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tart)
set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(MODPATH  # Module search path
  -i ${TART_SOURCE_DIR}/stdlib
  -i ${TART_SOURCE_DIR}/libext/testing)
set(TART_OPTIONS -g -debug-errors -show-generated)
#set(TART_OPTIONS -debug-errors -show-generated)
set(TARTLN_OPTIONS -internalize -O0)

# Create and run a test executable for each source file
foreach(SRC_FILE ${TART_SRC})
  # Compile tart source
  string(REGEX REPLACE ".tart\$" ".bc" BC_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${BC_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} ${MODPATH} ${SRC_FILE}
      DEPENDS "${SRC_FILE}" tartc
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Link with stdlib
  string(REGEX REPLACE ".tart\$" ".s" ASM_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${ASM_FILE}
      COMMAND tartln -disable-fp-elim -filetype=asm ${TARTLN_OPTIONS} ${BC_FILE}
          "${PROJECT_BINARY_DIR}/stdlib/stdlib.bc"
          "${PROJECT_BINARY_DIR}/libext/testing/testing.lib.bc"
      DEPENDS "${BC_FILE}" tartln stdlib "${PROJECT_BINARY_DIR}/libext/testing/testing.lib.bc"
      COMMENT "Linking Tart bitcode file ${BC_FILE}")

  # Assemble and run
  string(REGEX REPLACE ".tart\$" "" EXE_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${EXE_FILE}
      COMMAND gcc -g -fno-omit-framepointer -c -O0 -x assembler ${ASM_FILE} -o ${EXE_FILE}.o
      COMMAND gcc -o ${EXE_FILE} -L${TART_BINARY_DIR}/runtime ${EXE_FILE}.o -lruntime
      #COMMAND dsymutil ${EXE_FILE}
      DEPENDS "${ASM_FILE}" runtime
      COMMENT "Assembling ${ASM_FILE}")

  add_custom_target("${EXE_FILE}.run" COMMAND ./${EXE_FILE} DEPENDS ${EXE_FILE})
  add_dependencies(check "${EXE_FILE}.run")
endforeach(SRC_FILE)

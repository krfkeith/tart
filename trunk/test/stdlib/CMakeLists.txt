# CMake build file for Tart/tart/stdlib

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TART_SRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tart)
set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(MODPATH ${TART_SOURCE_DIR}/stdlib)  # Module search path
set(TART_OPTIONS -g -debug-errors -show-generated)

# Create and run a test executable for each source file
foreach(SRC_FILE ${TART_SRC})
  # Compile tart source
  string(REGEX REPLACE ".tart\$" ".bc" BC_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${BC_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} -i${MODPATH} ${SRC_FILE}
      DEPENDS "${SRC_FILE}" tartc
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Link with stdlib
  string(REGEX REPLACE ".tart\$" ".s" ASM_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${ASM_FILE}
      COMMAND tartln -disable-fp-elim -filetype=asm -internalize -O0 ${BC_FILE} "${TART_BINARY_DIR}/stdlib/stdlib.bc"
      DEPENDS "${BC_FILE}" tartln stdlib
      COMMENT "Linking Tart bitcode file ${BC_FILE}")

  # Assemble and run
  string(REGEX REPLACE ".tart\$" "" EXE_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${EXE_FILE}
      COMMAND gcc -g -O0 -x assembler ${ASM_FILE} -o ${EXE_FILE} -L${TART_BINARY_DIR}/runtime -l runtime
      DEPENDS "${ASM_FILE}" runtime
      COMMENT "Assembling ${ASM_FILE}")

  add_custom_target("${EXE_FILE}.run" COMMAND ./${EXE_FILE} DEPENDS ${EXE_FILE})
  add_dependencies(check "${EXE_FILE}.run")
endforeach(SRC_FILE)

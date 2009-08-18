# CMake build file for Tart/tart/nolib

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
      COMMAND tartln -filetype=asm ${BC_FILE}
      DEPENDS "${BC_FILE}" tartln
      COMMENT "Linking Tart bc file ${BC_FILE}")

  # Assemble and run
  string(REGEX REPLACE ".tart\$" "" EXE_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${EXE_FILE}
      COMMAND gcc ${ASM_FILE} -o ${EXE_FILE}
      DEPENDS "${ASM_FILE}"
      COMMENT "Assembling ${ASM_FILE}")

  add_custom_target("${EXE_FILE}.run" COMMAND ./${EXE_FILE} DEPENDS ${EXE_FILE})
  add_dependencies(check "${EXE_FILE}.run")
endforeach(SRC_FILE)

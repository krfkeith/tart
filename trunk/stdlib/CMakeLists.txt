# CMake build file for Tart/stdlib

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TART_STDLIB_SRC
    RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
    tart/annex/*.tart
    tart/core/*.tart
    tart/collections/*.tart
    tart/gc/*.tart
    tart/gc/heap/*.tart
    tart/io/*.tart
    tart/reflect/*.tart
    tart/testing/*.tart
    tart/text/encodings/*.tart)

set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(MODPATH ${TART_SOURCE_DIR}/stdlib)  # Module search path
set(TART_OPTIONS
    -g
    -debug-errors
    -show-generated)

# Empty list of output files
set(STDLIB_BC_FILES)

# Create a command for each source file
foreach(SRC_FILE ${TART_STDLIB_SRC})
  string(REGEX REPLACE ".tart\$" ".bc" OUT_FILE "${SRC_FILE}")
  add_custom_command(OUTPUT ${OUT_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} -i${MODPATH} ${SRC_FILE}
      DEPENDS "${SRC_FILE}" tartc
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Remember the list of output files.
  set(STDLIB_BC_FILES ${STDLIB_BC_FILES} "${OUT_FILE}")
endforeach(SRC_FILE)

# Target to drive the conversion
add_custom_command(OUTPUT stdlib.bc
    COMMAND tartln -filetype=bc -o stdlib.bc ${STDLIB_BC_FILES}
    DEPENDS ${STDLIB_BC_FILES} tartln
    COMMENT "Linking stdlib.bc")

add_custom_target(stdlib DEPENDS stdlib.bc)

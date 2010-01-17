# CMake build file for lib/core

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TART_STDLIB_SRC
    RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
    tart/annex/*.tart
    tart/core/*.tart
    tart/collections/*.tart
    tart/config/*.tart
    tart/gc/*.tart
    tart/gc/heap/*.tart
    tart/io/*.tart
    tart/reflect/*.tart
    tart/text/encodings/*.tart)

set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(MODPATH ${TART_SOURCE_DIR}/lib/core)  # Module search path
set(TART_OPTIONS
    -g
    -debug-errors
#    -trace-def=asBuffer
)

# Empty list of output files
set(STDLIB_BC_FILES)

include(${CMAKE_CURRENT_BINARY_DIR}/libcore.deps OPTIONAL)

# Create a command for each source file
foreach(SRC_FILE ${TART_STDLIB_SRC})
  string(REGEX REPLACE ".tart\$" ".bc" OUT_FILE "${SRC_FILE}")
  
  # Generate the deps variable name
  string(REGEX REPLACE ".tart\$" "" DEPS_NAME "${SRC_FILE}")
  string(TOUPPER "${DEPS_NAME}" DEPS_NAME)
  string(REGEX REPLACE "[^a-zA-Z0-9]" "_" DEPS_NAME "${DEPS_NAME}")

  add_custom_command(OUTPUT ${OUT_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} -i${MODPATH} ${SRC_FILE}
      DEPENDS "${SRC_FILE}" tartc ${${DEPS_NAME}_DEPS}
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Remember the list of output files.
  set(STDLIB_BC_FILES ${STDLIB_BC_FILES} "${OUT_FILE}")
endforeach(SRC_FILE)

# Target to drive the conversion
add_custom_command(
    OUTPUT libcore.bc
    COMMAND tartln -filetype=bc -link-as-library -o libcore.bc ${STDLIB_BC_FILES}
    DEPENDS ${STDLIB_BC_FILES} tartln
    COMMENT "Linking libcore.bc")
    
# Generate dependency info
add_custom_command(
    OUTPUT libcore.deps
    COMMAND gendeps -o libcore.deps ${STDLIB_BC_FILES}
    DEPENDS ${STDLIB_BC_FILES} gendeps
    COMMENT "Generating dependencies for libcore.bc")

add_custom_target(libcore ALL DEPENDS libcore.bc libcore.deps)

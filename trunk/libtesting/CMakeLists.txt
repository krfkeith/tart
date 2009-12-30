# CMake build file for tart/libext/testing

set(CMAKE_VERBOSE_MAKEFILE ON)

# Module search path
set(MODPATH
    -i ${TART_SOURCE_DIR}/stdlib
    -i ${TART_SOURCE_DIR}/libtesting)

# Input files
file(GLOB TESTING_SRC
    RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
    tart/testing/*.tart)

set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(TESTING_BC_FILES)

# Create a command for each source file
foreach(SRC_FILE ${TESTING_SRC})
  string(REGEX REPLACE ".tart\$" ".bc" OUT_FILE "${SRC_FILE}")
  add_custom_command(
      OUTPUT ${OUT_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} ${MODPATH} ${SRC_FILE}
      DEPENDS "${SRC_FILE}" tartc
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Remember the list of output files.
  set(TESTING_BC_FILES ${TESTING_BC_FILES} "${OUT_FILE}")
endforeach(SRC_FILE)

# Target to drive the conversion
add_custom_command(
    OUTPUT libtesting.bc
    COMMAND tartln -filetype=bc -o libtesting.bc ${TESTING_BC_FILES}
    DEPENDS ${TESTING_BC_FILES} tartln
    COMMENT "Linking libtesting.bc")

add_custom_target(libtesting DEPENDS libtesting.bc)

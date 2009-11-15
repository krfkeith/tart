# CMake build file for Tart/tart/stdlib

# set(CMAKE_VERBOSE_MAKEFILE ON)

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

add_executable(testrunner testrunner.cpp)
add_dependencies(testrunner collector compiler unittest.run)
target_link_libraries(testrunner
    compiler
    ${LLVM_BIT_READER}
    ${LLVM_BIT_WRITER}
    ${LLVM_SELECTION_DAG}
    ${LLVM_ANALYSIS}
    ${LLVM_TARGET}
    ${LLVM_CORE}
    ${LLVM_SUPPORT}
    ${LLVM_SYSTEM}
    )
set_target_properties(testrunner PROPERTIES LINK_FLAGS ${LLVM_LD_FLAGS})

# Now run the tests on these files.
file(GLOB TART_SRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tart)
set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(MODPATH ${TART_SOURCE_DIR}/stdlib)  # Module search path
set(TART_OPTIONS -debug-errors -show-generated)

add_custom_target(failtests)
add_dependencies(check failtests)

# Create and run a test executable for each source file
foreach(SRC_FILE ${TART_SRC})
  # Compile tart source
  string(REGEX REPLACE ".tart\$" ".bc" BC_FILE "${SRC_FILE}")
  add_custom_command(TARGET failtests PRE_BUILD
      COMMAND testrunner ${TART_OPTIONS} -sourcepath ${SRCDIR} -i${MODPATH} ${SRC_FILE}
      DEPENDS "${SRC_FILE}" testrunner stdlib
      COMMENT "Running failure test ${SRC_FILE}")
endforeach(SRC_FILE)

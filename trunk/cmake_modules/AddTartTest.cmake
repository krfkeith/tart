# AddTartTest - defines the "add_tart_test" function

set(TARTC_FLAGS
  -debug-errors
  -nostdlib # Don't look for stdlib in it's installed location
)

function(add_tart_test TEST_NAME SRCLIST_VAR)
  include(${CMAKE_CURRENT_BINARY_DIR}/${TEST_NAME}.deps OPTIONAL)

  set(LNK_BC_FILE "${TEST_NAME}.lnk.bc")
  set(OPT_BC_FILE "${TEST_NAME}.opt.bc")
  set(OBJ_FILE "${TEST_NAME}${CMAKE_CXX_OUTPUT_EXTENSION}")
  set(EXE_FILE "${TEST_NAME}${CMAKE_EXECUTABLE_SUFFIX}")

  set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root

  set(TEST_CLIBS runtime)
  if (LIB_DL)
    set(TEST_CLIBS ${TEST_CLIBS} dl)
  endif (LIB_DL)

  # Compile each source file into a bitcode file
  set(BC_FILES)
  foreach(SRC_FILE ${${SRCLIST_VAR}})
    # Source file name
    string(REGEX REPLACE ".tart\$" ".bc" BC_FILE "${SRC_FILE}")

    # Generate the deps variable name
    string(REGEX REPLACE ".tart\$" "" BASE_NAME "${SRC_FILE}")
    string(TOUPPER "${BASE_NAME}" DEPS_NAME)
    string(REGEX REPLACE "[^a-zA-Z0-9]" "_" DEPS_NAME "${DEPS_NAME}")

    # Compile tart source
    add_custom_command(OUTPUT ${BC_FILE}
        COMMAND tartc ${TARTC_FLAGS} -sourcepath ${SRCDIR} ${MODPATH} "${SRC_FILE}"
        MAIN_DEPENDENCY "${SRC_FILE}"
        DEPENDS ${BC_LIBS} ${${DEPS_NAME}_DEPS}
        COMMENT "Compiling Tart source file ${SRC_FILE}")

    # Remember the list of output files.
    set(BC_FILES ${BC_FILES} "${BC_FILE}")
  endforeach(SRC_FILE)

  # Link bitcode files
  add_custom_command(OUTPUT ${LNK_BC_FILE}
      COMMAND ${LLVM_LD}
          -disable-opt
          -link-as-library
          -o ${LNK_BC_FILE}
          ${BC_FILES} ${BC_LIBS}
      DEPENDS ${BC_FILES} ${BC_LIBS}
      COMMENT "Linking Tart bitcode file: ${LNK_BC_FILE}")

  # Optimize object files
  add_custom_command(OUTPUT ${OPT_BC_FILE}
      COMMAND ${LLVM_OPT} ${OPT_FLAGS} -o ${OPT_BC_FILE} ${LNK_BC_FILE}
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

  # Generate dependency info
  add_custom_command(OUTPUT ${TEST_NAME}.deps
      COMMAND gendeps -o ${TEST_NAME}.deps ${BC_FILES}
      DEPENDS ${BC_FILES} gendeps
      COMMENT "Generating dependencies for tests")

  add_executable(${EXE_FILE} EXCLUDE_FROM_ALL ${OBJ_FILE})
  add_dependencies(${EXE_FILE} ${LIB_DEPS})
  target_link_libraries(${EXE_FILE} ${TEST_CLIBS})

  add_custom_target(${TEST_NAME}.run COMMAND ./${EXE_FILE} DEPENDS ${EXE_FILE} ${TEST_NAME}.deps)
  add_dependencies(check ${TEST_NAME}.run)
endfunction(add_tart_test)

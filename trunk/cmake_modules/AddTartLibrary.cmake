# AddTartLibrary - defines the "add_tart_library" function

function(add_tart_library LibName SourceRoot SourceFiles)
  include(${CMAKE_CURRENT_BINARY_DIR}/lib${LibName}.deps OPTIONAL)

  set(BC_FILES)
  foreach(SRC_FILE ${SourceFiles})
    # Construct the name of the output file from the source file
    string(REGEX REPLACE ".tart\$" ".bc" OUT_FILE "${SRC_FILE}")

    # Generate the deps variable name
    string(REGEX REPLACE ".tart\$" "" DEPS_NAME "${SRC_FILE}")
    string(TOUPPER "${DEPS_NAME}" DEPS_NAME)
    string(REGEX REPLACE "[^a-zA-Z0-9]" "_" DEPS_NAME "${DEPS_NAME}")

    add_custom_command(
        OUTPUT ${OUT_FILE}
        COMMAND tartc ${TARTC_OPTIONS} -sourcepath ${SourceRoot} ${TART_MODULE_PATH} ${SRC_FILE}
        DEPENDS "${SourceRoot}/${SRC_FILE}" tartc ${${DEPS_NAME}_DEPS}
        COMMENT "Compiling Tart source file ${SRC_FILE}")

    # Remember the list of output files.
    set(BC_FILES ${BC_FILES} "${OUT_FILE}")
  endforeach(SRC_FILE)

  # Link phase
  add_custom_command(
      OUTPUT lib${LibName}.bc
      COMMAND tartln -filetype=bc -link-as-library -o lib${LibName}.bc ${BC_FILES}
      DEPENDS ${BC_FILES} tartln
      COMMENT "Linking lib${LibName}.bc")
  
  # Generate dependency info
  add_custom_command(
      OUTPUT lib${LibName}.deps
      COMMAND gendeps -o lib${LibName}.deps ${BC_FILES}
      DEPENDS ${BC_FILES} gendeps
      COMMENT "Generating dependencies for lib${LibName}.bc")

endfunction(add_tart_library)

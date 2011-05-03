# CMake build file for tart/test/stdlib

set(CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB TART_SRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.tart)
source_group(StdLibTests FILES ${TART_SRC})
set(SRCDIR ${CMAKE_CURRENT_SOURCE_DIR}) # Source file root
set(MODPATH  # Module search path
  -i ${TART_BINARY_DIR}/lib/std/libstd.bc
#  -i ${TART_SOURCE_DIR}/lib/std
  -i ${TART_SOURCE_DIR}/lib/testing)

set(BC_LIBS
  "${PROJECT_BINARY_DIR}/lib/std/libstd.bc"
  "${PROJECT_BINARY_DIR}/lib/testing/libtesting.bc"
  "${PROJECT_BINARY_DIR}/lib/gc1/libgc1.bc"
  )

set(TART_OPTIONS
  -debug-errors
  -nostdlib
#  -trace-def=InterfaceTest
)
#set(TART_OPTIONS -debug-errors -show-generated -show-size)
#set(TARTLN_OPTIONS -O2)
#set(TARTLN_OPTIONS -internalize -O0)

set(GCC_OPTIONS -lruntime)
set(TEST_LIBS runtime)

if (LIB_DL)
  set(GCC_OPTIONS ${GCC_OPTIONS} -ldl)
  set(TEST_LIBS ${TEST_LIBS} dl)
endif (LIB_DL)

set(TEST_BC_FILES)

set(GENERATE_ASM 0)
set(USE_LLVM_BINARIES 1)
set(GENERATE_DEBUG_INFO 1)

include(${CMAKE_CURRENT_BINARY_DIR}/test.deps OPTIONAL)

set(GC_PLUGIN "${PROJECT_BINARY_DIR}/linker/libgc${CMAKE_SHARED_LIBRARY_SUFFIX}")
set(REFLECTOR_PLUGIN "${PROJECT_BINARY_DIR}/linker/libreflector${CMAKE_SHARED_LIBRARY_SUFFIX}")
set(PUBLIC_SYMBOLS "main,String_create,TraceAction_traceDescriptors,GC_static_roots")

if (GENERATE_DEBUG_INFO)
  set(TART_OPTIONS ${TART_OPTIONS} -g)
  set(TARTLN_OPTIONS -O2 -disable-inlining)
  set(OPT_FLAGS
      -disable-inlining
      -load="${REFLECTOR_PLUGIN}"
      -internalize-public-api-list=${PUBLIC_SYMBOLS}
#      -std-compile-opts
#      -std-link-opts
#      -instcombine
#      -simplifycfg
#      -adce
#      -globaldce
#      -globalopt
      -globaldce
      -staticroots
      -reflector
  )
else (GENERATE_DEBUG_INFO)
  set(TARTLN_OPTIONS -internalize -O2)
  set(OPT_FLAGS
      -O2
      -strip-debug
      -load="${REFLECTOR_PLUGIN}"
      -internalize-public-api-list=${PUBLIC_SYMBOLS}
#     -mergefunc
      -std-compile-opts
      -std-link-opts
      -instcombine
      -simplifycfg
      -adce
      -globaldce
      -globalopt
      -staticroots
      -reflector
      -globaldce
  )
endif (GENERATE_DEBUG_INFO)

set_source_files_properties("${PROJECT_BINARY_DIR}/lib/std/libstd.bc" PROPERTIES GENERATED 1)

# Create and run a test executable for each source file
foreach(SRC_FILE ${TART_SRC})
  # Source file name
  string(REGEX REPLACE ".tart\$" ".bc" BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".out.bc" OUT_BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".opt.bc" OPT_BC_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" ".s" ASM_FILE "${SRC_FILE}")
  string(REGEX REPLACE ".tart\$" "${CMAKE_CXX_OUTPUT_EXTENSION}" OBJ_FILE "${SRC_FILE}")  
  string(REGEX REPLACE ".tart\$" "${CMAKE_EXECUTABLE_SUFFIX}" EXE_FILE "${SRC_FILE}")

  # Generate the deps variable name
  string(REGEX REPLACE ".tart\$" "" DEPS_NAME "${SRC_FILE}")
  string(TOUPPER "${DEPS_NAME}" DEPS_NAME)
  string(REGEX REPLACE "[^a-zA-Z0-9]" "_" DEPS_NAME "${DEPS_NAME}")
  
  # Compile tart source
  set_source_files_properties("${PROJECT_BINARY_DIR}/lib/std/libstd.bc" PROPERTIES GENERATED 1)
  add_custom_command(OUTPUT ${BC_FILE}
      COMMAND tartc ${TART_OPTIONS} -sourcepath ${SRCDIR} ${MODPATH} ${SRC_FILE}
      MAIN_DEPENDENCY "${SRC_FILE}" 
      DEPENDS "${PROJECT_BINARY_DIR}/lib/std/libstd.bc" ${${DEPS_NAME}_DEPS}
      COMMENT "Compiling Tart source file ${SRC_FILE}")

  # Remember the list of output files.
  set(TEST_BC_FILES ${TEST_BC_FILES} "${BC_FILE}")
  
  # Link with libstd
  if (GENERATE_ASM)
    add_custom_command(OUTPUT ${ASM_FILE}
        COMMAND tartln -disable-fp-elim -filetype=asm -o ${ASM_FILE} ${TARTLN_OPTIONS} ${BC_FILE}
            ${BC_LIBS}
        MAIN_DEPENDENCY "${BC_FILE}" 
        DEPENDS ${BC_LIBS}
        COMMENT "Linking Tart bitcode file ${BC_FILE}")
      
    # Convert to assembly
    #add_custom_command(OUTPUT ${ASM_FILE}
    #    COMMAND ${LLVM_OPT} ${OUT_BC_FILE} | ${LLVM_LLC} -disable-fp-elim -o ${ASM_FILE}
    #    DEPENDS "${OUT_BC_FILE}"
    #    COMMENT "Generating assembly for file ${BC_FILE}")

    # Assemble and run
    add_custom_command(OUTPUT ${EXE_FILE}
        COMMAND gcc -g -fno-omit-framepointer -c -O0 -x assembler ${ASM_FILE} -o ${EXE_FILE}.o
        COMMAND gcc -o ${EXE_FILE} -L${TART_BINARY_DIR}/runtime ${EXE_FILE}.o ${GCC_OPTIONS}
        MAIN_DEPENDENCY "${ASM_FILE}"
        DEPENDS runtime
        COMMENT "Assembling ${ASM_FILE}")
  elseif (USE_LLVM_BINARIES)
    # Link bitcode files
    add_custom_command(OUTPUT ${OUT_BC_FILE}
        COMMAND ${LLVM_LD}
            -disable-opt
            -link-as-library
            -o ${OUT_BC_FILE}
            ${BC_FILE} ${BC_LIBS}
        MAIN_DEPENDENCY "${BC_FILE}" 
        DEPENDS ${BC_LIBS}
        COMMENT "Linking Tart bitcode file ${BC_FILE}")

    # Optimize object files
    add_custom_command(OUTPUT ${OPT_BC_FILE}
        COMMAND ${LLVM_OPT} ${OPT_FLAGS}
            -o ${OPT_BC_FILE}
            ${OUT_BC_FILE}
        MAIN_DEPENDENCY "${OUT_BC_FILE}" 
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

    # Link executable
    add_executable(${EXE_FILE} EXCLUDE_FROM_ALL ${OBJ_FILE})
    # TODO: This is incorrect, should be deps of the above step but CMake doesn't allow that.
    add_dependencies(${EXE_FILE} libstd libtesting libgc1)
    target_link_libraries(${EXE_FILE} ${TEST_LIBS})
  else (GENERATE_ASM)
    # Use tartln and don't generate assembly.
    add_custom_command(OUTPUT ${OBJ_FILE}
        COMMAND tartln -disable-fp-elim -filetype=obj -o ${OBJ_FILE} ${TARTLN_OPTIONS} ${BC_FILE}
            ${BC_LIBS}
        MAIN_DEPENDENCY "${BC_FILE}" 
        DEPENDS
#            libstd libtesting libgc1
            ${BC_LIBS}
        COMMENT "Linking Tart bitcode file ${BC_FILE}")

    # Link executable
    add_executable(${EXE_FILE} EXCLUDE_FROM_ALL ${OBJ_FILE})
    # TODO: This is incorrect, should be deps of the above step but CMake doesn't allow that.
    add_dependencies(${EXE_FILE} libstd libtesting libgc1)
    target_link_libraries(${EXE_FILE} ${TEST_LIBS})
  endif (GENERATE_ASM)

  # Generate debugging symbols
  # TODO(talin): This doesn't work and I have no idea why
  #if ($DSYMUTIL)
#    add_custom_command(OUTPUT "${EXE_FILE}.dSYM/Contents/Resources/DWARF/${EXE_FILE}"
#        COMMAND ${DSYMUTIL} ${EXE_FILE}
#        MAIN_DEPENDENCY "${EXE_FILE}"
#        COMMENT "Generating debug symbols for ${EXE_FILE}")
  #endif ($DSYMUTIL)

  add_custom_target("${EXE_FILE}.run" COMMAND ./${EXE_FILE} DEPENDS ${EXE_FILE})
  add_dependencies("${EXE_FILE}.run" libstd libtesting libgc1)
  add_dependencies(check "${EXE_FILE}.run")
endforeach(SRC_FILE)

# Generate dependency info
add_custom_target(test.deps
    COMMAND gendeps -o test.deps ${TEST_BC_FILES}
    DEPENDS ${TEST_BC_FILES} gendeps
    COMMENT "Generating dependencies for tests")

add_dependencies(check test.deps)

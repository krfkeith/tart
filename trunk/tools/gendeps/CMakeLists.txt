# CMake build file for Tart/tools

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

execute_process(
  COMMAND ${LLVM_CONFIG} --libs bitreader asmparser ${LLVM_TARGETS}
  OUTPUT_VARIABLE LLVM_GENDEPS_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

add_executable(gendeps gendeps.cpp)
target_link_libraries(gendeps
    ${LLVM_GENDEPS_LIBS}
    )
set_target_properties(gendeps PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

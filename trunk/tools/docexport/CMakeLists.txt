# CMake build file for Tart/tools

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

execute_process(
  COMMAND ${LLVM_CONFIG} --libs core bitwriter ${LLVM_TARGETS}
  OUTPUT_VARIABLE LLVM_DOCEXPORT_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

file(GLOB DOCEXPORT_SOURCES *.cpp)
file(GLOB DOCEXPORT_HEADERS *.h)

add_executable(docexport EXCLUDE_FROM_ALL ${DOCEXPORT_SOURCES} ${DOCEXPORT_HEADERS})
add_dependencies(docexport compiler)
target_link_libraries(docexport compiler ${LLVM_DOCEXPORT_LIBS})
set_target_properties(docexport PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

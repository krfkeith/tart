# CMake build file for Tart/tools

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

execute_process(
  COMMAND ${LLVM_CONFIG} --libs bitwriter ${LLVM_TARGETS}
  OUTPUT_VARIABLE LLVM_TARTC_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

if (CMAKE_COMPILER_IS_GNUCXX)
   if ($PROFILE_TARTC)
     add_definitions(-pg)
     set(CMAKE_EXE_LINKER_FLAGS -pg)
   endif ($PROFILE_TARTC)
endif (CMAKE_COMPILER_IS_GNUCXX)

add_executable(tartc tartc.cpp)
add_dependencies(tartc compiler)
target_link_libraries(tartc compiler ${LLVM_TARTC_LIBS})
set_target_properties(tartc PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

install(TARGETS tartc RUNTIME DESTINATION bin)

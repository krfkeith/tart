# CMake build file for tools/tartln

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

execute_process(
  COMMAND ${LLVM_CONFIG} --libs ipo bitwriter linker ${LLVM_TARGETS}
  OUTPUT_VARIABLE LLVM_TARTLN_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Extra flags for GCC (C++ only)
if (CMAKE_COMPILER_IS_GNUCXX)
   if ($PROFILE_TARTLN)
     add_definitions(-pg)
     set(CMAKE_EXE_LINKER_FLAGS -pg)
   endif ($PROFILE_TARTLN)
endif (CMAKE_COMPILER_IS_GNUCXX)

add_executable(tartln tartln.cpp)
target_link_libraries(tartln
    linker_reflect
    linker_common
    gcstrategy
    ${LLVM_TARTLN_LIBS}
    )
set_target_properties(tartln PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

install(TARGETS tartln RUNTIME DESTINATION bin)

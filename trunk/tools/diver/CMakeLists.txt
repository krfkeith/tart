# CMake build file for tools/diver

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

execute_process(
  COMMAND ${LLVM_CONFIG} --libs bitreader asmparser debuginfo ${LLVM_TARGETS}
  OUTPUT_VARIABLE LLVM_DIVER_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

add_executable(diver diver.cpp)
target_link_libraries(diver compiler ${LLVM_DIVER_LIBS})
set_target_properties(diver PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

# If the compiler is clang, then generate an example bitcode file for us.
# Go ahead and use diver.cpp.
if (CMAKE_COMPILER_IS_CLANG)
  string(REGEX MATCHALL "([^ ]+)" CLANG_FLAGS ${LLVM_CFLAGS})
  add_custom_command(OUTPUT diver.bc
    COMMAND clang -fno-rtti -fno-exceptions -fPIC ${CLANG_FLAGS} -g -c ${CMAKE_CURRENT_SOURCE_DIR}/diver.cpp -emit-llvm -o diver.bc
    COMMENT "Creating diver.bc example file"
  )
  add_custom_target(diver.exa DEPENDS diver.bc)
endif (CMAKE_COMPILER_IS_CLANG)

# CMake build file for Tart/tools

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

add_executable(gendeps gendeps.cpp)
target_link_libraries(gendeps
    ${LLVM_BIT_READER}
    ${LLVM_ASM_PARSER}
    ${LLVM_TARGET}
    ${LLVM_CORE}
    ${LLVM_SUPPORT}
    )
set_target_properties(gendeps PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

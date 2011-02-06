# CMake build file for Tart/tools

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

if (CMAKE_COMPILER_IS_GNUCXX)
   if ($PROFILE_TARTC)
     add_definitions(-pg)
     set(CMAKE_EXE_LINKER_FLAGS -pg)
   endif ($PROFILE_TARTC)
endif (CMAKE_COMPILER_IS_GNUCXX)

add_executable(tartc tartc.cpp)
add_dependencies(tartc compiler)
target_link_libraries(tartc
    compiler
    ${LLVM_ALL_BACKENDS}
    ${LLVM_IPO}
    ${LLVM_LINKER}
    ${LLVM_SELECTION_DAG}
    ${LLVM_ARCHIVE}
    ${LLVM_BIT_READER}
    ${LLVM_BIT_WRITER}
    ${LLVM_ASM_PRINTER}
    ${LLVM_ASM_PARSER}
    ${LLVM_CODE_GEN}
    ${LLVM_SCALAR_OPTS}
    ${LLVM_IPA}
    ${LLVM_TRANSFORM_UTILS}
    ${LLVM_ANALYSIS}
    ${LLVM_TARGET}
    ${LLVM_MC}
    ${LLVM_CORE}
    ${LLVM_SUPPORT}
    )
set_target_properties(tartc PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

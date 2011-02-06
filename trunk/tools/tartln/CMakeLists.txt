# CMake build file for tools/tartln

# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

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
    ${LLVM_IPO}
    ${LLVM_LINKER}
    ${LLVM_ARCHIVE}
    ${LLVM_BIT_READER}
    ${LLVM_ALL_BACKENDS}
    ${LLVM_SELECTION_DAG}
    ${LLVM_BIT_WRITER}
    ${LLVM_ASM_PRINTER}
    ${LLVM_MC_PARSER}
    ${LLVM_ASM_PARSER}
    ${LLVM_CODE_GEN}
    ${LLVM_SCALAR_OPTS}
    ${LLVM_INSTCOMBINE}
    ${LLVM_IPA}
    ${LLVM_TRANSFORM_UTILS}
    ${LLVM_ANALYSIS}
    ${LLVM_TARGET}
    ${LLVM_CORE}
    ${LLVM_MC}
    ${LLVM_SUPPORT}
    )
set_target_properties(tartln PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

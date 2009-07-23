# CMake build file for Tart/tools

cmake_policy(SET CMP0008 NEW)
 
# Create a command for each source file
set(SEARCH_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SYSTEM_LIBRARY_PATH} ${LIB} /usr/local/lib)

add_executable(tartc tartc/tartc.cpp)
add_dependencies(tartc collector compiler)
target_link_libraries(tartc
    compiler collector
    ${LLVM_BIT_READER}
    ${LLVM_BIT_WRITER}
    ${LLVM_SELECTION_DAG}
    ${LLVM_ANALYSIS} 
    ${LLVM_TARGET}
    ${LLVM_SUPPORT}
    ${LLVM_SYSTEM}
    ${LLVM_CORE})

add_executable(tartln tartln/tartln.cpp)
target_link_libraries(tartln
    ${LLVM_ARM_ASM_PRINTER_O}
    ${LLVM_ARM_CODE_GEN_O}
    ${LLVM_ALPHA_ASM_PRINTER_O}
    ${LLVM_ALPHA_CODE_GEN_O}
    ${LLVM_C_BACKEND_O}
    ${LLVM_CELL_SPU_ASM_PRINTER_O}
    ${LLVM_CELL_SPU_CODE_GEN_O}
    ${LLVM_CPP_BACKEND_O}
    ${LLVM_IA64_ASM_PRINTER_O}
    ${LLVM_IA64_CODE_GEN_O}
    ${LLVM_MSIL_O}
    ${LLVM_MIPS_ASM_PRINTER_O}
    ${LLVM_MIPS_CODE_GEN_O}
    ${LLVM_PIC16_O}
    ${LLVM_POWER_PC_ASM_PRINTER_O}
    ${LLVM_POWER_PC_CODE_GEN_O}
    ${LLVM_SPARC_CODE_GEN_O}
    ${LLVM_SPARC_ASM_PRINTER_O}
    ${LLVM_X86_ASM_PRINTER_O}
    ${LLVM_X86_CODE_GEN_O}
    ${LLVM_IPO} 
    ${LLVM_IPA} 
    ${LLVM_SCALAR_OPTS} 
    ${LLVM_ANALYSIS} 
    ${LLVM_TRANSFORM_UTILS} 
    ${LLVM_CODE_GEN} 
    ${LLVM_BIT_READER}
    ${LLVM_BIT_WRITER}
    ${LLVM_ASM_PRINTER} 
    ${LLVM_ASM_PARSER} 
    ${LLVM_LINKER} 
    ${LLVM_SELECTION_DAG}
    ${LLVM_TARGET} 
    ${LLVM_CORE}
    ${LLVM_SYSTEM}
    ${LLVM_SUPPORT})
